// shlurd:  a limited understanding of small worlds
// Copyright 2017-2018 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import SprEnglishLemmas._

import scala.collection._
import scala.collection.JavaConverters._
import scala.util._
import scala.sys.process._

import org.jgrapht.graph._
import org.jgrapht.alg.shortestpath._

case class SprParseComplexityException()
    extends RuntimeException("Expression too complex")
{
}

object SprHeuristicSynthesizer extends SprEnglishWordAnalyzer
{
  class SpanEdge extends DefaultEdge
  {
    val set = new mutable.LinkedHashSet[SprSyntaxTree]
  }

  case class SpanChoice(
    set : Set[SprSyntaxTree],
    span : Range
  )

  sealed trait ScoredEntry
  {
    def getScore : SilPhraseScore
  }

  case class PartialEntry(
    choice : SpanChoice,
    score : SilPhraseScore
  ) extends ScoredEntry
  {
    override def getScore = score
  }

  case class CompleteEntry(
    tree : SprSyntaxTree,
    score : SilPhraseScore
  ) extends ScoredEntry
  {
    override def getScore = score
  }

  implicit object ScoreOrdering extends Ordering[ScoredEntry]
  {
    def compare(e1: ScoredEntry, e2: ScoredEntry) =
      e1.getScore.compareTo(e2.getScore)
  }

  val leafSomething = makeLeaf("something")
  val npSomething = SptNP(SptNN(leafSomething))

  lazy val phrasePatternTrie = (new SprPhrasePatternTrie).importText(
    ResourceUtils.getResourceSource(
      "/english/phrase-structure.txt"))

  def maybeLowerCase(word : String) : String =
  {
    if (word == "I") {
      word
    } else {
      word.toLowerCase
    }
  }
}

class SprHeuristicSynthesizer(
  context : SprContext,
  scorer : SilPhraseScorer,
  requireTopLevel : Boolean,
  words : Seq[String])
    extends SprEnglishWordAnalyzer
{
  import SprHeuristicSynthesizer._

  private val queue = new mutable.PriorityQueue[ScoredEntry]

  private val pending = new mutable.Queue[SprSyntaxTree]

  private val rewriter = new SprPhraseRewriter(
    new SprEnglishSyntaxAnalyzer(
      false, SPR_STRICTNESS_TIGHT, false))

  private val spanGraph = new SimpleDirectedGraph[Int, SpanEdge](
      classOf[SpanEdge])

  private val produced = new mutable.HashSet[SprSyntaxTree]

  private val tokens = words.map(maybeLowerCase)

  private val trie = phrasePatternTrie

  private val maxPatternLength = trie.getMaxPatternLength

  private val phraseGraph = SprPhraseGraph()

  private var cost = 0

  private val silMemo =
    new mutable.HashMap[SprSyntaxTree, Option[(SilPhrase, SprSyntaxTree)]]

  def synthesize(seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    seq.foreach(updatePhraseGraph)

    seq.zipWithIndex.foreach({
      case (set, index) => {
        val rangeStart = index
        val rangeEnd = index + 1
        val span = range(rangeStart until rangeEnd)
        val choice = SpanChoice(set, span)
        enqueue(PartialEntry(choice, SilPhraseScore.neutral))
        spanGraph.addVertex(rangeStart)
        spanGraph.addVertex(rangeEnd)
        val edge = spanGraph.addEdge(rangeStart, rangeEnd)
        edge.set ++= set
      }
    })

    produceMore
  }

  def getCost = cost

  private def scoreTree(tree : SprSyntaxTree) : SilPhraseScore =
  {
    scorer.computeGlobalScore(silMemo(tree).get._1)
  }

  private def produceMore() : Stream[SprSyntaxTree] =
  {
    pump
    if (pending.isEmpty) {
      Stream.empty
    } else {
      Stream.cons(pending.dequeue, produceMore)
    }
  }

  private def pump()
  {
    while (pending.isEmpty && queue.nonEmpty) {
      val entry = queue.dequeue
      process(entry)
    }
  }

  private def enqueue(entry : ScoredEntry)
  {
    queue += entry
  }

  private def accept(tree : SprSyntaxTree) : Boolean =
  {
    if (requireTopLevel) {
      acceptTopLevel(rewriter, tree)
    } else {
      acceptReplacement(rewriter, tree)
    }
  }

  private def process(entry : ScoredEntry)
  {
    entry match {
      case pe : PartialEntry => {
        processPartialEntry(pe)
      }
      case CompleteEntry(tree, _) => {
        if (!produced.contains(tree)) {
          pending += tree
          produced += tree
        }
      }
    }
  }

  private def enumeratePaths(span : Range) : Set[Seq[SpanChoice]] =
  {
    if (span.isEmpty) {
      Set(Seq.empty)
    } else {
      val alg = new AllDirectedPaths(spanGraph)
      alg.getAllPaths(span.start, span.end, true, null).asScala.map(
        path => {
          path.getEdgeList.asScala.map(edge => {
            SpanChoice(
              edge.set,
              range(spanGraph.getEdgeSource(edge) until
                spanGraph.getEdgeTarget(edge)))
          })
        }
      ).toSet
    }
  }

  private def processPartialEntry(entry : PartialEntry)
  {
    val leftSpan = range(0 until entry.choice.span.start)
    val rightSpan = range(entry.choice.span.end until words.size)
    val leftSeqs = enumeratePaths(leftSpan)
    val rightSeqs = enumeratePaths(rightSpan)

    val seen = new mutable.HashSet[SpanChoice]

    leftSeqs.foreach(leftSeq => {
      rightSeqs.foreach(rightSeq => {
        val seq = leftSeq ++ Seq(entry.choice) ++ rightSeq
        processPath(entry, seq, seen)
      })
    })
  }

  private def processPath(
    entry : PartialEntry,
    seq : Seq[SpanChoice],
    seen : mutable.HashSet[SpanChoice])
  {
    val setSeq = seq.map(_.set)
    val activeSpan = entry.choice.span
    val startUpperBound = seq.takeWhile(_.span.start <= activeSpan.start).size
    val startLowerBound =
      seq.size -
        seq.dropWhile(_.span.start + maxPatternLength < activeSpan.start).size
    range(startLowerBound until startUpperBound).foreach(start => {
      val minLength = seq.view(start, seq.size).takeWhile(
        _.span.start < activeSpan.start).size
      trie.matchPatterns(setSeq, start, minLength).foreach({
        case (length, replacementSet) => {
          assert(length >= minLength)
          val span = range(
            seq(start).span.start until
              seq(start + length - 1).span.end)
          val newChoice = SpanChoice(replacementSet, span)
          if (!seen.contains(newChoice)) {
            seen += newChoice
            processReplacement(entry, seq, start, length, newChoice)
          }
        }
      })
    })
  }

  private def processReplacement(
    entry : PartialEntry,
    seq : Seq[SpanChoice],
    start : Int,
    length : Int,
    choice : SpanChoice
  )
  {
    val filteredSet = choice.set.filter(
      tree => acceptReplacement(rewriter, tree))
    if (filteredSet.nonEmpty) {
      updatePhraseGraph(filteredSet)
      val scoredGroups = filteredSet.groupBy(scoreTree)
      scoredGroups.foreach({
        case (score, set) => {
          val newScore = score.combine(entry.score)
          val edge = Option(
            spanGraph.getEdge(choice.span.start, choice.span.end)).
            getOrElse(spanGraph.addEdge(choice.span.start, choice.span.end))
          val newSet = set -- edge.set
          if (newSet.nonEmpty) {
            edge.set ++= newSet
            val newEntry = PartialEntry(
              SpanChoice(newSet, choice.span), newScore)
            enqueue(newEntry)
            if ((start == 0) && (length == seq.size)) {
              newSet.foreach(
                tree => {
                  if (accept(tree)) {
                    enqueue(CompleteEntry(tree, scoreTree(tree)))
                  }
                }
              )
            }
          }
        }
      })
    }
  }

  private[parser] def analyzeWords() : Seq[Set[SprSyntaxTree]] =
  {
    val quote = DQUOTE
    tokens.zip(words).zipWithIndex.map {
      case ((token, word), iToken) => {
        if (word.startsWith(quote) && word.endsWith(quote)
          && (word.size > 1))
        {
          val tree : SprSyntaxTree = SptNNQ(makeLeaf(
            word.stripPrefix(quote).stripSuffix(quote)))
          Set(tree)
        } else {
          context.wordLabeler.labelWord(token, word, iToken)
        }
      }
    }
  }

  private def updatePhraseGraph(replacements : Set[SprSyntaxTree])
  {
    replacements.foreach(updatePhraseGraphFor)
  }

  private def updatePhraseGraphFor(phrase : SprSyntaxTree)
  {
    phraseGraph.addVertex(phrase)
    phrase.children.foreach(term => {
      phraseGraph.addVertex(term)
      phraseGraph.addEdge(phrase, term)
    })
  }

  private def displayDotty(dot : String)
  {
    val dotStream = new java.io.ByteArrayInputStream(dot.getBytes)
      ("dotty -" #< dotStream).!!
  }

  private[parser] def displayGraph(
    accepted : => Set[SprPhraseGraph.SprPhraseVertex])
  {
    val dot = SprPhraseGraph.render(phraseGraph, accepted)
    displayDotty(dot)
  }

  private def acceptReplacement(
    rewriter : SprPhraseRewriter,
    tree : SprSyntaxTree,
    allowConjunctive : Boolean = true) : Boolean =
  {
    attemptReplacement(rewriter, tree, allowConjunctive).nonEmpty
  }

  private def acceptTopLevel(
    rewriter : SprPhraseRewriter,
    tree : SprSyntaxTree) : Boolean =
  {
    val replacement = attemptReplacement(rewriter, tree)
    if (replacement.nonEmpty) {
      tree match {
        case _ : SptS => true
        case _ : SptSBARQ => true
        case _ : SptSINV => true
        case _ : SptSQ => {
          val sil = replacement.get._1
          val querier = new SilPhraseRewriter
          var accepted = true
          def findDangling = querier.queryMatcher {
            case _ : SilDanglingVerbModifier => {
              accepted = false
            }
          }
          querier.query(findDangling, sil)
          accepted
        }
        case _ => false
      }
    } else {
      false
    }
  }

  private def attemptReplacement(
    rewriter : SprPhraseRewriter,
    tree : SprSyntaxTree,
    allowConjunctive : Boolean = true) : Option[(SilPhrase, SprSyntaxTree)] =
  {
    cost += 1
    if (cost > 1000000) {
      throw SprParseComplexityException()
    }
    def tryRewrite(phrase : SilUnknownPhrase) =
    {
      tryPhrase(rewriter, phrase, allowConjunctive)
    }
    silMemo.getOrElseUpdate(tree, {
      tree match {
        case SptS(vp : SptVP) => {
          tryRewrite(
            SilExpectedSentence(
              SptS(npSomething, vp)))
        }
        case s : SptS => {
          tryRewrite(
            SilExpectedSentence(s))
        }
        case sbarq : SptSBARQ => {
          tryRewrite(
            SilExpectedSentence(sbarq))
        }
        case sinv : SptSINV => {
          tryRewrite(
            SilExpectedSentence(sinv))
        }
        case sq @ SptSQ(vp : SptVP) => {
          tryRewrite(
            SilExpectedSentence(
              SptS(npSomething, vp)))
        }
        case sq : SptSQ => {
          tryRewrite(
            SilExpectedSentence(sq))
        }
        // FIXME required because we currently only accept "does" as auxiliary
        case SptVP(SptVBZ(leaf)) if (leaf.lemma == LEMMA_DO) => {
          tryRewrite(
            SilExpectedSentence(
              SptS(npSomething, SptVP(SptVB(makeLeaf(LEMMA_HAVE))))))
        }
        case vp : SptVP => {
          tryRewrite(
            SilExpectedSentence(
              SptS(npSomething, vp)))
        }
        case np : SptNP => {
          val dispossessed = {
            if (np.children.last.isPossessive) {
              SptNP(np.children.dropRight(1):_*)
            } else {
              np
            }
          }
          tryRewrite(
            SilExpectedReference(dispossessed))
        }
        case advp : SptADVP => {
          tryRewrite(
            SilExpectedVerbModifier(advp, None))
        }
        case adjp : SptADJP => {
          tryRewrite(
            SilExpectedComplementState(adjp))
        }
        case pp : SptPP => {
          tryRewrite(
            SilExpectedVerbModifier(pp, None))
        }
        case tmod : SptTMOD => {
          val result = tryPhrase(
            rewriter, SilExpectedVerbModifier(tmod, None),
            allowConjunctive)
          result.map(_._1) match {
            case Some(SilAdpositionalVerbModifier(
              SilAdposition.ADVERBIAL_TMP, ref)
            ) => {
              // FIXME discriminate excns
              if (Try(SprParser.interpretTemporal(ref)).isSuccess) {
                result
              } else {
                None
              }
            }
            case _ => None
          }
        }
        case _ => {
          Some(tupleN((SilUnrecognizedReference(tree), tree)))
        }
      }
    })
  }

  private def tryPhrase(
    rewriter : SprPhraseRewriter,
    phrase : SilUnknownPhrase,
    allowConjunctive : Boolean) : Option[(SilPhrase, SprSyntaxTree)] =
  {
    def rejectResult(sil : SilPhrase) : Boolean =
    {
      if (sil.hasUnknown) {
        true
      } else if (sil.isConjunctive && !allowConjunctive) {
        true
      } else {
        sil match {
          case SilNounReference(
            SilWordInflected("longer"), DETERMINER_NONE, _
          ) => true
          case _ => false
        }
      }
    }

    val syntaxTree = phrase.syntaxTree
    if ((syntaxTree.children.size == 1) &&
      syntaxTree.label == syntaxTree.firstChild.label)
    {
      None
    } else {
      val transformed = rewriter.rewritePhrase(phrase)
      if (rejectResult(transformed)) {
        None
      } else {
        Some(tupleN((transformed, syntaxTree)))
      }
    }
  }
}
