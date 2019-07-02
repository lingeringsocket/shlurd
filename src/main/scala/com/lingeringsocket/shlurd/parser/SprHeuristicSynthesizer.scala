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
import com.lingeringsocket.shlurd.jgrapht._

import SprEnglishLemmas._
import SprPennTreebankLabels._

import scala.collection._
import scala.util._
import scala.sys.process._

import org.jgrapht._
import org.jgrapht.graph._

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

  val specialCasing = Set("I", LABEL_LPAREN, LABEL_RPAREN)

  lazy val phrasePatternMatcher = loadMatcher

  private def loadMatcher() =
  {
    val matcher = new SprPhrasePatternMatcher
    val source = ResourceUtils.getResourceSource(
      "/english/phrase-structure.txt")
    SprGrammar.buildMatcher(source, matcher)
    matcher
  }

  def maybeLowerCase(word : String) : String =
  {
    if (specialCasing.contains(word)) {
      word
    } else {
      word.toLowerCase
    }
  }
}

trait SprHeuristicFilter
{
  def accept(
    tree : SprSyntaxTree,
    replacement : SilPhrase) : Boolean
}

object SprHeuristicAcceptAll extends SprHeuristicFilter
{
  override def accept(
    tree : SprSyntaxTree,
    replacement : SilPhrase) : Boolean =
  {
    true
  }
}

object SprHeuristicAcceptCompleteSentence extends SprHeuristicFilter
{
  override def accept(
    tree : SprSyntaxTree, replacement : SilPhrase) : Boolean =
  {
    if (replacement.hasUnknown) {
      false
    } else {
      tree match {
        case _ : SptS => true
        case _ : SptSBARQ => true
        case _ : SptSINV => true
        case _ : SptSQ => {
          val querier = new SilPhraseRewriter
          var accepted = true
          def findDangling = querier.queryMatcher {
            case _ : SilDanglingVerbModifier => {
              accepted = false
            }
          }
          querier.query(findDangling, replacement)
          accepted
        }
        case _ => false
      }
    }
  }
}

sealed trait SprHeuristicStamina
case object HEURISTIC_STAMINA_COMPLETE
    extends SprHeuristicStamina
case object HEURISTIC_STAMINA_STOP_AFTER_FIRST
    extends SprHeuristicStamina

class SprHeuristicSynthesizer(
  context : SprContext,
  filter : SprHeuristicFilter,
  stamina : SprHeuristicStamina,
  words : Seq[String])
    extends SprEnglishWordAnalyzer
{
  import SprHeuristicSynthesizer._

  private val queue = new mutable.PriorityQueue[ScoredEntry]

  private val pending = new mutable.Queue[SprSyntaxTree]

  private val rewriterIntermediate = new SprPhraseRewriter(
    context,
    new SprEnglishSyntaxAnalyzer(
      false, SPR_STRICTNESS_TIGHT, false))

  private val rewriterFinal = new SprPhraseRewriter(
    context,
    new SprEnglishSyntaxAnalyzer(
      false, SPR_STRICTNESS_TIGHT))

  private val spanGraph = new SimpleDirectedGraph[Int, SpanEdge](
    classOf[SpanEdge])

  private val produced = new mutable.HashSet[SprSyntaxTree]

  private val tokens = words.map(maybeLowerCase)

  private val patternMatcher = phrasePatternMatcher

  private val maxPatternLength = patternMatcher.getMaxPatternLength

  private val phraseGraph = SprPhraseGraph()

  private var cost = 0

  private val silMemo =
    new mutable.HashMap[SprSyntaxTree, Option[(SilPhrase, SprSyntaxTree)]]

  def synthesize(seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    seq.foreach(set => {
      updatePhraseGraph(set)
      // FIXME should do this recursively
      set.foreach(p => updatePhraseGraph(p.children))
    })

    seq.zipWithIndex.foreach({
      case (set, index) => {
        val rangeStart = index
        val rangeEnd = index + 1
        seedChoice(rangeStart, rangeEnd, set)
      }
    })

    range(2 until seq.size).foreach(length => {
      seq.indices.sliding(length).filter(_.size == length).foreach(indices => {
        val rangeStart = indices.head
        val rangeEnd = indices.last + 1
        val subSeq = seq.slice(rangeStart, rangeEnd)
        val set = detectCompoundNoun(subSeq) ++
          detectCompoundAdverb(subSeq) ++ detectCompoundVerb(subSeq)
        if (set.nonEmpty) {
          updatePhraseGraph(set)
          seedChoice(rangeStart, rangeEnd, set)
        }
      })
    })

    produceMore
  }

  private def detectCompoundNoun(seq : Seq[Set[SprSyntaxTree]])
      : Set[SprSyntaxTree] =
  {
    val components = seq.map(
      alternatives => {
        alternatives.find(_.isNoun).getOrElse(alternatives.head)
      }
    )
    if (context.wordLabeler.isCompoundNoun(components)) {
      Set(SptNNC(components:_*))
    } else {
      Set.empty
    }
  }

  private def detectCompoundAdverb(seq : Seq[Set[SprSyntaxTree]])
      : Set[SprSyntaxTree] =
  {
    val components = seq.map(
      alternatives => {
        alternatives.find(_.isAdverb).getOrElse(alternatives.head)
      }
    )
    if (context.wordLabeler.isCompoundAdverb(components)) {
      Set(SptRBC(components:_*))
    } else {
      Set.empty
    }
  }

  private def detectCompoundVerb(seq : Seq[Set[SprSyntaxTree]])
      : Set[SprSyntaxTree] =
  {
    val components = seq.map(
      alternatives => {
        alternatives.find(_.isVerb).getOrElse(alternatives.head)
      }
    )
    if (context.wordLabeler.isCompoundVerb(components)) {
      Set(SptVBC(components:_*))
    } else {
      Set.empty
    }
  }

  private def seedChoice(
    rangeStart : Int, rangeEnd : Int, set : Set[SprSyntaxTree])
  {
    val span = range(rangeStart until rangeEnd)
    val choice = SpanChoice(set, span)
    enqueue(PartialEntry(choice, SilPhraseScore.neutral))
    spanGraph.addVertex(rangeStart)
    spanGraph.addVertex(rangeEnd)
    val edge = spanGraph.addEdge(rangeStart, rangeEnd)
    edge.set ++= set
  }

  def getCost = cost

  private def stopEarly() : Boolean =
  {
    stamina match {
      case HEURISTIC_STAMINA_COMPLETE => false
      case HEURISTIC_STAMINA_STOP_AFTER_FIRST => {
        produced.nonEmpty
      }
    }
  }

  private def scoreTree(tree : SprSyntaxTree) : SilPhraseScore =
  {
    context.scorer.computeGlobalScore(silMemo(tree).get._1)
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
    while (pending.isEmpty && queue.nonEmpty && !stopEarly) {
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
    silMemo.remove(tree)
    val attempted = attemptReplacement(rewriterFinal, tree)
    attempted match {
      case Some((replacement, _)) => filter.accept(tree, replacement)
      case _ => false
    }
  }

  private def process(entry : ScoredEntry)
  {
    entry match {
      case pe : PartialEntry => {
        processPartialEntry(pe)
      }
      case CompleteEntry(tree, score) => {
        if (!produced.contains(tree)) {
          pending += tree
          produced += tree
        }
      }
    }
  }

  private def edgeToChoice(edge : SpanEdge) : SpanChoice =
  {
    val edgeStart = spanGraph.getEdgeSource(edge)
    val edgeEnd = spanGraph.getEdgeTarget(edge)
    SpanChoice(
      edge.set,
      range(edgeStart until edgeEnd))
  }

  private def pathStream(span : Range) : Stream[Stream[SpanChoice]] =
  {
    if (span.isEmpty) {
      Stream(Stream.empty)
    } else {
      new SpanPathStreamer(span).pathStream.map(_.map(edgeToChoice))
    }
  }

  class SpanPathStreamer(span : Range)
      extends GraphPathStreamer[Int, SpanEdge](spanGraph, span.start)
  {
    private val terminator = span.end + 1

    override protected def isExcluded(v : Int) : Boolean =
    {
      v >= terminator
    }
  }

  private def processPartialEntry(entry : PartialEntry)
  {
    val leftSpan = range(0 until entry.choice.span.start)
    val rightSpan = range(entry.choice.span.end until words.size)
    val leftSeqs = pathStream(leftSpan)

    val seen = new mutable.HashSet[SpanChoice]
    val deltaGraph = DeltaGraph(spanGraph)

    leftSeqs.foreach(leftSeq => {
      val leftPlus = leftSeq :+ entry.choice
      val rightSeqs = pathStream(rightSpan)
      val skips = new mutable.BitSet
      var lowerBound = 0
      rightSeqs.foreach(rightSeq => {
        val concatenated = leftPlus #::: rightSeq
        val newLowerBound = processPath(
          entry, concatenated, seen, lowerBound, deltaGraph, skips)
        if (lowerBound > 0) {
          assert(newLowerBound == lowerBound)
        }
        lowerBound = newLowerBound
      })
    })

    deltaGraph.applyModifications
  }

  private def processPath(
    entry : PartialEntry,
    stream : Stream[SpanChoice],
    seen : mutable.HashSet[SpanChoice],
    oldLowerBound : Int,
    deltaGraph : Graph[Int, SpanEdge],
    skips : mutable.BitSet) : Int =
  {
    val setStream = stream.map(_.set)
    val activeSpan = entry.choice.span
    val iActive = stream.indexWhere(_.span.start == activeSpan.start)
    assert(iActive > -1)
    val startLowerBound = math.max(
      oldLowerBound, (iActive - (maxPatternLength - 1)))
    val startUpperBound = iActive + 1
    var newLowerBound = oldLowerBound
    var curr = setStream.drop(startLowerBound)
    range(startLowerBound until startUpperBound).foreach(start => {
      if (!skips.contains(start)) {
        val minLength = startUpperBound - start
        cost += 1
        val results = patternMatcher.matchPatterns(curr, minLength)
        results.foreach({
          case (length, replacementSet) => {
            if (replacementSet.isEmpty) {
              if ((start + length) <= iActive) {
                skips += start
                if (start == newLowerBound) {
                  newLowerBound += 1
                }
              }
            } else {
              assert(length >= minLength)
              val span = range(
                stream(start).span.start until
                  stream(start + length - 1).span.end)
              val newChoice = SpanChoice(replacementSet, span)
              if (!seen.contains(newChoice)) {
                seen += newChoice
                processReplacement(
                  entry, stream, start, length, newChoice, deltaGraph)
              }
            }
          }
        })
      }
      curr = curr.tail
    })
    newLowerBound
  }

  private def processReplacement(
    entry : PartialEntry,
    stream : Stream[SpanChoice],
    start : Int,
    length : Int,
    choice : SpanChoice,
    deltaGraph : Graph[Int, SpanEdge]
  )
  {
    val filteredSet = choice.set.filter(
      tree => acceptReplacement(tree))
    if (filteredSet.nonEmpty) {
      updatePhraseGraph(filteredSet)
      val scoredGroups = filteredSet.groupBy(scoreTree)
      scoredGroups.foreach({
        case (score, set) => {
          val newScore = score.combine(entry.score)
          val edge = Option(
            deltaGraph.getEdge(choice.span.start, choice.span.end)).
            getOrElse(deltaGraph.addEdge(choice.span.start, choice.span.end))
          val newSet = set -- edge.set
          if (newSet.nonEmpty) {
            edge.set ++= newSet
            val newEntry = PartialEntry(
              SpanChoice(newSet, choice.span), newScore)
            enqueue(newEntry)
            if ((start == 0) && (length == stream.size)) {
              newSet.foreach(
                tree => {
                  if (accept(tree)) {
                    enqueue(CompleteEntry(tree, scoreTree(tree)))
                  }
                  silMemo.remove(tree)
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

  private def updatePhraseGraph(replacements : Iterable[SprSyntaxTree])
  {
    replacements.foreach(phraseGraph.addPhrase)
  }

  private def displayDotty(dot : String)
  {
    val dotStream = new java.io.ByteArrayInputStream(dot.getBytes)
    shellCommand("xdot -" #< dotStream).!!
  }

  private[parser] def displayGraph(
    accepted : => Set[SprSyntaxTree])
  {
    val dot = phraseGraph.render(accepted)
    displayDotty(dot)
  }

  private def acceptReplacement(
    tree : SprSyntaxTree,
    allowConjunctive : Boolean = true) : Boolean =
  {
    attemptReplacement(rewriterIntermediate, tree, allowConjunctive).nonEmpty
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
            if (np.children.last.isPossessiveClitic) {
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
            SilExpectedVerbModifier(advp))
        }
        case adjp : SptADJP => {
          tryRewrite(
            SilExpectedComplementState(adjp))
        }
        case pp : SptPP => {
          tryRewrite(
            SilExpectedVerbModifier(pp))
        }
        case tmod : SptTMOD => {
          val result = tryPhrase(
            rewriter,
            SilExpectedVerbModifier(tmod),
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
        false
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
