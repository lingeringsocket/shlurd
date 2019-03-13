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
import scala.util._

case class SprParseComplexityException()
    extends RuntimeException("Expression too complex")
{
}

object SprWordnetParser extends SprEnglishWordAnalyzer
{
  private val leafSomething = makeLeaf("something")
  private[parser] val npSomething = SptNP(SptNN(leafSomething))

  def maybeLowerCase(word : String) : String =
  {
    if (word == "I") {
      word
    } else {
      word.toLowerCase
    }
  }

  private lazy val phrasePatternTrie = (new SprPhrasePatternTrie).importText(
    ResourceUtils.getResourceSource(
      "/english/phrase-structure.txt"))
}

class SprWordnetParser(
  context : SprContext,
  words : Seq[String],
  guessedQuestion : Boolean,
  terminator : Option[String] = None)
    extends SprEnglishWordAnalyzer
{
  import SprWordnetParser._

  private val strictRewriter = new SprPhraseRewriter(
    new SprEnglishSyntaxAnalyzer(
      guessedQuestion, SPR_STRICTNESS_FULL, false))

  private val looseRewriter = new SprPhraseRewriter(
    new SprEnglishSyntaxAnalyzer(
      guessedQuestion, SPR_STRICTNESS_LOOSE, false))

  private val mediumRewriter = new SprPhraseRewriter(
    new SprEnglishSyntaxAnalyzer(
      guessedQuestion, SPR_STRICTNESS_MEDIUM, false))

  private var cost = 0

  private val tokens = words.map(maybeLowerCase)

  private val screen =
    new mutable.HashMap[SprSyntaxTree, Option[(SilPhrase, SprSyntaxTree)]]

  private val memo =
    new mutable.HashSet[Seq[Set[SprSyntaxTree]]]

  private val trie = phrasePatternTrie

  private val expansions = trie.generateExpansions

  private val successors = trie.generateSuccessors

  private val graph = SprPhraseGraph()

  def getCost = cost

  def buildAll(seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    def explode(tree : SprSyntaxTree) : Set[SprSyntaxTree] =
    {
      Set(tree) ++ tree.children.flatMap(explode)
    }

    seq.foreach(set => {
      val collapsedSet = updateGraph(set)
      assert(set == collapsedSet)
    })
    val stream = buildAllImpl(seq).force

    if (false) {
      import scala.sys.process._
      val dot = SprPhraseGraph.render(graph, stream.toSet.flatMap(explode))
      val dotStream = new java.io.ByteArrayInputStream(dot.getBytes)
        ("dotty -" #< dotStream).!!
    }

    stream
  }

  private def buildAllImpl(seq : Seq[Set[SprSyntaxTree]])
      : Stream[SprSyntaxTree] =
  {
    val strictResult = buildNew(strictRewriter, seq).force
    if (allResultsUnknown(strictRewriter, strictResult)) {
      // FIXME this is a fallback to handle cases like
      // "There is a multimedia server", where strict mode rejects
      // adjectival nouns such as multimedia; need a less hacky solution.
      clear
      val mediumResult = buildNew(mediumRewriter, seq).force
      if (allResultsUnknown(mediumRewriter, mediumResult)) {
        clear
        // in case buildNew collapsed phrases too aggressively
        val strictUncollapsed = buildViaTrie(strictRewriter, seq)
        if (allResultsUnknown(strictRewriter, strictUncollapsed)) {
          clear
          val looseUncollapsed = buildViaTrie(looseRewriter, seq)
          looseUncollapsed
        } else {
          strictUncollapsed
        }
      } else {
        mediumResult
      }
    } else {
      strictResult
    }
  }

  private def clear()
  {
    screen.clear
    memo.clear
    cost = 0
  }

  private var filterTime = 0L

  private def filterSeq(seq : Seq[Set[SprSyntaxTree]])
      : Seq[Set[SprSyntaxTree]] =
  {
    val startTime = System.nanoTime
    def filterLast(set : Set[SprSyntaxTree]) = {
      set.filter(tree => {
        successors.get(tree.label) match {
          case Some(successorSet) => {
            successorSet.isEmpty
          }
          case _ => true
        }
      })
    }
    val filtered = {
      if (seq.size == 1) {
        Seq(filterLast(seq.head))
      } else {
        seq.sliding(2).map(pair => {
          val first = pair.head
          if (pair.size == 1) {
            first.filter(tree => {
              successors.get(tree.label) match {
                case Some(successorSet) => {
                  successorSet.isEmpty
                }
                case _ => true
              }
            })
          } else {
            val second = pair.last.map(_.label)
            val secondExpanded = second.flatMap(s =>
              expansions.get(s).getOrElse(Set.empty) ++ Set(s))
            first.filter(tree => {
              successors.get(tree.label) match {
                case Some(successorSet) => {
                  if (successorSet.isEmpty) {
                    true
                  } else {
                    successorSet.exists(successor => {
                      secondExpanded.contains(successor)
                    })
                  }
                }
                case _ => true
              }
            })
          }
        }).toSeq :+ filterLast(seq.last)
      }
    }
    assert(filtered.size == seq.size)
    filterTime += (System.nanoTime - startTime)
    filtered
  }

  private def buildNew(
    rewriter : SprPhraseRewriter,
    seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    val rangeMaps = range(0 to seq.size - 1).flatMap(start => {
      trie.matchPatterns(seq, start).flatMap({
        case (length, replacementSet) => {
          val filteredSet = replacementSet.filter(
            tree => {
              (tree.isNounPhrase || tree.isAdjectivePhrase) &&
              acceptReplacement(rewriter, tree, false)
            }
          )
          if (filteredSet.isEmpty) {
            None
          } else {
            Some(tupleN((Range(start, start + length), filteredSet)))
          }
        }
      })
    })
    val newSeq = new mutable.ArrayBuffer[Set[SprSyntaxTree]]
    newSeq ++= seq
    val overlaps = new Array[Int](seq.size)
    val sortedRanges = rangeMaps.filter(_._1.size > 1).sortBy(_._1.size).reverse
    sortedRanges.foreach {
      case (range, replacements) => {
        if (!range.exists(index => (overlaps(index) > 0))) {
          range.foreach(index => {
            overlaps(index) += 1
            newSeq(index) = Set.empty
          })
          newSeq(range.start) = updateGraph(replacements)
        }
      }
    }
    val collapsedSeq = newSeq.filter(_.nonEmpty)
    clear
    buildViaTrie(rewriter, collapsedSeq)
  }

  private def buildViaTrie(
    rewriter : SprPhraseRewriter,
    seqIn : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    if (memo.contains(seqIn)) {
      Stream.empty
    } else {
      memo += seqIn
      // FIXME rehabilitate filter?
      // val seq = filterSeq(seqIn)
      val seq = seqIn
      if (seq.exists(_.isEmpty)) {
        Stream.empty
      } else {
        range(0 to seq.size - 1).toStream.flatMap(start => {
          trie.matchPatterns(seq, start).toStream.flatMap({
            case (length, replacementSet) => {
              val filteredSet = replacementSet.filter(
                tree => acceptReplacement(rewriter, tree))
              if (filteredSet.isEmpty) {
                Stream.empty
              } else {
                val collapsedSet = updateGraph(filteredSet)
                val patched = seq.patch(start, Seq(collapsedSet), length)
                val more = buildViaTrie(rewriter, patched)
                val current = {
                  if (patched.size == 1) {
                    patched.head.toStream.filter(
                      tree => acceptTopLevel(rewriter, tree)
                    )
                  } else {
                    Stream.empty
                  }
                }
                current ++ more
              }
            }
          })
        })
      }
    }
  }

  private def updateGraph(replacements : Set[SprSyntaxTree])
      : Set[SprSyntaxTree] =
  {
    replacements.foreach(updateGraphFor)
    replacements
  }

  private def updateGraphFor(phrase : SprSyntaxTree)
  {
    graph.addVertex(phrase)
    phrase.children.foreach(term => {
      graph.addVertex(term)
      graph.addEdge(phrase, term)
    })
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

  private def acceptReplacement(
    rewriter : SprPhraseRewriter,
    tree : SprSyntaxTree,
    allowConjunctive : Boolean = true) : Boolean =
  {
    attemptReplacement(rewriter, tree, allowConjunctive).nonEmpty
  }

  private def allResultsUnknown(
    rewriter : SprPhraseRewriter,
    stream : Stream[SprSyntaxTree]) : Boolean =
  {
    stream.forall(tree => {
      tryPhrase(rewriter, SilExpectedSentence(tree), true) match {
        case Some((phrase, _)) => {
          phrase.hasUnknown
        }
        case _ => true
      }
    })
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
    screen.getOrElseUpdate(tree, {
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
}
