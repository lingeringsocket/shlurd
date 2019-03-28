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

import scala.collection._

class SprBlackboardParser(
  context : SprContext,
  scorer : SilPhraseScorer,
  requireTopLevel : Boolean,
  words : Seq[String],
  guessedQuestion : Boolean,
  terminator : Option[String] = None)
    extends
    SprAbstractWordnetParser(context, words, guessedQuestion, terminator)
{
  sealed trait ScoredEntry
  {
    def getScore : SilPhraseScore
  }

  case class PartialEntry(
    seq : Seq[Set[SprSyntaxTree]],
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

  private val queue = new mutable.PriorityQueue[ScoredEntry]

  private val pending = new mutable.Queue[SprSyntaxTree]

  val rewriter = strictRewriter

  def buildAll(seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
  {
    seq.foreach(updateGraph)
    process(PartialEntry(seq, SilPhraseScore.neutral))

    // FIXME do the scoring incrementally instead of at the end
    if (false) {
      val result = produceMore.toList
      result.sortBy(scoreTree).reverse.toStream
    } else {
      produceMore
    }
  }

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
        pending += tree
      }
    }
  }

  private def processPartialEntry(entry : PartialEntry)
  {
    val seq = entry.seq
    if (!seenMemo.contains(seq)) {
      seenMemo += seq
      range(0 until seq.size).foreach(start => {
        trie.matchPatterns(seq, start).foreach({
          case (length, replacementSet) => {
            val filteredSet = replacementSet.filter(
              tree => acceptReplacement(rewriter, tree))
            if (filteredSet.nonEmpty) {
              val collapsedSet = updateGraph(filteredSet)
              val scoredGroups = collapsedSet.groupBy(scoreTree)
              scoredGroups.foreach({
                case (score, set) => {
                  val newScore = score.combine(entry.score)
                  val patched = seq.patch(start, Seq(set), length)
                  val newEntry = PartialEntry(patched, newScore)
                  enqueue(newEntry)
                  if (patched.size == 1) {
                    patched.head.foreach(
                      tree => {
                        if (accept(tree)) {
                          enqueue(CompleteEntry(tree, scoreTree(tree)))
                        }
                      }
                    )
                  }
                }
              })
            }
          }
        })
      })
    }
  }
}
