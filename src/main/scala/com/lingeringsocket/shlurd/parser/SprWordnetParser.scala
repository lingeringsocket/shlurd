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
import SprPennTreebankLabels._
import SprEnglishLemmas._
import SprEnglishAffixes._

import net.sf.extjwnl.data._
import net.sf.extjwnl.dictionary._

import scala.collection._
import scala.collection.JavaConverters._
import scala.util._
import scala.io._

case class SprParseComplexityException()
    extends RuntimeException("Expression too complex")
{
}

object SprWordnetParser
{
  private val partsOfSpeech = POS.getAllPOS.asScala.toSet
  private val dictionary = Dictionary.getDefaultResourceInstance
  private val morphology = dictionary.getMorphologicalProcessor

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
    Source.fromFile(
      SprParser.getResourcePath("/english/phrase-structure.txt")))

  // adapted from
  // http://www.d.umn.edu/~tpederse/Group01/WordNet/wordnet-stoplist.html
  private val stopList = Set(
    "I", "an", "as", "at", "by", "he", "it", "do", "at", "off",
    "his", "me", "or", "thou", "us", "who", "must", "ca", "may", "in",
    "does", "have"
  )
  private def makeLeaf(
    label : String, token : String, lemma : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, lemma, token)
  }

  private def makeLeaf(
    label : String, token : String) : SprSyntaxLeaf =
  {
    SprSyntaxLeaf(label, token, token)
  }

  private def makeLeaf(
    token : String) : SprSyntaxLeaf =
  {
    makeLeaf(token, token, token)
  }
}

class SprWordnetParser(words : Seq[String],
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

  def getCost = cost

  def buildAll(seq : Seq[Set[SprSyntaxTree]]) : Stream[SprSyntaxTree] =
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
          newSeq(range.start) = replacements
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
                val patched = seq.patch(start, Seq(filteredSet), length)
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

  private def acceptTopLevel(
    rewriter : SprPhraseRewriter,
    tree : SprSyntaxTree) : Boolean =
  {
    if (acceptReplacement(rewriter, tree)) {
      tree match {
        case _ : SptS => true
        case _ : SptSBARQ => true
        case _ : SptSINV => true
        case _ : SptSQ => true
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
            SilWord("longer", _), DETERMINER_NONE, _
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
    tokens.zip(words).zipWithIndex.map {
      case ((token, word), iToken) => {
        val (tokenPrefix, tokenSuffix) = {
          val iHyphen = token.lastIndexOf('-')
          if (iHyphen < 0) {
            tupleN(("", token))
          } else {
            tupleN((token.take(iHyphen + 1), token.drop(iHyphen + 1)))
          }
        }
        val indexWords : Set[SprSyntaxTree] = {
          if (token.contains('_')) {
            Set(SptNN(makeLeaf(word, word, word)))
          } else if (stopList.contains(tokenSuffix) ||
            maybeDeterminerFor(token).nonEmpty)
          {
            // FIXME some determiners may have other POS roles, e.g.
            // "no" can be a noun or interjection
            Set.empty
          } else if (((token != word) && (iToken > 0)) ||
            (isProper(token) && (iToken == 0)))
          {
            Set(SptNNP(makeLeaf(word, word, word)))
          } else {
            val pairs = partsOfSpeech.flatMap(pos => {
              morphology.lookupAllBaseForms(pos, tokenSuffix).asScala.map(
                lemma => tupleN((pos, lemma))).toSet
            })
            val rawWords = pairs.flatMap {
              case (pos, lemma) => {
                Option(dictionary.getIndexWord(pos, lemma))
              }
            }
            val filteredWords = {
              if (rawWords.exists(_.getLemma == LEMMA_BE)) {
                rawWords.filter(
                  indexWord => (indexWord.getLemma == LEMMA_BE) &&
                    indexWord.getPOS == POS.VERB)
              } else if (token == LEMMA_THERE) {
                rawWords.filterNot(_.getPOS == POS.NOUN)
              } else if (token == LEMMA_OR) {
                rawWords.filterNot(_.getLemma == LEMMA_OR)
              } else {
                rawWords.filterNot(raw => (raw.getLemma == tokenSuffix) &&
                  rawWords.exists(other =>
                    ((other != raw) && (other.getPOS == raw.getPOS) &&
                      (other.getLemma != tokenSuffix))))
              }
            }
            filteredWords.filterNot(SprWordnetScorer.isAcronym).flatMap(
              indexWord => makePreTerminals(
                word, token, tokenPrefix, tokenSuffix,
                indexWord, (iToken == 0), filteredWords))
          }
        }
        val combined = {
          if (isCoordinatingConjunction(token)) {
            Set(SptCC(makeLeaf(word, token)))
          } else {
            maybeDeterminerFor(token).map(
              determiner => (SptDT(makeLeaf(word, token)))).toSet
          }
        } ++ {
          if (isPronounWord(token)) {
            if (isPossessiveAdjective(token)) {
              Set(SptPRP_POS(makeLeaf(word, token)))
            } else {
              Set(SptPRP(makeLeaf(word, token)))
            }
          } else {
            Set.empty
          }
        } ++ {
          def leaf = makeLeaf(word, token)
          token match {
            case (
              LEMMA_MUST | LEMMA_MAY |
                LEMMA_COULD | LEMMA_SHOULD | LEMMA_CAN
            )=> {
              Set(SptMD(leaf))
            }
            case LEMMA_THERE => {
              Set(SptNP(SptEX(leaf)))
            }
            case LEMMA_THAT => {
              Set(SptIN(leaf),
                SptWDT(leaf))
            }
            case LEMMA_WHO | LEMMA_WHOM => Set(SptWP(leaf))
            case LEMMA_HOW | LEMMA_WHERE => {
              Set(SptWRB(leaf))
            }
            case LEMMA_WHAT | LEMMA_WHICH => {
              Set(SptWP(leaf),
                SptWDT(leaf))
            }
            case LEMMA_EQUIVALENTLY => {
              Set(SptRB(leaf))
            }
            case LEMMA_DO => {
              Set(SptVBP(leaf))
            }
            case "does" => {
              Set(SptVBZ(makeLeaf(word, token, LEMMA_DO)))
            }
            case LEMMA_HAVE => {
              Set(SptVBP(leaf))
            }
            case LEMMA_NO => {
              Set(SptRB(leaf))
            }
            case "an" => {
              Set(SptDT(makeLeaf(word, token, LEMMA_A)))
            }
            case "off" => {
              Set(SptJJ(leaf), SptRB(leaf))
            }
            case _ => {
              Set.empty
            }
          }
        } ++ {
          if ((isAdposition(token) ||
            isSubordinatingConjunction(token)) &&
            (token != LEMMA_TO))
          {
            Set(SptIN(makeLeaf(word, token)))
          } else {
            Set.empty
          }
        } ++ {
          indexWords
        }
        if (combined.nonEmpty) {
          combined
        } else {
          def leaf = makeLeaf(word, token)
          val set : Set[SprSyntaxTree] = token match {
            case LABEL_COMMA => Set(SptCOMMA(leaf))
            case "'" | "'s" => Set(SptPOS(leaf))
            // FIXME proper handling for all contractions
            case "ca" => Set(SptMD(makeLeaf(token, token, LEMMA_CAN)))
            case "n't" => Set(SptRB(makeLeaf(token, token, LEMMA_NOT)))
            case LEMMA_TO => Set(SptTO(leaf))
            case _ => {
              if (SprParser.isTerminator(token)) {
                Set(SptDOT(leaf))
              } else {
                val noun = {
                  if (iToken == 0) {
                    SptNNP(makeLeaf(word, word, word))
                  } else {
                    SptNN(leaf)
                  }
                }
                Set(noun)
              }
            }
          }
          set
        }
      }
    }
  }

  private def makePreTerminals(
    word : String, token : String,
    tokenPrefix : String, tokenSuffix : String,
    indexWord : IndexWord,
    forceProper : Boolean, alternatives : Set[IndexWord])
      : Set[SprSyntaxTree] =
  {
    val lemma = indexWord.getLemma
    val label = indexWord.getPOS match {
      case POS.ADJECTIVE => LABEL_JJ
      case POS.ADVERB => LABEL_RB
      case POS.NOUN => {
        if ((tokenSuffix != lemma) || SprWordnetScorer.isPlural(indexWord)) {
          LABEL_NNS
        } else {
          if (forceProper) {
            LABEL_NNP
          } else {
            LABEL_NN
          }
        }
      }
      case POS.VERB => {
        if (tokenSuffix != lemma) {
          if (tokenSuffix.endsWith(SUFFIX_ING)) {
            LABEL_VBG
          } else {
            // FIXME this is lame
            if (lemma == LEMMA_BE) {
              token match {
                case "was" | "were" => LABEL_VBD
                case "is" => LABEL_VBZ
                case _ => LABEL_VBP
              }
            } else if (token.endsWith("d") ||
              (tokenSuffix.take(2) != lemma.take(2)))
            {
              LABEL_VBD
            } else {
              LABEL_VBZ
            }
          }
        } else {
          LABEL_VBP
        }
      }
    }
    val labels = Set(label) ++ {
      if ((label == LABEL_VBD) && (lemma != LEMMA_BE) && (lemma != LEMMA_DO)) {
        Set(LABEL_VBN)
      } else {
        Set.empty
      }
     }
    labels.map(label => {
      // try to match the way CoreNLP lemmatizes gerunds and participles
      val conformedLemma = {
        label match {
          case LABEL_VBN => {
            if (SprWordnetScorer.isPotentialNoun(tokenSuffix)) {
              tokenSuffix
            } else {
              lemma
            }
          }
          case LABEL_JJ => {
            if (SprWordnetScorer.isPotentialNoun(tokenSuffix)) {
              lemma
            } else {
              alternatives.find(v => (v.getPOS == POS.VERB)).
                map(_.getLemma).getOrElse(lemma)
            }
          }
          case _ => lemma
        }
      }
      val leaf = {
        if (label == LABEL_NNP) {
          makeLeaf(word, word, word)
        } else {
          makeLeaf(word, token, tokenPrefix + conformedLemma)
        }
      }
      SprSyntaxRewriter.recompose(label, Seq(leaf))
    })
  }
}
