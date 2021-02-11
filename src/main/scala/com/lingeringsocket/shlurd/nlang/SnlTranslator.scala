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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import scala.collection._
import scala.jdk.CollectionConverters._

import org.slf4j._

import net.sf.extjwnl.data._

import SprPennTreebankLabels._

sealed trait SnlTranslationDirection
{
  def reverse : SnlTranslationDirection
}
case object TRANSLATE_FIRST_TO_SECOND extends SnlTranslationDirection
{
  override def reverse = TRANSLATE_SECOND_TO_FIRST
}
case object TRANSLATE_SECOND_TO_FIRST extends SnlTranslationDirection
{
  override def reverse = TRANSLATE_FIRST_TO_SECOND
}

object SnlTranslator
{
  private val logger = LoggerFactory.getLogger(classOf[SnlTranslator])

  def neutralizePronouns(
    sentence : SilSentence
  ) : SilSentence =
  {
    val querier = new SilPhraseQuerier
    def neutralize = querier.queryMatcher {
      case pr : SilPronounReference => {
        pr.clearWord()
        pr.clearPronounMap()
      }
    }
    querier.query(neutralize, sentence)
    sentence
  }
}

class SnlTranslator(
  val annotator : SilAnnotator,
  alignment : SnlWordnetAlignment,
  direction : SnlTranslationDirection,
  scorerOpt : Option[SilPhraseScorer] = None
)
{
  import SnlTranslator._
  import SilPhraseRewriter._

  val (sourceTongue, targetTongue) = direction match {
    case TRANSLATE_FIRST_TO_SECOND => tupleN(
      alignment.getFirstTongue,
      alignment.getSecondTongue
    )
    case TRANSLATE_SECOND_TO_FIRST => tupleN(
      alignment.getSecondTongue,
      alignment.getFirstTongue
    )
  }

  val sourceWordnet = sourceTongue.getWordnet

  val targetWordnet = targetTongue.getWordnet

  def translate(
    input : SilSentence) : SilSentence =
  {
    val debugger = new SutDebugger(logger)
    debugger.debug(s"TRANSLATION INPUT = " + input)
    debugger.debug(s"TRANSLATING FROM = " + sourceTongue.getIdentifier)
    debugger.debug(s"TRANSLATING TO = " + targetTongue.getIdentifier)
    val intermediate = input match {
      case _ : SilUnparsedSentence => input
      case _ => translateImpl(input)
    }
    // FIXME use pattern matching to do this all over the tree, not
    // just at the top; also maybe for non-actions as well?
    val output = intermediate match {
      case ps @ SilPredicateSentence(
        ap : SilActionPredicate,
        _,
        _
      ) => {
        scorerOpt match {
          case Some(scorer) => {
            ps.copy(predicate = reorderActionSenses(ps, ap, scorer))
          }
          case _ => {
            ps
          }
        }
      }
      case _ => intermediate
    }
    debugger.debug(s"TRANSLATION OUTPUT = " + output)
    output
  }

  private def chooseLemma(
    word : SilWord,
    senses : Seq[Synset],
    pos : POS) : SilWord =
  {
    // FIXME what about compound words?  Also should maybe
    // only preserve senses with the same lemma?
    val mixedLemmas = senses.map(
      _.getWords.asScala.map(_.getLemma))
    val filteredLemmas = mixedLemmas.map(seq => {
      seq.filterNot(_.head.isUpper)
    })
    val senseLemmas = {
      if (filteredLemmas.exists(_.nonEmpty)) {
        filteredLemmas
      } else {
        mixedLemmas
      }
    }
    val lemmas = senseLemmas.flatten.distinct.filterNot(_.startsWith("`"))
    // FIXME optimize computation
    val lemmaCounts = lemmas.map(lemma => {
      tupleN(lemma, senseLemmas.count(_.contains(lemma)))
    }).toMap
    val maxCount = lemmaCounts.values.max
    val maxLemmas = lemmaCounts.filter(_._2 == maxCount)
    val lemma = {
      if (maxLemmas.size == 1) {
        maxLemmas.head._1
      } else {
        lemmas.head
        val candidateLemmas = mixedLemmas.find(
          _.exists(
            lemma => !lemma.head.isUpper)).getOrElse(mixedLemmas.head)
        targetTongue.chooseVariant(pos, candidateLemmas)
      }
    }
    SilWord("", lemma, targetWordnet.getSenseId(senses))
  }

  private def chooseWord(
    word : SilWord,
    senses : Seq[Synset],
    pos : POS) : SilWord =
  {
    if (senses.isEmpty) {
      word.withSense("")
    } else {
      val translatedSenses = senses.flatMap(synset => {
        alignment.mapSense(synset, direction)
      }).distinct
      if (translatedSenses.isEmpty) {
        // we must discard original sense identifiers, since they aren't
        // relevant in the target tongue
        word.withSense("")
      } else {
        chooseLemma(word, translatedSenses, pos)
      }
    }
  }

  private def reorderActionSenses(
    ps : SilPredicateSentence,
    ap : SilActionPredicate,
    scorer : SilPhraseScorer) : SilActionPredicate =
  {
    val verb = ap.verb
    if (verb.senseId.isEmpty) {
      ap
    } else {
      val sortedSenses = targetWordnet.findSenses(
        verb.senseId
      ).map(sense => {
        val lemma = chooseLemma(verb, Seq(sense), POS.VERB)
        val score = scorer.computeGlobalScore(ps.copy(
          predicate = ap.withNewWord(lemma)))
        tupleN(sense, score)
      }).sortBy(_._2).reverse.map(_._1)
      ap.withNewWord(chooseLemma(verb, sortedSenses.take(1), POS.VERB))
    }
  }

  private def reorderNounSenses(
    ref : SilNounReference,
    scorer : SilPhraseScorer) : SilNounReference =
  {
    val noun = ref.noun
    if (noun.senseId.isEmpty) {
      ref
    } else {
      val sortedSenses = targetWordnet.findSenses(
        noun.senseId
      ).map(sense => {
        // FIXME should do this higher in the phrase when possible
        val chosen = chooseLemma(noun, Seq(sense), POS.NOUN)
        val score = {
          if (chosen.isProper != noun.isProper) {
            SilPhraseScore.neutral
          } else {
            val testSentence = SilPredicateSentence(
              SilStatePredicate(
                annotator.determinedNounRef(
                  chosen,
                  DETERMINER_DEFINITE,
                  COUNT_PLURAL),
                SprPredefWord(PD_EXIST)(targetTongue).toUninflected,
                SilExistenceState()
              )
            )
            scorer.computeGlobalScore(testSentence)
          }
        }
        tupleN(sense, score)
      }).sortBy(_._2).reverse.map(_._1)
      ref.withNewWord(chooseLemma(noun, sortedSenses.take(1), POS.NOUN))
    }
  }

  private def translateSense(phrase : SilPhrase) =
  {
    val translatedPhrase = {
      val word = phrase.maybeWord.get
      val senses = sourceWordnet.findSenses(word.senseId)
      val translatedWord = {
        if (senses.isEmpty) {
          word
        } else {
          chooseWord(word, senses, senses.head.getPOS)
        }
      }
      phrase.withNewWord(translatedWord)
    }
    translatedPhrase match {
      case nr : SilNounReference if (scorerOpt.nonEmpty) => {
        reorderNounSenses(nr, scorerOpt.get)
      }
      case x => x
    }
  }

  private def translateState(phrase : SilPhrase) =
  {
    val word = phrase.maybeWord.get
    val translatedWord = sourceTongue.getStatePredefFromLemma(
      word.toLemma
    ) match {
      case Some(predef) => {
        SilWord.uninflected(
          targetTongue.getStatePredefLemma(predef))
      }
      case _ => {
        word.withSense("")
      }
    }
    phrase.withNewWord(translatedWord)
  }

  protected def translateImpl(
    input : SilSentence) : SilSentence =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    val normalized = rewriter.rewriteCombined(
      sourceTongue.getTranslationSourceRules(),
      input,
      SilRewriteOptions(repeat = true))
    def translateWords = replacementMatcher(
      "translateWords", {
        case pr : SilPronounReference => {
          annotator.pronounRef(
            pr.person,
            pr.gender,
            pr.count,
            targetTongue,
            pr.proximity,
            pr.politeness)
        }
        case rp : SilRelationshipPredicate => {
          val lemma = rp.getVerb.toLemma
          sourceTongue.relLemmaMap.get(lemma) match {
            case Some(predef) => {
              rp.withNewWord(SilWord.uninflected(
                targetTongue.getRelPredefLemma(predef)))
            }
            case _ if (
              sourceTongue.getStatePredefFromLemma(lemma).nonEmpty
            ) => translateState(rp)
            case _ => {
              translateSense(rp)
            }
          }
        }
        case sp : SilStatePredicate => {
          translateState(sp)
        }
        case phrase : SilPhrase if (
          phrase.maybeWord.flatMap(w => sourceTongue.predefForLemma(
            w.toLemma)).nonEmpty
        ) => {
          val pd = sourceTongue.predefForLemma(phrase.maybeWord.get.toLemma).get
          val newLemma = targetTongue.predefLemma(pd)
          phrase.withNewWord(SilWord.uninflected(newLemma))
        }
        case phrase : SilPhrase if (
          phrase.maybeWord.map(_.senseId).getOrElse("").nonEmpty
        ) => {
          translateSense(phrase)
        }
        case ap : SilAdpositionalPhrase => {
          // FIXME all the adpositions, and context-sensitive
          sourceTongue.predefForLemma(
            ap.adposition.word.toLemma, LABEL_IN
          ) match {
            case Some(pd) => {
              val newLemma = targetTongue.predefLemma(pd)
              ap.withNewWord(SilWord.uninflected(newLemma))
            }
            case _ => ap
          }
        }
      }
    )
    val generic = neutralizePronouns(
      rewriter.rewrite(translateWords, normalized))
    rewriter.rewriteCombined(
      targetTongue.getTranslationTargetRules(annotator),
      generic,
      SilRewriteOptions(repeat = true))
  }
}
