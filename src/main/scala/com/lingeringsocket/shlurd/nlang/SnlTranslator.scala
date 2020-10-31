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

import scala.jdk.CollectionConverters._

import org.slf4j._

import SprPennTreebankLabels._

sealed trait SnlTranslationDirection
case object TRANSLATE_FIRST_TO_SECOND extends SnlTranslationDirection
case object TRANSLATE_SECOND_TO_FIRST extends SnlTranslationDirection

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
  annotator : SilAnnotator,
  alignment : SnlWordnetAlignment,
  direction : SnlTranslationDirection
)
{
  import SnlTranslator._

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
    val rewriter = new SilPhraseRewriter(annotator)
    val normalized = rewriter.rewriteCombined(
      sourceTongue.getTranslationSourceRules(),
      input,
      SilRewriteOptions(repeat = true))
    def translateWords = rewriter.replacementMatcher(
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
          sourceTongue.relLemmaMap.get(rp.getVerb.toLemma) match {
            case Some(predef) => {
              rp.withNewWord(SilWord.uninflected(
                targetTongue.getRelPredefLemma(predef)))
            }
            case _ => rp
          }
        }
        case sp : SilStatePredicate => {
          val predef = sourceTongue.getStatePredefFromLemma(sp.getVerb.toLemma)
          sp.withNewWord(SilWord.uninflected(
            targetTongue.getStatePredefLemma(predef)))
        }
        case phrase : SilPhrase if (
          phrase.maybeWord.map(_.senseId).getOrElse("").nonEmpty
        ) => {
          val word = phrase.maybeWord.get
          val senses = sourceWordnet.findSenses(word.senseId)
          val pos = senses.head.getPOS
          val translatedSenses = senses.flatMap(synset => {
            alignment.mapSense(synset, direction)
          })
          val translatedSenseId = sourceWordnet.getSenseId(translatedSenses)
          // FIXME what about compound words?  Also should maybe
          // only preserve senses with the same lemma?
          val lemmas = translatedSenses.head.getWords.asScala.map(_.getLemma)
          val lemma = targetTongue.chooseVariant(pos, lemmas)
          val translatedWord = SilWord("", lemma, translatedSenseId)
          phrase.withNewWord(translatedWord)
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
    val output = rewriter.rewriteCombined(
      targetTongue.getTranslationTargetRules(),
      generic,
      SilRewriteOptions(repeat = true))
    debugger.debug(s"TRANSLATION OUTPUT = " + output)
    output
  }
}
