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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.platonic._

class PhlebTranslator(
  val alignment : SnlWordnetAlignment,
  val playerToInterpreter : SnlTranslationDirection
)
{
  def getPlayerTongue = alignment.getSourceTongue(playerToInterpreter)

  def getInterpreterTongue = alignment.getTargetTongue(playerToInterpreter)

  def newInputTranslator(scorerOpt : Option[SilPhraseScorer] = None) =
  {
    val annotator = SpcAnnotator()
    new SnlTranslator(annotator, alignment, playerToInterpreter, scorerOpt)
  }

  def newResponseTranslator =
  {
    new SnlTranslator(SpcAnnotator(), alignment, playerToInterpreter.reverse)
  }

  def getAliases = PhlebAliases.english
}

object PhlebSpanishTranslator extends PhlebTranslator(
  SnlUtils.spanishEnglishAlignment,
  TRANSLATE_FIRST_TO_SECOND
) {
  override def getAliases = PhlebAliases.spanish

  override def newInputTranslator(scorerOpt : Option[SilPhraseScorer] = None) =
  {
    val annotator = SpcAnnotator()
    new SnlTranslator(annotator, alignment, playerToInterpreter, scorerOpt)
    {
      override protected def translateImpl(
        input : SilSentence) : SilSentence =
      {
        super.translateImpl(input) match {
          case SilPredicateSentence(
            ap @ SilActionPredicate(
              pr @ SilPronounReference(
                PERSON_THIRD,
                GENDER_NEUTER,
                COUNT_SINGULAR,
                // FIXME catch it earlier and require elided
                PROXIMITY_ENTITY | PROXIMITY_ELIDED,
                _
              ),
              verb,
              directObject,
              modifiers
            ),
            tam,
            formality
          ) if (tam.isIndicative) => {
            val command = SilPredicateSentence(
              SilActionPredicate(
                annotator.pronounRef(
                  PERSON_SECOND,
                  GENDER_SOMEONE,
                  COUNT_SINGULAR,
                  getInterpreterTongue,
                  PROXIMITY_ELIDED,
                  POLITENESS_RESPECTFUL
                ),
                verb,
                directObject,
                modifiers
              ),
              tam.withMood(MOOD_IMPERATIVE),
              formality
            )
            command.predicate.setInflectedAttributes(
              ap.getInflectedAttributes)
            command
          }
          case output => output
        }
      }
    }
  }
}
