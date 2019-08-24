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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

import SprEnglishLemmas._

object SpcImplicationMapper
{
  def extractPlaceholder(
    ref : SilReference,
    refMap : SpcRefMap
  ) : Option[SpcTransientEntity] =
  {
    refMap.get(ref) match {
      case Some(set : Set[SpcEntity]) if (set.size == 1) => {
        set.head match {
          // FIXME verify that it's actually a placeholder
          case e : SpcTransientEntity => Some(e)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def extractNoun(ref : SilReference) : Option[SilWord] =
  {
    ref match {
      case SilNounReference(
        noun, _, _
      ) => {
        Some(noun)
      }
      case SilStateSpecifiedReference(
        SilNounReference(noun, _, _),
        _ : SilPropertyState
      ) => {
        Some(noun)
      }
      case _ => {
        None
      }
    }
  }

  def flipVariable(
    sentencePrinter : SilSentencePrinter,
    reference : SilReference,
    default : => SilReference) : SilReference =
  {
    reference match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, count
      ) => {
        SilNounReference(noun, DETERMINER_UNIQUE, count)
      }
      case SilNounReference(
        noun, DETERMINER_UNIQUE, count
      ) => {
        SilNounReference(noun, DETERMINER_NONSPECIFIC, count)
      }
      case SilStateSpecifiedReference(
        SilNounReference(
          noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
        ),
        SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
      ) => {
        val ordinalSecond = sentencePrinter.sb.ordinalNumber(2)
        SilStateSpecifiedReference(
          SilNounReference(
            noun, DETERMINER_UNIQUE, COUNT_SINGULAR
          ),
          SilPropertyState(SilWord(ordinalSecond))
        )
      }
      case SilStateSpecifiedReference(sub, state) => {
        val flipped = flipVariable(sentencePrinter, sub, sub)
        if (flipped == sub) {
          default
        } else {
          SilStateSpecifiedReference(flipped, state)
        }
      }
      case _ => default
    }
  }

  def findPlaceholderCorrespondence(
    ref : SilReference,
    placeholderMap : Option[SpcRefMap]
  ) : (Boolean, Set[SilReference]) =
  {
    placeholderMap match {
      case Some(refMap) => {
        val placeholder = extractPlaceholder(ref, refMap)
        if (placeholder.nonEmpty) {
          tupleN((
            true,
            refMap.keySet.filter(other => {
              (ref != other) &&
              (extractPlaceholder(other, refMap) == placeholder)
            })
          ))
        } else {
          tupleN((
            false,
            Set.empty
          ))
        }
      }
      case _ => {
        ref match {
          case SilNounReference(
            noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
          ) => {
            tupleN((
              true,
              Set(SilNounReference(
                noun, DETERMINER_UNIQUE, COUNT_SINGULAR))))
          }
          case _ => {
            tupleN((
              false,
              Set.empty
            ))
          }
        }
      }
    }
  }
}

class SpcImplicationMapper(
  responder : SpcResponder
)
{
  def validateImplication(
    conditional : SilConditionalSentence,
    additionalConsequents : Seq[SilPredicateSentence]
  ) : SpcRefMap =
  {
    val antecedentRefs = validateAssertionPredicate(
      conditional, conditional.antecedent)
    val consequentRefs = validateAssertionPredicate(
      conditional, conditional.consequent, Some(antecedentRefs))
    val additionalConsequentRefs = additionalConsequents.flatMap(ac => {
      validateAssertionPredicate(
        conditional, ac.predicate, Some(antecedentRefs)).toSeq
    })
    antecedentRefs ++ consequentRefs ++ additionalConsequentRefs
  }

  def validateAssertionPredicate(
    belief : SilSentence,
    predicate : SilPredicate,
    antecedentRefs : Option[SpcRefMap] = None)
      : SpcRefMap =
  {
    val resultCollector = SmcResultCollector[SpcEntity]()
    val scope = new SmcPhraseScope(
      antecedentRefs.getOrElse(Map.empty),
      responder.mindScope
    )
    responder.resolveReferences(
      predicate, resultCollector, true, false, scope
    ) matchPartial {
      case Failure(ShlurdException(code, msg)) => {
        throw UnacceptableBeliefExcn(code, msg, belief)
      }
      case Failure(e) => {
        throw e
      }
    }
    if (antecedentRefs.isEmpty) {
      val refs = SilUtils.collectReferences(predicate, true)
      val variableCounters = new mutable.HashMap[SilWord, Int]
      val pairs = refs.flatMap(ref => {
        val nounOpt = ref match {
          case SilNounReference(
            noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
          ) => {
            if (variableCounters.contains(noun)) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(noun)
          }
          case SilStateSpecifiedReference(
            SilNounReference(noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
            SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
          ) => {
            if (variableCounters.getOrElse(noun, 0) != 1) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(noun)
          }
          case SilStateSpecifiedReference(
            SilNounReference(noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            SilPropertyState(SilWordLemma(qualifier))
          ) => {
            val sb = responder.sentencePrinter.sb
            sb.ordinalValue(qualifier) match {
              case Success(ordinal) => {
                if (variableCounters.getOrElse(noun, 0) != (ordinal - 1)) {
                  throw InvalidBeliefExcn(
                    ShlurdExceptionCode.AssertionInvalidVariable, belief)
                }
                Some(noun)
              }
              case _ => None
            }
          }
          case _ => None
        }
        nounOpt.map(noun => {
          val form = responder.deriveType(ref, resultCollector.refMap)
          val placeholder = makePlaceholder(form, noun, variableCounters)
          tupleN((ref, Set(placeholder)))
        })
      })
      pairs.toMap
    } else {
      resultCollector.refMap
    }
  }

  private def makePlaceholder(
    form : SpcForm,
    noun : SilWord,
    variableCounters : mutable.Map[SilWord, Int]) : SpcEntity =
  {
    val number = variableCounters.getOrElse(noun, 0)
    variableCounters.put(noun, number + 1)
    val name = noun.toNounLemma + "-" + number.toString
    SpcTransientEntity(form, name, name)
  }
}
