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
      case SilAppositionalReference(primary, _) => {
        extractNoun(primary)
      }
      case SilOptionallyDeterminedReference(
        SilNounReference(noun, _), _
      ) => {
        Some(noun)
      }
      case SilDeterminedReference(
        SilStateSpecifiedReference(
          SilNounReference(noun, _),
          _
        ),
        _
      ) => {
        Some(noun)
      }
      case SilStateSpecifiedReference(
        SilOptionallyDeterminedReference(SilNounReference(noun, _), _),
        _
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
      case SilAppositionalReference(primary, _) => {
        flipVariable(sentencePrinter, primary, default)
      }
      case SilDeterminedReference(
        SilNounReference(noun, count), DETERMINER_NONSPECIFIC
      ) => {
        SilDeterminedNounReference(noun, DETERMINER_UNIQUE, count)
      }
      case SilDeterminedReference(
        SilNounReference(noun, count), DETERMINER_UNIQUE
      ) => {
        SilDeterminedNounReference(noun, DETERMINER_NONSPECIFIC, count)
      }
      case SilStateSpecifiedReference(
        SilMandatorySingular(
          SilNounReference(
            noun, _
          )
        ),
        SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
      ) => {
        val ordinalSecond = sentencePrinter.sb.ordinalNumber(2)
        SilDeterminedReference(
          SilStateSpecifiedReference(
            SilNounReference(noun, COUNT_SINGULAR),
            SilPropertyState(SilWord(ordinalSecond))),
          DETERMINER_UNIQUE
        )
      }
      case SilDeterminedReference(
        SilStateSpecifiedReference(
          SilMandatorySingular(
            SilNounReference(
              noun, _
            )
          ),
          SilPropertyState(qualifier)
        ),
        determiner @ (DETERMINER_UNIQUE | DETERMINER_NONSPECIFIC)
      ) => {
        SilReference.determined(
          SilStateSpecifiedReference(
            SilNounReference(
              noun, COUNT_SINGULAR
            ),
            SilPropertyState(qualifier)
          ),
          determiner match {
            case DETERMINER_UNIQUE => DETERMINER_NONSPECIFIC
            case DETERMINER_NONSPECIFIC => DETERMINER_UNIQUE
            case _ => determiner
          }
        )
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
          case SilDeterminedReference(
            SilMandatorySingular(SilNounReference(noun, _)),
            DETERMINER_NONSPECIFIC
          ) => {
            tupleN((
              true,
              Set(SilDeterminedNounReference(
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
    additionalConsequents : Seq[SilPredicateSentence],
    alternative : Option[SilPredicateSentence]
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
    val alternativeRefs = alternative.toSeq.flatMap(ap => {
      validateAssertionPredicate(
        conditional, ap.predicate, Some(antecedentRefs)).toSeq
    })
    antecedentRefs ++ consequentRefs ++ additionalConsequentRefs ++
      alternativeRefs
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
      val allRefs = SilUtils.collectReferences(predicate, true)
      val minusRefs = allRefs.flatMap {
        case SilAppositionalReference(primary, _) => {
          Some(primary)
        }
        case _ => None
      }.toSet
      val refs = allRefs.filterNot(minusRefs.contains)
      val variableCounters = new mutable.HashMap[SilWord, Int]
      val pairs = refs.flatMap(ref => {
        val primary = ref match {
          case SilAppositionalReference(p, _) => p
          case _ => ref
        }
        val nounOpt = primary match {
          case SilDeterminedReference(
            SilMandatorySingular(SilNounReference(noun, _)),
            DETERMINER_NONSPECIFIC
          ) => {
            if (variableCounters.contains(noun)) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(noun)
          }
          case SilStateSpecifiedReference(
            SilMandatorySingular(SilNounReference(noun, _)),
            SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
          ) => {
            if (variableCounters.getOrElse(noun, 0) != 1) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(noun)
          }
          case SilDeterminedReference(
            SilStateSpecifiedReference(
              SilMandatorySingular(
                SilNounReference(
                  noun, _)
              ),
              SilPropertyState(SilWordLemma(qualifier))),
            DETERMINER_NONSPECIFIC
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
          if (form == responder.unknownType) {
            if ((noun.toNounLemma != SpcMeta.ENTITY_METAFORM_NAME)
              && !SpcBeliefRecognizer.recognizeWordLabel(Seq(noun)).nonEmpty
            ) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.UnknownForm,
                belief)
            }
          }
          val placeholder = makePlaceholder(form, noun, variableCounters)
          tupleN((ref, Set(placeholder)))
        })
      })
      pairs.toMap
    } else {
      resultCollector.refMap.filterKeys(_ match {
        case _ : SilNounReference => false
        case SilStateSpecifiedReference(
          _, SilPropertyState(SilWordLemma(lemma))
        ) => {
          (lemma == LEMMA_ANOTHER)
        }
        case _ : SilConjunctiveReference => false
        case _ => true
      })
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
