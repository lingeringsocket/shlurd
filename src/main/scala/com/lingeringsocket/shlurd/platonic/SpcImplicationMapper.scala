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
        SilNounReference(noun), _
      ) => {
        Some(noun)
      }
      case SilDeterminedReference(
        SilStateSpecifiedReference(
          SilNounReference(noun),
          _
        ),
        _
      ) => {
        Some(noun)
      }
      case SilStateSpecifiedReference(
        SilOptionallyDeterminedReference(SilNounReference(noun), _),
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
    tongueIn : SprTongue,
    annotator : SpcAnnotator,
    sentencePrinter : SilSentencePrinter,
    reference : SilReference,
    default : => SilReference) : SilReference =
  {
    implicit val tongue = tongueIn

    reference match {
      case SilAppositionalReference(primary, _) => {
        flipVariable(tongue, annotator, sentencePrinter, primary, default)
      }
      case SilDeterminedReference(
        SilCountedNounReference(noun, count),
        _ : SilIndefiniteDeterminer
      ) => {
        annotator.determinedNounRef(noun, DETERMINER_DEFINITE, count)
      }
      case SilDeterminedReference(
        SilCountedNounReference(noun, count), DETERMINER_DEFINITE
      ) => {
        annotator.determinedNounRef(noun, DETERMINER_SOME, count)
      }
      case SilStateSpecifiedReference(
        SilMandatorySingular(
          noun
        ),
        SilPropertyState(SprPredefWord(PD_ANOTHER))
      ) => {
        val ordinalSecond = sentencePrinter.sb.ordinalNumber(
          2, SilUtils.getGender(reference, tongue))
        annotator.determinedRef(
          annotator.stateSpecifiedRef(
            annotator.nounRef(noun),
            SilPropertyState(SilWord(ordinalSecond))),
          DETERMINER_DEFINITE
        )
      }
      case SilDeterminedReference(
        SilStateSpecifiedReference(
          SilCountedNounReference(noun, count),
          SilPropertyState(qualifier)
        ),
        determiner @ (
          DETERMINER_DEFINITE | (_ : SilIndefiniteDeterminer)
        )
      ) => {
        annotator.determinedRef(
          annotator.stateSpecifiedRef(
            annotator.nounRef(noun, count),
            SilPropertyState(qualifier)
          ),
          determiner match {
            case DETERMINER_DEFINITE => DETERMINER_SOME
            case _ : SilIndefiniteDeterminer => DETERMINER_DEFINITE
            case _ => determiner
          }
        )
      }
      case _ => default
    }
  }

  def findPlaceholderCorrespondence(
    annotator : SpcAnnotator,
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
            SilMandatorySingular(noun),
            DETERMINER_NONSPECIFIC
          ) => {
            tupleN((
              true,
              Set(annotator.determinedNounRef(
                noun, DETERMINER_DEFINITE, COUNT_SINGULAR))))
          }
          case SilDeterminedReference(
            SilCountedNounReference(noun, count),
            _ : SilUnlimitedDeterminer
          ) => {
            tupleN((
              true,
              Set(annotator.determinedNounRef(
                noun, DETERMINER_DEFINITE, count))))
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
  private implicit val tongue = responder.getMind.getTongue

  def validateImplication(
    conditional : SilConditionalSentence,
    additionalConsequents : Seq[SilPredicateSentence],
    alternative : Option[SilPredicateSentence]
  ) : SpcRefMap =
  {
    val annotator = SpcAnnotator()
    val antecedentRefs = validateAssertionPredicate(
      annotator, conditional, conditional.antecedent)
    val consequentRefs = validateAssertionPredicate(
      annotator, conditional, conditional.consequent, Some(antecedentRefs))
    val additionalConsequentRefs = additionalConsequents.flatMap(ac => {
      validateAssertionPredicate(
        annotator, conditional, ac.predicate, Some(antecedentRefs)).toSeq
    })
    val alternativeRefs = alternative.toSeq.flatMap(ap => {
      validateAssertionPredicate(
        annotator, conditional, ap.predicate, Some(antecedentRefs)).toSeq
    })
    antecedentRefs ++ consequentRefs ++ additionalConsequentRefs ++
      alternativeRefs
  }

  def validateAssertionPredicate(
    annotator : SpcAnnotator,
    belief : SilSentence,
    predicateIn : SilPredicate,
    antecedentRefs : Option[SpcRefMap] = None)
      : SpcRefMap =
  {
    val predicate = annotator.copy(
      predicateIn,
      SilPhraseCopyOptions(preserveBasicNotes = true))
    val resultCollector = SpcResultCollector(annotator)
    resultCollector.analyzingAssertion = true
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
            SilCountedNounReference(noun, count),
            _ : SilIndefiniteDeterminer
          ) => {
            if (variableCounters.contains(noun)) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(tupleN((noun, count)))
          }
          case SilStateSpecifiedReference(
            SilMandatorySingular(noun),
            SilPropertyState(SprPredefWord(PD_ANOTHER))
          ) => {
            if (variableCounters.getOrElse(noun, 0) != 1) {
              throw InvalidBeliefExcn(
                ShlurdExceptionCode.AssertionInvalidVariable, belief)
            }
            Some(tupleN((noun, COUNT_SINGULAR)))
          }
          case SilDeterminedReference(
            SilStateSpecifiedReference(
              SilCountedNounReference(noun, count),
              SilPropertyState(SilWordLemma(qualifier))),
            _ : SilIndefiniteDeterminer
          ) => {
            val sb = responder.sentencePrinter.sb
            sb.ordinalValue(qualifier) match {
              case Success(ordinal) => {
                if (variableCounters.getOrElse(noun, 0) != (ordinal - 1)) {
                  throw InvalidBeliefExcn(
                    ShlurdExceptionCode.AssertionInvalidVariable, belief)
                }
                Some(tupleN((noun, count)))
              }
              case _ => None
            }
          }
          case _ => None
        }
        nounOpt.map {
          case(noun, count) => {
            val form = responder.deriveType(
              annotator, ref, resultCollector.refMap)
            if (form == responder.unknownType) {
              if ((noun.toNounLemma != SpcMeta.ENTITY_METAFORM_NAME)
                && !SpcBeliefRecognizer.recognizeWordLabel(Seq(noun)).nonEmpty
              ) {
                throw InvalidBeliefExcn(
                  ShlurdExceptionCode.UnknownForm,
                  belief)
              }
            }
            val placeholder = makePlaceholder(
              form, noun, count, variableCounters)
            tupleN((ref, Set(placeholder)))
          }
        }
      })
      pairs.toMap
    } else {
      resultCollector.refMap.view.filterKeys(_ match {
        case _ : SilNounReference => false
        case SilStateSpecifiedReference(
          _, SilPropertyState(SilWordLemma(lemma))
        ) => {
          (lemma == PD_ANOTHER.toLemma)
        }
        case _ : SilConjunctiveReference => false
        case _ => true
      }).toMap
    }
  }

  private def makePlaceholder(
    form : SpcForm,
    noun : SilWord,
    count : SilCount,
    variableCounters : mutable.Map[SilWord, Int]) : SpcEntity =
  {
    val number = variableCounters.getOrElse(noun, 0)
    variableCounters.put(noun, number + 1)
    val baseName = noun.toNounLemma + "-" + number.toString
    val name = count match {
      case COUNT_PLURAL => SpcMeta.PLACEHOLDER_MULTI + baseName
      case _ => baseName
    }
    SpcTransientEntity(form, name, name)
  }
}
