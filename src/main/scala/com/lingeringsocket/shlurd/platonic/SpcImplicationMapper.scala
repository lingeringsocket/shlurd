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
    referenceMap : Map[SilReference, Set[SpcEntity]]
  ) : Option[SpcTransientEntity] =
  {
    referenceMap.get(ref) match {
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
}

class SpcImplicationMapper(
  responder : SpcResponder
)
{
  def validateImplication(
    conditional : SilConditionalSentence,
    additionalConsequents : Seq[SilPredicateSentence]
  )
      : Map[SilReference, Set[SpcEntity]] =
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
    antecedentRefs : Option[Map[SilReference, Set[SpcEntity]]] = None)
      : Map[SilReference, Set[SpcEntity]] =
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
      val refs = SilUtils.collectReferences(predicate)
      val variableCounters = new mutable.HashMap[SilWord, Int]
      val pairs = refs.flatMap(_ match {
        case nr @ SilNounReference(
          noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
        ) => {
          if (variableCounters.contains(noun)) {
            throw InvalidBeliefExcn(
              ShlurdExceptionCode.AssertionInvalidVariable, belief)
          }
          val form = responder.deriveType(nr, resultCollector.referenceMap)
          val placeholder = makePlaceholder(form, noun, variableCounters)
          Some((nr, Set(placeholder)))
        }
        case sr @ SilStateSpecifiedReference(
          snr @ SilNounReference(noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
          SilPropertyState(SilWordLemma(LEMMA_ANOTHER))
        ) => {
          val form = responder.deriveType(snr, resultCollector.referenceMap)
          if (variableCounters.getOrElse(noun, 0) != 1) {
            throw InvalidBeliefExcn(
              ShlurdExceptionCode.AssertionInvalidVariable, belief)
          }
          val placeholder = makePlaceholder(form, noun, variableCounters)
          Some((sr, Set(placeholder)))
        }
        case _ => None
      })
      pairs.toMap
    } else {
      resultCollector.referenceMap
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
