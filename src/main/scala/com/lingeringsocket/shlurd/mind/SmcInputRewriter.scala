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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

class SmcInputRewriter[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType <:SmcCosmos[EntityType, PropertyType]
](
  mind : SmcMind[EntityType, PropertyType, CosmosType]
) extends SmcPhraseRewriter
{
  def normalizeInput(
    sentence : SilSentence) : SilSentence =
  {
    rewrite(
      convertProgressive,
      sentence)
  }

  // FIXME this rewrite is just a hack to keep tests passing for the moment
  private def convertProgressive = replacementMatcher {
    case SilPredicateSentence(
      SilActionPredicate(
        subject, action, None, modifiers),
      tam, formality
    ) if (tam.isProgressive) => {
      SilPredicateSentence(
        SilStatePredicate(
          subject, SilPropertyState(action), modifiers),
        tam.withAspect(ASPECT_SIMPLE), formality)
    }
    case SilPredicateQuery(
      SilActionPredicate(
        subject, action, None, modifiers),
      question, answerInflection, tam, formality
    ) if (tam.isProgressive) => {
      SilPredicateQuery(
        SilStatePredicate(
          subject, SilPropertyState(action), modifiers),
        question, answerInflection,
        tam.withMood(MOOD_INTERROGATIVE), formality)
    }
  }

  def bindPredicateWildcard(predicate : SilPredicate, objRef : SilReference)
      : SilPredicate =
  {
    rewrite(
      replacePredicateWildcard(objRef),
      predicate)
  }

  private def replacePredicateWildcard(
    objRef : SilReference) = replacementMatcher
  {
    case ref : SilNounReference if containsWildcard(ref, false) => {
      objRef
    }
  }
}
