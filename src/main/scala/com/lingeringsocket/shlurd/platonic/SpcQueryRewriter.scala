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

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

class SpcQueryRewriter(
  question : SilQuestion,
  answerInflection : SilInflection)
    extends SmcQueryRewriter(question, answerInflection)
{
  override def rewritePredicate = combineRules(
    rewriteContainmentPredicate, rewriteActionPredicate)

  def rewriteContainmentPredicate = replacementMatcher(
    "rewriteContainmentPredicate", {
      case SilStatePredicate(subject, state, modifiers) => {
        val specified = SilStatePredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          scopedRewrite(state, INFLECT_ADPOSITIONED),
          modifiers)
        specified.state match {
          case SilAdpositionalState(SilAdposition.IN, container) => {
            SilRelationshipPredicate(
              specified.subject,
              SilGenitiveReference(
                container,
                SilNounReference(SilWord(SmcLemmas.LEMMA_CONTAINEE))),
              REL_IDENTITY.toVerb,
              modifiers
            )
          }
          case _ => {
            specified
          }
        }
      }
      case SilRelationshipPredicate(subject, complement,
        verb, modifiers
      ) => {
        val rewrittenComplement = question match {
          case QUESTION_WHERE => {
            SilGenitiveReference(
              complement,
              SilNounReference(SilWord(SmcLemmas.LEMMA_CONTAINER)))
          }
          case _ => complement
        }
        SilRelationshipPredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          rewrittenComplement, verb, modifiers)
      }
    }
  )
}
