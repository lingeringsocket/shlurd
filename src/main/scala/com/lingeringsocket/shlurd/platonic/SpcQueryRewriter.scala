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
  annotator : SilAnnotator,
  question : SilQuestion,
  answerInflection : SilInflection)
    extends SmcQueryRewriter(annotator, question, answerInflection)
{
  override def rewritePredicate = combineRules(
    rewriteContainmentPredicate, rewriteActionPredicate)

  def rewriteContainmentPredicate = replacementMatcher(
    "rewriteContainmentPredicate", {
      case SilStatePredicate(subject, verb, state, modifiers) => {
        val specified = SilStatePredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          verb,
          scopedRewrite(state, INFLECT_ADPOSITIONED),
          modifiers)
        specified.state match {
          case SilAdpositionalState(SilAdposition.IN, container) => {
            SilRelationshipPredicate(
              specified.subject,
              REL_PREDEF_IDENTITY.toVerb,
              SilGenitiveReference(
                container,
                annotator.nounRef(SilWord(SmcLemmas.LEMMA_CONTAINEE))),
              modifiers
            )
          }
          case _ => {
            specified
          }
        }
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        val rewrittenComplement = question match {
          case QUESTION_WHERE => {
            SilGenitiveReference(
              complement,
              annotator.nounRef(SilWord(SmcLemmas.LEMMA_CONTAINER)))
          }
          case _ => complement
        }
        SilRelationshipPredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          verb, rewrittenComplement, modifiers)
      }
    }
  )
}
