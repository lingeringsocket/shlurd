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
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._

class SpcQueryRewriter(
  tongueIn : SprTongue,
  annotator : SpcAnnotator,
  question : SilQuestion,
  answerInflection : SilInflection)
    extends SmcQueryRewriter(tongueIn, annotator, question, answerInflection)
{
  private implicit val tongue = tongueIn

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
          case SilAdpositionalState(SprMagicAdposition(MW_IN), container) => {
            SilRelationshipPredicate(
              specified.subject,
              REL_PREDEF_IDENTITY.toVerb(tongue),
              annotator.genitiveRef(
                container,
                annotator.nounRef(SilWord(SmcIdeals.ROLE_CONTAINEE))),
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
            annotator.genitiveRef(
              complement,
              annotator.nounRef(SilWord(SmcIdeals.ROLE_CONTAINER)))
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
