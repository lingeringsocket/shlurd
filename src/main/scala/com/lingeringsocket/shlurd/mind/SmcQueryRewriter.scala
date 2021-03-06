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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

class SmcQueryRewriter(
  tongueIn : SprTongue,
  annotator : SilAnnotator,
  question : SilQuestion,
  answerInflection : SilInflection)
    extends SilPhraseRewriter(annotator)
{
  import SilPhraseRewriter._

  private implicit val tongue = tongueIn

  def rewriteSpecifier(determinedSubs : Set[SilReference]) = replacementMatcher(
    "rewriteSpecifier", {
      case cr @ SilCountedNounReference(
        noun, count
      ) if (!noun.isProper && !determinedSubs.contains(cr)) => {
        val determiner = noun match {
          case SprPredefWord(PD_WHO | PD_WHOM | PD_WHAT | PD_WHERE) => {
            DETERMINER_VARIABLE
          }
          case _ => DETERMINER_ANY
        }
        annotator.determinedNounRef(noun, determiner, count)
      }
    }
  )

  def rewritePredicate = combineRules(
    rewriteStatePredicate, rewriteRelationshipPredicate, rewriteActionPredicate)

  def rewriteStatePredicate = replacementMatcher(
    "rewriteStatePredicate", {
      case SilStatePredicate(subject, verb, state, modifiers) => {
        SilStatePredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          verb,
          scopedRewrite(state, INFLECT_ADPOSITIONED),
          modifiers)
      }
    }
  )

  def rewriteRelationshipPredicate = replacementMatcher(
    "rewriteRelationshipPredicate", {
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        SilRelationshipPredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          verb, complement, modifiers)
      }
    }
  )

  def rewriteActionPredicate = replacementMatcher(
    "rewriteActionPredicate", {
      case SilActionPredicate(subject, verb, directObject, modifiers) => {
        SilActionPredicate(
          scopedRewrite(subject, INFLECT_NOMINATIVE),
          verb,
          directObject.map(scopedRewrite(_, INFLECT_ACCUSATIVE)),
          modifiers.map(scopedRewrite(_, INFLECT_ADPOSITIONED))
        )
      }
    }
  )

  protected def scopedRewrite[PhraseType <: SilPhrase](
    phrase : PhraseType,
    requiredInflection : SilInflection) : PhraseType =
  {
    if (requiredInflection == answerInflection) {
      val determinedRefs =
        SilUtils.collectReferences(phrase).filter(
          _.isInstanceOf[SilDeterminedReference]).toSet
      val determinedSubs =
        determinedRefs.flatMap(ref => SilUtils.collectReferences(ref).toSet)
      rewrite(rewriteSpecifier(determinedSubs), phrase)
    } else {
      phrase
    }
  }
}
