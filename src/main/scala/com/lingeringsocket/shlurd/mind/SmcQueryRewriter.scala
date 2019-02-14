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

class SmcQueryRewriter(
  question : SilQuestion,
  answerInflection : SilInflection)
    extends SilPhraseRewriter
{
  def rewriteSpecifier = replacementMatcher {
    case SilNounReference(
      noun, DETERMINER_UNSPECIFIED, count
    ) if (!noun.isProper) => {
      SilNounReference(noun, DETERMINER_ANY, count)
    }
  }

  def rewritePredicate = replacementMatcher {
    case SilStatePredicate(subject, state, modifiers) => {
      SilStatePredicate(
        scopedRewrite(subject, INFLECT_NOMINATIVE),
        scopedRewrite(state, INFLECT_ADPOSITIONED),
        modifiers)
    }
    case SilRelationshipPredicate(subject, complement,
      relationship, modifiers
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
        rewrittenComplement, relationship, modifiers)
    }
    case SilActionPredicate(subject, action, directObject, modifiers) => {
      SilActionPredicate(
        scopedRewrite(subject, INFLECT_NOMINATIVE),
        action,
        directObject.map(scopedRewrite(_, INFLECT_ACCUSATIVE)),
        modifiers.map(scopedRewrite(_, INFLECT_ADPOSITIONED))
      )
    }
  }

  private def scopedRewrite[PhraseType <: SilPhrase](
    phrase : PhraseType,
    requiredInflection : SilInflection) : PhraseType =
  {
    if (requiredInflection == answerInflection) {
      rewrite(rewriteSpecifier, phrase)
    } else {
      phrase
    }
  }
}
