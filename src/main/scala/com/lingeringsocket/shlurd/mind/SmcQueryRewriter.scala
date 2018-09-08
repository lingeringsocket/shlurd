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

import SprEnglishLemmas._

class SmcQueryRewriter(question : SilQuestion) extends SilPhraseRewriter
{
  def rewriteSpecifier = replacementMatcher {
    case SilNounReference(
      noun, DETERMINER_UNSPECIFIED, count
    ) if (!noun.isProper) =>
      {
        SilNounReference(noun, DETERMINER_ANY, count)
      }
  }

  def rewritePredicate = replacementMatcher {
    case SilStatePredicate(subject, state, modifiers) => {
      SilStatePredicate(
        rewrite(rewriteSpecifier, subject),
        state,
        modifiers)
    }
    case SilRelationshipPredicate(subject, complement,
      relationship, modifiers
    ) => {
      val rewrittenComplement = question match {
        case QUESTION_WHERE => {
          SilGenitiveReference(
            complement,
            SilNounReference(SilWord(LEMMA_CONTAINER)))
        }
        case _ => complement
      }
      SilRelationshipPredicate(
        rewrite(rewriteSpecifier, subject),
        rewrittenComplement, relationship, modifiers)
    }
    case ap : SilActionPredicate => {
      rewrite(rewriteSpecifier, ap)
    }
  }
}
