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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._

class ShlurdQueryRewriter extends ShlurdPhraseRewriter
{
  def rewriteSpecifier = replacementMatcher {
    case ShlurdNounReference(
      noun, DETERMINER_UNSPECIFIED, count
    ) =>
      {
        ShlurdNounReference(noun, DETERMINER_ANY, count)
      }
  }

  def rewritePredicate = replacementMatcher {
    case ShlurdStatePredicate(subject, state) => {
      ShlurdStatePredicate(
        rewrite(rewriteSpecifier, subject),
        state)
    }
    case ShlurdRelationshipPredicate(subject, complement, relationship) => {
      ShlurdRelationshipPredicate(
        rewrite(rewriteSpecifier, subject),
        complement, relationship)
    }
  }
}