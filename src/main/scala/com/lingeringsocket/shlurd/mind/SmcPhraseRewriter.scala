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

import com.lingeringsocket.shlurd.parser._

import SprEnglishLemmas._

class SmcPhraseRewriter extends SilPhraseRewriter
{
  protected val querier = new SilPhraseRewriter

  def containsWildcard(
    phrase : SilPhrase,
    includeConjunctions : Boolean = true) : Boolean =
  {
    var wildcard = false
    def matchWildcard = querier.queryMatcher {
      case SilConjunctiveReference(
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _,
        _
      ) => {
        if (includeConjunctions) {
          wildcard = true
        }
      }
      case SilNounReference(
        _,
        DETERMINER_ANY | DETERMINER_SOME | DETERMINER_ALL,
        _
      ) => {
        wildcard = true
      }
      case SilNounReference(
        SilWord(LEMMA_WHO, LEMMA_WHO) |
          SilWord(LEMMA_WHOM, LEMMA_WHOM) |
          SilWord(LEMMA_WHERE, LEMMA_WHERE),
        _,
        _
      ) => {
        wildcard = true
      }
    }
    querier.query(matchWildcard, phrase)
    wildcard
  }
}
