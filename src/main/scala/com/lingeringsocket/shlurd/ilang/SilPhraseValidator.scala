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
package com.lingeringsocket.shlurd.ilang

object SilPhraseValidator
{
  def isValid(phrase : SilPhrase) : Boolean =
  {
    val querier = new SilPhraseQuerier
    var problem = ""
    def validation = querier.queryMatcher {
      case SilNullState() => {
        problem = "contains SilNullState"
      }
      case SilStateSpecifiedReference(ref, _) => {
        if (!ref.acceptsSpecifiers) {
          problem = "contains SilStateSpecifiedReference for " + ref
        }
      }
    }
    querier.query(validation, phrase)
    if (problem.isEmpty) {
      true
    } else {
      throw new RuntimeException("Invalid phrase: " + phrase + ": " + problem)
    }
  }

  def validatePhrase(phrase : SilPhrase) : Unit =
  {
    assert(SilPhraseValidator.isValid(phrase))
  }
}
