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

import org.specs2.mutable._

class SilPhraseScorerSpec extends Specification
{
  "SilPhraseScorer" should
  {
    "compare scores" in
    {
      SilPhraseScore.pro(1) must be equalTo SilPhraseScore.numeric(1)
      SilPhraseScore.con(1) must be equalTo SilPhraseScore.numeric(-1)
      SilPhraseScore.neutral must be equalTo SilPhraseScore.numeric(0)
      SilPhraseScore.pro(1) must be greaterThan SilPhraseScore.neutral
      SilPhraseScore.pro(2) must be greaterThan SilPhraseScore.pro(1)
      SilPhraseScore.pro(1) must be greaterThan SilPhraseScore.con(1)
      SilPhraseScore.neutral must be greaterThan SilPhraseScore.con(1)
      SilPhraseScore.con(1) must be greaterThan SilPhraseScore.con(2)
    }

    "add scores" in
    {
      SilPhraseScore.neutral + SilPhraseScore.neutral must be equalTo
        SilPhraseScore.neutral
      SilPhraseScore.neutral + SilPhraseScore.pro(1) must be equalTo
        SilPhraseScore.pro(1)
      SilPhraseScore.neutral + SilPhraseScore.con(1) must be equalTo
        SilPhraseScore.con(1)
      SilPhraseScore.pro(1) + SilPhraseScore.pro(2) must be equalTo
        SilPhraseScore.pro(3)
      SilPhraseScore.con(1) + SilPhraseScore.con(2) must be equalTo
        SilPhraseScore.con(3)
      SilPhraseScore.pro(1) + SilPhraseScore.con(2) must be equalTo
        SilPhraseScore(1, 2)
    }

    "compare score sums" in
    {
      SilPhraseScore.pro(2) + SilPhraseScore.con(1) must be greaterThan
        SilPhraseScore.pro(1) + SilPhraseScore.con(1)
      SilPhraseScore.pro(1) + SilPhraseScore.con(1) must be greaterThan
        SilPhraseScore.pro(2) + SilPhraseScore.con(2)
    }
  }
}
