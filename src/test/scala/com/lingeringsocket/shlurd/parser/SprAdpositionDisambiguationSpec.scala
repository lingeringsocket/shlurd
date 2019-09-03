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
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

object SprAdpositionDisambiguationSpec
{
  sealed trait UsagePreference
  case object ADVERBIAL extends UsagePreference
  case object ADJECTIVAL extends UsagePreference
}

class SprAdpositionDisambiguationSpec extends Specification
{
  import SprAdpositionDisambiguationSpec._

  private def expectUsage(
    input : String,
    expectedUsage : UsagePreference) =
  {
    val phrase = SprParser(input).parseOne
    var adverbial = 0
    var adjectival = 0
    val querier = new SilPhraseQuerier
    def matcher = querier.queryMatcher {
      case _ : SilAdpositionalVerbModifier => {
        adverbial += 1
      }
      case _ : SilAdpositionalState => {
        adjectival += 1
      }
    }
    querier.query(matcher, phrase)
    (adverbial + adjectival) must be equalTo 1
    val actualUsage = {
      if (adverbial > 0) {
        ADVERBIAL
      } else {
        ADJECTIVAL
      }
    }
    expectedUsage must be equalTo actualUsage
  }

  "SprAdpositionDisambiguation" should
  {
    "handle difficult cases" in
    {
      skipped("not working yet")
      expectUsage(
        "what happened the day before the fight",
        ADJECTIVAL)
      expectUsage(
        "I kill the dwarf with the axe",
        ADVERBIAL)
    }

    "disambiguate adposition usage" in
    {
      expectUsage(
        "throw the axe at the thief",
        ADVERBIAL)
      expectUsage(
        "the door with the knocker is unlocked",
        ADJECTIVAL)
      expectUsage(
        "I bow before the throne",
        ADVERBIAL)
      expectUsage(
        "where was the football before the kitchen",
        ADVERBIAL)
    }
  }
}
