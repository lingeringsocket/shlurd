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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

import SprSpanishLemmas._

class SprSpanishTongueSpec extends Specification
{
  private val tongue = new SprSpanishTongue(
    new ShlurdExternalWordnet("/spanish_net.xml")
  )

  "SprSpanishTongue" should
  {
    "produce pronoun lemmas" in
    {
      val pronounLemmas = tongue.getPronounLemmas

      // spot-check
      tongue.pronounLemma(
        PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
        PROXIMITY_ENTITY, INFLECT_NOMINATIVE
      ) must be equalTo(LEMMA_YO)
      pronounLemmas must contain(LEMMA_YO)

      pronounLemmas.foreach(lemma => {
        val (person, count, gender, inflection, proximityOpt, possesseeCount) =
          tongue.analyzePronoun(lemma)
        val lemmaProduced = tongue.pronounLemma(
          person, gender, count,
          proximityOpt.getOrElse(PROXIMITY_ENTITY),
          inflection, possesseeCount)
        lemmaProduced must be equalTo lemma
      })

      pronounLemmas.size must be equalTo 56
    }
  }
}
