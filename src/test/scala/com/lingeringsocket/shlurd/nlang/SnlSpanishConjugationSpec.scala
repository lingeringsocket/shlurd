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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd.ilang._

import SnlSpanishLemmas._

import org.specs2.mutable._

class SnlSpanishConjugationSpec extends Specification
{
  "SpanishMorphology" should
  {
    "conjugate irregular" in
    {
      SnlSpanishConjugation.conjugateVerb(
        LEMMA_SER,
        SnlSpanishConjugationCoord(
          PERSON_FIRST,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_INDICATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("soy")
    }

    "conjugate regular" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_FIRST,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_INDICATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivo")
    }

    "conjugate tú subjunctive" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_SUBJUNCTIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivas")
    }

    "conjugate usted imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_THIRD,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("viva")
    }

    "conjugate ustedes imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_THIRD,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivan")
    }

    "conjugate vosotros imperative" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_IMPERATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("vivid")
    }

    "conjugate vosotros subjunctive" in
    {
      SnlSpanishConjugation.conjugateVerb(
        "vivir",
        SnlSpanishConjugationCoord(
          PERSON_SECOND,
          COUNT_PLURAL,
          TENSE_PRESENT,
          MOOD_SUBJUNCTIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("viváis")
    }
  }
}
