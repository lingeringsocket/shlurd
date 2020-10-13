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
import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import SnlEnglishLemmas._

class SnlEnglishTongueSpec extends Specification
{
  // FIXME minimize fallout from race conditions in extjwnl
  sequential

  private implicit val tongue = SnlUtils.defaultTongue

  "SnlEnglishTongue" should
  {
    "produce pronoun lemmas" in
    {
      val pronounLemmas = tongue.getPronounLemmas

      // spot-check
      tongue.pronounLemma(
        PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
        PROXIMITY_ENTITY, SilPoliteness.DEFAULT, INFLECT_NOMINATIVE
      ) must be equalTo(LEMMA_I)
      pronounLemmas must contain(LEMMA_I)

      def fold(s : String) = s.replace("elves", "elf")

      pronounLemmas.foreach(lemma => {
        val (person, count, gender, inflection, proximityOpt, _, politeness) =
          tongue.analyzePronoun(lemma)
        val lemmaProduced = tongue.pronounLemma(
          person, gender, count,
          proximityOpt.getOrElse(PROXIMITY_ENTITY),
          politeness, inflection)
        fold(lemmaProduced) must be equalTo fold(lemma)
      })

      pronounLemmas.size must be equalTo 35
    }

    "detect potential gerunds" in
    {
      tongue.isPotentialGerund("running") must beTrue
      tongue.isPotentialGerund("king") must beFalse
      tongue.isPotentialGerund("run") must beFalse
      tongue.isPotentialGerund("red") must beFalse
    }

    "detect potential plurals" in
    {
      tongue.isPotentialPlural("horses") must beTrue
      tongue.isPotentialPlural("mice") must beTrue
      tongue.isPotentialPlural("horse") must beFalse
    }

    "transform predefs" in
    {
      val forwardSize = SnlEnglishLexicon.predefToLemma.size
      val inverseSize = SnlEnglishLexicon.lemmaToPredef.size
      forwardSize must be equalTo inverseSize + 1

      val specialCases = SnlEnglishLexicon.predefToLemma.values.groupBy(x => x).
        filter(_._2.size > 1).map(_._1).toSet
      specialCases must be equalTo Set(LEMMA_NEITHER)

      SprPredefWord(PD_NEITHER_NOUN).toLemma must be equalTo(
        LEMMA_NEITHER)
      SprPredefWord(PD_NEITHER_DETERMINER).toLemma must be equalTo(
        LEMMA_NEITHER)

      val CORRECT = "correct"
      val INCORRECT = "incorrect"
      (SilWord(LEMMA_NEITHER) match {
        case SprPredefWord(PD_NEITHER_NOUN) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT

      (SilWord(LEMMA_NEITHER) match {
        case SprPredefDeterminerWord(PD_NEITHER_DETERMINER) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT
    }
  }
}
