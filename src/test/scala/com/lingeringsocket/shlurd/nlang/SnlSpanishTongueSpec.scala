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

import SnlSpanishLemmas._

class SnlSpanishTongueSpec extends Specification
{
  private implicit val tongue = new SnlSpanishTongue(
    new SnlExternalWordnet("/spanish_net.xml")
  )

  "SnlSpanishTongue" should
  {
    "pluralize words" in
    {
      tongue.pluralizeNoun("casa") must be equalTo "casas"
      tongue.pluralizeNoun("color") must be equalTo "colores"
      tongue.pluralizeNoun("avión") must be equalTo "aviones"
      tongue.pluralizeNoun("actriz") must be equalTo "actrices"
      tongue.pluralizeNoun("frac") must be equalTo "fraques"
      tongue.pluralizeNoun("zigzag") must be equalTo "zigzagues"
      tongue.pluralizeNoun("lunes") must be equalTo "lunes"
      tongue.pluralizeNoun("tórax") must be equalTo "tórax"
      tongue.pluralizeNoun("inglés") must be equalTo "ingleses"
      tongue.pluralizeNoun("sofá") must be equalTo "sofás"
      tongue.pluralizeNoun("esquí") must be equalTo "esquíes"

      // in certain cases, stress position changes
      tongue.pluralizeNoun("carácter") must be equalTo "caracteres"
      tongue.pluralizeNoun("espécimen") must be equalTo "especímenes"
      tongue.pluralizeNoun("régimen") must be equalTo "regímenes"

      // otherwise, preserve stress position
      // FIXME:  not working yet

      // tongue.pluralizeNoun("imagen") must be equalTo "imágenes"
    }

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

      pronounLemmas.size must be equalTo 61
    }

    "transform predefs" in
    {
      val forwardSize = SnlSpanishLexicon.predefToLemma.size
      val inverseSize = SnlSpanishLexicon.lemmaToPredef.size
      forwardSize must be equalTo inverseSize + 3

      val specialCases = SnlSpanishLexicon.predefToLemma.values.groupBy(x => x).
        filter(_._2.size > 1).map(_._1).toSet
      specialCases must be equalTo Set(LEMMA_NINGUNO, LEMMA_NI, LEMMA_OTRO)

      SprPredefWord(PD_NEITHER_NOUN).toLemma must be equalTo(
        LEMMA_NINGUNO)
      SprPredefWord(PD_NONE_NOUN).toLemma must be equalTo(
        LEMMA_NINGUNO)
      SprPredefWord(PD_NEITHER_DETERMINER).toLemma must be equalTo(
        LEMMA_NI)
      SprPredefWord(PD_NOR).toLemma must be equalTo(
        LEMMA_NI)
      SprPredefWord(PD_ANOTHER).toLemma must be equalTo(
        LEMMA_OTRO)
      SprPredefWord(PD_OTHER).toLemma must be equalTo(
        LEMMA_OTRO)

      val CORRECT = "correct"
      val INCORRECT = "incorrect"
      (SilWord(LEMMA_NINGUNO) match {
        case SprPredefWord(PD_NONE_NOUN) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT
      (SilWord(LEMMA_NI) match {
        case SprPredefWord(PD_NOR) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT
      (SilWord(LEMMA_OTRO) match {
        case SprPredefWord(PD_OTHER) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT

      (SilWord(LEMMA_NI) match {
        case SprPredefDeterminerWord(PD_NEITHER_DETERMINER) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT
      (SilWord(LEMMA_OTRO) match {
        case SprPredefDeterminerWord(PD_ANOTHER) => CORRECT
        case _ => INCORRECT
      }) must be equalTo CORRECT
    }
  }
}
