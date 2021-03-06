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

import SnlSpanishLemmas._
import SprPennTreebankLabels._

class SnlSpanishTongueSpec extends SprWordLabelerSpecification
{
  // FIXME minimize fallout from race conditions in extjwnl
  sequential

  private implicit val tongue = SnlUtils.spanishTongue

  trait SpanishLabelingContext extends SprLabelingContext
  {
    private val wordLabeler = new SprWordnetLabeler(tongue)

    override protected def labeler = wordLabeler
  }

  "SnlSpanishTongue" should
  {
    "label words" in new SpanishLabelingContext
    {
      labelWord("perro") must be equalTo Set(LABEL_NN)
      labelWord("perros") must be equalTo Set(LABEL_NNS)
      labelWord("cabra") must be equalTo Set(LABEL_NN)
      labelWord("cabras") must be equalTo Set(LABEL_NNS)
      labelWord("crisis") must be equalTo Set(LABEL_NN)
      labelWord("leones") must be equalTo Set(LABEL_NNS)
      labelWord("ir") must be equalTo Set(LABEL_VB)
      labelWord("caminar") must be equalTo Set(LABEL_NN, LABEL_VB)
      labelWord("camino") must be equalTo Set(LABEL_NN, LABEL_VB)
      labelWord("caminé") must be equalTo Set(LABEL_VBD)
      labelWord("caminaré") must be equalTo Set(LABEL_VBF)
      labelWord("rojo") must be equalTo Set(LABEL_JJ, LABEL_NN)
      labelWord("roto") must be equalTo Set(LABEL_JJ, LABEL_VBN)
      labelWord("dormido") must be equalTo Set(LABEL_JJ, LABEL_VBN)
      labelWord("duerme") must be equalTo Set(LABEL_VB)
      labelWord("hay") must be equalTo Set(LABEL_VB)
    }

    "lemmatize words" in new SpanishLabelingContext
    {
      lemmatizeNoun("perro") must be equalTo "perro"
      lemmatizeNoun("perros") must be equalTo "perro"
      lemmatizeAmbiguousNoun("leones") must be equalTo(
        Set("león", "leon", "leone"))
      lemmatizeAdjective("dormido") must be equalTo "dormido"
      lemmatizeAdjective("dormida") must be equalTo "dormido"
      lemmatizeVerb("duerme") must be equalTo "dormir"
      lemmatizeVerb("hay") must be equalTo "haber"
    }

    "pluralize words" in
    {
      // full tests are in morphala
      tongue.pluralizeNoun("perro") must be equalTo "perros"
    }

    "derive gender" in
    {
      // explicitly coded in our lexicon
      tongue.deriveGender(SilWord(
        "perro"
      )) must be equalTo GENDER_MASCULINE
      tongue.deriveGender(SilWord(
        "elefante"
      )) must be equalTo GENDER_MASCULINE
      tongue.deriveGender(SilWord(
        "perra"
      )) must be equalTo GENDER_FEMININE
      tongue.deriveGender(SilWord(
        "problema"
      )) must be equalTo GENDER_MASCULINE
      tongue.deriveGender(SilWord(
        "ciudad"
      )) must be equalTo GENDER_FEMININE

      // made-up regulars
      tongue.deriveGender(SilWord(
        "hydroxicloro"
      )) must be equalTo GENDER_MASCULINE
      tongue.deriveGender(SilWord(
        "hydroxiclore"
      )) must be equalTo GENDER_MASCULINE
      tongue.deriveGender(SilWord(
        "hydroxiclora"
      )) must be equalTo GENDER_FEMININE

      // made-up irregulars
      tongue.deriveGender(SilWord(
        "hydroxicloridad"
      )) must be equalTo GENDER_FEMININE
      tongue.deriveGender(SilWord(
        "hydroxiclorema"
      )) must be equalTo GENDER_MASCULINE
    }

    "produce pronoun lemmas" in
    {
      val pronounLemmas = tongue.getPronounLemmas

      // spot-check
      tongue.pronounLemma(
        PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
        PROXIMITY_ENTITY, SilPoliteness.DEFAULT, INFLECT_NOMINATIVE
      ) must be equalTo(LEMMA_YO)

      tongue.pronounLemma(
        PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
        PROXIMITY_ENTITY, POLITENESS_RESPECTFUL, INFLECT_NOMINATIVE
      ) must be equalTo(LEMMA_USTED)

      tongue.pronounLemma(
        PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL,
        PROXIMITY_ENTITY, SilPoliteness.DEFAULT, INFLECT_NOMINATIVE
      ) must be equalTo(LEMMA_NOSOTROS)

      pronounLemmas must contain(LEMMA_YO)
      pronounLemmas must contain(LEMMA_NOSOTROS)
      pronounLemmas must contain(LEMMA_USTED)

      pronounLemmas.foreach(lemma => {
        val (person, count, gender, inflections,
          proximityOpt, possesseeCount, politeness
        ) = tongue.analyzePronoun(lemma)
        inflections.foreach(inflection => {
          val lemmaProduced = tongue.pronounLemma(
            person, gender, count,
            proximityOpt.getOrElse(PROXIMITY_ENTITY),
            politeness, inflection, possesseeCount)
          lemmaProduced must be equalTo lemma
        })
      })

      pronounLemmas.size must be equalTo 63
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
