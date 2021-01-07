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

class SnlSpanishParserSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private val context = SprContext(SnlUtils.spanishTongue)

  private val NOUN_PEDRO = SilWord("Pedro")

  private val NOUN_PERRO = SilWord("perro")

  private val NOUN_DONDE = SilWord("dónde")

  private val VERB_CAMINO = SilWord("camino", "caminar")

  private val VERB_CAMINA = SilWord("camina", "caminar")

  private val VERB_CAMINANDO = SilWord("caminando", "caminar")

  private val VERB_BEBO = SilWord("bebo", "beber")

  private val VERB_BEBISTE = SilWord("bebiste", "beber")

  private val VERB_BEBERA = SilWord("beberá", "beber")

  private val VERB_BESA = SilWord("besa", "besar")

  private val VERB_VIVIR = SilWord("vivir")

  private val VERB_ES = SilWord("es", LEMMA_SER)

  private val VERB_ESTAR = SilWord(LEMMA_ESTAR)

  private val VERB_ESTOY = SilWord("estoy", LEMMA_ESTAR)

  private val VERB_ESTA = SilWord("está", LEMMA_ESTAR)

  private val VERB_HAY = SilWord("hay", LEMMA_HABER)

  private val VERB_AFEITO_ME = SilWord("afeito", "afeitarse")

  private val VERB_AFEITAS_TE = SilWord("afeitas", "afeitarse")

  private val VERB_AFEITA_SE = SilWord("afeita", "afeitarse")

  private val VERB_AFEITAMOS_NOS = SilWord("afeitamos", "afeitarse")

  private val STATE_TRISTE = SilWord("triste")

  private def parse(input : String) =
  {
    SprParser(input, context).parseOne.sentence
  }

  "Spanish SprParser" should
  {
    "parse an action sentence" in
    {
      val input = "Pedro camina"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.nounRef(NOUN_PEDRO),
            VERB_CAMINA
          )
        )
    }

    "parse a progressive sentence" in
    {
      val input = "estoy caminando"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
              PROXIMITY_ELIDED),
            VERB_CAMINANDO
          ),
          SilTam.indicative.progressive
        )
    }

    "parse a preterite sentence" in
    {
      val input = "bebiste"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
              PROXIMITY_ELIDED),
            VERB_BEBISTE
          ),
          SilTam.indicative.past
        )
    }

    "parse a future sentence" in
    {
      val input = "beberá"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR,
              PROXIMITY_ELIDED),
            VERB_BEBERA
          ),
          SilTam.indicative.future
        )
    }

    "parse an identity relation sentence" in
    {
      val input = "Pedro es un perro"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilRelationshipPredicate(
            annotator.nounRef(NOUN_PEDRO),
            VERB_ES,
            annotator.determinedNounRef(
              NOUN_PERRO,
              DETERMINER_NONSPECIFIC
            )
          )
        )
    }

    "parse a property state sentence" in
    {
      val input = "yo estoy triste"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR),
            VERB_ESTOY,
            SilPropertyState(STATE_TRISTE)
          )
        )
    }

    "parse a pronoun" in
    {
      val input = "yo camino"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR),
            VERB_CAMINO
          )
        )
    }

    "parse another pronoun" in
    {
      val input = "ella camina"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR),
            VERB_CAMINA
          )
        )
    }

    "parse a polite pronoun" in
    {
      val input = "usted camina"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
              politeness = POLITENESS_RESPECTFUL),
            VERB_CAMINA
          ),
          formality = SilFormality(politeness = POLITENESS_RESPECTFUL)
        )
    }

    "parse a conjugation which is the same for -ar and -er" in
    {
      val input = "yo bebo"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR),
            VERB_BEBO
          )
        )
    }

    "parse a modal" in
    {
      val input = "debo de vivir"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR,
              PROXIMITY_ELIDED),
            VERB_VIVIR
          ),
          SilTam.indicative.withModality(MODAL_SHOULD)
        )
    }

    "parse another modal" in
    {
      val input = "tienen que vivir"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_THIRD, GENDER_NEUTER, COUNT_PLURAL,
              PROXIMITY_ELIDED),
            VERB_VIVIR
          ),
          SilTam.indicative.withModality(MODAL_MUST)
        )
    }

    "parse a modal without an adposition" in
    {
      val input = "puedes vivir"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR,
              PROXIMITY_ELIDED),
            VERB_VIVIR
          ),
          SilTam.indicative.withModality(MODAL_CAPABLE)
        )
    }

    "parse modal may" in
    {
      val input = "un perro puede estar triste"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            annotator.determinedNounRef(
              NOUN_PERRO,
              DETERMINER_NONSPECIFIC
            ),
            VERB_ESTAR,
            SilPropertyState(STATE_TRISTE)
          ),
          SilTam.indicative.withModality(MODAL_MAY)
        )
    }

    "parse impersonal haber" in
    {
      val input = "hay un perro"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            annotator.determinedNounRef(
              NOUN_PERRO,
              DETERMINER_NONSPECIFIC
            ),
            VERB_HAY,
            SilExistenceState(Some(VERB_HAY))
          )
        )
    }

    "parse personal a" in
    {
      val input = "un perro besa a Pedro"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.determinedNounRef(
              NOUN_PERRO,
              DETERMINER_NONSPECIFIC
            ),
            VERB_BESA,
            Some(annotator.nounRef(NOUN_PEDRO))
          )
        )
    }

    "parse reflexives" in
    {
      parse("nos afeitamos") must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_PLURAL, PROXIMITY_ELIDED),
            VERB_AFEITAMOS_NOS
          )
        )
      parse("me afeito") must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR, PROXIMITY_ELIDED),
            VERB_AFEITO_ME
          )
        )
      parse("te afeitas") must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR, PROXIMITY_ELIDED),
            VERB_AFEITAS_TE
          )
        )
      parse("nosotros nos afeitamos") must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_FIRST, GENDER_MASCULINE, COUNT_PLURAL),
            VERB_AFEITAMOS_NOS
          )
        )
      parse("Pedro se afeita") must be equalTo
        SilPredicateSentence(
          SilActionPredicate(
            annotator.nounRef(NOUN_PEDRO),
            VERB_AFEITA_SE
          )
        )
    }

    "parse where" in
    {
      val input = "dónde está Pedro?"
      parse(input) must be equalTo
        SilPredicateQuery(
          SilRelationshipPredicate(
            annotator.nounRef(NOUN_DONDE),
            VERB_ESTA,
            annotator.nounRef(NOUN_PEDRO)
          ),
          QUESTION_WHERE,
          INFLECT_NOMINATIVE,
          SilTam.interrogative
        )
    }

    "reject ungrammatical sentences" in
    {
      // this one is fine
      parse("debo de vivir").hasUnknown must beFalse

      // but this one is not
      parse("debo que vivir").hasUnknown must beTrue

      // nor is this one
      parse("ella me lo da un beso").hasUnknown must beTrue
    }
  }
}
