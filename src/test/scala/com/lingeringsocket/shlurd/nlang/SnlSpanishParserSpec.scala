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

class SnlSpanishParserSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private val wordnet = new SnlExternalWordnet(
    "/spanish_net.xml")

  private val context = SprContext(new SnlSpanishTongue(wordnet))

  private val NOUN_PEDRO = SilWord("Pedro")

  private val NOUN_PERRO = SilWord("perro")

  private val VERB_CAMINO = SilWord("camino", "caminar")

  private val VERB_CAMINA = SilWord("camina", "caminar")

  private val VERB_CAMINANDO = SilWord("caminando", "caminar")

  private val VERB_BEBO = SilWord("bebo", "beber")

  private val VERB_BEBISTE = SilWord("bebiste", "beber")

  private val VERB_BEBERA = SilWord("beberá", "beber")

  private val VERB_ES = SilWord("es", "ser")

  private val VERB_ESTOY = SilWord("estoy", "estar")

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
  }
}
