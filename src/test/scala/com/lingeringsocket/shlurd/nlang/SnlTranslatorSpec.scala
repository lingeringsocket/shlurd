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

import org.specs2.mutable._

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

class SnlTranslatorSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private val alignment = SnlUtils.spanishEnglishAlignment

  private def translate(
    s : String,
    direction : SnlTranslationDirection,
    politenessOverride : Option[SilPoliteness]) =
  {
    val translator = new SnlTranslator(
      annotator,
      alignment,
      direction)
    val context = SprContext(translator.sourceTongue)
    val result = SprParser(s, context).parseOne
    val analyzer =
      new SprWordnetSenseAnalyzer(
        translator.sourceTongue,
        annotator)
    val analyzed = analyzer.analyze(result.sentence)
    val formalized = politenessOverride match {
      case Some(newPoliteness) => {
        analyzed.withNewTamFormality(
          analyzed.tam,
          analyzed.formality.copy(politeness = newPoliteness))
      }
      case _ => analyzed
    }
    val translatedSentence = translator.translate(formalized)
    val printer = translator.targetTongue.newSentencePrinter(
      translator.targetTongue)
    printer.print(translatedSentence)
  }

  private def checkEnglishToSpanish(
    english : String,
    spanish : String,
    politenessOverride : Option[SilPoliteness] = None) =
  {
    translate(
      english, TRANSLATE_SECOND_TO_FIRST, politenessOverride
    ) must be equalTo(
      spanish
    )
  }

  private def checkSpanishToEnglish(
    english : String,
    spanish : String,
    politenessOverride : Option[SilPoliteness] = None) =
  {
    translate(
      spanish, TRANSLATE_FIRST_TO_SECOND, politenessOverride
    ) must be equalTo(
      english
    )
  }

  private def checkBidirectional(
    english : String,
    spanish : String,
    politenessOverride : Option[SilPoliteness] = None) =
  {
    checkEnglishToSpanish(english, spanish, politenessOverride)
    checkSpanishToEnglish(english, spanish, politenessOverride)
  }

  "SnlTranslator" should
  {
    "translate sentences" in
    {
      checkBidirectional(
        "the man kills a dog.",
        "el hombre mata un perro."
      )
    }

    "translate pronouns" in
    {
      checkBidirectional(
        "I kill a dog.",
        "yo mato un perro."
      )
    }

    "expand elided pronouns" in
    {
      checkSpanishToEnglish(
        "they drink lemonade.",
        "beben limonada."
      )
    }

    "translate adpositions" in
    {
      checkBidirectional(
        "we float in the ocean.",
        "nosotros flotamos en el mar."
      )
    }

    "translate conjunctions" in
    {
      checkBidirectional(
        "the dog and the cat are asleep.",
        "el perro y el gato están dormidos."
      )
    }

    "translate adjectives" in
    {
      checkBidirectional(
        "Charlie is asleep.",
        "Charlie está dormido."
      )
    }

    "translate adverbs" in
    {
      checkBidirectional(
        "she draws rapidly.",
        "ella dibuja rápido."
      )
    }

    "translate past tense" in
    {
      checkBidirectional(
        "the dog barked.",
        "el perro ladró."
      )
    }

    "translate personal a" in
    {
      checkBidirectional(
        "I pommel Littlefinger.",
        "yo abofeteo a Littlefinger."
      )
    }

    "translate dative pronoun to Spanish" in
    {
      skipped("need to expand dative")
      checkEnglishToSpanish(
        "I give her a kiss.",
        "yo le doy a ella un beso."
      )
    }

    "translate dative pronoun to English" in
    {
      skipped("need to rearrange the pronouns")
      checkSpanishToEnglish(
        "I give her a kiss.",
        "yo le doy a ella un beso."
      )
    }

    "translate indirect object pronoun to Spanish" in
    {
      skipped("need to infer dative case based on verb")
      checkEnglishToSpanish(
        "I give it to her.",
        "yo se lo doy a ella."
      )
    }

    "translate indirect object pronoun to English" in
    {
      skipped("need to rearrange the pronouns")
      checkSpanishToEnglish(
        "I give it to her.",
        "yo se lo doy a ella."
      )
    }

    "translate imperative polite" in
    {
      checkBidirectional(
        "kill the fly.",
        "mate la mosca.",
        Some(POLITENESS_RESPECTFUL)
      )
    }

    "translate imperative familiar" in
    {
      // for SpanishToEnglish, it would be indistiguishable from indicative
      checkEnglishToSpanish(
        "kill the fly.",
        "mata la mosca."
      )
    }

    "translate progressive" in
    {
      checkBidirectional(
        "the cat is flaying the dog.",
        "el gato está desollando el perro."
      )
    }

    "translate second person familiar" in
    {
      checkBidirectional(
        "you are attractive.",
        "tú estás atractivo."
      )
    }

    "translate second person polite" in
    {
      checkSpanishToEnglish(
        "you are attractive.",
        "usted está atractivo."
      )
    }

    "translate second person polite to Spanish" in
    {
      skipped("need to override politeness top down")
      checkEnglishToSpanish(
        "you are attractive.",
        "usted está atractivo.",
        Some(POLITENESS_RESPECTFUL)
      )
    }

    "translate second person direct object pronoun" in
    {
      checkBidirectional(
        "I love you.",
        "yo te amo."
      )
    }

    "translate intransitive progressive" in
    {
      skipped("not working yet")
      checkBidirectional(
        "the dog is barking.",
        "el perro está ladrando."
      )
    }

    "translate modal" in
    {
      checkBidirectional(
        "we must go.",
        "nosotros tenemos que ir."
      )
    }
  }
}
