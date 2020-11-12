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
    politenessOverride : Option[SilPoliteness],
    scorerOpt : Option[SilPhraseScorer]) =
  {
    val translator = new SnlTranslator(
      annotator,
      alignment,
      direction,
      scorerOpt)
    val context = new SprContext(
      wordLabeler = new SprWordnetLabeler(translator.sourceTongue),
      scorer = new SnlTranslatingScorer(
        new SprWordnetScorer(translator.sourceTongue),
        translator.sourceTongue,
        translator.targetTongue,
        alignment,
        direction),
      genderAnalyzer = translator.sourceTongue)
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
    politenessOverride : Option[SilPoliteness] = None,
    scorerOpt : Option[SilPhraseScorer] = None) =
  {
    translate(
      english, TRANSLATE_SECOND_TO_FIRST, politenessOverride, scorerOpt
    ) must be equalTo(
      spanish
    )
  }

  private def checkSpanishToEnglish(
    english : String,
    spanish : String,
    politenessOverride : Option[SilPoliteness] = None,
    scorerOpt : Option[SilPhraseScorer] = None) =
  {
    translate(
      spanish, TRANSLATE_FIRST_TO_SECOND, politenessOverride, scorerOpt
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

  private class WordPreferenceScorer(
    nounLemma : String, verbLemma : String) extends SilPhraseScorer
  {
    override def computeLocalScore(phrase : SilPhrase) : SilPhraseScore =
    {
      phrase match {
        case ap : SilActionPredicate => {
          if (ap.verb.toLemma == verbLemma) {
            SilPhraseScore.proBig
          } else {
            SilPhraseScore.neutral
          }
        }
        case nr : SilNounReference => {
          if (nr.noun.toNounLemma == nounLemma) {
            SilPhraseScore.proBig
          } else {
            SilPhraseScore.neutral
          }
        }
        case _ => SilPhraseScore.neutral
      }
    }
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
        "she sneezes rapidly.",
        "ella estornuda rápido."
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
      checkEnglishToSpanish(
        "I give her a kiss.",
        "yo le doy un beso a ella."
      )
    }

    "translate dative pronoun to English" in
    {
      checkSpanishToEnglish(
        "I give her a kiss.",
        "yo le doy a ella un beso."
      )
      checkSpanishToEnglish(
        "I give him a kiss.",
        "yo le doy a él un beso."
      )
      checkSpanishToEnglish(
        "I give him a kiss.",
        "yo le doy un beso."
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
      // FIXME without a prior gendered referent, the translation should
      // use "I give her it"
      checkSpanishToEnglish(
        "I give her him.",
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

    "translate who" in
    {
      checkBidirectional(
        "who am I?",
        "quién soy yo?"
      )
    }

    "translate transitive" in
    {
      checkSpanishToEnglish(
        "it gets the axe.",
        "coge el hacha.")
    }

    "translate preferred word sense" in
    {
      checkSpanishToEnglish(
        "it throws the axe.",
        "tira el hacha.",
        scorerOpt = Some(new WordPreferenceScorer("axe", "throw")))
      checkSpanishToEnglish(
        "it pulls the hatchet.",
        "tira el hacha.",
        scorerOpt = Some(new WordPreferenceScorer("hatchet", "pull")))
    }

    "translate where" in
    {
      checkSpanishToEnglish(
        "where am I?",
        "dónde estoy yo?"
      )
      // FIXME should be estar instead
      checkEnglishToSpanish(
        "where am I?",
        "dónde soy yo?"
      )
    }
  }
}
