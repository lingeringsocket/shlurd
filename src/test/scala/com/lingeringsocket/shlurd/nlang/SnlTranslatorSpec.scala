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

  private def translate(s : String, direction : SnlTranslationDirection) =
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
    // FIXME need full analysis
    val analyzed = analyzer.analyze(result.sentence)
    val translatedSentence = translator.translate(analyzed)
    val printer = translator.targetTongue.newSentencePrinter(
      translator.targetTongue)
    printer.print(translatedSentence)
  }

  private def checkTranslation(english : String, spanish : String) =
  {
    translate(english, TRANSLATE_SECOND_TO_FIRST) must be equalTo spanish
    translate(spanish, TRANSLATE_FIRST_TO_SECOND) must be equalTo english
  }

  "SnlTranslator" should
  {
    "translate sentences" in
    {
      checkTranslation(
        "the man kills a dog.",
        "el hombre mata un perro."
      )
    }

    "translate pronouns" in
    {
      checkTranslation(
        "I kill a dog.",
        "yo mato un perro."
      )
    }
  }
}
