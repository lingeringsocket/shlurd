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

  private val analyzer =
    new SprWordnetSenseAnalyzer(
      alignment.getSecondTongue,
      annotator)

  private val printer = alignment.getFirstTongue.newSentencePrinter(
    alignment.getFirstTongue)

  private val translator = new SnlTranslator(
    annotator,
    alignment)

  private def translate(s : String) =
  {
    val result = SprParser(s, SnlUtils.defaultContext).parseOne
    // FIXME need full analysis
    val analyzed = analyzer.analyze(result.sentence)
    val translatedSentence = translator.translate(
      analyzed, TRANSLATE_SECOND_TO_FIRST)
    printer.print(translatedSentence)
  }

  "SnlTranslator" should
  {
    "translate sentences" in
    {
      translate(
        "the man kisses a salamander."
      ) must be equalTo(
        "el hombre besa una salamandra."
      )
    }
  }
}
