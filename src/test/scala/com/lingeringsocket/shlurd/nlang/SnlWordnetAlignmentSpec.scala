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

import net.sf.extjwnl.data._

import org.specs2.mutable._

class SnlWordnetAlignmentSpec extends Specification
{
  private val alignment = SnlUtils.spanishEnglishAlignment

  private def checkMapping(pos : POS, spanish : String, english : String) =
  {
    val spanishSenses = alignment.getFirstWordnet.getWordSenses(pos, spanish)
    spanishSenses.size must be equalTo 1
    val spanishSynset = spanishSenses.head
    val englishSynsetOpt = alignment.mapSense(
      spanishSynset, TRANSLATE_FIRST_TO_SECOND)
    val result = englishSynsetOpt must beSome.which(_.containsWord(english))
    englishSynsetOpt.foreach(englishSynset => {
      val spanishSynsetOpt = alignment.mapSense(
        englishSynset, TRANSLATE_SECOND_TO_FIRST)
      spanishSynsetOpt must beSome(spanishSynset)
    })
    result
  }

  "SnlWordnetAlignment" should
  {
    "map word senses between Spanish and English" in
    {
      checkMapping(POS.NOUN, "claustrofobia", "claustrophobia")
      checkMapping(POS.VERB, "defenestrar", "defenestrate")
      checkMapping(POS.ADJECTIVE, "picaresco", "picaresque")
      checkMapping(POS.ADVERB, "científicamente", "scientifically")
    }
  }
}
