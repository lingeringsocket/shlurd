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
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

class SprSpanishParserSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private val wordnet = new ShlurdExternalWordnet(
    ResourceUtils.getResourcePath("/nanonet"))

  private val context = SprContext(
    new SprWordnetLabeler(new SprSpanishWordAnalyzer(wordnet)),
    new SilWordnetScorer(new SprSpanishWordAnalyzer(wordnet))
  )

  private val NOUN_PEDRO = SilWord("Pedro")

  private val VERB_CAMINO = SilWord("camino", "caminar")

  private val VERB_CAMINA = SilWord("camina", "caminar")

  private def parse(input : String) =
  {
    SprParser(input, context).parseOne.sentence
  }

  "Spanish SprParser" should
  {
    "parse a simple sentence" in
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
  }
}
