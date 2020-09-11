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

object SnlUtils
{
  val defaultWordnet = SnlPrincetonWordnet

  val defaultTongue = new SnlEnglishTongue(defaultWordnet)

  val defaultPhraseScorer = new SilWordnetScorer(defaultTongue)

  val defaultSentencePrinter = defaultTongue.newSentencePrinter(defaultTongue)

  def defaultWordLabeler = new SprWordnetLabeler(defaultTongue)

  val defaultContext = SprContext(
    defaultWordLabeler,
    defaultPhraseScorer,
    SilBasicAnnotator(),
    SilGenderPreserver)

  def debug(s : String)
  {
    SprParser.tokenize(s).foreach(sentence => {
      val parser = SprParser.prepareOne(
        SnlUtils.defaultContext, sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }
}
