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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import SprPennTreebankLabels._

object SnlUtils
{
  val englishWordnet = SnlPrincetonWordnet

  lazy val spanishWordnet = new SnlExternalWordnet("/extjwnl_data_spa.xml")

  lazy val spanishEnglishAlignment = new SnlWordnetAlignment(
    "/com/lingeringsocket/extjwnl/data/mcr30-2016/spa/",
    spanishWordnet, englishWordnet)

  val defaultWordnet : SprWordnet = englishWordnet

  val englishTongue = new SnlEnglishTongue(englishWordnet)

  lazy val spanishTongue = new SnlSpanishTongue(spanishWordnet)

  val defaultTongue : SprTongue = englishTongue

  val defaultPhraseScorer = new SprWordnetScorer(defaultTongue)

  val defaultSentencePrinter = defaultTongue.newSentencePrinter(defaultTongue)

  def defaultWordLabeler = new SprWordnetLabeler(defaultTongue)

  val defaultContext = SprContext(
    defaultWordLabeler,
    defaultPhraseScorer,
    SilBasicAnnotator(),
    SilGenderPreserver)

  val stopListPunct = Set(
    LABEL_LPAREN, LABEL_RPAREN, LABEL_LCURLY, LABEL_RCURLY
  )

  def debug(s : String, context : SprContext = SnlUtils.defaultContext) : Unit =
  {
    SprParser.tokenize(s).foreach(sentence => {
      val parser = SprParser.prepareOne(
        context, sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }

  def readLexicon(resource : String) : Set[String] =
  {
    val words = ResourceUtils.getResourceSource(resource).getLines()
    Set(words.toSeq:_*)
  }

  def readGenderMap(resource : String) : Map[String, String] =
  {
    val entries = ResourceUtils.getResourceSource(resource).getLines()
    entries.toSeq.map(entry => {
      val i = entry.lastIndexOf(' ')
      val (word, noisy) = entry.splitAt(i)
      val gender = noisy.stripPrefix(" {").stripSuffix("}")
      assert(Set("m", "f", "mf", "mp", "fp", "mfp").contains(gender), gender)
      tupleN(
        word,
        gender
      )
    }).toMap
  }
}
