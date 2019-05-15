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

import eus.ixa.ixa.pipe.ml.tok._

import scala.collection.JavaConverters._

import java.io._

case class SprToken(
  text : String,
  start : Int,
  end : Int
)

trait SprTokenizedSentence
{
  def text : String

  def tokens : Seq[SprToken]

  def offsetText : String
}

case class SprPlainTokenizedSentence(
  text : String,
  tokens : Seq[SprToken],
  offsetText : String
) extends SprTokenizedSentence
{
}

trait SprTokenizer
{
  def tokenize(input : String) : Seq[SprTokenizedSentence]
}

class SprIxaTokenizer extends SprTokenizer
{
  override def tokenize(input : String) : Seq[SprTokenizedSentence] =
  {
    val properties = new java.util.Properties
    properties.put("language", "en")
    properties.put("untokenizable", "no")
    properties.put("hardParagraph", "no")
    // ugh
    val folded = input.replace('_', '#')
    val textSegment = RuleBasedSegmenter.readText(
      new BufferedReader(new StringReader(folded)))
    val segmenter = new RuleBasedSegmenter(textSegment, properties)
    val tokenizer = new RuleBasedTokenizer(textSegment, properties)
    val sentences = segmenter.segmentSentence
    val tokenizedSentences = tokenizer.tokenize(sentences)
    assert(sentences.size == tokenizedSentences.size)
    sentences.zip(tokenizedSentences.asScala).map {
      case (sentence, tokenizedSentence) => {
        SprPlainTokenizedSentence(
          sentence.trim.replace('#', '_'),
          tokenizedSentence.asScala.map(
            t => SprToken(
              t.getTokenValue.trim.replace('#', '_'),
              t.startOffset,
              t.startOffset + t.tokenLength)),
          input)
      }
    }
  }
}
