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

import edu.stanford.nlp.simple._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import eus.ixa.ixa.pipe.ml.tok._

import scala.collection.JavaConverters._

import java.util._
import java.io._

trait SprTokenizedSentence
{
  def text : String

  def tokens : Seq[String]
}

case class SprPlainTokenizedSentence(
  text : String,
  tokens : Seq[String]
) extends SprTokenizedSentence
{
}

class SprCorenlpTokenizedSentence(val corenlpSentence : Sentence)
    extends SprTokenizedSentence
{
  override def text = corenlpSentence.text

  override def tokens = corenlpSentence.originalTexts.asScala

  def lemmas = corenlpSentence.lemmas.asScala

  def incomingDeps =
  {
    val props = new Properties
    props.setProperty(
      "depparse.model",
      "edu/stanford/nlp/models/parser/nndep/english_SD.gz")
    corenlpSentence.incomingDependencyLabels(props).asScala.map(_.orElse(""))
  }
}

trait SprTokenizer
{
  def tokenize(input : String) : Seq[SprTokenizedSentence]
}

class SprCorenlpTokenizer extends SprTokenizer
{
  override def tokenize(input : String) : Seq[SprCorenlpTokenizedSentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala.map(sentence => {
      new SprCorenlpTokenizedSentence(sentence)
    })
  }
}

object SprIxaTokenizer
{
  val nullErr = new PrintStream(new OutputStream {
    def write(b : Int) {}
  })
}

class SprIxaTokenizer extends SprTokenizer
{
  import SprIxaTokenizer._

  override def tokenize(input : String) : Seq[SprTokenizedSentence] =
  {
    val savedErr = System.err
    System.setErr(nullErr)
    try {
      val properties = new java.util.Properties
      properties.put("language", "en")
      properties.put("untokenizable", "no")
      properties.put("hardParagraph", "no")
      val textSegment = RuleBasedSegmenter.readText(
        new BufferedReader(new StringReader(input)))
      val segmenter = new RuleBasedSegmenter(textSegment, properties)
      val tokenizer = new RuleBasedTokenizer(textSegment, properties)
      val sentences = segmenter.segmentSentence
      val tokenizedSentences = tokenizer.tokenize(sentences)
      assert(sentences.size == tokenizedSentences.size)
      sentences.zip(tokenizedSentences.asScala).map {
        case (sentence, tokenizedSentence) => {
          SprPlainTokenizedSentence(
            sentence.trim,
            tokenizedSentence.asScala.map(_.getTokenValue.trim))
        }
      }
    } finally {
      System.setErr(savedErr)
    }
  }
}
