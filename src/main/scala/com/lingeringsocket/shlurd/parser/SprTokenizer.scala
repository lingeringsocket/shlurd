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

import eus.ixa.ixa.pipe.ml.tok._

import scala.collection._
import scala.jdk.CollectionConverters._

import java.io._

import SprPennTreebankLabels._

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

object SprIxaTokenizer
{
  private val HIDDEN_DOT = '\u00b7'
}

class SprIxaTokenizer extends SprTokenizer
{
  import SprIxaTokenizer._

  override def tokenize(input : String) : Seq[SprTokenizedSentence] =
  {
    val properties = new java.util.Properties
    properties.put("language", "en")
    properties.put("untokenizable", "no")
    properties.put("hardParagraph", "no")
    // ugh
    val folded = fold(input)
    val textSegment = RuleBasedSegmenter.readText(
      new BufferedReader(new StringReader(folded)))
    val segmenter = new RuleBasedSegmenter(textSegment, properties)
    val tokenizer = new RuleBasedTokenizer(unfoldDot(textSegment), properties)
    val sentences = segmenter.segmentSentence.map(unfoldDot)
    val tokenizedSentences = tokenizer.tokenize(sentences)
    assert(sentences.size == tokenizedSentences.size)
    sentences.zip(tokenizedSentences.asScala).map {
      case (sentence, tokenizedSentence) => {
        SprPlainTokenizedSentence(
          unfoldHash(sentence),
          fixPossessives(
            tokenizedSentence.asScala.map(
              t => SprToken(
                unfoldHash(t.getTokenValue).
                  replace("(", LABEL_LPAREN).replace(")", LABEL_RPAREN).
                  replace("{", LABEL_LCURLY).replace("}", LABEL_RCURLY),
                t.startOffset,
                t.startOffset + t.tokenLength))),
          input)
      }
    }
  }

  private def fold(input : String) : String =
  {
    hideDotsInsideQuotes(input.replace('_', '#'))
  }

  private def unfoldHash(input : String) : String =
  {
    input.trim.replace('#', '_')
  }

  private def unfoldDot(input : String) : String =
  {
    input.replace(HIDDEN_DOT, '.')
  }

  private def hideDotsInsideQuotes(input : String) : String =
  {
    var inside = false
    val sb = new mutable.StringBuilder(input.size)
    input.foreach(c => {
      c match {
        case DQUOTE_CHAR => {
          inside = !inside
          sb += c
        }
        case '.' => {
          if (inside) {
            sb += HIDDEN_DOT
          } else {
            sb += c
          }
        }
        case _ => {
          sb += c
        }
      }
    })
    sb.toString
  }

  private def fixPossessives(seq : Seq[SprToken]) : Seq[SprToken] =
  {
    val pattern = Seq("'", "s")
    val prefix = seq.take(2)
    if (seq.isEmpty) {
      seq
    } else if (prefix.map(_.text) == pattern) {
      SprToken("'s", prefix.head.start, prefix.last.end) +:
        fixPossessives(seq.drop(2))
    } else {
      seq.head +: fixPossessives(seq.tail)
    }
  }
}
