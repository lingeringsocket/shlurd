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

import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

class SprBlackboardParserSpec extends Specification with SprEnglishWordAnalyzer
{
  private def parse(
    input : String,
    scorer : SilPhraseScorer = SilNeutralPhraseScorer,
    requireTopLevel : Boolean = true) : Seq[SprSyntaxTree] =
  {
    val tokenizer = SprWordnetParsingStrategy.newTokenizer

    val sentences = tokenizer.tokenize(input)
    assert(sentences.size == 1)
    val sentence = sentences.head

    val parser = new SprBlackboardParser(
      SprContext(),
      scorer,
      requireTopLevel,
      sentence.tokens.map(_.text),
      false
    )
    val result = parser.buildAll(parser.analyzeWords).toList
    if (false) {
      parser.displayGraph(result.toSet)
    }
    result
  }

  "SprBlackboardParser" should
  {
    "parse a noun phrase" in
    {
      val input = "a plover"
      parse(input, SilNeutralPhraseScorer, false) must be equalTo
        Seq(SptNP(
          SptDT(makeLeaf("a")),
          SptNN(makeLeaf("plover"))))
    }

    "parse a complete sentence" in
    {
      val input = "I am a plover"
      parse(input) must be equalTo
        Seq(
          SptS(
            SptNP(
              SptPRP(
                makeLeaf("I"))),
            SptVP(
              SptVBP(
                makeLeaf("am", "am", "be")),
              SptNP(
                SptDT(makeLeaf("a")),
                SptNN(makeLeaf("plover"))))))
    }

    "parse an ambiguous sentence" in
    {
      val input = "kill the thief with the ax"
      val instrumental =
        SptS(
          SptVP(
            SptVBP(
              makeLeaf("kill")),
            SptNP(
              SptDT(makeLeaf("the")),
              SptNN(makeLeaf("thief"))),
            SptPP(
              SptIN(
                makeLeaf("with")),
              SptNP(
                SptDT(makeLeaf("the")),
                SptNN(makeLeaf("ax"))))))
      val qualified =
        SptS(
          SptVP(
            SptVBP(
              makeLeaf("kill")),
            SptNP(
              SptNP(
                SptDT(makeLeaf("the")),
                SptNN(makeLeaf("thief"))),
              SptPP(
                SptIN(
                  makeLeaf("with")),
                SptNP(
                  SptDT(makeLeaf("the")),
                  SptNN(makeLeaf("ax")))))))

      // order is non-deterministic
      parse(input).filterNot(_.isInstanceOf[SptSQ]) must
        containTheSameElementsAs(Seq(instrumental, qualified))

      val preferInstrumental = new SilPhraseScorer {
        override def computeLocalScore(phrase : SilPhrase) =
        {
          phrase match {
            case _ : SilAdpositionalVerbModifier => {
              SilPhraseScore.proBig
            }
            case _ => {
              SilPhraseScore.neutral
            }
          }
        }
      }

      val preferQualified = new SilPhraseScorer {
        override def computeLocalScore(phrase : SilPhrase) =
        {
          phrase match {
            case _ : SilAdpositionalState => {
              SilPhraseScore.proBig
            }
            case _ => {
              SilPhraseScore.neutral
            }
          }
        }
      }

      parse(input, preferInstrumental).
        filterNot(_.isInstanceOf[SptSQ]) must be equalTo(
          Seq(instrumental, qualified))

      parse(input, preferQualified).
        filterNot(_.isInstanceOf[SptSQ]) must be equalTo(
          Seq(qualified, instrumental))
    }
  }
}
