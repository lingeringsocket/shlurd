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

class SprHeuristicSynthesizerSpec
    extends Specification with SprEnglishWordAnalyzer
{
  private def parse(
    input : String,
    scorer : SilPhraseScorer = SilNeutralPhraseScorer,
    filter : SprHeuristicFilter = SprHeuristicAcceptCompleteSentence,
    stamina : SprHeuristicStamina = HEURISTIC_STAMINA_COMPLETE)
      : Seq[SprSyntaxTree] =
  {
    val tokenizer = SprHeuristicParsingStrategy.newTokenizer

    val sentences = tokenizer.tokenize(input)
    assert(sentences.size == 1)
    val sentence = sentences.head

    val synthesizer = new SprHeuristicSynthesizer(
      SprContext(scorer = scorer),
      filter,
      stamina,
      sentence.tokens.map(_.text)
    )
    val result = synthesizer.synthesize(synthesizer.analyzeWords).toList
    if (false) {
      synthesizer.displayGraph(result.toSet)
    }
    result
  }

  "SprHeuristicSynthesizer" should
  {
    "parse a noun phrase" in
    {
      val input = "a plover"
      parse(input, SilNeutralPhraseScorer, SprHeuristicAcceptAll) must
      be equalTo
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

      val rejectSQ = new SprHeuristicFilter
      {
        override def accept(
          tree : SprSyntaxTree,
          replacement : SilPhrase) : Boolean =
        {
          tree match {
            case _ : SptSQ => false
            case _ => SprHeuristicAcceptCompleteSentence.accept(
              tree, replacement)
          }
        }
      }

      // order is non-deterministic
      parse(input, SilNeutralPhraseScorer, rejectSQ) must
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

      parse(input, preferInstrumental, rejectSQ) must
        be equalTo(
          Seq(instrumental, qualified))

      parse(
        input, preferInstrumental,
        rejectSQ, HEURISTIC_STAMINA_STOP_AFTER_FIRST) must be equalTo(
          Seq(instrumental))

      parse(input, preferQualified,
        rejectSQ, HEURISTIC_STAMINA_STOP_AFTER_FIRST) must be equalTo(
          Seq(qualified))
    }
  }
}
