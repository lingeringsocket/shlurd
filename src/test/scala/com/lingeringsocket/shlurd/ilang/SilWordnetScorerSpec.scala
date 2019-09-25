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
package com.lingeringsocket.shlurd.ilang

import org.specs2.mutable._

class SilWordnetScorerSpec extends Specification
{
  private val scorer = new SilWordnetScorer

  private val annotator = SilBasicAnnotator()

  private val pronoun =
    annotator.pronounRef(PERSON_FIRST, GENDER_FEMININE, COUNT_SINGULAR)

  private val genitiveInvalid = annotator.genitiveRef(pronoun, pronoun)

  "SilWordnetScorer" should
  {
    "score local phrase" in
    {
      scorer.computeLocalScore(pronoun) must be equalTo
        SilPhraseScore.neutral
      scorer.computeLocalScore(genitiveInvalid) must be equalTo
        SilPhraseScore.conBig
    }

    "score global phrase" in
    {
      val conjunction = annotator.conjunctiveRef(
        DETERMINER_ALL,
        Seq(genitiveInvalid, genitiveInvalid))
      scorer.computeLocalScore(conjunction) must be equalTo
        SilPhraseScore.conSmall
      scorer.computeGlobalScore(conjunction) must be equalTo
        (SilPhraseScore.conBig + SilPhraseScore.conBig +
          SilPhraseScore.conSmall)
    }

    "score transitive verbs" in
    {
      val actionTransitive = SilActionPredicate(
        pronoun,
        SilWord("kill"),
        Some(pronoun)
      )
      val actionIntransitive = SilActionPredicate(
        pronoun,
        SilWord("kill")
      )
      scorer.computeLocalScore(actionTransitive) must be greaterThan
        scorer.computeLocalScore(actionIntransitive)
    }

    "score verb frames" in
    {
      def score(adp : SilAdposition) = {
        scorer.computeLocalScore(
          SilActionPredicate(
            pronoun,
            SilWord("distinguish"),
            Some(pronoun),
            Seq(SilAdpositionalVerbModifier(adp, pronoun))))
      }
      score(SilAdposition.FROM) must be greaterThan score(SilAdposition.ON)
    }
  }
}
