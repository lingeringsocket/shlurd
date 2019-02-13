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

class SilWordnetSenseAnalyzerSpec extends Specification
{
  private val analyzer = new SilWordnetSenseAnalyzer

  private val pronounI =
    SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)

  private val pronounIt =
    SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_SINGULAR)

  private def analyze(action : SilActionPredicate) : String =
  {
    analyzer.analyze(action).action.senseKey
  }

  "SilWordnetSenseAnalyzer" should
  {
    "analyze simple action verb sense" in
    {
      val lemma = "snore"
      val senseKey = "v:17024"
      val action = SilActionPredicate(
        pronounI,
        SilWord(lemma))
      analyze(action) must be equalTo senseKey
      val actionWithSense = SilActionPredicate(
        pronounI,
        SilWord.withSense(lemma, senseKey))
      analyze(actionWithSense) must be equalTo senseKey
      val actionTransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma),
        Some(pronounIt))
      analyze(actionTransitive) must beEmpty
    }

    "analyze action verb transitivity sense" in
    {
      val lemma = "smoke"
      val senseKeyIntransitive = "v:2773880"
      val senseKeyTransitive = "v:1200739"
      val actionIntransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma))
      analyze(actionIntransitive).split('|').toSeq must
        containTheSameElementsAs(Seq(senseKeyIntransitive, senseKeyTransitive))
      val actionTransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma),
        Some(pronounIt))
      analyze(actionTransitive) must be equalTo senseKeyTransitive
    }
  }
}
