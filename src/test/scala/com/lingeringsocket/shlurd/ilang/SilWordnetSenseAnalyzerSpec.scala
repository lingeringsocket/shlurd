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
  private val annotator = SilBasicAnnotator()

  private val analyzer = new SilWordnetSenseAnalyzer(annotator)

  private val pronounI =
    annotator.pronounRef(PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR)

  private val pronounIt =
    annotator.pronounRef(PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR)

  private def analyze(action : SilActionPredicate) : String =
  {
    analyzer.analyze(action).verb.senseId
  }

  private def analyzeComplement(
    verb : SilRelationshipPredicate) : String =
  {
    analyzer.analyze(verb).complement match {
      case SilOptionallyDeterminedReference(
        SilNounReference(noun), _
      ) => {
        noun.senseId
      }
      case _ => {
        ""
      }
    }
  }

  "SilWordnetSenseAnalyzer" should
  {
    "analyze simple action verb sense" in
    {
      val lemma = "snore"
      val senseId = "v:17024"
      val action = SilActionPredicate(
        pronounI,
        SilWord(lemma))
      analyze(action) must be equalTo senseId
      val actionWithSense = SilActionPredicate(
        pronounI,
        SilWord.withSense(lemma, senseId))
      analyze(actionWithSense) must be equalTo senseId
      val actionTransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma),
        Some(pronounIt))
      analyze(actionTransitive) must beEmpty
    }

    "analyze action verb transitivity sense" in
    {
      val lemma = "smoke"
      val senseIdIntransitive = "v:2773880"
      val senseIdTransitive = "v:1200739"
      val actionIntransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma))
      analyze(actionIntransitive).split('|').toSeq must
        containTheSameElementsAs(Seq(senseIdIntransitive, senseIdTransitive))
      val actionTransitive = SilActionPredicate(
        pronounI,
        SilWord(lemma),
        Some(pronounIt))
      analyze(actionTransitive) must be equalTo senseIdTransitive
    }

    "analyze ambiguous noun" in
    {
      val lemma = "whale"
      val identity = SilRelationshipPredicate(
        pronounI,
        REL_PREDEF_IDENTITY.toVerb,
        annotator.determinedNounRef(SilWord(lemma), DETERMINER_NONSPECIFIC)
      )
      analyzeComplement(identity) must be equalTo "n:10148670|n:2065397"
    }

    "analyze compound noun" in
    {
      val word = SilCompoundWord(Seq(
        SilWord("wise"),
        SilWord("guy")))
      val identity = SilRelationshipPredicate(
        pronounI,
        REL_PREDEF_IDENTITY.toVerb,
        annotator.determinedNounRef(word, DETERMINER_NONSPECIFIC)
      )
      analyzeComplement(identity) must be equalTo "n:10803789"
    }
  }
}
