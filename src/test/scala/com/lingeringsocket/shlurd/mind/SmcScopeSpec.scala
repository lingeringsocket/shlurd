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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._

import SprEnglishLemmas._

class SmcScopeSpec extends Specification
{
  type CosmosType = ZooCosmos
  type MindType = ZooMind
  type RefMap = SmcRefMap[SmcEntity]

  private val cosmos = new ZooCosmos

  private val noEntities = Set[SmcEntity]()

  private val unresolvedMsg =
    "Sorry, when you say 'it' I don't know who or what you mean."

  private val ambiguousMsg =
    "Sorry, when you say 'it', it's ambiguous."

  private val sentencePrinter = new SilSentencePrinter

  private val ordinalFirst = sentencePrinter.sb.ordinalNumber(1)

  private val ordinalSecond = sentencePrinter.sb.ordinalNumber(2)

  abstract class ScopeContext extends Scope
  {
    protected val mind = new ZooMind(cosmos)

    protected val annotator = SmcAnnotator[SmcEntity]()

    protected val firstPersonRef =
      annotator.pronounRef(PERSON_FIRST, GENDER_SOMEONE, COUNT_SINGULAR, mind)

    protected val thirdPersonRef =
      annotator.pronounRef(PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR, mind)

    protected val nigelRef = annotator.nounRef(SilWord("Nigel"))

    protected val cliveRef = annotator.nounRef(SilWord("Clive"))

    protected val tigerRef =
      annotator.determinedRef(
        annotator.nounRef(SilWord("tiger")), DETERMINER_NONSPECIFIC)

    protected val anotherTigerRef =
      annotator.stateSpecifiedRef(
        annotator.nounRef(SilWord("tiger")),
        SilPropertyState(SilWord(LEMMA_ANOTHER)))

    protected val communicationContext =
      SmcCommunicationContext[SmcEntity](
        Some(ZooVisitor),
        Some(ZooKeeper))
    protected val mindScope = new SmcMindScope[
      SmcEntity, SmcProperty, ZooCosmos, ZooMind](
      mind, sentencePrinter
    )
  }

  "SmcScope" should
  {
    "resolve nouns at mind scope"  in new ScopeContext
    {
      mindScope.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set.empty
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(None, Set(ZooTiger))
      )
      mindScope.resolveQualifiedNoun(
        SilWord("bear"),
        REF_SUBJECT,
        Set.empty
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(None, Set(ZooPolarBear, ZooGrizzlyBear))
      )
      mindScope.resolveQualifiedNoun(
        SilWord("bear"),
        REF_SUBJECT,
        Set("polar")
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(None, Set(ZooPolarBear))
      )
      mindScope.resolveQualifiedNoun(
        SilWord("bear"),
        REF_SUBJECT,
        Set(ordinalFirst)
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(None, noEntities)
      )
    }

    "fail to resolve unknown noun"  in new ScopeContext
    {
      mindScope.resolveQualifiedNoun(
        SilWord("quagga"),
        REF_SUBJECT,
        Set.empty
      ) must beFailedTry.withThrowable[RuntimeException](
        "I don't know about this name: quagga"
      )
    }

    "resolve nouns at phrase scope" in new ScopeContext
    {
      val oneRefMap = SmcRefMap[SmcEntity](
        tigerRef -> Set()
      )
      val twoRefMap = SmcRefMap[SmcEntity](
        tigerRef -> Set(),
        anotherTigerRef -> Set()
      )
      val phraseScope1 = new SmcPhraseScope(oneRefMap, mindScope)
      val phraseScope2 = new SmcPhraseScope(twoRefMap, mindScope)
      phraseScope1.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set.empty
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(tigerRef), noEntities)
      )
      phraseScope2.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set.empty
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(tigerRef), noEntities)
      )
      phraseScope2.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set(ordinalFirst)
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(tigerRef), noEntities)
      )
      phraseScope2.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set(ordinalSecond)
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(anotherTigerRef), noEntities)
      )
      phraseScope1.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set(ordinalFirst)
      ) must beFailedTry.withThrowable[ShlurdException](
        "Sorry, when you say 'first tiger', I don't know which you mean."
      )
      phraseScope1.resolveQualifiedNoun(
        SilWord("tiger"),
        REF_SUBJECT,
        Set(ordinalSecond)
      ) must beFailedTry.withThrowable[ShlurdException](
        "Sorry, when you say 'second tiger', I don't know which you mean."
      )
    }

    "resolve pronouns at mind scope" in new ScopeContext
    {
      mindScope.resolvePronoun(
        annotator,
        communicationContext,
        firstPersonRef
      ) must beSuccessfulTry.withValue(SmcScopeOutput(None, Set(ZooVisitor)))
    }

    "resolve spatial deixis at mind scope" in new ScopeContext
    {
      mindScope.resolveSpatialDeictic(
        annotator,
        communicationContext,
        SilWord(LEMMA_HERE),
        DISTANCE_HERE
      ) must beSuccessfulTry.withValue(SmcScopeOutput(None, Set(ZooVisitor)))
      mindScope.resolveSpatialDeictic(
        annotator,
        communicationContext,
        SilWord(LEMMA_THERE),
        DISTANCE_THERE
      ) must beSuccessfulTry.withValue(SmcScopeOutput(None, Set(ZooKeeper)))
    }

    "fail to resolve pronoun without antecedent" in new ScopeContext
    {
      mindScope.resolvePronoun(
        annotator,
        communicationContext,
        thirdPersonRef
      ) must beFailedTry.withThrowable[ShlurdException](unresolvedMsg)
    }

    "resolve pronouns at phrase scope" in new ScopeContext
    {
      val refMap = SmcRefMap[SmcEntity](
        nigelRef -> Set(ZooKeeper)
      )
      val phraseScope = new SmcPhraseScope(refMap, mindScope)
      phraseScope.resolvePronoun(
        annotator,
        communicationContext,
        thirdPersonRef
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(nigelRef), Set(ZooKeeper))
      )
    }

    "fail to resolve ambiguous pronoun" in new ScopeContext
    {
      val refMap = SmcRefMap[SmcEntity](
        nigelRef -> Set(ZooKeeper),
        cliveRef -> Set(ZooVisitor)
      )
      val phraseScope = new SmcPhraseScope(refMap, mindScope)
      phraseScope.resolvePronoun(
        annotator,
        communicationContext,
        thirdPersonRef
      ) must beFailedTry.withThrowable[ShlurdException](ambiguousMsg)
    }
  }
}
