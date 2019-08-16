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

import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._

class SmcScopeSpec extends Specification
{
  type CosmosType = ZooCosmos
  type MindType = ZooMind
  type ReferenceMap = Map[SilReference, Set[SmcEntity]]

  private val cosmos = new ZooCosmos

  private val unresolvedMsg = "pronoun cannot be resolved"

  abstract class ScopeContext extends Scope
  {
    protected val mind = new ZooMind(cosmos)

    protected val communicationContext =
      SmcCommunicationContext[SmcEntity](
        Some(ZooVisitor),
        Some(ZooKeeper))
    protected val mindScope = new SmcMindScope[
      SmcEntity, SmcProperty, ZooCosmos, ZooMind](mind)
  }

  "SmcScope" should
  {
    "resolve pronouns" in new ScopeContext
    {
      val firstPersonSingular =
        SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR)
      val thirdPersonSingular =
        SilPronounReference(PERSON_THIRD, GENDER_N, COUNT_SINGULAR)
      mindScope.resolvePronoun(
        communicationContext,
        firstPersonSingular
      ) must beSuccessfulTry.withValue(SmcScopeOutput(None, Set(ZooVisitor)))
      mindScope.resolvePronoun(
        communicationContext,
        thirdPersonSingular
      ) must beFailedTry.withThrowable[RuntimeException](unresolvedMsg)
      val nigelRef = SilNounReference(SilWord("Nigel"))
      val referenceMap = Map[SilReference, Set[SmcEntity]](
        nigelRef -> Set(ZooKeeper)
      )
      val phraseScope = new SmcPhraseScope(referenceMap, mindScope)
      phraseScope.resolvePronoun(
        communicationContext,
        thirdPersonSingular
      ) must beSuccessfulTry.withValue(
        SmcScopeOutput(Some(nigelRef), Set(ZooKeeper))
      )
    }
  }
}

