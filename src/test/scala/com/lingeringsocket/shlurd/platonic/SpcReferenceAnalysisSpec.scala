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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._

class SpcReferenceAnalysisSpec extends Specification
{
  trait AnalysisContext extends Scope
  {
    protected val cosmos = new SpcCosmos

    protected def process(input : String, response : String) =
    {
      val mind = new SpcMind(cosmos)
      mind.startConversation
      mind.startNarrative
      val responder = new SpcResponder(
        mind, ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams(
          verbosity = RESPONSE_TERSE),
        new SmcExecutor[SpcEntity] {
          override def executeAction(
            predicate : SilActionPredicate,
            referenceMap : Map[SilReference, Set[SpcEntity]]
          ) : Option[String] =
          {
            Some("OK.")
          }

          override def executeImperative(
            predicate : SilPredicate,
            referenceMap : Map[SilReference, Set[SpcEntity]]
          ) : Option[String] =
          {
            Some("OK.")
          }
        })
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence) must be equalTo response
      mind.getConversation.getUtterances.head.referenceMap
    }

    protected def processBelief(input : String) =
    {
      process(input, "OK.")
    }

    protected def expectUnique(
      entities : Iterable[SpcEntity]) : SpcEntity =
    {
      entities.size must be equalTo(1)
      entities.head
    }

    protected def expectProperName(name : String) : SpcEntity =
    {
      expectUnique(
        cosmos.getEntities.filter(_.properName == name))
    }
  }

  "SpcReferenceAnalysis" should
  {
    "analyze references" in new AnalysisContext
    {
      val ivan = "Ivan"
      val boris = "Boris"
      val ivanRef = SilNounReference(SilWord(ivan))
      val borisRef = SilNounReference(SilWord(boris))
      processBelief("a mule is a kind of animal") must beEmpty
      processBelief("a mule may be grumpy or happy") must beEmpty
      processBelief("if a mule kicks a kick-target, " +
        "then equivalently the mule is grumpy")
      val isaResult = processBelief(s"$ivan is a mule")
      val ivanEntity = expectProperName(ivan)
      val ivanSet = Set(ivanEntity)
      isaResult must be equalTo Map(
        ivanRef -> ivanSet
      )
      processBelief("if a mule is happy, " +
        s"then the mule kicks $ivan")
      processBelief(s"$boris is a mule")
      val borisEntity = expectProperName(boris)
      val borisSet = Set(borisEntity)
      processBelief(s"$ivan is grumpy") must be equalTo Map(
        ivanRef -> ivanSet
      )
      process(s"is $ivan a mule", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet
      )
      process(s"does $ivan kick $boris", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet,
        borisRef -> borisSet
      )
      process(s"shout at $boris", "OK.") must be equalTo Map(
        borisRef -> borisSet
      )
      process(s"$boris is happy", "OK.") must be equalTo Map(
        borisRef -> borisSet
      )
    }
  }
}
