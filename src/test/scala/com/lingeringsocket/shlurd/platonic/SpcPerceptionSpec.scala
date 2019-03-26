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

import com.lingeringsocket.shlurd.mind._

import org.specs2.mutable._
import org.specs2.specification._

class SpcPerceptionSpec extends Specification
{
  trait PerceptionContext extends Scope
  {
    protected val noumenalCosmos = new SpcCosmos

    protected val phenomenalCosmos = new SpcCosmos

    protected val perception =
      new SpcPerception(noumenalCosmos, phenomenalCosmos)

    protected def process(input : String, cosmos : SpcCosmos) =
    {
      val sentence = cosmos.newParser(input).parseOne
      val mind = new SpcMind(cosmos)
      val responder = new SpcResponder(
        mind, ACCEPT_NEW_BELIEFS,
        SmcResponseParams(
          throwRejectedBeliefs = true,
          verbosity = RESPONSE_TERSE))
      responder.process(sentence)
    }

    protected def processNoumenal(input : String) =
    {
      process(input, noumenalCosmos)
    }

    protected def processPhenomenal(input : String) =
    {
      process(input, phenomenalCosmos)
    }

    protected def processBelief(input : String) =
    {
      processNoumenal(input) must be equalTo "OK."
    }

    protected def expectProperName(name : String) : SpcEntity =
    {
      expectUnique(
        noumenalCosmos.getEntities.filter(_.properName == name))
    }

    protected def expectUnique(
      entities : Iterable[SpcEntity]) : SpcEntity =
    {
      entities.size must be equalTo(1)
      entities.head
    }
  }

  "SpcPerception" should
  {
    "perceive phenomena" in new PerceptionContext
    {
      val timestamp = SpcTimestamp.ZERO

      processBelief("a pet must be an animal")
      processBelief("a person may have pets")
      processBelief("a person with a pet is an owner")
      processBelief("a girl is a kind of person")
      processBelief("a pig is a kind of animal")
      processBelief("Fern is a girl")
      processBelief("a pig may be hungry or full")
      phenomenalCosmos.copyFrom(noumenalCosmos)
      processBelief("Wilbur is a pig")
      processBelief("Wilbur is Fern's pet")
      processBelief("Wilbur is hungry")

      val wilbur = expectProperName("Wilbur")
      val fern = expectProperName("Fern")

      processNoumenal("is there a pig") must be equalTo "Yes."
      processNoumenal("is there a girl") must be equalTo "Yes."
      processNoumenal("does Fern have a pet") must be equalTo "Yes."
      processNoumenal("does Wilbur have an owner") must be equalTo "Yes."
      processNoumenal("is Wilbur hungry") must be equalTo "Yes."

      processPhenomenal("is there a pig") must be equalTo "No."
      processPhenomenal("is there a girl") must be equalTo "Yes."
      processPhenomenal("does Fern have a pet") must be equalTo "No."

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      perception.perceiveEntity(wilbur, timestamp)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      processPhenomenal("is there a pig") must be equalTo "Yes."
      processPhenomenal("is there a girl") must be equalTo "Yes."
      processPhenomenal("does Fern have a pet") must be equalTo "No."
      processPhenomenal("does Wilbur have an owner") must be equalTo "No."
      processPhenomenal("is Wilbur hungry") must be equalTo "I don't know."

      perception.perceiveEntityAssociations(fern, timestamp)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      processPhenomenal("is there a pig") must be equalTo "Yes."
      processPhenomenal("is there a girl") must be equalTo "Yes."
      processPhenomenal("does Fern have a pet") must be equalTo "Yes."
      processPhenomenal("does Wilbur have an owner") must be equalTo "Yes."
      processPhenomenal("is Wilbur hungry") must be equalTo "I don't know."

      perception.perceiveEntityProperties(wilbur, timestamp)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      processPhenomenal("is Wilbur hungry") must be equalTo "Yes."
    }

    "remember timestamps" in new PerceptionContext
    {
      val timestampZero = SpcTimestamp.ZERO
      val timestampOne = timestampZero.successor

      timestampZero must not be equalTo(timestampOne)
      timestampZero.isBefore(timestampOne) must beTrue
      timestampZero.isAfter(timestampOne) must beFalse
      timestampOne.isBefore(timestampZero) must beFalse
      timestampOne.isAfter(timestampZero) must beTrue

      processBelief("a pig is a kind of animal")
      phenomenalCosmos.copyFrom(noumenalCosmos)
      processBelief("Wilbur is a pig")
      processBelief("Babe is a pig")

      val wilbur = expectProperName("Wilbur")
      val babe = expectProperName("Babe")

      perception.perceiveEntity(wilbur, timestampZero)

      perception.getEntityTimestamp(wilbur) must beSome(timestampZero)
      perception.getEntityTimestamp(babe) must beNone

      perception.perceiveEntity(babe, timestampOne)

      perception.getEntityTimestamp(babe) must beSome(timestampOne)

      perception.perceiveEntity(wilbur, timestampOne)

      perception.getEntityTimestamp(wilbur) must beSome(timestampOne)
    }
  }
}