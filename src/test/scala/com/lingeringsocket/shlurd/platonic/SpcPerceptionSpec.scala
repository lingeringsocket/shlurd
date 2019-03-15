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

    protected def interpret(input : String, cosmos : SpcCosmos) =
    {
      val sentence = cosmos.newParser(input).parseOne
      val mind = new SpcMind(cosmos)
      val interpreter = new SpcInterpreter(
        mind, ACCEPT_NEW_BELIEFS,
        SmcResponseParams(
          throwRejectedBeliefs = true,
          verbosity = RESPONSE_TERSE))
      interpreter.interpret(sentence)
    }

    protected def interpretNoumenal(input : String) =
    {
      interpret(input, noumenalCosmos)
    }

    protected def interpretPhenomenal(input : String) =
    {
      interpret(input, phenomenalCosmos)
    }

    protected def interpretBelief(input : String) =
    {
      interpretNoumenal(input) must be equalTo "OK."
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
      interpretBelief("a pet must be an animal")
      interpretBelief("a person may have pets")
      interpretBelief("a person with a pet is an owner")
      interpretBelief("a girl is a kind of person")
      interpretBelief("a pig is a kind of animal")
      interpretBelief("Fern is a girl")
      interpretBelief("a pig may be hungry or full")
      phenomenalCosmos.copyFrom(noumenalCosmos)
      interpretBelief("Wilbur is a pig")
      interpretBelief("Wilbur is Fern's pet")
      interpretBelief("Wilbur is hungry")

      val wilbur = expectProperName("Wilbur")
      val fern = expectProperName("Fern")

      interpretNoumenal("is there a pig") must be equalTo "Yes."
      interpretNoumenal("is there a girl") must be equalTo "Yes."
      interpretNoumenal("does Fern have a pet") must be equalTo "Yes."
      interpretNoumenal("does Wilbur have an owner") must be equalTo "Yes."
      interpretNoumenal("is Wilbur hungry") must be equalTo "Yes."

      interpretPhenomenal("is there a pig") must be equalTo "No."
      interpretPhenomenal("is there a girl") must be equalTo "Yes."
      interpretPhenomenal("does Fern have a pet") must be equalTo "No."

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      perception.perceiveEntity(wilbur)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      interpretPhenomenal("is there a pig") must be equalTo "Yes."
      interpretPhenomenal("is there a girl") must be equalTo "Yes."
      interpretPhenomenal("does Fern have a pet") must be equalTo "No."
      interpretPhenomenal("does Wilbur have an owner") must be equalTo "No."
      interpretPhenomenal("is Wilbur hungry") must be equalTo "I don't know."

      perception.perceiveEntityAssociations(fern)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      interpretPhenomenal("is there a pig") must be equalTo "Yes."
      interpretPhenomenal("is there a girl") must be equalTo "Yes."
      interpretPhenomenal("does Fern have a pet") must be equalTo "Yes."
      interpretPhenomenal("does Wilbur have an owner") must be equalTo "Yes."
      interpretPhenomenal("is Wilbur hungry") must be equalTo "I don't know."

      perception.perceiveEntityProperties(wilbur)

      noumenalCosmos.sanityCheck must beTrue
      phenomenalCosmos.sanityCheck must beTrue

      interpretPhenomenal("is Wilbur hungry") must be equalTo "Yes."
    }
  }
}
