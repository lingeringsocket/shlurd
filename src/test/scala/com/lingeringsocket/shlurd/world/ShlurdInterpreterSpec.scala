// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import scala.collection._
import scala.util._

class ShlurdInterpreterSpec extends Specification
{
  private val world = ZooWorld

  type StateChangeInvocation = ShlurdStateChangeInvocation[ShlurdEntity]

  private def interpret(input : String) =
  {
    val sentence = ShlurdParser(input).parseOne
    val interpreter = new ShlurdInterpreter(world) {
      override protected def executeInvocation(
        invocation : StateChangeInvocation)
      {
        throw new RuntimeException("unexpected invocation")
      }
    }
    interpreter.interpret(sentence)
  }

  private def interpretCommandExpected(
    input : String, invocation : StateChangeInvocation) =
  {
    val sentence = ShlurdParser(input).parseOne
    var actualInvocation : Option[StateChangeInvocation] = None
    val interpreter = new ShlurdInterpreter(world) {
      override protected def executeInvocation(
        invocation : StateChangeInvocation)
      {
        actualInvocation = Some(invocation)
      }
    }
    interpreter.interpret(sentence) must be equalTo(
      "Okay, I will get right on that.")
    actualInvocation must be equalTo(Some(invocation))
  }

  "ShlurdInterpreter" should
  {
    "interpret questions" in
    {
      interpret("is the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      interpret("is the lion awake") must be equalTo(
        "No, the lion is not awake.")
      interpret("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      interpret("is there a tiger") must be equalTo(
        "Yes, there is a tiger.")
      interpret("is there a lion and a tiger") must be equalTo(
        "Yes, there is a lion and a tiger.")
      interpret("is there a lion or a peacock") must be equalTo(
        "Yes, there is a lion or a peacock.")
      interpret("is there a peacock") must be equalTo(
        "No, there is not a peacock.")
      // FIXME:  improve response phrasing
      interpret("is there no peacock") must be equalTo(
        "Yes, there is no peacock.")
      interpret("is there a lion and a peacock") must be equalTo(
        "No, there is not a lion and a peacock.")
      interpret("is there a bengal tiger") must be equalTo(
        "No, there is not a bengal tiger.")
      interpret("is there a bear") must be equalTo(
        "Yes, there is a bear.")
      interpret("is there a grizzly bear") must be equalTo(
        "Yes, there is a grizzly bear.")
      interpret("is there a polar bear") must be equalTo(
        "Yes, there is a polar bear.")
      interpret("is there a kodiak bear") must be equalTo(
        "No, there is not a kodiak bear.")
      interpret("is the polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is the grizzly bear asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("is the kodiak bear asleep") must be equalTo(
        "I don't know about any such bear")
      interpret("is the bear asleep") must be equalTo(
        "I am not sure which bear you mean")
      interpret("is any bear asleep") must be equalTo(
        "Yes, a bear is asleep.")
      interpret("is some bear asleep") must be equalTo(
        "Yes, some bear is asleep.")
      interpret("are any bears asleep") must be equalTo(
        "Yes, some bears are asleep.")
      interpret("are all bears asleep") must be equalTo(
        "No, all bears are not asleep.")
      interpret("is there an aardvark") must be equalTo(
        "I don't know about this animal: aardvark")
    }

    "interpret statements" in
    {
      interpret("the lion is asleep") must be equalTo(
        "Right, the lion is asleep.")
      interpret("the lion is awake") must be equalTo(
        "Oh, really?")
      interpret("the tiger is asleep") must be equalTo(
        "Oh, really?")
      interpret("the tiger is awake") must be equalTo(
        "Right, the tiger is awake.")
    }

    "interpret commands" in
    {
      interpretCommandExpected(
        "awake the lion",
        ShlurdStateChangeInvocation(Set(ZooLion), "awake"))
      interpretCommandExpected(
        "awake the lion and the tiger",
        ShlurdStateChangeInvocation(Set(ZooLion), "awake"))
      interpretCommandExpected(
        "awake the polar bear and the lion",
        ShlurdStateChangeInvocation(Set(ZooLion, ZooPolarBear), "awake"))
      interpret("awake the tiger") must be equalTo(
        "But it already is.")
    }
  }

  trait NamedObject {
    def name : String
  }

  sealed case class ZooAnimalEntity(name : String)
      extends ShlurdEntity with NamedObject
  object ZooLion extends ZooAnimalEntity("lion")
  object ZooTiger extends ZooAnimalEntity("tiger")
  object ZooPolarBear extends ZooAnimalEntity("polar bear")
  object ZooGrizzlyBear extends ZooAnimalEntity("grizzly bear")
  object ZooPeacock extends ZooAnimalEntity("peacock")

  object ZooAnimalSleepinessProperty extends ShlurdProperty

  sealed case class ZooAnimalSleepiness(name : String) extends NamedObject
  object ZooAnimalAwake extends ZooAnimalSleepiness("awake")
  object ZooAnimalAsleep extends ZooAnimalSleepiness("asleep")

  object ZooWorld extends ShlurdWorld[ShlurdEntity, ShlurdProperty]
  {
    private def index[T <: NamedObject](set : Set[T]) =
      Map(set.map(x => (x.name, x)).toSeq:_*)

    private val animals =
      index(Set(ZooLion, ZooTiger, ZooPolarBear, ZooGrizzlyBear, ZooPeacock))

    private val sleepinessValues = index(Set(ZooAnimalAwake, ZooAnimalAsleep))

    // if an animal doesn't appear here, we don't have one at the
    // zoo
    private val asleep =
      Map(ZooLion -> true, ZooTiger -> false, ZooPolarBear -> true,
        ZooGrizzlyBear -> false)

    override def resolveEntity(
      lemma : String,
      context : ShlurdReferenceContext,
      qualifiers : Set[String]) =
    {
      val name = (qualifiers.toSeq ++ Seq(lemma)).mkString(" ")
      if (animals.filterKeys(_.endsWith(lemma)).isEmpty) {
        fail("I don't know about this animal: " + name)
      } else {
        Success(
          animals.filterKeys(_.endsWith(name)).
            values.filter(asleep.contains(_)).toSet)
      }
    }

    override def resolveProperty(
      entity : ShlurdEntity,
      lemma : String) =
    {
      entity match {
        case a : ZooAnimalEntity => {
          if (sleepinessValues.contains(lemma)) {
            Success(ZooAnimalSleepinessProperty)
          } else {
            fail("I don't know about this state: " + lemma)
          }
        }
        case _ => fail("I don't know about this entity: " + entity)
      }
    }

    override def evaluateEntityPropertyPredicate(
      entity : ShlurdEntity,
      property : ShlurdProperty,
      lemma : String) =
    {
      entity match {
        case animal : ZooAnimalEntity => {
          property match {
            case ZooAnimalSleepinessProperty => {
              sleepinessValues.get(lemma) match {
                case Some(ZooAnimalAwake) => Success(!asleep(animal))
                case Some(ZooAnimalAsleep) => Success(asleep(animal))
                case _ => fail("I don't know about this state: " + lemma)
              }
            }
            case _ => fail("I don't know about this property: " + property)
          }
        }
        case _ => fail("I don't know about this entity: " + entity)
      }
    }

    override def evaluateEntityLocationPredicate(
      entity : ShlurdEntity,
      location : ShlurdEntity,
      locative : ShlurdLocative) =
    {
      fail("I don't know about locations yet")
    }
  }
}
