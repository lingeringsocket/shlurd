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

import spire.math._

import scala.collection._
import scala.util._

class ShlurdInterpreterSpec extends Specification
{
  private val world = ZooWorld

  type StateChangeInvocation = ShlurdStateChangeInvocation[ShlurdEntity]

  private def interpret(
    input : String,
    params : ShlurdInterpreterParams = ShlurdInterpreterParams()) =
  {
    val sentence = ShlurdParser(input).parseOne
    val interpreter = new ShlurdInterpreter(world, params) {
      override protected def executeInvocation(
        invocation : StateChangeInvocation)
      {
        throw new RuntimeException("unexpected invocation")
      }
    }
    interpreter.interpret(sentence)
  }

  private def interpretCommandExpected(
    input : String,
    invocation : StateChangeInvocation) =
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
    interpreter.interpret(sentence) must be equalTo("OK.")
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
      interpret("is there any tiger") must be equalTo(
        "Yes, there is a tiger.")
      // FIXME:  for some reason, CoreNLP interprets this as
      // a statement instead of a question
      // interpret("are there any tigers")
      interpret("is there any goat") must be equalTo(
        "Yes, there are 3 of them.")
      interpret("is there a lion and a tiger") must be equalTo(
        "Yes, there is a lion and a tiger.")
      interpret("is there a lion or a peacock") must be equalTo(
        "Yes, there is a lion.")
      interpret("is there a peacock") must be equalTo(
        "No, there is not a peacock.")
      interpret("is there a hippogriff or a peacock") must be equalTo(
        "No, there is neither a hippogriff nor a peacock.")
      interpret("is there a hippogriff, a peacock, or a salamander") must
        be equalTo(
          "No, there is neither a hippogriff, a peacock, nor a salamander.")
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
        "But I don't know about any such bear.")
      interpret("is the bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
      interpret("is any bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is any polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is some bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("are the bears asleep") must be equalTo(
        "No, the bears are not asleep.")
      interpret("are any bears asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("are all bears asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("are all goats asleep") must be equalTo(
        "Yes, all goats are asleep.")
      interpret("are any goats asleep") must be equalTo(
        "Yes, all 3 of them are asleep.")
      val lowLimit = ShlurdInterpreterParams().copy(listLimit = 1)
      interpret("are any goats asleep", lowLimit) must be equalTo(
        "Yes, all 3 of them are asleep.")
      interpret("are any goats awake") must be equalTo(
        "No, no goats are awake.")
      interpret("are all goats awake") must be equalTo(
        "No, none of them are awake.")
      interpret("is there an aardvark") must be equalTo(
        "I don't know about this animal: aardvark")
      interpret("is the sloth awake") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      interpret("is the lion or the polar bear awake") must be equalTo(
        "No, neither the lion nor the polar bear is awake.")
      interpret("is the grizzly bear or the tiger awake") must be equalTo(
        "Yes, both of them are awake.")
      interpret("is the sloth or the tiger asleep") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the lion awake") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      interpret("are the tiger and the grizzly bear awake") must be equalTo(
        "Yes, the tiger and the grizzly bear are awake.")
      interpret("are the bears and the lion asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("are the bears and the lion awake") must be equalTo(
        "No, neither the lion nor the polar bear is awake.")
      interpret("are the bears and the lion awake", lowLimit) must be equalTo(
        "No, 2 of them are not awake.")
      interpret("are the tiger and the lion asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the tiger in the cage") must be equalTo(
        "Please be more specific about which cage you mean.")
      interpret("is the tiger in the big cage") must be equalTo(
        "Yes, the tiger is in the big cage.")
      interpret("is there a tiger in the big cage") must be equalTo(
        "Yes, there is a tiger in the big cage.")
      interpret("is the tiger in the small cage") must be equalTo(
        "No, the tiger is not in the small cage.")
      interpret("is there a tiger in the small cage") must be equalTo(
        "No, there is not a tiger in the small cage.")
      interpret("is the tiger in the big cage awake") must be equalTo(
        "Yes, the tiger in the big cage is awake.")
      interpret("is the tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      // FIXME:  this one gets mistakenly interpreted as an identity:
      // "is the grizzly == bear in the cage"
      /*
      interpret("is the grizzly bear in the cage") must be equalTo(
        "Yes, the grizzly bear is in the cage.")
       */
      interpret("is the goat on the farm awake") must be equalTo(
        "No, the goat on the farm is not awake.")
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
      interpret("the lion or the polar bear is awake") must be equalTo(
        "Oh, really?")
    }

    "interpret commands" in
    {
      val awake = ShlurdWord("awake", "awake")
      val asleep = ShlurdWord("sleepify", "asleep")
      interpretCommandExpected(
        "awake the lion",
        ShlurdStateChangeInvocation(Set(ZooLion), awake))
      interpretCommandExpected(
        "awake the lion and the tiger",
        ShlurdStateChangeInvocation(Set(ZooLion), awake))
      interpretCommandExpected(
        "awake the polar bear and the lion",
        ShlurdStateChangeInvocation(Set(ZooLion, ZooPolarBear), awake))
      interpret("awake the tiger") must be equalTo(
        "But the tiger is awake already.")
      interpretCommandExpected(
        "asleep the tiger",
        ShlurdStateChangeInvocation(Set(ZooTiger), asleep))
      interpret("asleep the goats") must be equalTo(
        "But the goats are asleep already.")
      interpret("asleep the lion") must be equalTo(
        "But the lion is asleep already.")
      interpret("asleep the lion and the goats") must be equalTo(
        "But the lion and the goats are asleep already.")
      interpretCommandExpected(
        "awake the goat on the farm.",
        ShlurdStateChangeInvocation(Set(ZooDomesticGoat), awake))
      interpretCommandExpected(
        "asleep the tiger in the big cage.",
        ShlurdStateChangeInvocation(Set(ZooTiger), asleep))

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
  object ZooSloth extends ZooAnimalEntity("sloth")
  object ZooPeacock extends ZooAnimalEntity("peacock")
  object ZooHippogriff extends ZooAnimalEntity("hippogriff")
  object ZooSalamander extends ZooAnimalEntity("salamander")
  object ZooMountainGoat extends ZooAnimalEntity("mountain goat")
  object ZooDomesticGoat extends ZooAnimalEntity("domestic goat")
  object ZooSiberianGoat extends ZooAnimalEntity("siberian goat")

  sealed case class ZooLocationEntity(name : String)
      extends ShlurdEntity with NamedObject
  object ZooFarm extends ZooLocationEntity("farm")
  object ZooBigCage extends ZooLocationEntity("big cage")
  object ZooSmallCage extends ZooLocationEntity("small cage")

  object ZooAnimalSleepinessProperty extends ShlurdProperty
  {
    override def getStates : Map[String, String] = Map(
      "awake" -> "awake",
      "asleep" -> "sleepify"
    )
  }

  sealed case class ZooAnimalSleepiness(name : String) extends NamedObject
  object ZooAnimalAwake extends ZooAnimalSleepiness("awake")
  object ZooAnimalAsleep extends ZooAnimalSleepiness("asleep")

  object ZooWorld extends ShlurdWorld[ShlurdEntity, ShlurdProperty]
  {
    private def index[T <: NamedObject](set : Set[T]) =
      Map(set.map(x => (x.name, x)).toSeq:_*)

    private val animals =
      index(Set(ZooLion, ZooTiger, ZooPolarBear,
        ZooGrizzlyBear, ZooSloth,
        ZooMountainGoat, ZooDomesticGoat, ZooSiberianGoat,
        ZooPeacock, ZooHippogriff, ZooSalamander))

    private val locations =
      index(Set(ZooFarm, ZooBigCage, ZooSmallCage))

    private val sleepinessValues = index(Set(ZooAnimalAwake, ZooAnimalAsleep))

    // if an animal doesn't appear here, we don't have one at the
    // zoo
    private val asleep = Map(
      ZooLion -> Trilean.True,
      ZooTiger -> Trilean.False,
      ZooPolarBear -> Trilean.True,
      ZooGrizzlyBear -> Trilean.False,
      ZooMountainGoat -> Trilean.True,
      ZooDomesticGoat -> Trilean.True,
      ZooSiberianGoat -> Trilean.True,
      ZooSloth -> Trilean.Unknown)

    private val containment : Map[ShlurdEntity, ZooLocationEntity] =
      Map(
        ZooLion -> ZooBigCage,
        ZooTiger -> ZooBigCage,
        ZooPolarBear -> ZooSmallCage,
        ZooGrizzlyBear -> ZooFarm,
        ZooDomesticGoat -> ZooFarm)

    override def resolveEntity(
      lemma : String,
      context : ShlurdReferenceContext,
      qualifiers : Set[String]) =
    {
      val name = (qualifiers.toSeq ++ Seq(lemma)).mkString(" ")
      if (context == REF_LOCATION) {
        Success(locations.filterKeys(_.endsWith(name)).values.toSet)
      } else {
        if (animals.filterKeys(_.endsWith(lemma)).isEmpty) {
          fail("I don't know about this animal: " + name)
        } else {
          Success(
            animals.filterKeys(_.endsWith(name)).
              values.filter(asleep.contains(_)).toSet)
        }
      }
    }

    override def resolveProperty(
      entity : ShlurdEntity,
      lemma : String) =
    {
      entity match {
        case a : ZooAnimalEntity => {
          if (sleepinessValues.contains(lemma)) {
            Success((ZooAnimalSleepinessProperty, lemma))
          } else {
            fail("I don't know about this state: " + lemma)
          }
        }
        case _ => fail("I don't know about this entity: " + entity)
      }
    }

    override def specificReference(
      entity : ShlurdEntity,
      determiner : ShlurdDeterminer) =
    {
      entity match {
        case animal : ZooAnimalEntity => {
          val words = animal.name.split(" ")
          val entityReference = ShlurdEntityReference(
            ShlurdWord(words.last, words.last), determiner)
          if (words.size == 1) {
            entityReference
          } else {
            ShlurdReference.qualified(
              entityReference, words.dropRight(1).map(
                q => ShlurdWord(q, q)))
          }
        }
        case _ => ShlurdUnknownReference
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
                case _ =>
                  fail("I don't know about this state: " + lemma)
              }
            }
            case _ =>
              fail("I don't know about this property: " + property)
          }
        }
        case _ =>
          fail("I don't know about this entity: " + entity)
      }
    }

    override def evaluateEntityLocationPredicate(
      entity : ShlurdEntity,
      location : ShlurdEntity,
      locative : ShlurdLocative) =
    {
      if ((locative != LOC_INSIDE) && (locative != LOC_ON)) {
        Success(Trilean.False)
      } else {
        containment.get(entity) match {
          case Some(actualLocation) =>
            Success(Trilean(location == actualLocation))
          case _ =>
            Success(Trilean.False)
        }
      }
    }
  }
}
