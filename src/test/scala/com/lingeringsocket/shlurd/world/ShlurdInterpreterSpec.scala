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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import spire.math._

import scala.collection._
import scala.util._

import ShlurdEnglishLemmas._

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
    "deal with problem cases" in
    {
      skipped("maybe one day")
      // FIXME:  we don't deal with negated questions yet
      interpret("which goats are not awake") must be equalTo(
        "The domestic goat, the siberian goat, " +
          "and the mountain goat are not awake.")
      // FIXME:  we should use the same rules as for DETERMINER_UNIQUE
      interpret("is any tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      // FIXME:  ought to allow quantified existence queries
      interpret("how many bears are there") must be equalTo(
        "There are 2 bears.")
      interpret("how many bears are there in the small cage") must be equalTo(
        "There is 1 bear in the small cage.")
      // FIXME: this one is kinda cute
      interpret("is bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
    }

    "interpret questions" in
    {
      interpret("is the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      // FIXME:  better would be "Yes, the lion is asleep."
      interpret("are the lions asleep") must be equalTo(
        "Yes, the lions are asleep.")
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
      interpret("are there any tigers") must be equalTo(
        "Yes, there is a tiger.")
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
      interpret("is grizzly bear asleep") must be equalTo(
        "No, grizzly bear is not asleep.")
      interpret("is the kodiak bear asleep") must be equalTo(
        "But I don't know about any such bear.")
      interpret("is kodiak bear asleep") must be equalTo(
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
        "Sorry, I don't know what you mean by aardvark.")
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
        "No, neither the polar bear nor the lion is awake.")
      interpret("are the bears and the lion awake", lowLimit) must be equalTo(
        "No, 2 of them are not awake.")
      interpret("are the tiger and the lion asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the grizzly bear in any cage") must be equalTo(
        "No, the grizzly bear is in no cage.")
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
      interpret("is the goat on the farm awake") must be equalTo(
        "No, the goat on the farm is not awake.")
      interpret("which goat is awake") must be equalTo(
        "No goat is awake.")
      interpret("which goats are awake") must be equalTo(
        "No goats are awake.")
      interpret("what goat is awake") must be equalTo(
        "No goat is awake.")
      val list = "The domestic goat, the siberian goat, " +
        "and the mountain goat are asleep."
      interpret("which goat is asleep") must be equalTo(list)
      interpret("which goats are asleep") must be equalTo(list)
      interpret("which goat in the farm is asleep") must be equalTo(
        "The domestic goat is asleep.")
      interpret("which goat is asleep in the farm") must be equalTo(
        "The domestic goat is asleep.")
      interpret("which goat in the farm is awake") must be equalTo(
        "No goat in the farm is awake.")
      interpret("how many goats are awake") must be equalTo(
        "No goats are awake.")
      interpret("how many goats are asleep") must be equalTo(
        "All 3 of them are asleep.")
      interpret("how many goats are asleep in the farm") must be equalTo(
        "1 of them is asleep.")
      interpret("how many goats in the farm are asleep") must be equalTo(
        "1 of them is asleep.")
      interpret("how many mountain goats are asleep") must be equalTo(
        "1 of them is asleep.")
      interpret("how many lions or polar bears are asleep") must be equalTo(
        "Both of them are asleep.")
      interpret("am I in the big cage") must be equalTo(
        "No, you are not in the big cage.")
      interpret("are you in the big cage") must be equalTo(
        "Yes, I am in the big cage.")
      interpret("is he in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      interpret("is his tiger in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      interpret("are we in the big cage") must be equalTo(
        "Sorry, when you say 'we' I don't know who or what you mean.")
      interpret("are they in the big cage") must be equalTo(
        "Sorry, when you say 'they' I don't know who or what you mean.")
      interpret("is my tiger in the big cage") must be equalTo(
        "Yes, your tiger is in the big cage.")
      interpret("is your tiger in the big cage") must be equalTo(
        "But I don't know about any such tiger.")
      interpret("is my lion in the big cage") must be equalTo(
        "But I don't know about any such lion.")
      interpret("is your lion in the big cage") must be equalTo(
        "Yes, my lion is in the big cage.")
      interpret("who is in the big cage") must be equalTo(
        "Muldoon is in the big cage.")
      interpret("does Muldoon have a lion") must be equalTo(
        "Yes, Muldoon has a lion.")
      interpret("does Malcolm have a lion") must be equalTo(
        "No, Malcolm does not have a lion.")
      interpret("has Muldoon a tiger") must be equalTo(
        "No, Muldoon does not have a tiger.")
      interpret("does Malcolm have a tiger") must be equalTo(
        "Yes, Malcolm has a tiger.")
      interpret("do I have a lion") must be equalTo(
        "No, you do not have a lion.")
      interpret("do you have a lion") must be equalTo(
        "Yes, I have a lion.")
      interpret("do I have a tiger") must be equalTo(
        "Yes, you have a tiger.")
      interpret("do you have a tiger") must be equalTo(
        "No, I do not have a tiger.")
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

    "respond to unrecognized phrases" in
    {
      interpret("colorless green ideas slumber furiously") must be equalTo(
        "Sorry, I cannot understand what you said.")
      interpret("My squeaking door is open.") must be equalTo(
        "I think you are saying that some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("My squeaking doors are open.") must be equalTo(
        "I think you are saying that some entity is open, but " +
          "I can't understand the phrase \"my squeaking doors\"")
      interpret("My squeaking door can be open.") must be equalTo(
        "I think you are saying that some entity can be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("My squeaking door may not be open.") must be equalTo(
        "I think you are saying that some entity may not be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Is my squeaking door open?") must be equalTo(
        "I think you are asking whether some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      // FIXME:  capitalization
      interpret("The door is how I want it.") must be equalTo(
        "I think you are saying something about the door, but " +
          "I can't understand the phrase \"how i want it\"")
      interpret("Is the door how I want it?") must be equalTo(
        "I think you are asking something about the door, but " +
          "I can't understand the phrase \"how i want it\"")
      interpret("The cake is a lie with a side of bacon.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie with a side of bacon\"")
      interpret("A lie with a side of bacon is a cake.") must be equalTo(
        "I think you are saying that some entity is a cake, but " +
          "I can't understand the phrase \"a lie with a side of bacon\"")
      interpret("The cake has a lie with a side of bacon.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie with a side of bacon\"")
      interpret("A lie with a side of bacon has a cake.") must be equalTo(
        "I think you are saying that some entity has a cake, but " +
          "I can't understand the phrase \"a lie with a side of bacon\"")
      interpret("Butterflies with a side of bacon are cakes.") must be equalTo(
        "I think you are saying that some entities are cakes, but " +
          "I can't understand the phrase \"butterflies with a side of bacon\"")
      interpret("There is my squeaking door.") must be equalTo(
        "I think you are saying that some entity is there, but " +
          "I can't understand the phrase \"my squeaking door\"")
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

  sealed case class ZooPersonEntity(name : String)
      extends ShlurdEntity with NamedObject
  object ZooKeeper extends ZooPersonEntity("Muldoon")
  object ZooVisitor extends ZooPersonEntity("Malcolm")

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

    private val people =
      index(Set(ZooKeeper, ZooVisitor))

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
        ZooVisitor -> ZooFarm,
        ZooKeeper -> ZooBigCage,
        ZooLion -> ZooBigCage,
        ZooTiger -> ZooBigCage,
        ZooPolarBear -> ZooSmallCage,
        ZooGrizzlyBear -> ZooFarm,
        ZooDomesticGoat -> ZooFarm)

    private val ownership : Map[ShlurdEntity, ZooPersonEntity] =
      Map(
        ZooLion -> ZooKeeper,
        ZooTiger -> ZooVisitor)

    override def resolveEntity(
      lemma : String,
      context : ShlurdReferenceContext,
      qualifiers : Set[String]) =
    {
      if (lemma == LEMMA_WHO) {
        Success(ShlurdParseUtils.orderedSet(
          people.values))
      } else {
        val name = (qualifiers.toSeq ++ Seq(lemma)).mkString(" ")
        if (context == REF_LOCATION) {
          Success(ShlurdParseUtils.orderedSet(
            locations.filterKeys(_.endsWith(name)).values))
        } else {
          if (animals.filterKeys(_.endsWith(lemma)).isEmpty) {
            val namedPeople = people.filterKeys(_.toLowerCase == lemma).values
            if (namedPeople.isEmpty) {
              fail("I don't know about this name: " + name)
            } else {
              Success(namedPeople.toSet)
            }
          } else {
            Success(
              animals.filterKeys(_.endsWith(name)).
                values.filter(asleep.contains(_)).toSet)
          }
        }
      }
    }

    override def resolvePronoun(
      person : ShlurdPerson,
      gender : ShlurdGender,
      count : ShlurdCount) : Try[Set[ShlurdEntity]] =
    {
      if (count == COUNT_SINGULAR) {
        person match {
          case PERSON_FIRST => return Success(Set(ZooVisitor))
          case PERSON_SECOND => return Success(Set(ZooKeeper))
          case _ =>
        }
      }
      fail("unsupported pronoun reference")
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
        case ZooPersonEntity(name) => {
          ShlurdEntityReference(
            ShlurdWord(name, name), DETERMINER_UNSPECIFIED)
        }
        case ZooLocationEntity(name) => {
          ShlurdEntityReference(
            ShlurdWord(name, name), DETERMINER_UNSPECIFIED)
        }
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
      locative : ShlurdLocative,
      qualifiers : Set[String]) : Try[Trilean] =
    {
      val map = locative match {
        case LOC_GENITIVE_OF => {
          if (!location.isInstanceOf[ZooPersonEntity]) {
            return Success(Trilean.False)
          }
          ownership
        }
        case LOC_INSIDE | LOC_ON => {
          if (!location.isInstanceOf[ZooLocationEntity]) {
            return Success(Trilean.False)
          }
          containment
        }
        case _ => {
          return Success(Trilean.False)
        }
      }
      map.get(entity) match {
        case Some(actualLocation) =>
          Success(Trilean(location == actualLocation))
        case _ =>
          Success(Trilean.False)
      }
    }
  }
}
