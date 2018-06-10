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

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import spire.math._

import scala.io._
import scala.util._

class SpcInterpreterSpec extends Specification
{
  private val states = Map(
    "alarm service service_on_off" -> "on",
    "multimedia service service_on_off" -> "off",
    "jackphone presence presence_on_off" -> "on",
    "jillphone presence presence_on_off" -> "off",
    "casperphone presence presence_on_off" -> "on",
    "yodaphone presence presence_on_off" -> "off",
    "stove stove_on_off" -> "off",
    "stove stove_hot_cold" -> "hot",
    "titanic boat boat_float_sink" -> "sink",
    "titanic boat vehicle_move_stop" -> "move",
    "herbie car vehicle_move_stop" -> "stop"
  )

  abstract class InterpreterContext(acceptNewBeliefs : Boolean = false)
      extends NameSpace
  {
    protected val cosmos = new SpcCosmos {
      override def evaluateEntityPropertyPredicate(
        entity : SpcEntity,
        property : SpcProperty,
        lemma : String) =
      {
        val qualifiedName =
          (entity.qualifiers.toSeq :+ entity.form.name
            :+ property.name).mkString(" ")
        states.get(qualifiedName) match {
          case Some(state) => Success(Trilean(state == lemma))
          case _ => super.evaluateEntityPropertyPredicate(
            entity, property, lemma)
        }
      }
    }

    protected val interpreter =
      new SpcInterpreter(new SpcMind(cosmos), acceptNewBeliefs)

    protected def loadBeliefs(resource : String)
    {
      val file = ShlurdParser.getResourceFile(resource)
      val source = Source.fromFile(file)
      cosmos.loadBeliefs(source)
    }

    protected def interpret(input : String, expected : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpret(sentence) must be equalTo(expected)
    }
  }

  "SpcInterpreter" should
  {
    "understand people" in new InterpreterContext
    {
      loadBeliefs("/ontologies/people.txt")
      interpret(
        "is Todd Dirk's friend",
        "Yes, Todd is Dirk's friend.")
      interpret(
        "is Dirk Todd's friend",
        "Yes, Dirk is Todd's friend.")
      interpret(
        "is Amanda Todd's sister",
        "Yes, Amanda is Todd's sister.")
      interpret(
        "is Dirk Todd's sister",
        "No, Dirk is not Todd's sister.")
      interpret(
        "is Todd Amanda's brother",
        "Yes, Todd is Amanda's brother.")
      interpret(
        "is Amanda Todd's friend",
        "No, Amanda is not Todd's friend.")
      // FIXME:  should clarify that Dirk actually has more than one friend
      interpret(
        "does Dirk have a friend",
        "Yes, Dirk has a friend.")
      interpret(
        "does Dirk have any friends",
        "Yes, Dirk has 2 of them.")
      interpret(
        "does Todd have friends",
        "Yes, Todd has friends.")
      interpret(
        "does Todd have any friends",
        "Yes, Todd has 1 of them.")
      interpret(
        "does Amanda have friends",
        "No, Amanda does not have friends.")
      interpret(
        "does Amanda have a friend",
        "No, Amanda does not have a friend.")
      interpret(
        "does Amanda have any friends",
        "No, Amanda has no friends.")
      interpret(
        "who is Todd",
        "Todd is Amanda's brother.")
      interpret(
        "who is Bart",
        "Bart is Rapunzel's owner.")
      interpret(
        "who is Todd's friend",
        "Todd's friend is Dirk.")
      interpret(
        "who are Todd's friends",
        "Todd's friend is Dirk.")
      interpret(
        "which person is Todd's friend",
        "Todd's friend is Dirk.")
      interpret(
        "who is Dirk's friend",
        "Dirk's friends are Todd and Bart.")
      interpret(
        "who is Amanda's friend",
        "No one is Amanda's friend.")
      interpret(
        "who are Amanda's friends",
        "No one is Amanda's friend.")
      interpret(
        "who has Amanda's friend",
        "But I don't know about any such friend.")
      interpret(
        "is Ford Todd's friend",
        "Sorry, I don't know about any 'Ford'.")
      interpret(
        "is Todd Ford's friend",
        "Sorry, I don't know about any 'Ford'.")
      // FIXME:  should clarify that they are not necessarily
      // friends OF EACH OTHER
      interpret(
        "who is a friend",
        "Dirk, Todd, and Bart are friends.")
      interpret(
        "is Amanda a friend",
        "No, Amanda is not a friend.")
      interpret(
        "is Amanda a brother",
        "No, Amanda is not a brother.")
      interpret(
        "is Amanda a dog",
        "No, Amanda is not a dog.")
      interpret(
        "is Amanda an owner",
        "No, Amanda is not an owner.")
      interpret(
        "is Amanda a groomer",
        "No, Amanda is not a groomer.")
      interpret(
        "is Rapunzel a dog",
        "Yes, Rapunzel is a dog.")
      interpret(
        "is Bart an owner",
        "Yes, Bart is an owner.")
      interpret(
        "is Amanda a robot",
        "Sorry, I don't know about any 'robot'.")
      interpret(
        "who is a person",
        "Dirk, Todd, Amanda, Bart, Hugo, Scott, and Arthur are persons.")
      interpret(
        "who is a brother",
        "Todd is a brother.")
      // FIXME have to use BLACKWING because Blackwing gets parsed
      // as -ing verb, heh
      interpret(
        "is BLACKWING an organization",
        "Yes, BLACKWING is an organization.")
      interpret(
        "is BLACKWING a conspiracy",
        "No, BLACKWING is not a conspiracy.")
      interpret(
        "who has an uncle",
        "No one has an uncle.")
      interpret(
        "which person has an uncle",
        "No person has an uncle.")
      interpret(
        "who has a friend",
        "Dirk and Todd have a friend.")
      interpret(
        "who has friends",
        "Dirk and Todd have friends.")
      interpret(
        "who is Ford",
        "Sorry, I don't know about any 'Ford'.")
      interpret(
        "who is Hugo",
        "Hugo is one of BLACKWING's operatives.")
      interpret(
        "who is Arthur",
        "Arthur is a person.")
      interpret(
        "is BLACKWING Hugo's employer",
        "Yes, BLACKWING is Hugo's employer.")
      interpret(
        "which organization is Hugo's employer",
        "Hugo's employer is BLACKWING.")
      interpret(
        "is BLACKWING Todd's employer",
        "No, BLACKWING is not Todd's employer.")
      interpret(
        "which organization is Todd's employer",
        "No organization is Todd's employer.")
    }

    "understand taxonomy" in new InterpreterContext
    {
      loadBeliefs("/ontologies/vehicles.txt")
      interpret(
        "is Herbie moving",
        "No, Herbie is not moving.")
      interpret(
        "is Herbie stopped",
        "Yes, Herbie is stopped.")
      interpret(
        "is Titanic moving",
        "Yes, Titanic is moving.")
      interpret(
        "is Titanic stopped",
        "No, Titanic is not stopped.")
      interpret(
        "is any boat stopped",
        "No, no boat is stopped.")
      interpret(
        "is any boat moving",
        "Yes, Titanic is moving.")
      interpret(
        "is any vehicle stopped",
        "Yes, Herbie is stopped.")
      interpret(
        "is any vehicle moving",
        "Yes, Titanic is moving.")
      interpret(
        "is Titanic floating",
        "No, Titanic is not floating.")
      interpret(
        "is Herbie floating",
        "Sorry, I don't know what 'float' means for Herbie.")
      interpret(
        "is any car floating",
        "Sorry, I don't know what 'float' means for a car.")
      interpret(
        "who is floating",
        "Sorry, I don't know what 'float' means for a person.")
      interpret(
        "is Herbie a car",
        "Yes, Herbie is a car.")
      interpret(
        "is Herbie a boat",
        "No, Herbie is not a boat.")
      interpret(
        "is Herbie a vehicle",
        "Yes, Herbie is a vehicle.")
      interpret(
        "is Titanic a boat",
        "Yes, Titanic is a boat.")
      interpret(
        "is Titanic a vehicle",
        "Yes, Titanic is a vehicle.")
      interpret(
        "is Titanic a car",
        "No, Titanic is not a car.")
      interpret(
        "how many vehicles are there",
        "There are 2 of them.")
      // FIXME resolve number agreement
      interpret(
        "which vehicles are there",
        "There is Herbie and Titanic.")
      interpret(
        "who is Herbie's owner",
        "Herbie's owner is Jim.")
      interpret(
        "who is Titanic's owner",
        "No one is Titanic's owner.")
    }

    "respond correctly when no person exists" in new InterpreterContext
    {
      interpret(
        "who is Ford",
        "Sorry, I don't know about any 'Ford'.")
    }

    "understand services" in new InterpreterContext
    {
      loadBeliefs("/ontologies/service.txt")
      loadBeliefs("/ontologies/miscServices.txt")
      interpret(
        "is there a multimedia service",
        "Yes, there is a multimedia service.")
      interpret(
        "is there an alarm service",
        "Yes, there is an alarm service.")
      interpret(
        "is there a laundry service",
        "No, there is not a laundry service.")
      interpret(
        "is the alarm service up",
        "Yes, the alarm service is up.")
      interpret(
        "is the alarm service on",
        "Yes, the alarm service is on.")
      interpret(
        "is the multimedia service up",
        "No, the multimedia service is not up.")
      interpret(
        "is any service up",
        "Yes, the alarm service is up.")
      interpret(
        "are any services up",
        "Yes, the alarm service is up.")
      interpret(
        "is any service down",
        "Yes, the multimedia service is down.")
      interpret(
        "is any service off",
        "Yes, the multimedia service is off.")
      interpret(
        "are all services up",
        "No, the multimedia service is not up.")
      interpret(
        "are all services running",
        "No, the multimedia service is not running.")
      interpret(
        "is the multimedia server up",
        "No, the multimedia server is not up.")
    }

    "understand presence" in new InterpreterContext
    {
      // FIXME:  in "is Jack home", interpret "home" as state instead of noun
      loadBeliefs("/ontologies/presence.txt")
      interpret("is Jack's presence on",
        "Yes, Jack's presence is on.")
      interpret("is Jack present",
        "Yes, Jack is present.")
      interpret("is Jack at home",
        "Yes, Jack is at home.")
      interpret("is Jack absent",
        "No, Jack is not absent.")
      interpret("is Jack away",
        "No, Jack is not away.")
      interpret("is Jill's presence on",
        "No, Jill's presence is not on.")
      interpret("is Jill present",
        "No, Jill is not present.")
      interpret("is Jill at home",
        "No, Jill is not at home.")
      interpret("is Jill absent",
        "Yes, Jill is absent.")
      interpret("is Jill away",
        "Yes, Jill is away.")
      interpret("is Jack on",
        "Sorry, I don't know what 'on' means for Jack.")
      interpret("is Casper's haunting on",
        "Yes, Casper's haunting is on.")
      interpret("is Casper present",
        "I don't know.")
      interpret("is Yoda's presence on",
        "No, Yoda's presence is not on.")
      interpret("is Yoda present",
        "No, Yoda is not present.")
      interpret("is Yoda on",
        "No, Yoda is not on.")
      interpret("is Yoda off",
        "Yes, Yoda is off.")
    }

    "understand multiple properties for same form" in new InterpreterContext
    {
      loadBeliefs("/ontologies/stove.txt")
      interpret("is there a stove?",
        "Yes, there is a stove.")
      interpret("is the stove hot?",
        "Yes, the stove is hot.")
      interpret("is the stove on?",
        "No, the stove is not on.")
    }

    "prevent new beliefs" in new InterpreterContext
    {
      interpret("There is a front door",
        "Sorry, I don't know about any 'door'.")
    }

    "accept new beliefs" in new InterpreterContext(true)
    {
      interpret("a door may be either open or closed",
        "OK.")
      interpret("there is a front door",
        "OK.")
      interpret("is the front door open",
        "I don't know.")
    }

    "reject invalid new beliefs" in new InterpreterContext(true)
    {
      interpret("there is a front door",
        "OK.")
      interpret("there is a big front door",
        "Previously I was told that there is a front door.  " +
          "So I am unclear how to interpret the belief that " +
          "there is a big front door.")
    }

    "reject cyclic taxonomy belief" in new InterpreterContext(true)
    {
      interpret("a bird is a kind of animal", "OK.")
      interpret("a duck is a kind of bird", "OK.")
      interpret("an animal is a kind of duck",
        "Previously I was told that a duck is a kind of a bird and a bird " +
          "is a kind of an animal.  So I am unable to accept that " +
          "an animal is a kind of duck.")
    }

    "validate constraints incrementally" in new InterpreterContext(true)
    {
      loadBeliefs("/ontologies/people.txt")
      interpret(
        "Amanda is Rapunzel's owner",
        "Previously I was told that a dog may have one owner and Bart " +
          "is Rapunzel's owner.  So it does not add up when I hear that " +
          "Amanda is Rapunzel's owner.")
    }
  }
}
