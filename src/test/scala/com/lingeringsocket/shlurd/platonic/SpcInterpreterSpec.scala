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
import com.lingeringsocket.shlurd.cosmos._

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

  abstract class InterpreterContext(
    acceptNewBeliefs : Boolean = false,
    params : ShlurdResponseParams = ShlurdResponseParams()
  ) extends NameSpace
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

      SpcPrimordial.initCosmos(this)
    }

    protected val interpreter =
      new SpcInterpreter(
        new SpcMind(cosmos), acceptNewBeliefs, params)

    protected val interpreterWithoutPronouns =
      new SpcInterpreter(
        new SpcMind(cosmos), acceptNewBeliefs, params.
          copy(thirdPersonPronouns = false))

    protected val interpreterTerse =
      new SpcInterpreter(
        new SpcMind(cosmos), acceptNewBeliefs, params.
          copy(verbosity = RESPONSE_TERSE))

    protected val interpreterEllipsis =
      new SpcInterpreter(
        new SpcMind(cosmos), acceptNewBeliefs, params.
          copy(verbosity = RESPONSE_ELLIPSIS))

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

    protected def interpretMatrix(
      input : String,
      expectedWithPronouns : String,
      expectedWithoutPronouns : String,
      expectedTerse : String,
      expectedEllipsis : String = "") =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpret(sentence) must be equalTo(
        expectedWithPronouns)
      interpreterWithoutPronouns.interpret(
        sentence) must be equalTo(expectedWithoutPronouns)
      interpreterTerse.interpret(
        sentence) must be equalTo(expectedTerse)
      if (!expectedEllipsis.isEmpty) {
        interpreterEllipsis.interpret(
          sentence) must be equalTo(expectedEllipsis)
      }
    }
  }

  "SpcInterpreter" should
  {
    "understand people" in new InterpreterContext
    {
      loadBeliefs("/ontologies/people.txt")
      interpretMatrix(
        "is Todd Dirk's friend",
        "Yes, he is Dirk's friend.",
        "Yes, Todd is Dirk's friend.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Dirk Todd's friend",
        "Yes, he is Todd's friend.",
        "Yes, Dirk is Todd's friend.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Amanda Todd's sister",
        "Yes, she is his sister.",
        "Yes, Amanda is Todd's sister.",
        "Yes.",
        "Yes, she is.")
      interpretMatrix(
        "is Amanda Todd's sibling",
        "Yes, she is his sibling.",
        "Yes, Amanda is Todd's sibling.",
        "Yes.",
        "Yes, she is.")
      interpretMatrix(
        "is Amanda Todd's brother",
        "No, she is not his brother.",
        "No, Amanda is not Todd's brother.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Dirk Todd's sister",
        "No, he is not Todd's sister.",
        "No, Dirk is not Todd's sister.",
        "No.",
        "No, he is not.")
      interpretMatrix(
        "is Todd Amanda's brother",
        "Yes, he is her brother.",
        "Yes, Todd is Amanda's brother.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Amanda Todd's friend",
        "No, she is not his friend.",
        "No, Amanda is not Todd's friend.",
        "No.",
        "No, she is not.")
      // FIXME:  should clarify that Dirk actually has more than one friend
      interpretMatrix(
        "does Dirk have a friend",
        "Yes, he has a friend.",
        "Yes, Dirk has a friend.",
        "Yes.",
        "Yes, he does.")
      interpretMatrix(
        "does Dirk have any friends",
        "Yes, he has 2 of them.",
        "Yes, Dirk has 2 of them.",
        "Yes.",
        "Yes, he does.")
      interpretMatrix(
        "does Todd have friends",
        "Yes, he has friends.",
        "Yes, Todd has friends.",
        "Yes.",
        "Yes, he does.")
      interpretMatrix(
        "does Todd have any friends",
        "Yes, he has 1 of them.",
        "Yes, Todd has 1 of them.",
        "Yes.",
        "Yes, he does.")
      interpretMatrix(
        "does Amanda have friends",
        "No, she does not have friends.",
        "No, Amanda does not have friends.",
        "No.",
        "No, she does not.")
      interpretMatrix(
        "does Amanda have a friend",
        "No, she does not have a friend.",
        "No, Amanda does not have a friend.",
        "No.",
        "No, she does not.")
      interpretMatrix(
        "does Amanda have any friends",
        "No, she has no friends.",
        "No, Amanda has no friends.",
        "No.",
        "No, she does not.")
      interpretMatrix(
        "who is Todd",
        "He is Amanda's brother.",
        "Todd is Amanda's brother.",
        "Amanda's brother.",
        "Amanda's brother.")
      interpretMatrix(
        "who is Bart",
        "She is Rapunzel's owner.",
        "Bart is Rapunzel's owner.",
        "Rapunzel's owner.")
      interpretMatrix(
        "who is Todd's friend",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      interpretMatrix(
        "who are Todd's friends",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      interpretMatrix(
        "which person is Todd's friend",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      interpretMatrix(
        "who is Dirk's friend",
        "His friends are Todd and Bart.",
        "Dirk's friends are Todd and Bart.",
        "Todd and Bart.")
      interpretMatrix(
        "who is Amanda's friend",
        "No one is her friend.",
        "No one is Amanda's friend.",
        "No one.")
      interpretMatrix(
        "who are Amanda's friends",
        "No one is her friend.",
        "No one is Amanda's friend.",
        "No one.")
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
      interpretMatrix(
        "is Amanda a friend",
        "No, she is not a friend.",
        "No, Amanda is not a friend.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Amanda a brother",
        "No, she is not a brother.",
        "No, Amanda is not a brother.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Amanda a dog",
        "No, she is not a dog.",
        "No, Amanda is not a dog.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Amanda an owner",
        "No, she is not an owner.",
        "No, Amanda is not an owner.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Amanda a groomer",
        "No, she is not a groomer.",
        "No, Amanda is not a groomer.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Rapunzel a dog",
        "Yes, it is a dog.",
        "Yes, Rapunzel is a dog.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is Bart an owner",
        "Yes, she is an owner.",
        "Yes, Bart is an owner.",
        "Yes.",
        "Yes, she is.")
      interpret(
        "is Amanda a robot",
        "Sorry, I don't know about any 'robot'.")
      interpret(
        "who is a person",
        "Dirk, Todd, Amanda, Bart, Hugo, Scott, and Arthur are persons.")
      interpret(
        "who is a man",
        "Dirk, Todd, Hugo, and Arthur are men.")
      interpret(
        "who is a brother",
        "Todd is a brother.")
      // FIXME have to use BLACKWING because Blackwing gets parsed
      // as -ing verb, heh
      interpretMatrix(
        "is BLACKWING an organization",
        "Yes, it is an organization.",
        "Yes, BLACKWING is an organization.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is BLACKWING a conspiracy",
        "No, it is not a conspiracy.",
        "No, BLACKWING is not a conspiracy.",
        "No.",
        "No, it is not.")
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
      interpretMatrix(
        "who is Hugo",
        "He is one of BLACKWING's operatives.",
        "Hugo is one of BLACKWING's operatives.",
        "One of BLACKWING's operatives.")
      interpretMatrix(
        "who is Arthur",
        "He is a man.",
        "Arthur is a man.",
        "A man.")
      interpretMatrix(
        "is Todd masculine",
        "Yes, he is masculine.",
        "Yes, Todd is masculine.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Todd feminine",
        "No, he is not feminine.",
        "No, Todd is not feminine.",
        "No.",
        "No, he is not.")
      interpretMatrix(
        "is Todd fantastic",
        "No, he is not fantastic.",
        "No, Todd is not fantastic.",
        "No.",
        "No, he is not.")
      interpretMatrix(
        "is Amanda feminine",
        "Yes, she is feminine.",
        "Yes, Amanda is feminine.",
        "Yes.",
        "Yes, she is.")
      interpretMatrix(
        "is Amanda masculine",
        "No, she is not masculine.",
        "No, Amanda is not masculine.",
        "No.",
        "No, she is not.")
      interpretMatrix(
        "is Amanda fantastic",
        "No, she is not fantastic.",
        "No, Amanda is not fantastic.",
        "No.",
        "No, she is not.")
      interpret(
        "is Scott masculine",
        "I don't know.")
      interpret(
        "is Scott feminine",
        "I don't know.")
      interpret(
        "is Scott fantastic",
        "I don't know.")
      interpretMatrix(
        "is BLACKWING Hugo's employer",
        "Yes, it is his employer.",
        "Yes, BLACKWING is Hugo's employer.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "which organization is Hugo's employer",
        "His employer is BLACKWING.",
        "Hugo's employer is BLACKWING.",
        "BLACKWING.")
      interpretMatrix(
        "is BLACKWING Todd's employer",
        "No, it is not his employer.",
        "No, BLACKWING is not Todd's employer.",
        "No.",
        "No, it is not.")
      interpretMatrix(
        "which organization is Todd's employer",
        "No organization is his employer.",
        "No organization is Todd's employer.",
        "No organization.")
    }

    "understand relatives" in new InterpreterContext
    {
      loadBeliefs("/ontologies/relatives.txt")
      interpret("who is Henry", "He is Titus' uncle.")
      interpret("who is Marion's aunt", "Her aunt is Laura.")
      interpret("who is Marion's auntie", "Her auntie is Laura.")
      interpret("who is Laura's niece", "Her nieces are Fancy and Marion.")
      interpret("Fancy is Laura's nephew?", "No, she is not Laura's nephew.")
      interpret("is Everard a person?", "I don't know.")
    }

    "understand taxonomy" in new InterpreterContext
    {
      loadBeliefs("/ontologies/vehicles.txt")
      interpretMatrix(
        "is Herbie moving",
        "No, he is not moving.",
        "No, Herbie is not moving.",
        "No.",
        "No, he is not.")
      interpretMatrix(
        "is Herbie stopped",
        "Yes, he is stopped.",
        "Yes, Herbie is stopped.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Titanic moving",
        "Yes, it is moving.",
        "Yes, Titanic is moving.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is Titanic stopped",
        "No, it is not stopped.",
        "No, Titanic is not stopped.",
        "No.",
        "No, it is not.")
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
        "are both Herbie and Titanic moving",
        "No, Herbie is not moving.")
      interpretMatrix(
        "is Titanic floating",
        "No, it is not floating.",
        "No, Titanic is not floating.",
        "No.",
        "No, it is not.")
      interpret(
        "is Herbie floating",
        "Sorry, I don't know what 'float' means for Herbie.")
      interpret(
        "is any car floating",
        "Sorry, I don't know what 'float' means for a car.")
      interpret(
        "who is floating",
        "Sorry, I don't know what 'float' means for a person.")
      interpretMatrix(
        "is Herbie a car",
        "Yes, he is a car.",
        "Yes, Herbie is a car.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Herbie a boat",
        "No, he is not a boat.",
        "No, Herbie is not a boat.",
        "No.",
        "No, he is not.")
      interpretMatrix(
        "is Herbie a vehicle",
        "Yes, he is a vehicle.",
        "Yes, Herbie is a vehicle.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "is Titanic a boat",
        "Yes, it is a boat.",
        "Yes, Titanic is a boat.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is Titanic the boat",
        "Yes, it is the boat.",
        "Yes, Titanic is the boat.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is Titanic a vehicle",
        "Yes, it is a vehicle.",
        "Yes, Titanic is a vehicle.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is Titanic a car",
        "No, it is not a car.",
        "No, Titanic is not a car.",
        "No.",
        "No, it is not.")
      interpret(
        "how many vehicles are there",
        "There are 2 of them.")
      interpretMatrix(
        "Herbie and Titanic are vehicles?",
        "Yes, they are vehicles.",
        "Yes, Herbie and Titanic are vehicles.",
        "Yes.",
        "Yes, they are.")
      // FIXME resolve number agreement
      interpret(
        "which vehicles are there",
        "There is Herbie and Titanic.")
      interpretMatrix(
        "who is Herbie's owner",
        "His owner is Jim.",
        "Herbie's owner is Jim.",
        "Jim.")
      interpretMatrix(
        "who is Titanic's owner",
        "No one is its owner.",
        "No one is Titanic's owner.",
        "No one.")
    }

    "deal with conjunctive plural noun" in new InterpreterContext
    {
      skipped("maybe one day")
      loadBeliefs("/ontologies/vehicles.txt")
      interpretMatrix(
        "are Herbie and Titanic vehicles",
        "Yes, they are vehicles.",
        "Yes, Herbie and Titanic are vehicles.",
        "Yes.",
        "Yes, they are.")
    }

    "respond correctly to disjunctive query" in new InterpreterContext
    {
      skipped("maybe one day")
      loadBeliefs("/ontologies/people.txt")
      interpretMatrix(
        "is Rapunzel or Amanda a dog",
        "Yes, Rapunzel is a dog.",
        "Yes, Rapunzel is a dog.",
        "Yes.",
        "Yes, Rapunzel is.")
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
      interpretMatrix(
        "is the alarm service up",
        "Yes, it is up.",
        "Yes, the alarm service is up.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is the alarm service on",
        "Yes, it is on.",
        "Yes, the alarm service is on.",
        "Yes.",
        "Yes, it is.")
      interpretMatrix(
        "is the multimedia service up",
        "No, it is not up.",
        "No, the multimedia service is not up.",
        "No.",
        "No, it is not.")
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
      interpretMatrix(
        "is the multimedia server up",
        "No, it is not up.",
        "No, the multimedia server is not up.",
        "No.",
        "No, it is not.")
    }

    "understand presence" in new InterpreterContext
    {
      // FIXME:  in "is Jack home", interpret "home" as state instead of noun
      // FIXME:  "Jack's presence" should become "it"
      loadBeliefs("/ontologies/presence.txt")
      interpretMatrix("is Jack's presence on",
        "Yes, his presence is on.",
        "Yes, Jack's presence is on.",
        "Yes.",
        "Yes, his presence is.")
      interpretMatrix("is Jack present",
        "Yes, he is present.",
        "Yes, Jack is present.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix("is Jack at home",
        "Yes, he is at home.",
        "Yes, Jack is at home.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix("is Jack absent",
        "No, he is not absent.",
        "No, Jack is not absent.",
        "No.",
        "No, he is not.")
      interpretMatrix("is Jack away",
        "No, he is not away.",
        "No, Jack is not away.",
        "No.",
        "No, he is not.")
      interpretMatrix("is Jill's presence on",
        "No, her presence is not on.",
        "No, Jill's presence is not on.",
        "No.",
        "No, her presence is not.")
      interpretMatrix("is Jill present",
        "No, she is not present.",
        "No, Jill is not present.",
        "No.",
        "No, she is not.")
      interpretMatrix("is Jill at home",
        "No, she is not at home.",
        "No, Jill is not at home.",
        "No.",
        "No, she is not.")
      interpretMatrix("is Jill absent",
        "Yes, she is absent.",
        "Yes, Jill is absent.",
        "Yes.",
        "Yes, she is.")
      interpretMatrix("is Jill away",
        "Yes, she is away.",
        "Yes, Jill is away.",
        "Yes.",
        "Yes, she is.")
      interpret("is Jack on",
        "Sorry, I don't know what 'on' means for Jack.")
      interpretMatrix("is Casper's haunting on",
        "Yes, his haunting is on.",
        "Yes, Casper's haunting is on.",
        "Yes.",
        "Yes, his haunting is.")
      interpret("is Casper present",
        "I don't know.")
      interpretMatrix("is Yoda's presence on",
        "No, his presence is not on.",
        "No, Yoda's presence is not on.",
        "No.",
        "No, his presence is not.")
      interpretMatrix("is Yoda present",
        "No, he is not present.",
        "No, Yoda is not present.",
        "No.",
        "No, he is not.")
      interpretMatrix("is Yoda on",
        "No, he is not on.",
        "No, Yoda is not on.",
        "No.",
        "No, he is not.")
      interpretMatrix("is Yoda off",
        "Yes, he is off.",
        "Yes, Yoda is off.",
        "Yes.",
        "Yes, he is.")
      interpretMatrix(
        "are Jill and Yoda absent",
        "Yes, they are absent.",
        "Yes, Jill and Yoda are absent.",
        "Yes.",
        "Yes, they are.")
    }

    "understand multiple properties for same form" in new InterpreterContext
    {
      loadBeliefs("/ontologies/stove.txt")
      interpret("is there a stove?",
        "Yes, there is a stove.")
      interpret("is the stove hot?",
        "Yes, it is hot.")
      interpretMatrix("is the stove on?",
        "No, it is not on.",
        "No, the stove is not on.",
        "No.",
        "No, it is not.")
    }

    "allow pronouns to be avoided" in new InterpreterContext(
      false, ShlurdResponseParams().copy(thirdPersonPronouns = false))
    {
      loadBeliefs("/ontologies/stove.txt")
      interpret("is the stove hot?",
        "Yes, the stove is hot.")
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
      interpret(
        "Scott is RowdyThree's operative",
        "Previously I was told that a person may have one employer and " +
          "BLACKWING is Scott's employer.  So it does not add up when I " +
          "hear that Scott is RowdyThree's operative.")
    }
  }
}
