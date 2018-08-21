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
import com.lingeringsocket.shlurd.mind._

import org.specs2.mutable._

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
    beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
    params : SmcResponseParams = SmcResponseParams()
  ) extends NameSpace
  {
    protected val cosmos = new SpcCosmos {
      override protected[platonic] def evaluateEntityProperty(
        entity : SpcEntity,
        property : SpcProperty) =
      {
        val qualifiedName =
          (entity.qualifiers.toSeq :+ entity.form.name
            :+ property.name).mkString(" ")
        states.get(qualifiedName) match {
          case Some(state) => Success(Some(state))
          case _ => super.evaluateEntityProperty(entity, property)
        }
      }

      SpcPrimordial.initCosmos(this)
    }

    protected val mind = new SpcMind(cosmos)

    protected val interpreter =
      new SpcInterpreter(
        mind, beliefAcceptance, params)

    protected val interpreterWithoutPronouns =
      new SpcInterpreter(
        mind, beliefAcceptance, params.
          copy(thirdPersonPronouns = false))

    protected val interpreterTerse =
      new SpcInterpreter(
        mind, beliefAcceptance, params.
          copy(verbosity = RESPONSE_TERSE))

    protected val interpreterEllipsis =
      new SpcInterpreter(
        mind, beliefAcceptance, params.
          copy(verbosity = RESPONSE_ELLIPSIS))

    protected def loadBeliefs(resource : String)
    {
      val file = SprParser.getResourceFile(resource)
      val source = Source.fromFile(file)
      cosmos.loadBeliefs(source)
    }

    protected def interpret(input : String, expected : String) =
    {
      val sentence = SprParser(input).parseOne
      interpreter.interpret(sentence, input) must be equalTo(expected)
    }

    protected def interpretTerse(
      input : String,
      expected : String)
    {
      val sentence = SprParser(input).parseOne
      interpreterTerse.interpret(
        sentence, input) must be equalTo(expected)
    }

    protected def interpretBelief(input : String) =
    {
      interpret(input, "OK.")
    }

    protected def interpretMatrix(
      input : String,
      expectedWithPronouns : String,
      expectedWithoutPronouns : String,
      expectedTerse : String,
      expectedEllipsis : String = "") =
    {
      val sentence = SprParser(input).parseOne
      interpreter.interpret(sentence, input) must be equalTo(
        expectedWithPronouns)
      interpreterWithoutPronouns.interpret(
        sentence, input) must be equalTo(expectedWithoutPronouns)
      interpreterTerse.interpret(
        sentence, input) must be equalTo(expectedTerse)
      if (!expectedEllipsis.isEmpty) {
        interpreterEllipsis.interpret(
          sentence, input) must be equalTo(expectedEllipsis)
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
        "Yes, he has two of them.",
        "Yes, Dirk has two of them.",
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
        "Yes, he has one of them.",
        "Yes, Todd has one of them.",
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

    "understand relatives" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/relatives.txt")
      interpret("who is Henry", "He is Titus' uncle.")
      interpret("who is Marion's aunt", "Her aunt is Laura.")
      interpret("who is the aunt of Marion", "Her aunt is Laura.")
      interpret("who is Marion's auntie", "Her auntie is Laura.")
      interpret("who is Laura's niece", "Her nieces are Fancy and Marion.")
      interpret("Fancy is Laura's nephew?", "No, she is not Laura's nephew.")
      interpret("is Everard a person?", "I don't know.")
      interpret("does Laura have a godmother", "Yes, she has a godmother.")
      interpret("who is Laura's godmother", "I don't know.")
      interpret("Marion is Laura's godmother?", "I don't know.")
      // FIXME we can rule this one out since a godmother must be a woman
      interpret("Henry is Laura's godmother?", "I don't know.")
      interpret("does Laura have a godfather",
        "No, she does not have a godfather.")
      interpretBelief("Fancy is Laura's godmother")
      interpretBelief("Titus is Laura's godfather")
      interpret("who is Laura's godmother", "Her godmother is Fancy.")
      interpret("who is Laura's godfather", "Her godfather is Titus.")

      cosmos.sanityCheck must beTrue
    }

    "understand locations" in new InterpreterContext
    {
      loadBeliefs("/ontologies/location.txt")

      interpretMatrix(
        "where is Jack",
        "He is in Herbie.",
        "Jack is in Herbie.",
        "Herbie.",
        "Herbie.")
      interpretTerse("where is Ubuntu", "Nowhere.")
      interpretTerse("where is Herbie", "I don't know.")
      interpretTerse("where is Christine", "I don't know.")
      interpretTerse("where is Chrissy", "Christine.")
      interpretTerse("where is Janet", "Christine.")
      interpretTerse("is Jack in Herbie", "Yes.")
      interpretTerse("is Jack in Christine", "No.")
      interpretTerse("is Chrissy in Herbie", "No.")
      interpretTerse("is Chrissy in Christine", "Yes.")
      interpretTerse("is Janet in Herbie", "No.")
      interpretTerse("is Janet in Christine", "Yes.")
      interpretTerse("who is in Herbie", "Jack.")
      interpretTerse("who is in Christine", "Chrissy and Janet.")
      interpretTerse("how many men are in Herbie", "One of them.")
      interpretTerse("how many women are in Herbie", "No women.")
      interpretTerse("how many men are in Christine", "No men.")
      interpretTerse("how many women are in Christine", "Two of them.")
      cosmos.sanityCheck must beTrue
    }

    "understand inverse associations" in new
      InterpreterContext(ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("a confessor must be a person")
      interpretBelief("a confessee must be a person")
      interpretBelief("a person may have confessees")
      interpretBelief("a person may have a confessor")
      interpretBelief("a person with a confessee is a confessor")
      interpretBelief("Eugene is John's confessor")
      interpretBelief("Eugene is Erik's confessor")
      interpretBelief("Jerold is Erik's confessor")
      interpretTerse("who are Eugene's confessees", "John.")
      interpretBelief("John has no confessor")
      interpretTerse("who is John's confessor", "No one.")
      interpretTerse("who are Eugene's confessees", "No one.")
    }

    "understand actions" in new InterpreterContext(ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("if an object moves to a location, " +
        "then the object is in the location")
      interpretBelief("if an object rolls into a location, " +
        "then the object moves to the location")
      interpretBelief("Percy is an object")
      interpretBelief("Thomas is an object")
      interpretBelief("Fuji is an object")
      interpretBelief("Kilimanjaro is an object")
      interpretBelief("Denali is an object")
      interpretBelief("Percy moves to Fuji")
      interpret("where is Percy", "It is in Fuji.")
      interpretBelief("Percy rolls into Kilimanjaro")
      interpret("where is Percy", "It is in Kilimanjaro.")

      interpret("Percy rolls to Denali",
        "Sorry, I cannot understand what you said.")
      interpret("where is Percy", "It is in Kilimanjaro.")

      interpretBelief("Percy and Thomas move to Denali")
      interpret("where is Percy", "It is in Denali.")
      interpret("where is Thomas", "It is in Denali.")

      interpretBelief("if a person drops an object, " +
        "then the object is in the person's container")
      interpretBelief("Curtis is a person")
      interpretBelief("the boiler is an object")
      interpretBelief("the engine is an object")
      interpretBelief("the bomb is an object")
      interpret("where is the bomb", "It is nowhere.")
      interpretBelief("Curtis is in the boiler")
      interpretBelief("Curtis drops the bomb")
      interpretBelief("Curtis moves to the engine")
      interpretTerse("where is Curtis", "The engine.")
      interpret("where is the bomb", "It is in the boiler.")
    }

    "understand indirect objects" in new
      InterpreterContext(ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("if a person gives an object to a recipient, " +
        "then the object is the recipient's containee")
      interpretBelief("if a person passes an object to a recipient, " +
        "then the person gives the object to the recipient")

      interpretBelief("Curtis is a person")
      interpretBelief("Andrew is a person")
      interpretBelief("the bomb is an object")
      interpret("where is the bomb", "It is nowhere.")
      interpretBelief("Curtis gives Andrew the bomb")
      interpretTerse("where is the bomb", "Andrew.")
      interpretBelief("Andrew passes the bomb to Curtis")
      interpretTerse("where is the bomb", "Curtis.")
    }

    "understand genitives in beliefs" in new
      InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("the wrench is an object")
      interpretBelief("the screwdriver is an object")
      interpretBelief("the engine is an object")
      interpretBelief("the wrench is Mason's possession")
      interpretBelief("the screwdriver is Mason's possession")
      interpretBelief("the engine's containees are Mason's possessions")
      interpretTerse("which objects are in the engine",
        "The wrench and the screwdriver.")
    }

    "understand epsilon beliefs" in new
      InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("the engine is an object")
      interpretBelief("Mason is a person")
      interpretBelief("the engine's containee is Mason's possession")
      interpretTerse("which objects are in the engine", "No objects.")
    }

    "understand compound subject references" in new
      InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("the engine is an object")
      interpretBelief("the wrench is an object")
      interpretBelief("the screwdriver is an object")
      interpretBelief("the saw is an object")
      interpretBelief("Edgar is a person")
      interpretBelief("the wrench is Edgar's possession")
      interpretBelief("the screwdriver is Edgar's possession")
      interpretBelief("Edgar's possessions are in the engine")
      interpretTerse("which objects are in the engine",
        "The wrench and the screwdriver.")
    }

    "understand unique determiner in genitive" in new
      InterpreterContext(ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("the engine is an object")
      interpretBelief("the box is an object")
      interpretBelief("the box is in the engine")
      interpretBelief("the wrench is an object")
      interpretBelief("the wrench's container is the box's container")
      interpretTerse("which objects are in the engine",
        "The box and the wrench.")
    }

    "understand taxonomy" in new InterpreterContext
    {
      loadBeliefs("/ontologies/vehicles.txt")

      interpret("is Herbie moving", "No, he is not moving.")

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
        "There are two of them.")
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
      interpretMatrix("is Jack's personal_presence on",
        "Yes, his personal_presence is on.",
        "Yes, Jack's personal_presence is on.",
        "Yes.",
        "Yes, his personal_presence is.")
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
      interpretMatrix("is Jill's personal_presence on",
        "No, her personal_presence is not on.",
        "No, Jill's personal_presence is not on.",
        "No.",
        "No, her personal_presence is not.")
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
      interpretMatrix("is Yoda's personal_presence on",
        "No, his personal_presence is not on.",
        "No, Yoda's personal_presence is not on.",
        "No.",
        "No, his personal_presence is not.")
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
      ACCEPT_NO_BELIEFS,
      SmcResponseParams().copy(thirdPersonPronouns = false))
    {
      loadBeliefs("/ontologies/stove.txt")
      interpret("is the stove hot?",
        "Yes, the stove is hot.")
    }

    "understand conversational pronoun references" in new InterpreterContext(
      ACCEPT_MODIFIED_BELIEFS,
      SmcResponseParams().copy(thirdPersonPronouns = false))
    {
      loadBeliefs("/ontologies/people.txt")
      mind.startConversation
      interpret("is she a dog",
        "Sorry, when you say 'she' I don't know who or what you mean.")
      interpret("who is Todd", "Todd is Amanda's brother.")
      interpret("is she a dog", "No, Amanda is not a dog.")
      interpret("is he Dirk's friend", "Yes, Todd is Dirk's friend.")
      interpretBelief("the jail is an object")
      interpretBelief("if a person teleports, then the person is in the jail")
      interpretBelief("Todd and Dirk teleport")
      interpret("are they in the jail", "Yes, Todd and Dirk are in the jail.")
    }

    "understand sequential timeframes" in new InterpreterContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("the key is an object")
      interpretBelief("the pocket is an object")
      interpretBelief("the purse is an object")
      interpretBelief("the shoe is an object")
      interpretTerse("where was the key before the pocket",
        "No narrative in progress.")
      mind.startNarrative
      interpretBelief("the key was in the pocket")
      interpretBelief("after that the key was in the purse")
      interpretBelief("after that the key was in the shoe")
      interpretTerse("where is the key", "The shoe.")
      interpretTerse("where was the key",
        "A timeframe must be specified.")
      interpretTerse("where was the key before the purse", "The pocket.")
      interpretTerse("where was the key after the purse", "The shoe.")
      interpretTerse("where was the key before the shoe", "The purse.")
      interpretTerse("where was the key after the pocket", "The purse.")
      interpretTerse("where was the key after the shoe",
        "No such timeframe in narrative.")
      interpretTerse("where was the key before the pocket",
        "No such timeframe in narrative.")
    }

    "understand relative timeframes" in new InterpreterContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("the key is an object")
      interpretBelief("the pocket is an object")
      interpretBelief("the purse is an object")
      interpretBelief("the shoe is an object")
      interpretBelief("the card is an object")
      mind.startNarrative
      interpretBelief("the key was in the pocket this afternoon")
      interpretBelief("this morning, the card was in the purse")
      interpretBelief("yesterday, the card was in the shoe")
      interpretBelief("this evening, the key was in the shoe")
      interpretBelief("this afternoon the card was in the shoe")
      interpretTerse("where was the card before the shoe", "The purse.")
      interpretTerse("where was the card before the purse", "The shoe.")
      interpretTerse("where was the key before the shoe", "The pocket.")
    }

    "detect causality violations" in  new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      mind.startNarrative
      interpretBelief("yesterday, Harvey was Elwood's pet")
      interpret("this afternoon, Elwood had no pets",
        "The belief that Elwood had no pets " +
          "contradicts the belief that Harvey is Elwood's pet.")
      interpretBelief("this afternoon, Calvin had no pets")
      interpret("yesterday, Hobbes was Calvin's pet",
        "The belief that Calvin has no pets " +
          "contradicts the belief that Hobbes is Calvin's pet.")
    }

    "understand progressive action predicates" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("If an item is filling an object, " +
        "then the item is the object's containee.")
      interpretBelief("If an item is occupying an object, " +
        "then the item is in the object.")

      interpretBelief("If an object is carrying an item, " +
        "then the item is the object's containee.")

      // FIXME this belief should be equivalent
      /*
      interpretBelief("If an object is carrying an item, " +
        "then the object is the item's container.")
       */

      interpretBelief("The wallet is an object.")
      interpretBelief("The pocket is an object.")
      interpretBelief("The money is an object.")
      interpretBelief("The card is an object.")
      interpretBelief("The key is an object.")
      interpretBelief("The money is in the wallet.")
      interpretBelief("The card is in the wallet.")
      interpretBelief("The key is in the pocket.")
      interpret("how many objects are in the wallet",
        "Two of them are in the wallet.")
      interpret("how many objects are in the pocket",
        "One of them is in the pocket.")
      interpret("how many objects are the wallet's containee",
        "Two of them are its containees.")
      interpret("how many objects are the pocket's containees",
        "One of them is its containee.")
      interpretMatrix("how many objects are filling the wallet",
        "Two of them are filling it.",
        "Two of them are filling the wallet.",
        "Two of them.",
        "Two of them.")
      interpretMatrix("how many objects is the wallet carrying",
        "It is carrying two of them.",
        "The wallet is carrying two of them.",
        "Two of them.",
        "Two of them.")
      interpretMatrix("how many objects are occupying the pocket",
        "One of them is occupying it.",
        "One of them is occupying the pocket.",
        "One of them.",
        "One of them.")
      interpretMatrix("which objects are filling the wallet",
        "The money and the card are filling it.",
        "The money and the card are filling the wallet.",
        "The money and the card.",
        "The money and the card.")
      interpretMatrix("which objects are occupying the pocket",
        "The key is occupying it.",
        "The key is occupying the pocket.",
        "The key.",
        "The key.")
    }

    "understand state queries" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      skipped("very soon now")
      interpretBelief(
        "an animal's color must be white, gray, yellow, or green")
      interpretBelief("Leo is an animal.")
      interpretBelief("Leo is yellow.")
      interpretTerse("what color is Leo", "yellow")
    }

    "prevent new beliefs" in new InterpreterContext
    {
      interpret("There is a front door",
        "Sorry, I don't know about any 'door'.")
    }

    "accept new beliefs" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a door may be either open or closed")
      interpretBelief("there is a front door")
      interpret("is the front door open",
        "I don't know.")
    }

    "understand property updates" in new
      InterpreterContext(ACCEPT_MODIFIED_BELIEFS)
    {
      interpretBelief("a door may be open or closed")
      interpretBelief("there is a door")
      interpretTerse("is the door open", "I don't know.")
      interpretBelief("the door is open")
      interpretTerse("is the door open", "Yes.")
      interpretTerse("is the door closed", "No.")
      interpretBelief("the door is closed")
      interpretTerse("is the door open", "No.")
      interpretTerse("is the door closed", "Yes.")

      cosmos.sanityCheck must beTrue
    }

    "reject invalid new beliefs" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("there is a front door")
      interpret("there is a big front door",
        "Previously I was told that there is a front door.  " +
          "So there is an ambiguous reference in the belief that " +
          "there is a big front door.")
    }

    "reject cyclic taxonomy belief" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a bird is a kind of animal")
      interpretBelief("a duck is a kind of bird")
      interpret("an animal is a kind of duck",
        "The belief that an animal is a kind of duck contradicts " +
          "the belief that a duck is a kind of a bird and " +
          "a bird is a kind of an animal.")
    }

    "reject incompatible form for role" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a person must have a lawyer")
      interpretBelief("a lawyer must be a weasel")
      interpretBelief("Michael is a snake")
      interpret("Michael is Donald's lawyer",
        "The belief that Michael is Donald's lawyer contradicts " +
          "the belief that a lawyer must be a weasel.")

      cosmos.sanityCheck must beTrue
    }

    "reject unknown actions" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("Superman is a person")
      interpretBelief("the kite is an object")
      interpret("Superman flies the kite",
        "Sorry, I cannot understand what you said.")
    }

    "prevent action cycles" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("if a person sends a recipient a message, " +
        "then the person conveys the recipient the message")
      interpretBelief("if a person conveys a recipient a message, " +
        "then the person sends the recipient the message")
      interpretBelief("Curtis is a person")
      interpretBelief("Andrew is a person")
      interpretBelief("the signal is a message")
      interpret("Curtis sends Andrew the signal",
        "Action beliefs are circular.")
    }

    "reify unknown person" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a person must have a lawyer")
      interpretBelief("Donald is a person")
      interpret("who is Donald's lawyer", "I don't know.")

      cosmos.sanityCheck must beTrue
    }

    "infer form from role" in new InterpreterContext(ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a lawyer must be a weasel")
      interpretBelief("Michael is Donald's lawyer")
      interpretTerse("is Michael a weasel", "Yes.")

      cosmos.sanityCheck must beTrue
    }

    "support roles with multiple forms" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
    {
      interpretBelief("a man is a kind of person")
      interpretBelief("a gentleman is a kind of man")
      interpretBelief("a footman must be a man")
      interpretBelief("a footman must be a plebeian")
      interpretBelief("a gentleman with a footman is a lord")
      interpretBelief("Bunter is Peter's footman")
      interpretTerse("is Bunter a footman", "Yes.")
      interpretTerse("is Bunter a man", "Yes.")
      interpretTerse("is Bunter a plebeian", "Yes.")
      interpretTerse("is Peter a gentleman", "Yes.")
      interpretTerse("who is Peter's footman", "Bunter.")
      interpretTerse("who is Bunter's lord", "Peter.")

      cosmos.sanityCheck must beTrue
    }

    "validate constraints incrementally" in new InterpreterContext(
      ACCEPT_NEW_BELIEFS)
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

    "validate constraints incrementally" in new InterpreterContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/people.txt")
      interpretBelief("Amanda is Rapunzel's owner")
      interpretTerse("who is Rapunzel's owner", "Amanda.")
      interpretBelief("Scott is RowdyThree's operative")
      interpretTerse("who is RowdyThree's operative", "Scott.")
    }
  }
}
