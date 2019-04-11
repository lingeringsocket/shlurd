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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

import scala.io._
import scala.util._

class SpcResponderSpec extends Specification
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
    "lusitania boat boat_cruise_sink" -> "sink",
    "lusitania boat vehicle_move_stop" -> "move",
    "herbie car vehicle_move_stop" -> "stop"
  )

  abstract class ResponderContext(
    beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
    params : SmcResponseParams = SmcResponseParams()
  ) extends Scope
  {
    protected val cosmos = new SpcCosmos {
      override def evaluateEntityProperty(
        entity : SpcEntity,
        propertyName : String,
        specific : Boolean) =
      {
        val qualifiedName =
          (entity.qualifiers.toSeq :+ entity.form.name
            :+ propertyName).mkString(" ")
        states.get(qualifiedName) match {
          case Some(state) => Success((
            findProperty(entity.form, propertyName), Some(state)))
          case _ => super.evaluateEntityProperty(entity, propertyName, specific)
        }
      }

      SpcPrimordial.initCosmos(this)
    }

    protected val mind = new SpcMind(cosmos)

    protected val responder =
      new SpcResponder(
        mind, beliefAcceptance, params)

    protected val responderWithoutPronouns =
      new SpcResponder(
        mind, beliefAcceptance, params.
          copy(thirdPersonPronouns = false))

    protected val responderTerse =
      new SpcResponder(
        mind, beliefAcceptance, params.
          copy(verbosity = RESPONSE_TERSE))

    protected val responderEllipsis =
      new SpcResponder(
        mind, beliefAcceptance, params.
          copy(verbosity = RESPONSE_ELLIPSIS))

    protected def loadBeliefs(resource : String)
    {
      val file = ResourceUtils.getResourceFile(resource)
      val source = Source.fromFile(file)
      mind.loadBeliefs(source)
    }

    protected def process(input : String, expected : String) =
    {
      val sentence = responder.newParser(input).parseOne
      s"pass:  $input" ==> (
        responder.process(sentence, input) === expected)
    }

    protected def processTerse(
      input : String,
      expected : String)
    {
      val sentence = responder.newParser(input).parseOne
      s"pass:  $input" ==> (
        responderTerse.process(sentence, input) === expected)
    }

    protected def processBelief(input : String) =
    {
      process(input, "OK.")
    }

    protected def processMatrix(
      input : String,
      expectedWithPronouns : String,
      expectedWithoutPronouns : String,
      expectedTerse : String,
      expectedEllipsis : String = "") =
    {
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence, input) must be equalTo(
        expectedWithPronouns)
      responderWithoutPronouns.process(
        sentence, input) must be equalTo(expectedWithoutPronouns)
      responderTerse.process(
        sentence, input) must be equalTo(expectedTerse)
      if (!expectedEllipsis.isEmpty) {
        responderEllipsis.process(
          sentence, input) must be equalTo(expectedEllipsis)
      }
    }
  }

  "SpcResponder" should
  {
    "understand people" in new ResponderContext
    {
      loadBeliefs("/ontologies/people.txt")
      processMatrix(
        "is Todd Dirk's friend",
        "Yes, he is Dirk's friend.",
        "Yes, Todd is Dirk's friend.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Dirk Todd's friend",
        "Yes, he is Todd's friend.",
        "Yes, Dirk is Todd's friend.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Amanda Todd's sister",
        "Yes, she is his sister.",
        "Yes, Amanda is Todd's sister.",
        "Yes.",
        "Yes, she is.")
      processMatrix(
        "is Amanda Todd's sibling",
        "Yes, she is his sibling.",
        "Yes, Amanda is Todd's sibling.",
        "Yes.",
        "Yes, she is.")
      processMatrix(
        "is Amanda Todd's brother",
        "No, she is not his brother.",
        "No, Amanda is not Todd's brother.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Dirk Todd's sister",
        "No, he is not Todd's sister.",
        "No, Dirk is not Todd's sister.",
        "No.",
        "No, he is not.")
      processMatrix(
        "is Todd Amanda's brother",
        "Yes, he is her brother.",
        "Yes, Todd is Amanda's brother.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Amanda Todd's friend",
        "No, she is not his friend.",
        "No, Amanda is not Todd's friend.",
        "No.",
        "No, she is not.")
      // FIXME:  should clarify that Dirk actually has more than one friend
      processMatrix(
        "does Dirk have a friend",
        "Yes, he has a friend.",
        "Yes, Dirk has a friend.",
        "Yes.",
        "Yes, he does.")
      processMatrix(
        "does Dirk have any friends",
        "Yes, he has two of them.",
        "Yes, Dirk has two of them.",
        "Yes.",
        "Yes, he does.")
      processMatrix(
        "does Todd have friends",
        "Yes, he has friends.",
        "Yes, Todd has friends.",
        "Yes.",
        "Yes, he does.")
      processMatrix(
        "does Todd have any friends",
        "Yes, he has one of them.",
        "Yes, Todd has one of them.",
        "Yes.",
        "Yes, he does.")
      processMatrix(
        "does Amanda have friends",
        "No, she does not have friends.",
        "No, Amanda does not have friends.",
        "No.",
        "No, she does not.")
      processMatrix(
        "does Amanda have a friend",
        "No, she does not have a friend.",
        "No, Amanda does not have a friend.",
        "No.",
        "No, she does not.")
      processMatrix(
        "does Amanda have any friends",
        "No, she has no friends.",
        "No, Amanda has no friends.",
        "No.",
        "No, she does not.")
      processMatrix(
        "who is Todd",
        "He is Amanda's brother.",
        "Todd is Amanda's brother.",
        "Amanda's brother.",
        "Amanda's brother.")
      processMatrix(
        "who is Bart",
        "She is Rapunzel's owner.",
        "Bart is Rapunzel's owner.",
        "Rapunzel's owner.")
      processMatrix(
        "who is Todd's friend",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      processMatrix(
        "who are Todd's friends",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      processMatrix(
        "which person is Todd's friend",
        "His friend is Dirk.",
        "Todd's friend is Dirk.",
        "Dirk.")
      processMatrix(
        "who is Dirk's friend",
        "His friends are Todd and Bart.",
        "Dirk's friends are Todd and Bart.",
        "Todd and Bart.")
      processMatrix(
        "who is Amanda's friend",
        "No one is her friend.",
        "No one is Amanda's friend.",
        "No one.")
      processMatrix(
        "who are Amanda's friends",
        "No one is her friend.",
        "No one is Amanda's friend.",
        "No one.")
      process(
        "who has Amanda's friend",
        "But I don't know about any such friend.")
      process(
        "is Ford Todd's friend",
        "Sorry, I don't know about any 'Ford'.")
      process(
        "is Todd Ford's friend",
        "Sorry, I don't know about any 'Ford'.")
      // FIXME:  should clarify that they are not necessarily
      // friends OF EACH OTHER
      process(
        "who is a friend",
        "Dirk, Todd, and Bart are friends.")
      processMatrix(
        "is Amanda a friend",
        "No, she is not a friend.",
        "No, Amanda is not a friend.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Amanda a brother",
        "No, she is not a brother.",
        "No, Amanda is not a brother.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Amanda a dog",
        "No, she is not a dog.",
        "No, Amanda is not a dog.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Amanda an owner",
        "No, she is not an owner.",
        "No, Amanda is not an owner.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Amanda a groomer",
        "No, she is not a groomer.",
        "No, Amanda is not a groomer.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Rapunzel a dog",
        "Yes, it is a dog.",
        "Yes, Rapunzel is a dog.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is Bart an owner",
        "Yes, she is an owner.",
        "Yes, Bart is an owner.",
        "Yes.",
        "Yes, she is.")
      process(
        "is Amanda a robot",
        "Sorry, I don't know about any 'robot'.")
      process(
        "who is a person",
        "Scott, Dirk, Todd, Hugo, Arthur, Amanda, and Bart are persons.")
      process(
        "who is a man",
        "Dirk, Todd, Hugo, and Arthur are men.")
      process(
        "who is a brother",
        "Todd is a brother.")
      // FIXME have to use BLACKWING because Blackwing gets parsed
      // as -ing verb, heh
      processMatrix(
        "is BLACKWING an organization",
        "Yes, it is an organization.",
        "Yes, BLACKWING is an organization.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is BLACKWING a conspiracy",
        "No, it is not a conspiracy.",
        "No, BLACKWING is not a conspiracy.",
        "No.",
        "No, it is not.")
      process(
        "who has an uncle",
        "No one has an uncle.")
      process(
        "which person has an uncle",
        "No person has an uncle.")
      process(
        "who has a friend",
        "Dirk and Todd have a friend.")
      process(
        "who has friends",
        "Dirk and Todd have friends.")
      process(
        "who is Ford",
        "Sorry, I don't know about any 'Ford'.")
      processMatrix(
        "who is Hugo",
        "He is one of BLACKWING's operatives.",
        "Hugo is one of BLACKWING's operatives.",
        "One of BLACKWING's operatives.")
      processMatrix(
        "who is Arthur",
        "He is a man.",
        "Arthur is a man.",
        "A man.")
      processMatrix(
        "is Todd masculine",
        "Yes, he is masculine.",
        "Yes, Todd is masculine.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Todd feminine",
        "No, he is not feminine.",
        "No, Todd is not feminine.",
        "No.",
        "No, he is not.")
      processMatrix(
        "is Todd fantastic",
        "No, he is not fantastic.",
        "No, Todd is not fantastic.",
        "No.",
        "No, he is not.")
      processMatrix(
        "is Amanda feminine",
        "Yes, she is feminine.",
        "Yes, Amanda is feminine.",
        "Yes.",
        "Yes, she is.")
      processMatrix(
        "is Amanda masculine",
        "No, she is not masculine.",
        "No, Amanda is not masculine.",
        "No.",
        "No, she is not.")
      processMatrix(
        "is Amanda fantastic",
        "No, she is not fantastic.",
        "No, Amanda is not fantastic.",
        "No.",
        "No, she is not.")
      process(
        "is Scott masculine",
        "I don't know.")
      process(
        "is Scott feminine",
        "I don't know.")
      process(
        "is Scott fantastic",
        "I don't know.")
      processMatrix(
        "is BLACKWING Hugo's employer",
        "Yes, it is his employer.",
        "Yes, BLACKWING is Hugo's employer.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "which organization is Hugo's employer",
        "His employer is BLACKWING.",
        "Hugo's employer is BLACKWING.",
        "BLACKWING.")
      processMatrix(
        "is BLACKWING Todd's employer",
        "No, it is not his employer.",
        "No, BLACKWING is not Todd's employer.",
        "No.",
        "No, it is not.")
      processMatrix(
        "which organization is Todd's employer",
        "No organization is his employer.",
        "No organization is Todd's employer.",
        "No organization.")
    }

    "understand relatives" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }
      loadBeliefs("/ontologies/relatives.txt")
      process("who is Henry", "He is Titus' uncle.")
      process("who is Marion's aunt", "Her aunt is Laura.")
      process("who is the aunt of Marion", "Her aunt is Laura.")
      process("who is Marion's auntie", "Her auntie is Laura.")
      process("who is Laura's niece", "Her nieces are Fancy and Marion.")
      process("Fancy is Laura's nephew?", "No, she is not Laura's nephew.")
      process("is Everard a person?", "I don't know.")
      process("does Laura have a godmother", "Yes, she has a godmother.")
      process("who is Laura's godmother", "I don't know.")
      process("Marion is Laura's godmother?",
        "No, she is not Laura's godmother.")
      process("Fancy is Laura's godmother?",
        "No, she is not Laura's godmother.")
      process("Henry is Laura's godmother?",
        "No, he is not her godmother.")
      process("does Laura have a godfather",
        "No, she does not have a godfather.")
      processBelief("Fancy is Laura's godmother")
      processBelief("Titus is Laura's wise guy")
      process("who is Laura's godmother",
        "Her godmother is Fancy.")
      process("who is Laura's godfather",
        "Her godfather is Titus.")
      process("Marion is Laura's godmother?",
        "No, she is not Laura's godmother.")
      process("Fancy is Laura's godmother?",
        "Yes, she is Laura's godmother.")
      process("does Laura have a wise guy",
        "Yes, she has a wise guy.")
      process("who is Henry's cleaning lady",
        "His cleaning ladies are Fancy and Marion.")

      cosmos.sanityCheck must beTrue
    }

    "understand locations" in new ResponderContext
    {
      loadBeliefs("/ontologies/containment.txt")
      loadBeliefs("/ontologies/location.txt")

      processMatrix(
        "where is Jack",
        "He is in Herbie.",
        "Jack is in Herbie.",
        "Herbie.",
        "Herbie.")
      processTerse("where is Ubuntu", "Nowhere.")
      processTerse("where is Herbie", "I don't know.")
      processTerse("where is Christine", "I don't know.")
      processTerse("where is Chrissy", "Christine.")
      processTerse("where is Janet", "Christine.")
      processTerse("is Jack in Herbie", "Yes.")
      processTerse("is Jack in Christine", "No.")
      processTerse("is Chrissy in Herbie", "No.")
      processTerse("is Chrissy in Christine", "Yes.")
      processTerse("is Janet in Herbie", "No.")
      processTerse("is Janet in Christine", "Yes.")
      processTerse("who is in Herbie", "Jack.")
      processTerse("who is in Christine", "Chrissy and Janet.")
      processTerse("how many men are in Herbie", "One of them.")
      processTerse("how many women are in Herbie", "No women.")
      processTerse("how many men are in Christine", "No men.")
      processTerse("how many women are in Christine", "Two of them.")
      processMatrix(
        "where is the helicopter",
        "But I don't know about any such helicopter.",
        "But I don't know about any such helicopter.",
        "But I don't know about any such helicopter.",
        "But I don't know about any such helicopter.")
      cosmos.sanityCheck must beTrue
    }

    "understand inverse associations" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      processBelief("a person is a kind of spc-someone")
      processBelief("a professor must be a person")
      processBelief("a student must be a person")
      processBelief("a person may have students")
      processBelief("a person may have a professor")
      processBelief("a person with a student is a professor")
      processBelief("Eugene is John's professor")
      processBelief("Eugene is Erik's professor")
      processBelief("Jerold is Erik's professor")
      processTerse("who are Eugene's students", "John.")
      processBelief("John has no professor")
      processTerse("who is John's professor", "No one.")
      processTerse("who are Eugene's students", "No one.")
    }

    "understand actions" in new ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("if an object moves to a location, " +
        "then the object is in the location")
      processBelief("if an object rolls into a location, " +
        "then the object moves to the location")
      processBelief("Percy is an object")
      processBelief("Thomas is an object")
      processBelief("Fuji is an object")
      processBelief("Kilimanjaro is an object")
      processBelief("Denali is an object")
      processBelief("Percy moves to Fuji")
      process("where is Percy", "It is in Fuji.")
      processBelief("Percy rolls into Kilimanjaro")
      process("where is Percy", "It is in Kilimanjaro.")

      process("Percy rolls to Denali",
        "I'm not sure how to interpret that.")
      process("where is Percy", "It is in Kilimanjaro.")

      processBelief("Percy and Thomas move to Denali")
      process("where is Percy", "It is in Denali.")
      process("where is Thomas", "It is in Denali.")

      processBelief("if a person drops an object, " +
        "then the object is in the person's container")
      processBelief("Curtis is a person")
      processBelief("the boiler is an object")
      processBelief("the engine is an object")
      processBelief("the bomb is an object")
      process("where is the bomb", "I don't know.")
      processBelief("Curtis is in the boiler")
      processBelief("Curtis drops the bomb")
      processBelief("Curtis moves to the engine")
      processTerse("where is Curtis", "The engine.")
      process("where is the bomb", "It is in the boiler.")
    }

    "understand indirect objects" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("if a person gives an object to a recipient, " +
        "then the object is the recipient's contained-object")
      processBelief("if a person passes an object to a recipient, " +
        "then the person gives the object to the recipient")

      processBelief("Curtis is a person")
      processBelief("Andrew is a person")
      processBelief("the bomb is an object")
      process("where is the bomb", "I don't know.")
      processBelief("Curtis gives Andrew the bomb")
      processTerse("where is the bomb", "Andrew.")
      processBelief("Andrew passes the bomb to Curtis")
      processTerse("where is the bomb", "Curtis.")
    }

    "understand past actions" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      mind.startConversation
      mind.startNarrative
      processBelief("a man is a kind of person")
      processBelief("a woman is a kind of person")
      processBelief("a man's gender must be masculine")
      processBelief("a woman's gender must be feminine")
      processBelief("Curtis is a man")
      processBelief("Andrea is a woman")
      processBelief("Thomas is a man")
      processBelief("the bomb is an object")
      processBelief("the wrench is an object")
      processBelief("the screwdriver is an object")
      processBelief("if a person receives an object, " +
        "then the object is the person's contained-object")
      processBelief("if a person gives an object to a recipient, " +
        "then the recipient receives the object")
      processBelief("if a person passes an object to a recipient, " +
        "then the person gives the object to the recipient")
      processBelief("Thomas passed the wrench to Andrea")
      processBelief("Curtis passed the bomb to her")
      processBelief("Curtis passed the screwdriver to Thomas")
      // FIXME irregular forms
      processMatrix(
        "what did Curtis give to Andrea",
        "He gived the bomb to her.",
        "Curtis gived the bomb to Andrea.",
        "The bomb.",
        "The bomb.")
      processTerse("what did Curtis give to Thomas", "The screwdriver.")
      processTerse("what did Thomas give to Andrea", "The wrench.")
      processMatrix(
        "who did Curtis give the bomb to",
        "He gived it to Andrea.",
        "Curtis gived the bomb to Andrea.",
        "Andrea.",
        "Andrea.")
      processMatrix(
        "who received the bomb",
        "Andrea received it.",
        "Andrea received the bomb.",
        "Andrea.",
        "Andrea.")
    }

    "understand genitives in beliefs" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the wrench is an object")
      processBelief("the screwdriver is an object")
      processBelief("the engine is an object")
      processBelief("the wrench is Mason's possession")
      processBelief("the screwdriver is Mason's possession")
      processBelief("the engine's contained-objects are Mason's possessions")
      processTerse("which objects are in the engine",
        "The wrench and the screwdriver.")
    }

    "understand constraints in beliefs" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a wire must be red or blue")
      processBelief("there is an important wire")
      processBelief("the important wire is red")
      processBelief("if a person cuts a wire, then the wire must be blue")
      processBelief("MacGyver is a person")
      process(
        "MacGyver cuts the important wire",
        "But the important wire is not blue.")
    }

    "understand nested triggers" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("a manager is a kind of person")
      processBelief("a worker is a kind of person")
      processBelief("a minion must be a worker")
      processBelief("a manager may have minions")
      processBelief("Scrooge is a manager")
      processBelief("Cratchit is a worker")
      processBelief("Cratchit is Scrooge's minion")
      processBelief("a person may be angry or sad")
      processBelief("if a manager is angry," +
        " then the manager strikes the manager's minions")
      processBelief("if a bully strikes a person, then the person is sad")
      processBelief("Scrooge is angry")
      processTerse("is Cratchit sad", "Yes.")
    }

    "understand subset matches" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("a fruit is a kind of object")
      processBelief("a fruit must be green or red")
      processBelief("an apple is a kind of fruit")
      processBelief("a tomato is a kind of fruit")
      processBelief("if a person sees an apple, then the apple is red")
      processBelief("Pippin is an apple")
      processBelief("EarlyGirl is a tomato")
      processBelief("Merry is a person")
      processBelief("the box is an object")
      processBelief("Pippin is in the box")
      processBelief("EarlyGirl is in the box")
      processBelief("Merry sees the box's contained-objects")
      processTerse("is Pippin red", "Yes.")
      processTerse("is EarlyGirl red", "I don't know.")
    }

    "understand epsilon beliefs" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("a person may have possessions")
      processBelief("the engine is an object")
      processBelief("Mason is a person")
      processBelief("the engine's contained-object is Mason's possession")
      processTerse("which objects are in the engine", "No objects.")
    }

    "understand compound subject references" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the engine is an object")
      processBelief("the wrench is an object")
      processBelief("the screwdriver is an object")
      processBelief("the saw is an object")
      processBelief("Edgar is a person")
      processBelief("the wrench is Edgar's possession")
      processBelief("the screwdriver is Edgar's possession")
      processBelief("Edgar's possessions are in the engine")
      processTerse("which objects are in the engine",
        "The wrench and the screwdriver.")
    }

    "understand unique determiner in genitive" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the engine is an object")
      processBelief("the box is an object")
      processBelief("the box is in the engine")
      processBelief("the wrench is an object")
      processBelief("the wrench's container is the box's container")
      processTerse("which objects are in the engine",
        "The box and the wrench.")
    }

    "understand negatives" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the wrench is an object")
      processBelief("the hammer is an object")
      processBelief("the screwdriver is an object")
      processBelief("the box is an object")
      processBelief("the wrench's container is the box")
      processBelief("the hammer and the screwdriver are in the box")
      processTerse("which objects are in the box",
        "The wrench, the hammer, and the screwdriver.")
      processBelief("the wrench's container is not the box")
      processTerse("which objects are in the box",
        "The hammer and the screwdriver.")
      processBelief("the hammer is no longer in the box")
      processTerse("which objects are in the box", "The screwdriver.")
      processBelief("the screwdriver is not in the box")
      processTerse("which objects are in the box", "No objects.")
    }

    "understand taxonomy" in new ResponderContext
    {
      loadBeliefs("/ontologies/vehicles.txt")

      process("is Herbie moving", "No, he is not moving.")

      processMatrix(
        "is Herbie moving",
        "No, he is not moving.",
        "No, Herbie is not moving.",
        "No.",
        "No, he is not.")
      processMatrix(
        "is Herbie stopped",
        "Yes, he is stopped.",
        "Yes, Herbie is stopped.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Lusitania moving",
        "Yes, it is moving.",
        "Yes, Lusitania is moving.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is Lusitania stopped",
        "No, it is not stopped.",
        "No, Lusitania is not stopped.",
        "No.",
        "No, it is not.")
      process(
        "is any boat stopped",
        "No, no boat is stopped.")
      process(
        "is any boat moving",
        "Yes, Lusitania is moving.")
      process(
        "is any vehicle stopped",
        "Yes, Herbie is stopped.")
      process(
        "is any vehicle moving",
        "Yes, Lusitania is moving.")
      process(
        "are both Herbie and Lusitania moving",
        "No, Herbie is not moving.")
      processMatrix(
        "is Lusitania sinking",
        "Yes, it is sinking.",
        "Yes, Lusitania is sinking.",
        "Yes.",
        "Yes, it is.")
      process(
        "is Herbie cruising",
        "Sorry, I don't know what 'cruise' means for Herbie.")
      process(
        "is any car cruising",
        "Sorry, I don't know what 'cruise' means for a car.")
      process(
        "who is cruising",
        "Sorry, I don't know what 'cruise' means for an spc-someone.")
      processMatrix(
        "is Herbie a car",
        "Yes, he is a car.",
        "Yes, Herbie is a car.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Herbie a boat",
        "No, he is not a boat.",
        "No, Herbie is not a boat.",
        "No.",
        "No, he is not.")
      processMatrix(
        "is Herbie a vehicle",
        "Yes, he is a vehicle.",
        "Yes, Herbie is a vehicle.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "is Lusitania a boat",
        "Yes, it is a boat.",
        "Yes, Lusitania is a boat.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is Lusitania the boat",
        "Yes, it is the boat.",
        "Yes, Lusitania is the boat.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is Lusitania a vehicle",
        "Yes, it is a vehicle.",
        "Yes, Lusitania is a vehicle.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is Lusitania a car",
        "No, it is not a car.",
        "No, Lusitania is not a car.",
        "No.",
        "No, it is not.")
      process(
        "how many vehicles are there",
        "There are two of them.")
      processMatrix(
        "Herbie and Lusitania are vehicles?",
        "Yes, they are vehicles.",
        "Yes, Herbie and Lusitania are vehicles.",
        "Yes.",
        "Yes, they are.")
      // FIXME resolve number agreement
      process(
        "which vehicles are there",
        "There is Herbie and Lusitania.")
      processMatrix(
        "who is Herbie's owner",
        "His owner is Jim.",
        "Herbie's owner is Jim.",
        "Jim.")
      processMatrix(
        "who is Lusitania's owner",
        "No one is its owner.",
        "No one is Lusitania's owner.",
        "No one.")
    }

    "deal with conjunctive plural noun" in new ResponderContext
    {
      skipped("maybe one day")
      loadBeliefs("/ontologies/vehicles.txt")
      processMatrix(
        "are Herbie and Lusitania vehicles",
        "Yes, they are vehicles.",
        "Yes, Herbie and Lusitania are vehicles.",
        "Yes.",
        "Yes, they are.")
    }

    "respond correctly to disjunctive query" in new ResponderContext
    {
      skipped("maybe one day")
      loadBeliefs("/ontologies/people.txt")
      processMatrix(
        "is Rapunzel or Amanda a dog",
        "Yes, Rapunzel is a dog.",
        "Yes, Rapunzel is a dog.",
        "Yes.",
        "Yes, Rapunzel is.")
    }

    "respond correctly when no person exists" in new ResponderContext
    {
      process(
        "who is Ford",
        "Sorry, I don't know about any 'Ford'.")
    }

    "understand services" in new ResponderContext
    {
      loadBeliefs("/ontologies/service.txt")
      loadBeliefs("/ontologies/miscServices.txt")
      process(
        "is there a multimedia service",
        "Yes, there is a multimedia service.")
      process(
        "is there an alarm service",
        "Yes, there is an alarm service.")
      process(
        "is there a laundry service",
        "No, there is not a laundry service.")
      processMatrix(
        "is the alarm service up",
        "Yes, it is up.",
        "Yes, the alarm service is up.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is the alarm service on",
        "Yes, it is on.",
        "Yes, the alarm service is on.",
        "Yes.",
        "Yes, it is.")
      processMatrix(
        "is the multimedia service up",
        "No, it is not up.",
        "No, the multimedia service is not up.",
        "No.",
        "No, it is not.")
      process(
        "is any service up",
        "Yes, the alarm service is up.")
      process(
        "are any services up",
        "Yes, the alarm service is up.")
      process(
        "is any service down",
        "Yes, the multimedia service is down.")
      process(
        "is any service off",
        "Yes, the multimedia service is off.")
      process(
        "are all services up",
        "No, the multimedia service is not up.")
      processMatrix(
        "is the multimedia server up",
        "No, it is not up.",
        "No, the multimedia server is not up.",
        "No.",
        "No, it is not.")

      skipped("ambiguous progressive")
      process(
        "are all services running",
        "No, the multimedia service is not running.")
    }

    "understand presence" in new ResponderContext
    {
      // FIXME:  in "is Jack home", interpret "home" as state instead of noun
      // FIXME:  "Jack's presence" should become "it"
      loadBeliefs("/ontologies/presence.txt")
      processMatrix("is Jack's ubiety on",
        "Yes, his ubiety is on.",
        "Yes, Jack's ubiety is on.",
        "Yes.",
        "Yes, his ubiety is.")
      processMatrix("is Jack present",
        "Yes, he is present.",
        "Yes, Jack is present.",
        "Yes.",
        "Yes, he is.")
      processMatrix("is Jack at home",
        "Yes, he is at home.",
        "Yes, Jack is at home.",
        "Yes.",
        "Yes, he is.")
      processMatrix("is Jack absent",
        "No, he is not absent.",
        "No, Jack is not absent.",
        "No.",
        "No, he is not.")
      processMatrix("is Jack away",
        "No, he is not away.",
        "No, Jack is not away.",
        "No.",
        "No, he is not.")
      processMatrix("is Jill's ubiety on",
        "No, her ubiety is not on.",
        "No, Jill's ubiety is not on.",
        "No.",
        "No, her ubiety is not.")
      processMatrix("is Jill present",
        "No, she is not present.",
        "No, Jill is not present.",
        "No.",
        "No, she is not.")
      processMatrix("is Jill at home",
        "No, she is not at home.",
        "No, Jill is not at home.",
        "No.",
        "No, she is not.")
      processMatrix("is Jill absent",
        "Yes, she is absent.",
        "Yes, Jill is absent.",
        "Yes.",
        "Yes, she is.")
      processMatrix("is Jill away",
        "Yes, she is away.",
        "Yes, Jill is away.",
        "Yes.",
        "Yes, she is.")
      process("is Jack on",
        "Sorry, I don't know what 'on' means for Jack.")
      processMatrix("is Casper's apparition on",
        "Yes, his apparition is on.",
        "Yes, Casper's apparition is on.",
        "Yes.",
        "Yes, his apparition is.")
      process("is Casper present",
        "I don't know.")
      processMatrix("is Yoda's ubiety on",
        "No, his ubiety is not on.",
        "No, Yoda's ubiety is not on.",
        "No.",
        "No, his ubiety is not.")
      processMatrix("is Yoda present",
        "No, he is not present.",
        "No, Yoda is not present.",
        "No.",
        "No, he is not.")
      processMatrix("is Yoda on",
        "No, he is not on.",
        "No, Yoda is not on.",
        "No.",
        "No, he is not.")
      processMatrix("is Yoda off",
        "Yes, he is off.",
        "Yes, Yoda is off.",
        "Yes.",
        "Yes, he is.")
      processMatrix(
        "are Jill and Yoda absent",
        "Yes, they are absent.",
        "Yes, Jill and Yoda are absent.",
        "Yes.",
        "Yes, they are.")
    }

    "understand multiple properties for same form" in new ResponderContext
    {
      loadBeliefs("/ontologies/stove.txt")
      process("is there a stove?",
        "Yes, there is a stove.")
      process("is the stove hot?",
        "Yes, it is hot.")
      processMatrix("is the stove on?",
        "No, it is not on.",
        "No, the stove is not on.",
        "No.",
        "No, it is not.")
    }

    "allow pronouns to be avoided" in new ResponderContext(
      ACCEPT_NO_BELIEFS,
      SmcResponseParams(thirdPersonPronouns = false))
    {
      loadBeliefs("/ontologies/stove.txt")
      process("is the stove hot?",
        "Yes, the stove is hot.")
    }

    "understand conversational pronoun references" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS,
      SmcResponseParams(thirdPersonPronouns = false))
    {
      loadBeliefs("/ontologies/containment.txt")
      loadBeliefs("/ontologies/people.txt")
      mind.startConversation
      process("is she a dog",
        "Sorry, when you say 'she' I don't know who or what you mean.")
      process("who is Todd", "Todd is Amanda's brother.")
      process("is she a dog", "No, Amanda is not a dog.")
      process("is he Dirk's friend", "Yes, Todd is Dirk's friend.")
      processBelief("the jail is an object")
      processBelief("if a person teleports, then the person is in the jail")
      processBelief("Todd and Dirk teleport")
      process("are they in the jail", "Yes, Todd and Dirk are in the jail.")
    }

    "understand sequential timeframes" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the key is an object")
      processBelief("the pocket is an object")
      processBelief("the purse is an object")
      processBelief("the shoe is an object")
      processTerse("where was the key before the pocket",
        "No narrative in progress.")
      mind.startNarrative
      processBelief("the key was in the pocket")
      processBelief("after that the key was in the purse")
      processBelief("after that the key was in the shoe")
      processTerse("where is the key", "The shoe.")
      processTerse("where was the key",
        "A timeframe must be specified.")
      processTerse("where was the key before the purse", "The pocket.")
      processTerse("where was the key after the purse", "The shoe.")
      processTerse("where was the key before the shoe", "The purse.")
      processTerse("where was the key after the pocket", "The purse.")
      processTerse("where was the key after the shoe",
        "No such timeframe and/or event in narrative.")
      processTerse("where was the key before the pocket",
        "No such timeframe and/or event in narrative.")
    }

    "understand relative timeframes" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the key is an object")
      processBelief("the pocket is an object")
      processBelief("the purse is an object")
      processBelief("the shoe is an object")
      processBelief("the card is an object")
      mind.startNarrative
      processBelief("the key was in the pocket this afternoon")
      processBelief("this morning, the card was in the purse")
      processBelief("yesterday, the card was in the shoe")
      processBelief("this evening, the key was in the shoe")
      processBelief("this afternoon the card was in the shoe")
      processTerse("where was the card before the shoe", "The purse.")
      processTerse("where was the card before the purse", "The shoe.")
      processTerse("where was the key before the shoe", "The pocket.")
    }

    "detect causality violations" in  new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      mind.startNarrative
      processBelief("yesterday, Harvey was Elwood's pet")
      process("this afternoon, Elwood had no pets",
        "The belief that Elwood had no pets " +
          "contradicts the belief that Harvey is Elwood's pet.")
      processBelief("this afternoon, Calvin had no pets")
      process("yesterday, Hobbes was Calvin's pet",
        "The belief that Calvin has no pets " +
          "contradicts the belief that Hobbes is Calvin's pet.")
    }

    "understand equivalent queries" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not working")
      }
      loadBeliefs("/ontologies/containment.txt")
      processBelief("A vapor is a kind of object.")
      processBelief("A solid is a kind of object.")
      processBelief("If a person sees a solid, " +
        "then equivalently the solid is in the person's container.")
      processBelief("Alcatraz is an object.")
      processBelief("Clint is a person.")
      processBelief("The gold is a solid.")
      processBelief("The oxygen is a vapor.")
      processBelief("The gold is in Alcatraz.")
      processBelief("The oxygen is in Alcatraz.")
      processBelief("Clint is in Alcatraz.")
      processTerse(
        "what does Clint see",
        "The gold.")
    }

    "understand progressive action predicates" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("If an item is filling an object, " +
        "equivalently the item is the object's contained-object.")
      processBelief("If an item is occupying an object, " +
        "equivalently the item is in the object.")
      processBelief("If an object is carrying an item, " +
        "equivalently the item is the object's contained-object.")

      // FIXME this belief should be equivalent
      /*
      processBelief("If an object is carrying an item, " +
        "then the object is the item's container.")
       */

      processBelief("The wallet is an object.")
      processBelief("The pocket is an object.")
      processBelief("The money is an object.")
      processBelief("The card is an object.")
      processBelief("The key is an object.")
      processBelief("The money is in the wallet.")
      processBelief("The card is in the wallet.")
      processBelief("The key is in the pocket.")
      process("how many objects are in the wallet",
        "Two of them are in the wallet.")
      process("how many objects are in the pocket",
        "One of them is in the pocket.")
      process("how many objects are the wallet's contained-object",
        "Two of them are its contained-objects.")
      process("how many objects are the pocket's contained-objects",
        "One of them is its contained-object.")
      processMatrix("how many objects are filling the wallet",
        "Two of them are filling the wallet.",
        "Two of them are filling the wallet.",
        "Two of them.",
        "Two of them.")
      processMatrix("how many objects is the wallet carrying",
        "The wallet is carrying two of them.",
        "The wallet is carrying two of them.",
        "Two of them.",
        "Two of them.")
      processMatrix("which objects is the wallet carrying",
        "The wallet is carrying the money and the card.",
        "The wallet is carrying the money and the card.",
        "The money and the card.",
        "The money and the card.")
      processMatrix("what is the wallet carrying",
        "The wallet is carrying the money and the card.",
        "The wallet is carrying the money and the card.",
        "The money and the card.",
        "The money and the card.")
      processMatrix("how many objects are occupying the pocket",
        "One of them is occupying the pocket.",
        "One of them is occupying the pocket.",
        "One of them.",
        "One of them.")
      processMatrix("which objects are filling the wallet",
        "The money and the card are filling the wallet.",
        "The money and the card are filling the wallet.",
        "The money and the card.",
        "The money and the card.")
      processMatrix("which objects are occupying the pocket",
        "The key is occupying the pocket.",
        "The key is occupying the pocket.",
        "The key.",
        "The key.")
    }

    "understand state queries" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief(
        "an animal's color must be white, gray, yellow, or green")
      processBelief("Leo is an animal.")
      processBelief("Leo is yellow.")
      processTerse("what color is Leo", "Yellow.")
    }

    "prevent new beliefs" in new ResponderContext
    {
      process("There is a big door",
        "Sorry, I don't know about any 'door'.")
    }

    "accept new beliefs" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a door may be either open or closed")
      processBelief("there is a big door")
      process("is the big door open",
        "I don't know.")
    }

    "understand property queries" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a sheep's color may be white or black")
      processBelief("Dolly is a sheep")
      process("what color is Dolly", "I don't know.")
      processBelief("Dolly is black")
      processMatrix(
        "what color is Dolly",
        "It is black.",
        "Dolly is black.",
        "Black.",
        "Black.")
    }

    "understand property updates" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      processBelief("a door may be open or closed")
      processBelief("there is a door")
      processTerse("is the door open", "I don't know.")
      processBelief("the door is open")
      processTerse("is the door open", "Yes.")
      processTerse("is the door closed", "No.")
      processBelief("the door is closed")
      processTerse("is the door open", "No.")
      processTerse("is the door closed", "Yes.")

      cosmos.sanityCheck must beTrue
    }

    "understand compound nouns" in new
      ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }

      processBelief("a butter knife is a kind of utensil")
      processBelief("there is a steak knife")
      process(
        "are there any butter knives",
        "No, there are no butter knives.")
      process(
        "are there any steak knives",
        "Yes, there is a steak knife.")
    }

    "reject invalid new beliefs" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("there is a white door")
      process("there is a big white door",
        "Previously I was told that there is a white door.  " +
          "So there is an ambiguous reference in the belief that " +
          "there is a big white door.")
    }

    "reject cyclic taxonomy belief" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a bird is a kind of animal")
      processBelief("a duck is a kind of bird")
      process("an animal is a kind of duck",
        "The belief that an animal is a kind of duck contradicts " +
          "the belief that a duck is a kind of a bird and " +
          "a bird is a kind of an animal.")
    }

    "reject incompatible form for role" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a person must have a lawyer")
      processBelief("a lawyer must be a weasel")
      processBelief("Michael is a snake")
      process("Michael is Donald's lawyer",
        "The belief that Michael is Donald's lawyer contradicts " +
          "the belief that a lawyer must be a weasel.")

      cosmos.sanityCheck must beTrue
    }

    "reject unknown actions" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("Superman is a person")
      processBelief("the kite is an object")
      process("Superman flies the kite",
        "I'm not sure how to interpret that.")
    }

    "reject unknown subject" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      skipped("broken for now")

      loadBeliefs("/ontologies/containment.txt")
      processBelief(
        "if a destroyer destroys an object, then the object has no container")
      processBelief("the football is an object")
      process("Geoff destroys the football",
        "Sorry, I don't know about any 'Geoff'.")
    }

    "prevent action cycles" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("if a person sends a recipient a message, " +
        "then the person conveys the recipient the message")
      processBelief("if a person conveys a recipient a message, " +
        "then the person sends the recipient the message")
      processBelief("Curtis is a person")
      processBelief("Andrew is a person")
      processBelief("the signal is a message")
      process("Curtis sends Andrew the signal",
        "Action beliefs are circular.")
    }

    "handle missing objects" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a tomato is a kind of vegetable")
      processBelief("an apple is a kind of vegetable")
      processBelief("an apple is a kind of poison")
      processBelief("Pippin is an apple")
      processBelief("EarlyGirl is a tomato")
      processBelief("Kenrokuen is a garden")
      processBelief("Eden is a garden")
      processBelief("Filoli is a garden")
      processBelief("a result must be a vegetable")
      processBelief("a garden may have results")
      processBelief("Pippin is Eden's result")
      processBelief("EarlyGirl is Filoli's result")
      processBelief("a person must be alive or dead")
      processBelief("Adam is a person")
      processBelief("if a person moves to a garden," +
        " then the person eats the garden's results")
      processBelief("if a person eats a poison, then the person is dead")
      processBelief("Adam is alive")
      processBelief("Adam moves to Kenrokuen")
      processTerse("is Adam dead", "No.")
      processBelief("Adam moves to Filoli")
      processTerse("is Adam dead", "No.")
      processBelief("Adam moves to Eden")
      processTerse("is Adam dead", "Yes.")
    }

    "enforce assertions" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a hobbit is a kind of person")
      processBelief("a man is a kind of person")
      processBelief("a dwarf is a kind of person")
      processBelief("Bilbo is a hobbit")
      processBelief("Sigurd is a man")
      processBelief("Gimli is a dwarf")
      processBelief("Smaug is a dragon")
      processBelief("Fafnir is a dragon")
      processBelief("Glamdring is a sword")

      process("Sigurd kills Fafnir", "I'm not sure how to interpret that.")
      processBelief("if a person kills a dragon, then the dragon is dead")
      process("Sigurd kills Fafnir", "OK.")

      processBelief("a hobbit cannot kill a dragon")
      processBelief("generally a dwarf can't kill a dragon")
      processBelief("a dwarf can kill a dragon with a sword")

      process("Bilbo kills Smaug", "A hobbit can not kill a dragon.")
      process("Bilbo kills Smaug with Glamdring",
        "A hobbit can not kill a dragon.")
      process("Gimli kills Smaug", "One does not simply kill a dragon.")
      process("Gimli kills Smaug with Glamdring", "OK.")
    }

    "reify unknown person" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a person is a kind of spc-someone")
      processBelief("a person must have a lawyer")
      processBelief("Donald is a person")
      process("who is Donald's lawyer", "I don't know.")

      cosmos.sanityCheck must beTrue
    }

    "infer form from role" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a lawyer must be a weasel")
      processBelief("Michael is Donald's lawyer")
      processTerse("is Michael a weasel", "Yes.")

      cosmos.sanityCheck must beTrue
    }

    "disambiguate based on context" in new
      ResponderContext(ACCEPT_MODIFIED_BELIEFS)
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }
      loadBeliefs("/ontologies/containment.txt")
      processBelief("Daniel is a person")
      processBelief("a lion is a kind of object")
      processBelief("a lion may be sad or angry")
      processBelief("there is a big lion")
      processBelief("a stomach is a kind of object")
      processBelief("there is a stomach")
      processBelief("the lion is sad")
      processBelief("if a person kicks a lion, then the lion is angry")
      processTerse("Daniel kicks the lion in the stomach", "OK.")
      processTerse("is the lion angry", "Yes.")
      processBelief("there is a small lion")
      processBelief("the small lion is sad")
      processBelief("the big lion is sad")
      processTerse("Daniel kicks the lion in the stomach",
        "Please be more specific about which lion you mean.")
      processBelief("the small lion is in the stomach")
      processTerse("which lion is in the stomach", "The small lion.")
      processTerse("Daniel kicks the lion in the stomach", "OK.")
      processTerse("is the small lion angry", "Yes.")
      processTerse("is the big lion angry", "No.")
    }

    "support roles with multiple forms" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a person is a kind of spc-someone")
      processBelief("a man is a kind of person")
      processBelief("a gentleman is a kind of man")
      processBelief("a footman must be a man")
      processBelief("a footman must be a plebeian")
      processBelief("a gentleman with a footman is a lord")
      processBelief("Bunter is Peter's footman")
      processTerse("is Bunter a footman", "Yes.")
      processTerse("is Bunter a man", "Yes.")
      processTerse("is Bunter a plebeian", "Yes.")
      processTerse("is Peter a gentleman", "Yes.")
      processTerse("who is Peter's footman", "Bunter.")
      processTerse("who is Bunter's lord", "Peter.")

      cosmos.sanityCheck must beTrue
    }

    "support transitive associations" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("A parent must be a patriarch.")
      processBelief("A child must be a patriarch.")
      processBelief("An ancestor must be a patriarch.")
      processBelief("A descendant must be a patriarch.")
      processBelief("A patriarch may have a parent.")
      processBelief("A patriarch may have children.")
      processBelief("A patriarch with a child is a parent.")
      processBelief("A patriarch may have ancestors.")
      processBelief("A patriarch may have descendants.")
      processBelief("A patriarch with a descendant is an ancestor.")
      processBelief("If a patriarch begets a child, " +
        "then the patriarch is the child's parent.")
      processBelief("If a patriarch begets a child, " +
        "then the patriarch is the child's ancestor.")
      processBelief("If a patriarch begets a child, " +
        "then the patriarch is the child's descendant's ancestor.")
      processBelief("If a patriarch begets a child, " +
        "then the patriarch's descendant's ancestors " +
        "are the patriarch's ancestors.")
      processBelief("Abraham is a patriarch.")
      processBelief("Isaac is a patriarch.")
      processBelief("Jacob is a patriarch.")
      processBelief("Ishmael is a patriarch.")
      processBelief("Joseph is a patriarch.")
      processBelief("Abraham begets Isaac.")
      processBelief("Abraham begets Ishmael.")
      processBelief("Jacob begets Joseph.")
      processBelief("Isaac begets Jacob.")
      def answer(b : Boolean) = if (b) "Yes." else "No."
      Seq(
        ("Abraham", "Isaac", true, true),
        ("Abraham", "Jacob", false, true),
        ("Abraham", "Ishmael", true, true),
        ("Abraham", "Joseph", false, true),
        ("Isaac", "Jacob", true, true),
        ("Isaac", "Joseph", false, true),
        ("Jacob", "Joseph", true, true),
        ("Abraham", "Abraham", false, false),
        ("Isaac", "Ishmael", false, false),
        ("Ishmael", "Jacob", false, false),
        ("Ishmael", "Joseph", false, false)
      ).foreach {
        case (
          p1, p2, isParent, isAncestor
        ) => {
          processTerse(s"Is ${p1} ${p2}'s parent?", answer(isParent))
          processTerse(s"Is ${p1} ${p2}'s ancestor?", answer(isAncestor))
          if (isParent) {
            processTerse(s"Is ${p2} ${p1}'s child?", answer(true))
          }
          if (isAncestor) {
            processTerse(s"Is ${p2} ${p1}'s descendant?", answer(true))
          }
        }
      }
    }

    "validate constraints incrementally" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/people.txt")
      process(
        "Amanda is Rapunzel's owner",
        "Previously I was told that a dog may have one owner and Bart " +
          "is Rapunzel's owner.  So it does not add up when I hear that " +
          "Amanda is Rapunzel's owner.")
      process(
        "Scott is ROWDYTHREE's operative",
        "Previously I was told that a person may have one employer and " +
          "BLACKWING is Scott's employer.  So it does not add up when I " +
          "hear that Scott is ROWDYTHREE's operative.")
    }

    "validate constraints incrementally" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/people.txt")
      processBelief("Amanda is Rapunzel's owner")
      processTerse("who is Rapunzel's owner", "Amanda.")
      processBelief("Scott is ROWDYTHREE's operative")
      processTerse("who is ROWDYTHREE's operative", "Scott.")
    }

    "derive types" >> new ResponderContext
    {
      loadBeliefs("/ontologies/people.txt")
      Seq(
        ("the dog", "dog"),
        ("a dog", "dog"),
        ("a frog", "spc-entity"),
        ("the big dog", "dog"),
        ("an organization with a problem", "organization"),
        ("Amanda", "woman"),
        ("Todd", "man"),
        ("it", "spc-entity"),
        ("Amanda and Todd", "person"),
        ("Amanda and BLACKWING", "spc-entity"),
        ("Todd's sister", "woman"),
        ("Rapunzel's owner", "person")
      ).foreach {
        case (
          subject, expectedType
        ) => {
          val input = s"$subject is hungry"
          val sentence = responder.newParser(input).parseOne
          val resultCollector = SmcResultCollector[SpcEntity]()
          responder.resolveReferences(sentence, resultCollector)
          val subjectRef = sentence match {
            case SilPredicateSentence(
              SilStatePredicate(
                subject,
                _,
                _),
              _,
              _
            ) => {
              subject
            }
            case _ => {
              throw new RuntimeException(s"unexpected sentence $sentence")
            }
          }
          responder.deriveType(subjectRef).name must
            be equalTo expectedType
        }
      }
    }
  }
}
