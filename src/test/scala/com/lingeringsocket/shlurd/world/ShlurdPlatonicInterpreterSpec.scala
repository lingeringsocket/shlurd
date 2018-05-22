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

import scala.io._
import scala.util._

class ShlurdPlatonicInterpreterSpec extends Specification
{
  private val states = Map(
    "alarm service" -> "on",
    "multimedia service" -> "off",
    "jackphone presence" -> "on",
    "jillphone presence" -> "off",
    "casperphone presence" -> "on",
    "yodaphone presence" -> "off"
  )

  trait InterpreterContext extends NameSpace
  {
    protected val world = new ShlurdPlatonicWorld {
      override def evaluateEntityPropertyPredicate(
        entity : ShlurdPlatonicEntity,
        property : ShlurdPlatonicProperty,
        lemma : String) =
      {
        val qualifiedName =
          (entity.qualifiers.toSeq :+ entity.form.name).mkString(" ")
        states.get(qualifiedName) match {
          case Some(state) => Success(Trilean(state == lemma))
          case _ => super.evaluateEntityPropertyPredicate(
            entity, property, lemma)
        }
      }
    }

    protected val interpreter = new ShlurdInterpreter(world)

    protected def loadBeliefs(resource : String)
    {
      val file = ShlurdParser.getResourceFile(resource)
      val source = Source.fromFile(file)
      world.loadBeliefs(source)
    }

    protected def interpret(input : String, expected : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpret(sentence) must be equalTo(expected)
    }
  }

  "ShlurdPlatonicInterpreter" should
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
        "Todd is Todd.")
      interpret(
        "who is Todd's friend",
        "Dirk is Todd's friend.")
      interpret(
        "who are Todd's friends",
        "Dirk is Todd's friend.")
      interpret(
        "which person is Todd's friend",
        "Dirk is Todd's friend.")
      interpret(
        "who is Dirk's friend",
        "Todd and Bart are Dirk's friends.")
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
        "Sorry, I don't know what you mean by 'Ford'.")
      interpret(
        "is Todd Ford's friend",
        "Sorry, I don't know what you mean by 'Ford'.")
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
        "Sorry, I don't know what you mean by 'robot'.")
      // FIXME:  should be "are people"?
      interpret(
        "who is a person",
        "Dirk, Todd, Amanda, and Bart are persons.")
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
        "Sorry, I don't know what you mean by 'on'.")
      interpret("is Casper's presence on",
        "Yes, Casper's presence is on.")
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
  }
}
