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

import scala.io._
import scala.util._

class ShlurdPlatonicInterpreterSpec extends Specification
{
  private val states = Map(
    "alarm service" -> "on",
    "multimedia service" -> "off"
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
          case _ => fail("unknown property")
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
      interpret(
        "does Dirk have a friend",
        "Yes, Dirk has a friend.")

      // FIXME:  these two do not currently work:
      // "does Todd have friends"
      // "does Todd have any friends"

      // FIXME:  understand personal names better
      interpret(
        "is Ford Todd's friend",
        "Sorry, I don't know what you mean by ford.")
      interpret(
        "is Todd Ford's friend",
        "Sorry, I don't know what you mean by ford.")
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
  }
}
