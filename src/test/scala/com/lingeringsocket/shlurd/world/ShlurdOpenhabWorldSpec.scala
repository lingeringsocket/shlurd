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

import scala.io._
import scala.util._

import spire.math._

class ShlurdOpenhabWorldSpec extends Specification
{
  trait WorldContext extends NameSpace
  {
    private val doorStates = Map(
      "GF_Garage_Door" -> "open",
      "GF_Garden_Door" -> "close",
      "GF_Bedroom_Door" -> "close",
      "FF_Bedroom_Door" -> "open"
    )

    protected val world = new ShlurdOpenhabWorld {
      override protected def evaluateState(
        entity : ShlurdPlatonicEntity, stateName : String) : Try[Trilean] =
      {
        Success(Trilean(doorStates(entity.name) == stateName))
      }
    }

    protected val interpreter = new ShlurdInterpreter(world)

    protected def interpret(input : String, expected : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpret(sentence) must be equalTo(expected)
    }
  }

  "ShlurdOpenhabWorld" should
  {
    "understand items" in new WorldContext
    {
      val file = ShlurdParser.getResourceFile("/ontologies/home.txt")
      val source = Source.fromFile(file)
      world.loadBeliefs(source)
      world.addItem("Home", "Our House", true, Set.empty)
      world.addItem("Junk", "Just Junk", true, Set.empty)
      world.addItem("GF", "Ground Floor", true, Set("Home"))
      world.addItem("FF", "First Floor", true, Set("Home"))
      world.addItem("GF_Garage", "Garage", true, Set("GF"))
      world.addItem("GF_Garden", "Garden", true, Set("GF"))
      world.addItem("GF_Bedroom", "Bedroom", true, Set("GF"))
      world.addItem("FF_Bedroom", "Bedroom", true, Set("FF", "Junk"))
      world.addItem("GF_Garage_Door", "Door", false, Set("GF_Garage"))
      world.addItem("GF_Garden_Door", "Door", false, Set("GF_Garden"))
      world.addItem("GF_Bedroom_Door", "Door", false, Set("GF_Bedroom"))
      world.addItem("FF_Bedroom_Door", "Door", false, Set("FF_Bedroom"))
      interpret(
        "is the door in the garage open",
        "Yes, the door in the garage is open.")
      interpret(
        "is the garage door open",
        "Yes, the garage door is open.")
      interpret(
        "is the garage door closed",
        "No, the garage door is not closed.")
      interpret(
        "is the garden door open",
        "No, the garden door is not open.")
      interpret(
        "is the garden door closed",
        "Yes, the garden door is closed.")
      interpret(
        "is the bedroom door on the ground floor closed",
        "Yes, the bedroom door on the ground floor is closed.")
      interpret(
        "is the bedroom door on the first floor closed",
        "No, the bedroom door on the first floor is not closed.")
      interpret(
        "is any door open on the ground floor",
        "Yes, the garage door is open.")
      interpret(
        "is any door open on the first floor",
        "Yes, the bedroom door on the first floor is open.")
      interpret(
        "is any bedroom door open",
        "Yes, the bedroom door on the first floor is open.")
    }
  }
}
