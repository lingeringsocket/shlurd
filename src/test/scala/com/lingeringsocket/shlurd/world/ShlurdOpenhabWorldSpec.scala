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

import scala.io._
import scala.util._

import spire.math._

class ShlurdOpenhabWorldSpec extends Specification
{
  trait WorldContext extends NameSpace
  {
    private val itemStates = Map(
      "GF_Garage_Door" -> "open",
      "GF_Garden_Door" -> "close",
      "GF_MasterBedroom_Door" -> "close",
      "GF_GuestBedroom_Door" -> "close",
      "FF_GuestBedroom_Door" -> "open",
      "GF_Garage_Light" -> "off",
      "GF_Garden_Light_Solar" -> "off",
      "GF_Garden_Light_Terrace" -> "off",
      "GF_GuestBedroom_Light" -> "on",
      "FF_Family_Light" -> "on",
      "FF_LivingRoom_Light" -> "on",
      "FF_LivingRoom_Heating" -> "on",
      "FF_Bath_Heating" -> "off",
      "FF_GuestBedroom_Light_Ceiling" -> "on",
      "FF_GuestBedroom_Light_Nightstand" -> "off",
      "Presence_Jack_Phone" -> "on",
      "Presence_Jill" -> "off"
    )

    protected val world = new ShlurdOpenhabWorld {
      override protected def evaluateState(
        entity : ShlurdPlatonicEntity, stateName : String) : Try[Trilean] =
      {
        itemStates.get(entity.name) match {
          case Some(currentState) => {
            Success(Trilean(currentState == stateName))
          }
          case _ => {
            Success(Trilean.Unknown)
          }
        }
      }
    }

    protected val interpreter = new ShlurdPlatonicInterpreter(world)

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
      world.addItem("Home", "Our House", true, Seq.empty)
      world.addItem("Phone", "Phone Presence", true, Seq("Home"))
      world.addItem("Junk", "Just Junk", true, Seq.empty)
      world.addItem("gGF", "Ground Floor", true, Seq("Home"))
      world.addItem("FF", "First Floor", true, Seq("Home"))
      world.addItem("GF_Garage", "Garage", true, Seq("gGF"))
      world.addItem("GF_Garden", "Garden", true, Seq("gGF"))
      world.addItem("GF_GuestBedroom", "Guest Bedroom", true, Seq("gGF"))
      world.addItem("GF_MasterBedroom", "Master Bedroom", true, Seq("gGF"))
      world.addItem("FF_Family", "Family Room", true, Seq("FF"))
      world.addItem("FF_LivingRoom", "Living Room", true, Seq("FF"))
      world.addItem("FF_GuestBedroom", "Guest Bedroom", true, Seq("FF", "Junk"))
      world.addItem("FF_Bath", "Bathroom", true, Seq("FF"))
      world.addItem(
        "FF_Family_Light", "Light", false,
        Seq("FF_Family", "gLight"))
      world.addItem(
        "FF_LivingRoom_Light", "Mood Light", false,
        Seq("FF_LivingRoom", "gLight"))
      world.addItem(
        "FF_LivingRoom_Heating", "Heating", false,
        Seq("FF_LivingRoom", "gHeating"))
      world.addItem(
        "GF_Garage_Door", "Door", false,
        Seq("GF_Garage", "gDoor"))
      world.addItem(
        "GF_Garage_Light", "Light", false,
        Seq("GF_Garage", "gLight"))
      world.addItem(
        "GF_Garden_Door", "Door", false,
        Seq("GF_Garden", "gDoor"))
      world.addItem(
        "GF_Garden_Light_Solar", "Solar", false,
        Seq("GF_Garden", "gLight"))
      world.addItem(
        "GF_Garden_Light_Terrace", "Terrace", false,
        Seq("GF_Garden", "gLight"))
      world.addItem(
        "GF_MasterBedroom_Door", "Door", false,
        Seq("GF_MasterBedroom", "gDoor"))
      world.addItem(
        "GF_GuestBedroom_Door", "Door", false,
        Seq("GF_GuestBedroom", "gDoor"))
      world.addItem(
        "GF_GuestBedroom_Light", "Light", false,
        Seq("GF_GuestBedroom", "gLight"))
      world.addItem(
        "FF_GuestBedroom_Door", "Door", false,
        Seq("FF_GuestBedroom", "gDoor"))
      world.addItem(
        "FF_GuestBedroom_Light_Nightstand",
        "Nightstand", false,
        Seq("FF_GuestBedroom", "gLight"))
      world.addItem(
        "FF_GuestBedroom_Light_Ceiling",
        "Ceiling", false,
        Seq("FF_GuestBedroom", "gLight"))
      world.addItem(
        "FF_Bath_Heating",
        "Bath", false,
        Seq("FF_Bath", "gHeating"))
      world.addItem(
        "Presence_Jack_Phone",
        "Jack", false,
        Seq("Phone"))
      world.addItem(
        "Presence_Jill",
        "Jill", false,
        Seq.empty)

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
        "is the guest bedroom door on the ground floor closed",
        "Yes, the guest bedroom door on the ground floor is closed.")
      interpret(
        "is the guest bedroom door on the first floor closed",
        "No, the guest bedroom door on the first floor is not closed.")
      interpret(
        "is any door open on the ground floor",
        "Yes, the garage door is open.")
      interpret(
        "is any door open on the first floor",
        "Yes, the guest bedroom door on the first floor is open.")
      interpret(
        "is any bedroom door open",
        "Yes, the guest bedroom door on the first floor is open.")
      interpret(
        "is any bedroom light off",
        "Yes, the guest bedroom nightstand light is off.")

      // FIXME:  there should be no ambiguity here
      interpret(
        "is any light in the guest bedroom on the first floor off",
        "Please be more specific about which bedroom you mean.")

      interpret(
        "is any guest bedroom light on the first floor off",
        "Yes, the guest bedroom nightstand light on the first floor is off.")
      interpret(
        "is any light in any bedroom off",
        "Yes, the guest bedroom nightstand light is off.")
      interpret(
        "is any light in the guest bedroom off",
        "Please be more specific about which bedroom you mean.")
      interpret(
        "is any printer on",
        "Sorry, I don't know what you mean by 'printer'.")
      interpret(
        "is there a stable",
        "Sorry, I don't know what you mean by 'stable'.")
      interpret(
        "is there a bedroom",
        "Yes, there is a bedroom.")
      interpret(
        "is there a bedroom on the first floor",
        "Yes, there is a bedroom on the first floor.")
      interpret(
        "is there a pink bedroom",
        "No, there is not a pink bedroom.")
      interpret(
        "is there a garage",
        "Yes, there is a garage.")
      interpret(
        "is there a garage on the ground floor",
        "Yes, there is a garage on the ground floor.")
      interpret(
        "is there a garage on the first floor",
        "No, there is not a garage on the first floor.")
      interpret(
        "is any light in the stable on",
        "Sorry, I don't know what you mean by 'stable'.")
      interpret(
        "is the solar light on",
        "No, the solar light is not on.")
      interpret(
        "is the solar light lit",
        "No, the solar light is not lit.")
      interpret(
        "is there a living room",
        "Yes, there is a living room.")
      interpret(
        "is there a master bedroom",
        "Yes, there is a master bedroom.")
      interpret(
        "is the living room light on",
        "Yes, the living room light is on.")
      interpret(
        "is the family room light on",
        "Yes, the family room light is on.")
      interpret(
        "which rooms are on the first floor",
        "The family room, the living room, " +
          "the guest bedroom, and the bathroom " +
          "are on the first floor.")
      interpret(
        "which lights on the first floor are on",
        "The family room light, " +
          "the living room mood light, " +
          "and the guest bedroom ceiling light are on.")
      interpret(
        "is the light in the living room lit",
        "Yes, the light in the living room is lit.")
      interpret(
        "is the living room light dark",
        "No, the living room light is not dark.")
      interpret(
        "is the light in the living room on",
        "Yes, the light in the living room is on.")
      interpret(
        "is the heating in the living room on",
        "Yes, the heating in the living room is on.")
      interpret(
        "is the heater in the living room on",
        "Yes, the heater in the living room is on.")
      interpret(
        "is the heat in the living room on",
        "Yes, the heat in the living room is on.")
      interpret(
        "turn on the heater in the living room",
        "But the heater in the living room is on already.")
      interpret(
        "is there any light in the garage",
        "Yes, there is a garage light.")
      interpret(
        "is there any light in the garden",
        "Yes, there are 2 of them.")
      interpret(
        "are any heaters off",
        "Yes, the bathroom heating is off.")
      interpret(
        "is Jack present",
        "Yes, Jack is present.")
      interpret(
        "is Jill present",
        "No, Jill is not present.")
      interpret(
        "is Larry present",
        "I don't know.")
      interpret(
        "is Jack absent",
        "No, Jack is not absent.")
      interpret(
        "is Jill absent",
        "Yes, Jill is absent.")
      interpret(
        "is Larry absent",
        "I don't know.")
    }
  }
}
