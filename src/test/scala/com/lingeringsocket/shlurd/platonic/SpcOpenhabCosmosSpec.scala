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
import org.specs2.specification._

import scala.io._
import scala.util._

class SpcOpenhabCosmosSpec extends Specification
{
  trait CosmosContext extends Scope
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

    protected val cosmos = new SpcOpenhabCosmos {
      override def evaluateEntityProperty(
        entity : SpcEntity, propertyName : String,
        specific : Boolean = false) =
      {
        itemStates.get(entity.name) match {
          case Some(state) => Success((
            findProperty(entity.form, propertyName), Some(state)))
          case _ => super.evaluateEntityProperty(entity, propertyName, specific)
        }
      }
    }

    protected val interpreter =
      new SpcInterpreter(new SpcMind(cosmos))

    protected def interpret(input : String, expected : String) =
    {
      val sentence = SprParser(input).parseOne
      interpreter.interpret(sentence, input) must be equalTo(expected)
    }
  }

  "SpcOpenhabCosmos" should
  {
    "understand items" in new CosmosContext
    {
      val file = SprParser.getResourceFile("/ontologies/home.txt")
      val source = Source.fromFile(file)
      cosmos.loadBeliefs(source)
      cosmos.addItem("Home", "Our House", true, Seq.empty)
      cosmos.addItem("Phone", "Phone Presence", true, Seq("Home"))
      cosmos.addItem("Junk", "Just Junk", true, Seq.empty)
      cosmos.addItem("gGF", "Ground Floor", true, Seq("Home"))
      cosmos.addItem("FF", "First Floor", true, Seq("Home"))
      cosmos.addItem("GF_Garage", "Garage", true, Seq("gGF"))
      cosmos.addItem("GF_Garden", "Garden", true, Seq("gGF"))
      cosmos.addItem("GF_GuestBedroom", "Guest Bedroom", true, Seq("gGF"))
      cosmos.addItem("GF_MasterBedroom", "Master Bedroom", true, Seq("gGF"))
      cosmos.addItem("FF_Family", "Family Room", true, Seq("FF"))
      cosmos.addItem("FF_LivingRoom", "Living Room", true, Seq("FF"))
      cosmos.addItem(
        "FF_GuestBedroom", "Guest Bedroom", true, Seq("FF", "Junk"))
      cosmos.addItem(
        "FF_Bath", "Bathroom", true, Seq("FF"))
      cosmos.addItem(
        "FF_Family_Light", "Light", false,
        Seq("FF_Family", "gLight"))
      cosmos.addItem(
        "FF_LivingRoom_Light", "Mood Light", false,
        Seq("FF_LivingRoom", "gLight"))
      cosmos.addItem(
        "FF_LivingRoom_Heating", "Heating", false,
        Seq("FF_LivingRoom", "gHeating"))
      cosmos.addItem(
        "GF_Garage_Door", "Door", false,
        Seq("GF_Garage", "gDoor"))
      cosmos.addItem(
        "GF_Garage_Light", "Light", false,
        Seq("GF_Garage", "gLight"))
      cosmos.addItem(
        "GF_Garden_Door", "Door", false,
        Seq("GF_Garden", "gDoor"))
      cosmos.addItem(
        "GF_Garden_Light_Solar", "Solar", false,
        Seq("GF_Garden", "gLight"))
      cosmos.addItem(
        "GF_Garden_Light_Terrace", "Terrace", false,
        Seq("GF_Garden", "gLight"))
      cosmos.addItem(
        "GF_MasterBedroom_Door", "Door", false,
        Seq("GF_MasterBedroom", "gDoor"))
      cosmos.addItem(
        "GF_GuestBedroom_Door", "Door", false,
        Seq("GF_GuestBedroom", "gDoor"))
      cosmos.addItem(
        "GF_GuestBedroom_Light", "Light", false,
        Seq("GF_GuestBedroom", "gLight"))
      cosmos.addItem(
        "FF_GuestBedroom_Door", "Door", false,
        Seq("FF_GuestBedroom", "gDoor"))
      cosmos.addItem(
        "FF_GuestBedroom_Light_Nightstand",
        "Nightstand", false,
        Seq("FF_GuestBedroom", "gLight"))
      cosmos.addItem(
        "FF_GuestBedroom_Light_Ceiling",
        "Ceiling", false,
        Seq("FF_GuestBedroom", "gLight"))
      cosmos.addItem(
        "FF_Bath_Heating",
        "Bath", false,
        Seq("FF_Bath", "gHeating"))
      cosmos.addItem(
        "Presence_Jack_Phone",
        "Jack", false,
        Seq("Phone"))
      cosmos.addItem(
        "Presence_Jill",
        "Jill", false,
        Seq.empty)

      interpret(
        "is the door in the garage open",
        "Yes, it is open.")
      interpret(
        "is the garage door open",
        "Yes, it is open.")
      interpret(
        "is the garage door closed",
        "No, it is not closed.")
      interpret(
        "is the garden door open",
        "No, it is not open.")
      interpret(
        "is the garden door closed",
        "Yes, it is closed.")
      interpret(
        "is the guest bedroom door on the ground floor closed",
        "Yes, it is closed.")
      interpret(
        "is the guest bedroom door on the first floor closed",
        "No, it is not closed.")
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
        "Yes, the guest bedroom nightstand light is off.")
      interpret(
        "is any light in any bedroom off",
        "Yes, the guest bedroom nightstand light is off.")
      interpret(
        "is any light in the guest bedroom off",
        "Please be more specific about which bedroom you mean.")
      interpret(
        "is any printer on",
        "Sorry, I don't know about any 'printer'.")
      interpret(
        "is there a stable",
        "Sorry, I don't know about any 'stable'.")
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
        "Sorry, I don't know about any 'stable'.")
      interpret(
        "is the solar light on",
        "No, it is not on.")
      interpret(
        "is the solar light lit",
        "No, it is not lit.")
      interpret(
        "is there a living room",
        "Yes, there is a living room.")
      interpret(
        "is there a master bedroom",
        "Yes, there is a master bedroom.")
      interpret(
        "is the living room light on",
        "Yes, it is on.")
      interpret(
        "is the family room light on",
        "Yes, it is on.")
      interpret(
        "which rooms are on the first floor",
        "The family room, the living room, " +
          "the guest bedroom, and the bathroom " +
          "are on the first floor.")
      interpret(
        "which lights on the first floor are on",
        "The family room light, " +
          "the living room mood light, " +
          "and the guest bedroom ceiling light on the first floor are on.")
      interpret(
        "is the light in the living room lit",
        "Yes, it is lit.")
      interpret(
        "is the living room light extinguished",
        "No, it is not extinguished.")
      interpret(
        "is the light in the living room on",
        "Yes, it is on.")
      interpret(
        "is the heating in the living room on",
        "Yes, it is on.")
      interpret(
        "is the heater in the living room on",
        "Yes, it is on.")
      interpret(
        "is the heat in the living room on",
        "Yes, it is on.")
      interpret(
        "turn on the heater in the living room",
        "But it is on already.")
      interpret(
        "is there any light in the garage",
        "Yes, there is a garage light.")
      interpret(
        "is there any light in the garden",
        "Yes, there are two of them.")
      interpret(
        "are any heaters off",
        "Yes, the bathroom heating is off.")
      interpret(
        "is Jack present",
        "Yes, he is present.")
      interpret(
        "is Jill present",
        "No, she is not present.")
      interpret(
        "is Larry present",
        "I don't know.")
      interpret(
        "is Jack absent",
        "No, he is not absent.")
      interpret(
        "is Jill absent",
        "Yes, she is absent.")
      interpret(
        "is Larry absent",
        "I don't know.")
    }
  }
}
