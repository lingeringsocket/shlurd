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

    protected val responder =
      new SpcResponder(new SpcOpenhabMind(cosmos))

    protected def process(input : String, expected : String) =
    {
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence, input) must be equalTo(expected)
    }
  }

  "SpcOpenhabCosmos" should
  {
    "understand items" in new CosmosContext
    {
      val file = ResourceUtils.getResourceFile("/ontologies/home.txt")
      val source = Source.fromFile(file)
      new SpcOpenhabMind(cosmos).loadBeliefs(source)
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

      process(
        "is the door in the garage open",
        "Yes, it is open.")
      process(
        "is the garage door open",
        "Yes, it is open.")
      process(
        "is the garage door closed",
        "No, it is not closed.")
      process(
        "is the garden door open",
        "No, it is not open.")
      process(
        "is the garden door closed",
        "Yes, it is closed.")
      process(
        "is the guest bedroom door on the ground floor closed",
        "Yes, it is closed.")
      process(
        "is the guest bedroom door on the first floor closed",
        "No, it is not closed.")
      process(
        "is any door open on the ground floor",
        "Yes, the garage door is open.")
      process(
        "is any door open on the first floor",
        "Yes, the guest bedroom door on the first floor is open.")
      process(
        "is any bedroom door open",
        "Yes, the guest bedroom door on the first floor is open.")
      process(
        "is any bedroom light off",
        "Yes, the guest bedroom nightstand light is off.")

      // FIXME:  there should be no ambiguity here
      process(
        "is any light in the guest bedroom on the first floor off",
        "Please be more specific about which bedroom you mean.")

      if (!SprParser.isCoreNLP) {
        process(
          "is any guest bedroom light on the first floor off",
          "Yes, the guest bedroom nightstand light is off.")
      }
      process(
        "is any light in any bedroom off",
        "Yes, the guest bedroom nightstand light is off.")
      process(
        "is any light in the guest bedroom off",
        "Please be more specific about which bedroom you mean.")
      process(
        "is any printer on",
        "Sorry, I don't know about any 'printer'.")
      process(
        "is there a stable",
        "Sorry, I don't know about any 'stable'.")
      process(
        "is there a bedroom",
        "Yes, there is a bedroom.")
      process(
        "is there a bedroom on the first floor",
        "Yes, there is a bedroom on the first floor.")
      process(
        "is there a pink bedroom",
        "No, there is not a pink bedroom.")
      process(
        "is there a garage",
        "Yes, there is a garage.")
      process(
        "is there a garage on the ground floor",
        "Yes, there is a garage on the ground floor.")
      process(
        "is there a garage on the first floor",
        "No, there is not a garage on the first floor.")
      process(
        "is any light in the stable on",
        "Sorry, I don't know about any 'stable'.")
      process(
        "is the solar light on",
        "No, it is not on.")
      process(
        "is the solar light lit",
        "No, it is not lit.")
      process(
        "is there a living room",
        "Yes, there is a living room.")
      process(
        "is there a master bedroom",
        "Yes, there is a master bedroom.")
      process(
        "is the living room light on",
        "Yes, it is on.")
      process(
        "is the family room light on",
        "Yes, it is on.")
      process(
        "which rooms are on the first floor",
        "The family room, the living room, " +
          "the guest bedroom, and the bathroom " +
          "are on the first floor.")
      if (!SprParser.isCoreNLP) {
        process(
          "which lights on the first floor are on",
          "The family room light, " +
            "the living room mood light, " +
            "and the guest bedroom ceiling light are on.")
        process(
          "which bedroom lights are on",
          "The guest bedroom light on the ground floor " +
            "and the guest bedroom ceiling light are on.")
      }
      process(
        "is the light in the living room lit",
        "Yes, it is lit.")
      process(
        "is the living room light extinguished",
        "No, it is not extinguished.")
      process(
        "is the light in the living room on",
        "Yes, it is on.")
      process(
        "is the heating in the living room on",
        "Yes, it is on.")
      process(
        "is the heater in the living room on",
        "Yes, it is on.")
      process(
        "is the heat in the living room on",
        "Yes, it is on.")
      if (!SprParser.isCoreNLP) {
        process(
          "turn on the heater in the living room",
          "But it is on already.")
      }
      process(
        "is there any light in the garage",
        "Yes, there is a garage light.")
      process(
        "is there any light in the garden",
        "Yes, there is a garden solar light and a garden terrace light.")
      process(
        "are any heaters off",
        "Yes, the bathroom heating is off.")
      process(
        "is Jack present",
        "Yes, he is present.")
      process(
        "is Jill present",
        "No, she is not present.")
      process(
        "is Larry present",
        "I don't know.")
      process(
        "is Jack absent",
        "No, he is not absent.")
      process(
        "is Jill absent",
        "Yes, she is absent.")
      process(
        "is Larry absent",
        "I don't know.")
      process(
        "how many garden lights are off",
        "Both of them are off.")
      process(
        "is the garage light in the garden",
        "No, it is not in the garden.")
    }
  }
}
