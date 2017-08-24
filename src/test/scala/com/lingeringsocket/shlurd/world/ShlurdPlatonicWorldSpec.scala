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

class ShlurdPlatonicWorldSpec extends Specification
{
  // world is mutable, so we need isolation
  isolated

  private val world = new ShlurdPlatonicWorld

  private def addBelief(input : String) =
  {
    val sentence = ShlurdParser(input).parseOne
    world.addBelief(sentence)
  }

  private def expectUniqueForm(name : String) =
  {
    val forms = world.getForms
    forms.size must be equalTo 1
    forms must have key name
  }

  private def expectDefaultProperty(form : ShlurdPlatonicForm) =
  {
    val properties = form.getProperties
    properties.size must be equalTo 1
    properties must have key(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
  }

  "ShlurdPlatonicWorld" should
  {
    "understand closed property state enumeration" in
    {
      addBelief("a door must be either open or closed")
      expectUniqueForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("open")
      states must contain("close")
      addBelief("a door may be open")
      property.getStates.size must be equalTo 2
    }

    "understand open property state enumeration" in
    {
      addBelief("a door may be either open or closed")
      addBelief("a door may be ajar")
      expectUniqueForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beFalse
      val states = property.getStates
      states.size must be equalTo 3
      states must contain("open")
      states must contain("close")
      states must contain("ajar")
    }

    "understand singleton property state" in
    {
      addBelief("a door must be closed")
      expectUniqueForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 1
      states must contain("close")
    }

    "understand qualified references" in
    {
      addBelief("there is a front door")
      addBelief("there is a back door")
      expectUniqueForm("door")
      val frontDoor = world.resolveEntity("door", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
      val backDoor = world.resolveEntity("door", REF_SUBJECT, Set("back"))
      backDoor must beSuccessfulTry.which(_.size == 1)
      frontDoor must not be equalTo(backDoor)
    }

    "load beliefs from a file" in
    {
      val file = ShlurdParser.getResourceFile("/ontologies/bit.txt")
      val source = Source.fromFile(file)
      world.loadBeliefs(source)
      expectUniqueForm("bit")
      val form = world.getForms("bit")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("on")
      states must contain("off")
    }

    "reject contradictory belief" in
    {
      addBelief("a door must be open or closed")
      addBelief("a door may be ajar") must
        throwA[ShlurdPlatonicWorld.ContradictoryBelief]
    }

    "reject ambiguous belief" in
    {
      addBelief("there is a front door")
      addBelief("there is a door") must
        throwA[ShlurdPlatonicWorld.AmbiguousBelief]
    }

    "reject another ambiguous belief" in
    {
      addBelief("there is a door")
      addBelief("there is a front door") must
        throwA[ShlurdPlatonicWorld.AmbiguousBelief]
    }

    "reject beliefs it cannot understand" in
    {
      addBelief("a green door must be either open or closed") must
        throwA[ShlurdPlatonicWorld.IncomprehensibleBelief]
    }
  }
}
