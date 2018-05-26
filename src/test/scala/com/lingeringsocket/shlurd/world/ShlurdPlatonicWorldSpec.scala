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

import ShlurdEnglishLemmas._

class ShlurdPlatonicWorldSpec extends Specification
{
  trait WorldContext extends NameSpace
  {
    protected val world = new ShlurdPlatonicWorld

    protected val interpreter = new ShlurdPlatonicBeliefInterpreter(world)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpretBelief(sentence)
    }

    protected def expectNamedForm(name : String) =
    {
      world.getForms.get(name) must beSome.which(_.name == name)
    }

    protected def expectDefaultProperty(form : ShlurdPlatonicForm) =
    {
      val properties = form.getProperties
      properties.size must be equalTo 1
      properties must have key(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
    }
  }

  "ShlurdPlatonicWorld" should
  {
    "understand closed property state enumeration" in new WorldContext
    {
      addBelief("a door must be either open or closed")
      expectNamedForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("open" -> "open")
      states must contain("close" -> "closed")
      addBelief("a door may be open")
      property.getStates.size must be equalTo 2
    }

    "understand open property state enumeration" in new WorldContext
    {
      addBelief("a door may be either open or closed")
      addBelief("a door may be ajar")
      expectNamedForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beFalse
      val states = property.getStates
      states.size must be equalTo 3
      states must contain("open" -> "open")
      states must contain("close" -> "closed")
      states must contain("ajar" -> "ajar")
    }

    "understand singleton property state" in new WorldContext
    {
      addBelief("a door must be closed")
      expectNamedForm("door")
      val form = world.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 1
      states must contain("close" -> "closed")
    }

    "understand qualified references" in new WorldContext
    {
      addBelief("there is a front door")
      addBelief("there is a back door")
      expectNamedForm("door")
      val frontDoor = world.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
      val backDoor = world.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("back"))
      backDoor must beSuccessfulTry.which(_.size == 1)
      frontDoor must not be equalTo(backDoor)
    }

    "understand genitives" in new WorldContext
    {
      addBelief("Joyce is a person")
      addBelief("Will is a person")
      addBelief("Jonathan is a person")
      addBelief("Lonnie is a person")
      addBelief("A mom is a person")
      addBelief("A person may have a mom")
      addBelief("A dad is a person")
      addBelief("A person may have a dad")
      addBelief("A son is a person")
      addBelief("A person may have sons")
      addBelief("An ex-husband is a person")
      addBelief("A person may have an ex-husband")
      addBelief("An ex-wife is a person")
      addBelief("A person may have an ex-wife")
      addBelief("Joyce is Will's mom")
      addBelief("Joyce is Jonathan's mom")
      addBelief("Will is Joyce's son")
      addBelief("Jonathan is Joyce's son")
      addBelief("Will is Lonnie's son")
      addBelief("Jonathan is Lonnie's son")
      addBelief("Lonnie is Will's dad")
      addBelief("Lonnie is Jonathan's dad")
      addBelief("Lonnie is Joyce's ex-husband")
      addBelief("Joyce is Lonnie's ex-wife")
      world.validateBeliefs

      val personLemma = LEMMA_PERSON
      expectNamedForm(personLemma)

      val joyces = world.resolveQualifiedNoun(
        personLemma, REF_SUBJECT, Set("joyce"))
      joyces must beSuccessfulTry.which(_.size == 1)
      val wills = world.resolveQualifiedNoun(
        personLemma, REF_SUBJECT, Set("will"))
      wills must beSuccessfulTry.which(_.size == 1)
      val jonathans = world.resolveQualifiedNoun(
        personLemma, REF_SUBJECT, Set("jonathan"))
      jonathans must beSuccessfulTry.which(_.size == 1)
      val lonnies = world.resolveQualifiedNoun(
        personLemma, REF_SUBJECT, Set("lonnie"))
      lonnies must beSuccessfulTry.which(_.size == 1)
      val joyce = joyces.get.head
      val will = wills.get.head
      val jonathan = jonathans.get.head
      val lonnie = lonnies.get.head
      Set(joyce, will, jonathan, lonnie).size must be equalTo 4
      world.resolveGenitive(will, "mom") must be equalTo Set(joyce)
      world.resolveGenitive(will, "dad") must be equalTo Set(lonnie)
      world.resolveGenitive(jonathan, "mom") must be equalTo Set(joyce)
      world.resolveGenitive(jonathan, "dad") must be equalTo Set(lonnie)
      world.resolveGenitive(joyce, "son") must be equalTo Set(will, jonathan)
      world.resolveGenitive(lonnie, "son") must be equalTo Set(will, jonathan)
      world.resolveGenitive(joyce, "mom") must beEmpty
      world.resolveGenitive(joyce, "dad") must beEmpty
      world.resolveGenitive(lonnie, "mom") must beEmpty
      world.resolveGenitive(lonnie, "dad") must beEmpty
      world.resolveGenitive(joyce, "ex-husband") must be equalTo Set(lonnie)
      world.resolveGenitive(joyce, "ex-wife") must beEmpty
      world.resolveGenitive(lonnie, "ex-wife") must be equalTo Set(joyce)
      world.resolveGenitive(lonnie, "ex-husband") must beEmpty

      addBelief("Bert is Will's mom") must
        throwA[ShlurdPlatonicWorld.IncomprehensibleBelief]
      addBelief("Joyce is Bert's mom") must
        throwA[ShlurdPlatonicWorld.IncomprehensibleBelief]
    }

    "require genitives to be defined before usage" in new WorldContext
    {
      addBelief("Joyce is a person")
      addBelief("Will is a person")
      addBelief("A mom is a person")
      addBelief("Joyce is Will's mom") must
        throwA[ShlurdPlatonicWorld.IncomprehensibleBelief]
    }

    "require mandatory genitives to be assigned" in new WorldContext
    {
      addBelief("Will is a person")
      addBelief("A mom is a person")
      addBelief("A person must have a mom")
      world.validateBeliefs must
        throwA[ShlurdPlatonicWorld.CardinalityViolation]
    }

    "prevent single valued genitives from being multiple" in new WorldContext
    {
      addBelief("Will is a person")
      addBelief("Joyce is a person")
      addBelief("Elle is a person")
      addBelief("A mom is a person")
      addBelief("A person must have a mom")
      addBelief("Joyce is Will's mom")
      addBelief("Elle is Will's mom") must
        throwA[ShlurdPlatonicWorld.CardinalityViolation]
    }

    "accept synonyms" in new WorldContext
    {
      val synonyms = world.getFormSynonyms
      addBelief("there is a front door")
      synonyms.addSynonym("portal", "door")
      synonyms.resolveSynonym("door") must be equalTo "door"
      synonyms.resolveSynonym("portal") must be equalTo "door"
      synonyms.resolveSynonym("gateway") must be equalTo "gateway"
      val frontDoor = world.resolveQualifiedNoun(
        "portal", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
    }

    "accept alternative phrasing" in new WorldContext
    {
      addBelief("A person must be either present or absent")
      addBelief("A person that is at home is present")
      addBelief("Lana is a person")
      expectNamedForm("person")
      val lana = world.resolveQualifiedNoun("person", REF_SUBJECT, Set("lana"))
      lana must beSuccessfulTry.which(_.size == 1)
      val entity = lana.get.head
      world.normalizeState(
        entity,
        SilAdpositionalState(
          ADP_AT,
          SilNounReference(SilWord("home")))
      ) must be equalTo(
        SilPropertyState(SilWord("present"))
      )
    }

    "load beliefs from a file" in new WorldContext
    {
      val file = ShlurdParser.getResourceFile("/ontologies/bit.txt")
      val source = Source.fromFile(file)
      world.loadBeliefs(source)
      expectNamedForm("bit")
      val form = world.getForms("bit")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicWorld.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("on" -> "on")
      states must contain("off" -> "off")
    }

    "reject contradictory belief" in new WorldContext
    {
      addBelief("a door must be open or closed")
      addBelief("a door may be ajar") must
        throwA[ShlurdPlatonicWorld.ContradictoryBelief]
    }

    "reject ambiguous belief" in new WorldContext
    {
      addBelief("there is a front door")
      addBelief("there is a door") must
        throwA[ShlurdPlatonicWorld.AmbiguousBelief]
    }

    "reject another ambiguous belief" in new WorldContext
    {
      addBelief("there is a door")
      addBelief("there is a front door") must
        throwA[ShlurdPlatonicWorld.AmbiguousBelief]
    }

    "reject beliefs it cannot understand" in new WorldContext
    {
      addBelief("a green door must be either open or closed") must
        throwA[ShlurdPlatonicWorld.IncomprehensibleBelief]
    }
  }
}
