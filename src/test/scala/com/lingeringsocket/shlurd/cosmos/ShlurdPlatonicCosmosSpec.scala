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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import scala.io._
import scala.util._

import ShlurdEnglishLemmas._

class ShlurdPlatonicCosmosSpec extends Specification
{
  trait CosmosContext extends NameSpace
  {
    protected val cosmos = new ShlurdPlatonicCosmos

    protected val interpreter = new ShlurdPlatonicBeliefInterpreter(cosmos)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpretBelief(sentence)
    }

    protected def expectNamedForm(name : String) =
    {
      cosmos.getForms.get(name) must beSome.which(_.name == name)
    }

    protected def expectDefaultProperty(form : ShlurdPlatonicForm) =
    {
      val properties = form.getProperties
      properties.size must be equalTo 1
      properties must have key(ShlurdPlatonicCosmos.DEFAULT_PROPERTY)
    }

    protected def expectUnique(
      entities : Try[collection.Set[ShlurdPlatonicEntity]]) =
    {
      entities must beSuccessfulTry.which(_.size == 1)
      entities.get.head
    }
  }

  "ShlurdPlatonicCosmos" should
  {
    "understand closed property state enumeration" in new CosmosContext
    {
      addBelief("a door must be either open or closed")
      expectNamedForm("door")
      val form = cosmos.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicCosmos.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("open" -> "open")
      states must contain("close" -> "closed")
      addBelief("a door may be open")
      property.getStates.size must be equalTo 2
    }

    "understand open property state enumeration" in new CosmosContext
    {
      addBelief("a door may be either open or closed")
      addBelief("a door may be ajar")
      expectNamedForm("door")
      val form = cosmos.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicCosmos.DEFAULT_PROPERTY)
      property.isClosed must beFalse
      val states = property.getStates
      states.size must be equalTo 3
      states must contain("open" -> "open")
      states must contain("close" -> "closed")
      states must contain("ajar" -> "ajar")
    }

    "understand singleton property state" in new CosmosContext
    {
      addBelief("a door must be closed")
      expectNamedForm("door")
      val form = cosmos.getForms("door")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicCosmos.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 1
      states must contain("close" -> "closed")
    }

    "understand qualified references" in new CosmosContext
    {
      addBelief("there is a front door")
      addBelief("there is a back door")
      expectNamedForm("door")
      val frontDoor = cosmos.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
      val backDoor = cosmos.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("back"))
      backDoor must beSuccessfulTry.which(_.size == 1)
      frontDoor must not be equalTo(backDoor)
    }

    "understand taxonomy" in new CosmosContext
    {
      addBelief("a dog is a kind of canine")
      addBelief("a duck is a kind of bird")
      addBelief("a mallard is a kind of duck")
      addBelief("a canvasback is a kind of duck")
      cosmos.validateBeliefs
      expectNamedForm("dog")
      expectNamedForm("canine")
      expectNamedForm("duck")
      expectNamedForm("bird")
      expectNamedForm("canvasback")
      val dog = cosmos.getForms("dog")
      val canine = cosmos.getForms("canine")
      val bird = cosmos.getForms("bird")
      val duck = cosmos.getForms("duck")
      val mallard = cosmos.getForms("mallard")
      val canvasback = cosmos.getForms("canvasback")
      cosmos.isHyponym(dog, canine) must beTrue
      cosmos.isHyponym(canine, dog) must beFalse
      cosmos.isHyponym(duck, bird) must beTrue
      cosmos.isHyponym(bird, duck) must beFalse
      cosmos.isHyponym(mallard, duck) must beTrue
      cosmos.isHyponym(mallard, bird) must beTrue
      cosmos.isHyponym(duck, mallard) must beFalse
      cosmos.isHyponym(bird, mallard) must beFalse
      cosmos.isHyponym(canvasback, duck) must beTrue
      cosmos.isHyponym(canvasback, bird) must beTrue
      cosmos.isHyponym(canvasback, mallard) must beFalse
      cosmos.isHyponym(mallard, canvasback) must beFalse
      cosmos.isHyponym(canine, bird) must beFalse
      cosmos.isHyponym(bird, canine) must beFalse
      cosmos.isHyponym(dog, duck) must beFalse
    }

    "understand property inheritance" in new CosmosContext
    {
      addBelief("a bird must be either happy or sad")
      addBelief("a duck is a kind of bird")
      addBelief("Daffy is a duck")
      addBelief("Woodstock is a bird")
      cosmos.validateBeliefs
      val bird = cosmos.getForms("bird")
      val duck = cosmos.getForms("duck")
      val daffy = expectUnique(
        cosmos.resolveQualifiedNoun(
          "duck", REF_SUBJECT, Set("daffy")))
      val woodstock = expectUnique(
        cosmos.resolveQualifiedNoun(
          "bird", REF_SUBJECT, Set("woodstock")))
      Seq(daffy, woodstock).foreach(entity => {
        val propertyTry = cosmos.resolveProperty(entity, "happy")
        propertyTry must beSuccessfulTry
        val (property, stateName) = propertyTry.get
        stateName must be equalTo "happy"
        property.isClosed must beTrue
        val states = property.getStates
        states.size must be equalTo 2
        states must contain("happy" -> "happy")
        states must contain("sad" -> "sad")
      })
    }

    "prevent taxonomy cycles" in new CosmosContext
    {
      addBelief("a duck is a kind of bird")
      // FIXME:  better exception type
      addBelief("a bird is a kind of duck") must
        throwA[IllegalArgumentException]
    }

    "understand genitives" in new CosmosContext
    {
      addBelief("Joyce is a person")
      addBelief("Will is a person")
      addBelief("Jonathan is a person")
      addBelief("Lonnie is a person")
      addBelief("A mom must be a person")
      addBelief("A person may have a mom")
      addBelief("A dad must be a person")
      addBelief("A person may have a dad")
      addBelief("A son must be a person")
      addBelief("A person may have sons")
      addBelief("An ex-husband must be a person")
      addBelief("A person may have an ex-husband")
      addBelief("An ex-wife must be a person")
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
      cosmos.validateBeliefs

      val personLemma = LEMMA_PERSON
      expectNamedForm(personLemma)

      val joyce = expectUnique(
        cosmos.resolveQualifiedNoun(
          personLemma, REF_SUBJECT, Set("joyce")))
      val will = expectUnique(
        cosmos.resolveQualifiedNoun(
          personLemma, REF_SUBJECT, Set("will")))
      val jonathan = expectUnique(
        cosmos.resolveQualifiedNoun(
          personLemma, REF_SUBJECT, Set("jonathan")))
      val lonnie = expectUnique(
        cosmos.resolveQualifiedNoun(
          personLemma, REF_SUBJECT, Set("lonnie")))
      Set(joyce, will, jonathan, lonnie).size must be equalTo 4
      cosmos.resolveGenitive(will, "mom") must be equalTo Set(joyce)
      cosmos.resolveGenitive(will, "dad") must be equalTo Set(lonnie)
      cosmos.resolveGenitive(jonathan, "mom") must be equalTo Set(joyce)
      cosmos.resolveGenitive(jonathan, "dad") must be equalTo Set(lonnie)
      cosmos.resolveGenitive(joyce, "son") must be equalTo Set(will, jonathan)
      cosmos.resolveGenitive(lonnie, "son") must be equalTo Set(will, jonathan)
      cosmos.resolveGenitive(joyce, "mom") must beEmpty
      cosmos.resolveGenitive(joyce, "dad") must beEmpty
      cosmos.resolveGenitive(lonnie, "mom") must beEmpty
      cosmos.resolveGenitive(lonnie, "dad") must beEmpty
      cosmos.resolveGenitive(joyce, "ex-husband") must be equalTo Set(lonnie)
      cosmos.resolveGenitive(joyce, "ex-wife") must beEmpty
      cosmos.resolveGenitive(lonnie, "ex-wife") must be equalTo Set(joyce)
      cosmos.resolveGenitive(lonnie, "ex-husband") must beEmpty

      addBelief("Bert is Will's mom") must
        throwA[UnknownPossesseeBeliefExcn]
      addBelief("Joyce is Bert's mom") must
        throwA[UnknownPossessorBeliefExcn]
    }

    "require genitives to be defined before usage" in new CosmosContext
    {
      addBelief("Joyce is a person")
      addBelief("Will is a person")
      addBelief("A mom is a person")
      addBelief("Joyce is Will's mom") must
        throwA[MissingAssocBeliefExcn]
    }

    "require mandatory genitives to be assigned" in new CosmosContext
    {
      addBelief("Will is a person")
      addBelief("A mom must be a person")
      addBelief("A person must have a mom")
      cosmos.validateBeliefs must
        throwA[CardinalityExcn]
    }

    "prevent single valued genitives from being multiple" in new CosmosContext
    {
      addBelief("Will is a person")
      addBelief("Joyce is a person")
      addBelief("Elle is a person")
      addBelief("A mom must be a person")
      addBelief("A person must have a mom")
      addBelief("Joyce is Will's mom")
      addBelief("Elle is Will's mom") must
        throwA[IncrementalCardinalityExcn]
    }

    "accept synonyms" in new CosmosContext
    {
      val synonyms = cosmos.getFormSynonyms
      addBelief("there is a front door")
      synonyms.addSynonym("portal", "door")
      synonyms.resolveSynonym("door") must be equalTo "door"
      synonyms.resolveSynonym("portal") must be equalTo "door"
      synonyms.resolveSynonym("gateway") must be equalTo "gateway"
      val frontDoor = cosmos.resolveQualifiedNoun(
        "portal", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
    }

    "accept alternative phrasing" in new CosmosContext
    {
      addBelief("A person must be either present or absent")
      addBelief("A person that is at home is present")
      addBelief("Lana is a person")
      expectNamedForm("person")
      val lana = cosmos.resolveQualifiedNoun("person", REF_SUBJECT, Set("lana"))
      lana must beSuccessfulTry.which(_.size == 1)
      val entity = lana.get.head
      cosmos.normalizeState(
        entity,
        SilAdpositionalState(
          ADP_AT,
          SilNounReference(SilWord("home")))
      ) must be equalTo(
        SilPropertyState(SilWord("present"))
      )
    }

    "load beliefs from a file" in new CosmosContext
    {
      val file = ShlurdParser.getResourceFile("/ontologies/bit.txt")
      val source = Source.fromFile(file)
      cosmos.loadBeliefs(source)
      expectNamedForm("bit")
      val form = cosmos.getForms("bit")
      expectDefaultProperty(form)
      val property = form.getProperties(ShlurdPlatonicCosmos.DEFAULT_PROPERTY)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("on" -> "on")
      states must contain("off" -> "off")
    }

    "reject contradictory belief" in new CosmosContext
    {
      addBelief("a door must be open or closed")
      addBelief("a door may be ajar") must
        throwA[ContradictoryBeliefExcn]
    }

    "reject ambiguous belief" in new CosmosContext
    {
      addBelief("there is a front door")
      addBelief("there is a door") must
        throwA[AmbiguousBeliefExcn]
    }

    "reject another ambiguous belief" in new CosmosContext
    {
      addBelief("there is a door")
      addBelief("there is a front door") must
        throwA[AmbiguousBeliefExcn]
    }

    "reject beliefs it cannot understand" in new CosmosContext
    {
      addBelief("he may be either open or closed") must
        throwA[IncomprehensibleBeliefExcn]
    }

    "reject beliefs it cannot implement" in new CosmosContext
    {
      addBelief("a green door must be either open or closed") must
        throwA[UnimplementedBeliefExcn]
    }
  }
}
