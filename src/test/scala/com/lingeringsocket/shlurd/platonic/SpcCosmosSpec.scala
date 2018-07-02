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
import com.lingeringsocket.shlurd.cosmos._

import org.specs2.mutable._

import scala.collection._
import scala.io._
import scala.util._

import ShlurdEnglishLemmas._

class SpcCosmosSpec extends Specification
{
  trait CosmosContext extends NameSpace
  {
    protected val cosmos = new SpcCosmos

    protected val interpreter = new SpcBeliefInterpreter(cosmos)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      interpreter.interpretBelief(sentence)
    }

    protected def expectNamedForm(name : String) =
    {
      val formOpt = cosmos.getForms.get(name)
      formOpt must beSome.which(_.name == name)
      formOpt.get
    }

    protected def expectNamedRole(name : String) =
    {
      val roleOpt = cosmos.getRoles.get(name)
      roleOpt must beSome.which(_.name == name)
      roleOpt.get
    }

    protected def expectSingleProperty(form : SpcForm)
        : SpcProperty =
    {
      val properties = form.getProperties
      properties.size must be equalTo 1
      form.getProperties.head._2
    }

    protected def expectPerson(name : String) : SpcEntity =
    {
      expectNamedForm(LEMMA_PERSON)
      expectUnique(cosmos.resolveQualifiedNoun(
        LEMMA_PERSON, REF_SUBJECT, Set(name)))
    }

    protected def expectProperName(name : String) : SpcEntity =
    {
      expectUnique(
        cosmos.getEntities.values.filter(_.properName == name))
    }

    protected def expectUnique(
      entities : Iterable[SpcEntity]) : SpcEntity =
    {
      entities.size must be equalTo(1)
      entities.head
    }

    protected def expectUnique(
      entities : Try[Iterable[SpcEntity]]) : SpcEntity =
    {
      entities must beSuccessfulTry.which(_.size == 1)
      entities.get.head
    }
  }

  "SpcCosmos" should
  {
    "understand closed property state enumeration" in new CosmosContext
    {
      addBelief("a door must be either open or closed")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
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
      addBelief("a door may be either open or ajar")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
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
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
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
      val dog = expectNamedForm("dog")
      val canine = expectNamedForm("canine")
      val bird = expectNamedForm("bird")
      val duck = expectNamedForm("duck")
      val mallard = expectNamedForm("mallard")
      val canvasback = expectNamedForm("canvasback")
      val graph = cosmos.getGraph
      graph.isHyponym(dog, canine) must beTrue
      graph.isHyponym(canine, dog) must beFalse
      graph.isHyponym(duck, bird) must beTrue
      graph.isHyponym(bird, duck) must beFalse
      graph.isHyponym(mallard, duck) must beTrue
      graph.isHyponym(mallard, bird) must beTrue
      graph.isHyponym(duck, mallard) must beFalse
      graph.isHyponym(bird, mallard) must beFalse
      graph.isHyponym(canvasback, duck) must beTrue
      graph.isHyponym(canvasback, bird) must beTrue
      graph.isHyponym(canvasback, mallard) must beFalse
      graph.isHyponym(mallard, canvasback) must beFalse
      graph.isHyponym(canine, bird) must beFalse
      graph.isHyponym(bird, canine) must beFalse
      graph.isHyponym(dog, duck) must beFalse
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

    "understand role inheritance" in new CosmosContext
    {
      addBelief("a man is a kind of person")
      addBelief("a sibling must be a person")
      addBelief("a brother is a kind of sibling")
      val person = cosmos.getForms(LEMMA_PERSON)
      val man = cosmos.getForms("man")
      val brother = cosmos.getRoles("brother")
      cosmos.getGraph.getFormForRole(brother) must be equalTo Some(person)
      addBelief("a brother must be a man")
      cosmos.getGraph.getFormForRole(brother) must be equalTo Some(man)
    }

    "prevent taxonomy cycles" in new CosmosContext
    {
      addBelief("a duck is a kind of bird")
      addBelief("a bird is a kind of duck") must
        throwA[ContradictoryBeliefExcn]
      addBelief("a sibling must be a person")
      addBelief("a brother is a kind of sibling")
      addBelief("a sibling is a kind of brother") must
        throwA[ContradictoryBeliefExcn]
    }

    "prevent form as hyponym for role" in new CosmosContext
    {
      // some forms
      addBelief("a man is a kind of person")
      addBelief("a buffoon is a kind of person")
      addBelief("a groomsman is a kind of man")
      // minion is a role
      addBelief("a minion must be a person")
      // should not be able to make an existing form a hyponym for a role
      addBelief("a groomsman is a kind of minion") must
        throwA[IncomprehensibleBeliefExcn]
      // should not be able to change an existing form into a role
      addBelief("a groomsman must be a buffoon") must
        throwA[IncomprehensibleBeliefExcn]
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
      addBelief("Will's dad is Lonnie")
      addBelief("Lonnie is Jonathan's dad")
      addBelief("Lonnie is Joyce's ex-husband")
      addBelief("Joyce is Lonnie's ex-wife")
      cosmos.validateBeliefs

      expectNamedForm(LEMMA_PERSON)
      expectNamedRole("mom")
      expectNamedRole("dad")
      expectNamedRole("son")
      expectNamedRole("ex-husband")
      expectNamedRole("ex-wife")

      val joyce = expectUnique(
        cosmos.resolveQualifiedNoun(
          LEMMA_PERSON, REF_SUBJECT, Set("joyce")))
      val will = expectUnique(
        cosmos.resolveQualifiedNoun(
          LEMMA_PERSON, REF_SUBJECT, Set("will")))
      val jonathan = expectUnique(
        cosmos.resolveQualifiedNoun(
          LEMMA_PERSON, REF_SUBJECT, Set("jonathan")))
      val lonnie = expectUnique(
        cosmos.resolveQualifiedNoun(
          LEMMA_PERSON, REF_SUBJECT, Set("lonnie")))
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
      SpcPrimordial.initCosmos(cosmos)

      // starting with a permanent form
      addBelief("Will is a person")
      addBelief("Joyce is a person")
      addBelief("Elle is a person")
      addBelief("A mom must be a person")
      addBelief("A person must have a mom")
      addBelief("Joyce is Will's mom")
      addBelief("Elle is Will's mom") must
        throwA[IncrementalCardinalityExcn]

      cosmos.sanityCheck must beTrue

      // starting with a tentative form
      addBelief("Harry's house is Hufflepuff")
      addBelief("Harry's house is Ravenclaw")
      addBelief("a student may have a house")
      addBelief("Harry is a student") must
        throwA[IncrementalCardinalityExcn]

      cosmos.sanityCheck must beTrue
    }

    "support inverse associations with tentative forms" in new CosmosContext
    {
      skipped("maybe one day")

      SpcPrimordial.initCosmos(cosmos)

      addBelief("Edison's invention is Byron")
      addBelief("a person with an invention is an inventor")
      addBelief("Edison is a person")

      cosmos.sanityCheck must beTrue
    }

    "accept synonyms" in new CosmosContext
    {
      val synonyms = cosmos.getIdealSynonyms
      addBelief("there is a front door")
      synonyms.addSynonym("portal", "door")
      synonyms.resolveSynonym("door") must be equalTo "door"
      synonyms.resolveSynonym("portal") must be equalTo "door"
      synonyms.resolveSynonym("gateway") must be equalTo "gateway"
      val frontDoor = cosmos.resolveQualifiedNoun(
        "portal", REF_SUBJECT, Set("front"))
      frontDoor must beSuccessfulTry.which(_.size == 1)
    }

    "accept override belief" in new CosmosContext
    {
      addBelief("a portal must be open or closed")
      addBelief("a door is a kind of portal")
      addBelief("a door must be open")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 1
      states must contain("open" -> "open")
    }

    "accept alternative phrasing" in new CosmosContext
    {
      addBelief("A person must be either present or absent")
      addBelief("A person that is at home is present")
      addBelief("Lana is a person")
      val entity = expectPerson("lana")
      cosmos.normalizeState(
        entity,
        SilAdpositionalState(
          ADP_AT,
          SilNounReference(SilWord("home")))
      ) must be equalTo(
        SilPropertyState(SilWord("present"))
      )
    }

    "prevent incompatible role modification" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a pet may have an owner")
      addBelief("Timmy is a freak")
      addBelief("Timmy is Lassie's owner")
      addBelief("an owner must be a person") must
        throwA[ContradictoryBeliefExcn]

      cosmos.sanityCheck must beTrue
    }

    "infer role for tentative form" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a pet may have an owner")
      addBelief("Timmy is Lassie's owner")
      addBelief("an owner must be a person")

      val timmy = expectProperName("Timmy")
      val person = expectNamedForm(LEMMA_PERSON)
      cosmos.getGraph.isHyponym(timmy.form, person) must beTrue

      cosmos.sanityCheck must beTrue
    }

    "accept beliefs in any order" in new CosmosContext
    {
      // entity association before form association
      addBelief("Bessie's owner is Jack")

      // entity before form
      addBelief("Lassie is a dog")

      // properties before taxonomy
      addBelief("a cow's flesh may be tender or tough")
      addBelief("a cow is a kind of animal")
      addBelief("a dog is a kind of animal")
      addBelief("a cow may have an owner")
      addBelief("an owner must be a person")

      // replace tentative form
      addBelief("Bessie is a cow")

      val animal = expectNamedForm("animal")
      val cow = expectNamedForm("cow")
      val cowFlesh = expectSingleProperty(cow)
      cowFlesh.name must be equalTo("flesh")
      cowFlesh.getStates.size must be equalTo 2
      val bessie = expectProperName("Bessie")
      bessie.form must be equalTo(cow)
      val jack = expectProperName("Jack")
      cosmos.resolveGenitive(bessie, "owner") must be equalTo Set(jack)

      addBelief("Bessie is an animal")
      val bessieAnimal = expectProperName("Bessie")
      bessieAnimal must be equalTo(bessie)

      addBelief("Babe is an animal")
      val babeAnimal = expectProperName("Babe")
      babeAnimal.form must be equalTo(animal)

      addBelief("Babe is a cow")
      val babeCow = expectProperName("Babe")
      babeCow.form must be equalTo(cow)

      addBelief("Babe is a dog") must
        throwA[ContradictoryBeliefExcn]

      cosmos.sanityCheck must beTrue
    }

    "distinguish entities" in new CosmosContext
    {
      addBelief("an interviewer is a kind of person")
      addBelief("there is an interviewer")
      addBelief("Larry is a person")
      val interviewer = expectUnique(
        cosmos.resolveQualifiedNoun(
          "interviewer", REF_SUBJECT, Set()))
      val larry = expectPerson("larry")
      larry must not be equalTo(interviewer)
    }

    "produce entity references" in new CosmosContext
    {
      addBelief("Lana is a person")
      val entity = expectPerson("lana")
      val properRef = SilNounReference(SilWord("Lana"))
      val specificRef = cosmos.specificReference(entity, DETERMINER_UNSPECIFIED)
      specificRef must be equalTo properRef
    }

    "clear all entities" in new CosmosContext
    {
      addBelief("Lana is a person")
      val entity = expectPerson("lana")
      cosmos.clear
      val cleared = cosmos.resolveQualifiedNoun(
        LEMMA_PERSON, REF_SUBJECT, Set("Lana"))
      cleared must beSuccessfulTry.which(_.isEmpty)
    }

    "support primordial forms" in new CosmosContext
    {
      cosmos.getForms.size must be equalTo 0
      SpcPrimordial.initCosmos(cosmos)
      cosmos.getForms.size must be equalTo 2
      val entity = expectNamedForm(LEMMA_ENTITY)
      val person = expectNamedForm(LEMMA_PERSON)
      val propGender = expectSingleProperty(person)
      propGender.name must be equalTo LEMMA_GENDER
      propGender.isClosed must beFalse
      val genderValues = propGender.getStates
      genderValues.size must be equalTo 2
      genderValues must contain(LEMMA_MASCULINE -> LEMMA_MASCULINE)
      genderValues must contain(LEMMA_FEMININE -> LEMMA_FEMININE)
      cosmos.getIdealSynonyms.resolveSynonym(LEMMA_WHO) must
        be equalTo LEMMA_PERSON
      cosmos.getIdealSynonyms.resolveSynonym(LEMMA_ACTUALITY) must
        be equalTo LEMMA_ENTITY
      val graph = cosmos.getGraph
      graph.getFormHypernyms(entity).toSeq must be equalTo(
        Seq(entity))
      graph.getFormHypernyms(person).toSeq must be equalTo(
        Seq(person, entity))
      graph.isHyponym(person, entity) must beTrue
      graph.isHyponym(entity, person) must beFalse
      graph.isHyponym(person, person) must beTrue
      graph.isHyponym(entity, entity) must beTrue
    }

    "elide redundant taxonomy edges" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a firefighter is a kind of person")
      val entity = expectNamedForm(LEMMA_ENTITY)
      val person = expectNamedForm(LEMMA_PERSON)
      val firefighter = expectNamedForm("firefighter")
      val graph = cosmos.getGraph
      graph.getFormHypernyms(entity).toSeq must be equalTo(
        Seq(entity))
      graph.getFormHypernyms(person).toSeq must be equalTo(
        Seq(person, entity))
      graph.getFormHypernyms(firefighter).toSeq must be equalTo(
        Seq(firefighter, person, entity))
      graph.isHyponym(firefighter, person) must beTrue
      graph.isHyponym(person, firefighter) must beFalse
      graph.isHyponym(person, entity) must beTrue
      graph.isHyponym(firefighter, entity) must beTrue
      cosmos.sanityCheck must beTrue
    }

    "load beliefs from a file" in new CosmosContext
    {
      val file = ShlurdParser.getResourceFile("/ontologies/bit.txt")
      val source = Source.fromFile(file)
      cosmos.loadBeliefs(source)
      val form = expectNamedForm("bit")
      val property = expectSingleProperty(form)
      property.isClosed must beTrue
      val states = property.getStates
      states.size must be equalTo 2
      states must contain("on" -> "on")
      states must contain("off" -> "off")
    }

    "reject contradictory belief" in new CosmosContext
    {
      addBelief("a door must be open or closed")
      addBelief("a door may be open or ajar") must
        throwA[ContradictoryBeliefExcn]
    }

    "reject contradictory override belief" in new CosmosContext
    {
      addBelief("a portal must be open or closed")
      addBelief("a door is a kind of portal")
      addBelief("a door may be open or ajar") must
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
