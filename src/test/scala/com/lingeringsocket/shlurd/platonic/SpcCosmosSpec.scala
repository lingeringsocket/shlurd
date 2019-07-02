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
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

import spire.math._

import scala.collection._
import scala.io._
import scala.util._

import SprEnglishLemmas._

class SpcCosmosSpec extends SpcProcessingSpecification
{
  trait CosmosContext extends ProcessingContext
  {
    protected def addBelief(input : String) =
    {
      val result = process(
        input,
        ACCEPT_NEW_BELIEFS,
        SmcResponseParams(throwRejectedBeliefs = true))
      result must be equalTo "OK."
    }
  }

  "SpcCosmos" should
  {
    "understand closed property state enumeration" in new CosmosContext
    {
      addBelief("a door must be either open or closed")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      property.domain must be equalTo PROPERTY_CLOSED_ENUM
      val states = cosmos.getPropertyStateMap(property)
      states.size must be equalTo 2
      states must contain("open" -> "open")
      states must contain("close" -> "closed")
      addBelief("a door may be open")
      cosmos.getPropertyStateMap(property).size must be equalTo 2
    }

    "understand open property state enumeration" in new CosmosContext
    {
      addBelief("a door may be either open or closed")
      addBelief("a door may be either open or ajar")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      property.domain must be equalTo PROPERTY_OPEN_ENUM
      val states = cosmos.getPropertyStateMap(property)
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
      property.domain must be equalTo PROPERTY_CLOSED_ENUM
      val states = cosmos.getPropertyStateMap(property)
      states.size must be equalTo 1
      states must contain("close" -> "closed")
    }

    "understand string property" in new CosmosContext
    {
      addBelief("a door's label must be an spc-string")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      property.domain must be equalTo PROPERTY_TYPE_STRING
      val states = cosmos.getPropertyStateMap(property)
      states must beEmpty
    }

    "understand qualified references" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a door must be open or closed")
      addBelief("there is a big door")
      addBelief("there is a small door")
      addBelief("the big door is open")
      addBelief("the small door is closed")
      addBelief("Usher is a house")
      addBelief("the big door is Usher's entrance")
      val door = expectNamedForm("door")
      val house = expectProperName("Usher")
      val property = expectSingleProperty(door)
      val bigDoorTry = cosmos.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("big"))
      bigDoorTry must beSuccessfulTry.which(_.size == 1)
      val smallDoorTry = cosmos.resolveQualifiedNoun(
        "door", REF_SUBJECT, Set("small"))
      smallDoorTry must beSuccessfulTry.which(_.size == 1)
      val bigDoor = bigDoorTry.get.head
      val smallDoor = smallDoorTry.get.head
      bigDoor must not be equalTo(smallDoor)
      cosmos.evaluateEntityProperty(bigDoor, property.name) must be equalTo
        Success((Some(property), Some("open")))
      cosmos.evaluateEntityProperty(smallDoor, property.name) must be equalTo
        Success((Some(property), Some("close")))
      resolveGenitive(house, "entrance") must be equalTo Set(bigDoor)
    }

    "understand specific references" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)

      addBelief("there is a door")
      addBelief("the door's creator is Sirius")
      cosmos.validateBeliefs
      val door = expectNamedForm("door")
      val anonDoor = expectFormSingleton(door)
      val sirius = expectProperName("Sirius")
      resolveGenitive(anonDoor, "creator") must be equalTo Set(sirius)

      addBelief("the door's maintainer is the android")
      val android = expectNamedForm("android")
      val anonAndroid = expectFormSingleton(android)
      resolveGenitive(anonDoor, "maintainer") must be equalTo Set(anonAndroid)
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

    "understand unique entity existence" in new CosmosContext
    {
      addBelief("the dog is a werewolf")
      val dog = expectNamedForm("dog")
      val werewolf = expectNamedForm("werewolf")
      val anonDog = expectFormSingleton(dog)
      // FIXME this should hold
      // cosmos.isHyponym(dog, werewolf) must beFalse
      cosmos.isHyponym(anonDog.form, werewolf) must beTrue
      cosmos.isHyponym(anonDog.form, dog) must beTrue
      cosmos.sanityCheck must beTrue
    }

    "understand property inheritance" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a bird's mood must be either happy or sad")
      addBelief("a duck is a kind of bird")
      addBelief("Daffy is a duck")
      addBelief("Daisy is a duck")
      addBelief("Woodstock is a bird")
      addBelief("Daffy is happy")
      addBelief("Woodstock is sad")
      cosmos.validateBeliefs

      val bird = resolveForm("bird")
      val duck = resolveForm("duck")
      val daffy = expectUnique(
        cosmos.resolveQualifiedNoun(
          "duck", REF_SUBJECT, Set("daffy")))
      val daisy = expectUnique(
        cosmos.resolveQualifiedNoun(
          "duck", REF_SUBJECT, Set("daisy")))
      val woodstock = expectUnique(
        cosmos.resolveQualifiedNoun(
          "bird", REF_SUBJECT, Set("woodstock")))
      cosmos.getFormHyponymRealizations(bird) must be equalTo
        Seq(woodstock, daffy, daisy)
      cosmos.getFormHypernymRealizations(bird) must be equalTo
        Seq(woodstock)
      cosmos.getFormHyponymRealizations(duck) must be equalTo
        Seq(daffy, daisy)
      cosmos.getFormHypernymRealizations(duck) must be equalTo
        Seq(daffy, daisy, woodstock)
      Seq(daffy, daisy, woodstock).foreach(entity => {
        val propertyTry = cosmos.resolvePropertyState(entity, "happy")
        propertyTry must beSuccessfulTry
        val (property, stateName) = propertyTry.get
        stateName must be equalTo "happy"
        property.domain must be equalTo PROPERTY_CLOSED_ENUM
        val states = cosmos.getPropertyStateMap(property)
        states.size must be equalTo 2
        states must contain("happy" -> "happy")
        states must contain("sad" -> "sad")
      })
      val birdMood = expectSingleProperty(bird)
      cosmos.getFormPropertyMap(duck) must beEmpty
      cosmos.evaluateEntityProperty(daffy, birdMood.name) must be equalTo
        Success((Some(birdMood), Some("happy")))
      cosmos.evaluateEntityPropertyPredicate(
        daffy, birdMood, "happy") must be equalTo Success(Trilean.True)
      cosmos.evaluateEntityPropertyPredicate(
        daffy, birdMood, "sad") must be equalTo Success(Trilean.False)
      cosmos.evaluateEntityPropertyPredicate(
        daffy, birdMood, "silly") must be equalTo Success(Trilean.False)
      cosmos.evaluateEntityProperty(woodstock, birdMood.name) must be equalTo
        Success((Some(birdMood), Some("sad")))
      cosmos.evaluateEntityProperty(daisy, birdMood.name) must be equalTo
        Success((Some(birdMood), None))
      cosmos.evaluateEntityPropertyPredicate(
        daisy, birdMood, "happy") must be equalTo Success(Trilean.Unknown)

      // this should fail since mood property is closed
      addBelief("Daisy's mood is grumpy") must
        throwA[ContradictoryBeliefExcn]

      // however, this should succeed since it creates
      // a new property
      addBelief("Daisy is grumpy")
      val duckProperty = expectSingleProperty(duck)
      cosmos.evaluateEntityProperty(daisy, duckProperty.name) must be equalTo
        Success((Some(duckProperty), Some("grumpy")))
      cosmos.evaluateEntityPropertyPredicate(
        daisy, duckProperty, "grumpy") must be equalTo Success(Trilean.True)
      cosmos.evaluateEntityPropertyPredicate(
        daisy, duckProperty, "happy") must be equalTo Success(Trilean.False)
      cosmos.evaluateEntityPropertyPredicate(
        daisy, duckProperty, "happy") must be equalTo Success(Trilean.False)
      cosmos.evaluateEntityPropertyPredicate(
        daisy, birdMood, "happy") must be equalTo Success(Trilean.Unknown)
      cosmos.evaluateEntityProperty(daffy, duckProperty.name) must be equalTo
        Success((Some(duckProperty), None))
      cosmos.evaluateEntityPropertyPredicate(
        daffy, duckProperty, "grumpy") must be equalTo Success(Trilean.Unknown)
      cosmos.evaluateEntityPropertyPredicate(
        daffy, duckProperty, "happy") must be equalTo Success(Trilean.Unknown)
      cosmos.evaluateEntityProperty(woodstock, duckProperty.name) must
        be equalTo Success((None, None))
    }

    "understand role inheritance" in new CosmosContext
    {
      addBelief("a man is a kind of person")
      addBelief("a sibling must be a person")
      addBelief("a brother is a kind of sibling")
      val person = resolveForm("person")
      val man = resolveForm("man")
      val brother = resolveRole("brother")
      cosmos.getGraph.getFormsForRole(brother) must be equalTo Iterable(person)
      addBelief("a brother must be a man")
      cosmos.getGraph.getFormsForRole(brother) must be equalTo Iterable(man)
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

      expectNamedForm("person")
      expectNamedRole("mom")
      expectNamedRole("dad")
      expectNamedRole("son")
      expectNamedRole("ex-husband")
      expectNamedRole("ex-wife")

      val joyce = expectUnique(
        cosmos.resolveQualifiedNoun(
          "person", REF_SUBJECT, Set("joyce")))
      val will = expectUnique(
        cosmos.resolveQualifiedNoun(
          "person", REF_SUBJECT, Set("will")))
      val jonathan = expectUnique(
        cosmos.resolveQualifiedNoun(
          "person", REF_SUBJECT, Set("jonathan")))
      val lonnie = expectUnique(
        cosmos.resolveQualifiedNoun(
          "person", REF_SUBJECT, Set("lonnie")))
      Set(joyce, will, jonathan, lonnie).size must be equalTo 4
      resolveGenitive(will, "mom") must be equalTo Set(joyce)
      resolveGenitive(will, "dad") must be equalTo Set(lonnie)
      resolveGenitive(jonathan, "mom") must be equalTo Set(joyce)
      resolveGenitive(jonathan, "dad") must be equalTo Set(lonnie)
      resolveGenitive(joyce, "son") must be equalTo Set(will, jonathan)
      resolveGenitive(lonnie, "son") must be equalTo Set(will, jonathan)
      resolveGenitive(joyce, "mom") must beEmpty
      resolveGenitive(joyce, "dad") must beEmpty
      resolveGenitive(lonnie, "mom") must beEmpty
      resolveGenitive(lonnie, "dad") must beEmpty
      resolveGenitive(joyce, "ex-husband") must be equalTo Set(lonnie)
      resolveGenitive(joyce, "ex-wife") must beEmpty
      resolveGenitive(lonnie, "ex-wife") must be equalTo Set(joyce)
      resolveGenitive(lonnie, "ex-husband") must beEmpty
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
      skipped("not working yet")

      SpcPrimordial.initCosmos(cosmos)

      addBelief("Edison's invention is Byron")
      addBelief("a person with an invention is an inventor")
      addBelief("Edison is a person")

      cosmos.sanityCheck must beTrue
    }

    "accept synonyms" in new CosmosContext
    {
      addBelief("there is a big door")
      addBelief("a portal is a door")
      cosmos.resolveIdealSynonym("door") must be equalTo "door"
      cosmos.resolveIdealSynonym("portal") must be equalTo "door"
      cosmos.resolveIdealSynonym("gateway") must be equalTo "gateway"
      val bigDoor = cosmos.resolveQualifiedNoun(
        "portal", REF_SUBJECT, Set("big"))
      bigDoor must beSuccessfulTry.which(_.size == 1)
    }

    "accept override belief" in new CosmosContext
    {
      addBelief("a portal must be open or closed")
      addBelief("a door is a kind of portal")
      addBelief("a door must be open")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      property.domain must be equalTo PROPERTY_CLOSED_ENUM
      val states = cosmos.getPropertyStateMap(property)
      states.size must be equalTo 1
      states must contain("open" -> "open")
    }

    "accept alternative phrasing" in new CosmosContext
    {
      addBelief("A person must be either present or absent")
      addBelief("A person that is at home is present")
      addBelief("Lana is a person")
      val entity = expectPerson("lana")
      cosmos.normalizeHyperFormState(
        entity.form,
        SilAdpositionalState(
          SilAdposition.AT,
          SilNounReference(SilWord("home")))
      ) must be equalTo(
        SilPropertyState(SilWord("present"))
      )
    }

    "accept assertions" in new CosmosContext
    {
      cosmos.getTriggers must beEmpty
      cosmos.getAssertions must beEmpty
      addBelief("a tailor can't kill a giant")
      cosmos.getAssertions.size must be equalTo(1)
      cosmos.getTriggers must beEmpty
    }

    "accept triggers" in new CosmosContext
    {
      cosmos.getTriggers must beEmpty
      cosmos.getAssertions must beEmpty
      addBelief("if a person is an object's possessor, " +
        "then the person is carrying the object")
      cosmos.getAssertions.size must be equalTo(1)
      cosmos.getTriggers.size must be equalTo(1)
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

    "update entity enum properties" in new CosmosContext
    {
      addBelief("a door may be open or closed")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      addBelief("there is a door")
      val entity = expectFormSingleton(form)
      cosmos.evaluateEntityPropertyPredicate(entity, property, "open") must
        be equalTo Success(Trilean.Unknown)
      cosmos.evaluateEntityProperty(entity, property.name) must
        be equalTo Success((Some(property), None))
      addBelief("the door is closed")
      cosmos.evaluateEntityPropertyPredicate(entity, property, "close") must
        be equalTo Success(Trilean.True)
      cosmos.evaluateEntityPropertyPredicate(entity, property, "open") must
        be equalTo Success(Trilean.False)
      cosmos.evaluateEntityProperty(entity, property.name) must
        be equalTo Success((Some(property), Some("close")))
    }

    "update entity string properties" in new CosmosContext
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }
      addBelief("a door's label must be an spc-string")
      val form = expectNamedForm("door")
      val property = expectSingleProperty(form)
      addBelief("there is a door")
      val entity = expectFormSingleton(form)
      cosmos.evaluateEntityProperty(entity, property.name) must
        be equalTo Success((Some(property), None))
      val epigram = "lasciate ogni speranza, voi ch'entrate"
      addBelief("the door's label is \"" + epigram + "\"")
      cosmos.evaluateEntityProperty(entity, property.name) must
        be equalTo Success((Some(property), Some(epigram)))
    }

    "infer role for tentative form" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a pet may have an owner")
      addBelief("Timmy is Lassie's owner")
      addBelief("an owner must be a person")

      val timmy = expectProperName("Timmy")
      val person = expectNamedForm("person")
      cosmos.isHyponym(timmy.form, person) must beTrue

      cosmos.sanityCheck must beTrue
    }

    "accept beliefs in any order" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)

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
      cosmos.getPropertyStateMap(cowFlesh).size must be equalTo 2
      val bessie = expectProperName("Bessie")
      bessie.form must be equalTo(cow)
      val jack = expectProperName("Jack")
      resolveGenitive(bessie, "owner") must be equalTo Set(jack)

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
      val specificRef = mind.specificReference(entity, DETERMINER_UNSPECIFIED)
      specificRef must be equalTo properRef
    }

    "support primordial forms" in new CosmosContext
    {
      cosmos.getForms.size must be equalTo 0
      SpcPrimordial.initCosmos(cosmos)
      cosmos.getForms.size must be greaterThan 0
      val entity = expectNamedForm(SpcMeta.ENTITY_METAFORM_NAME)
      val someone = expectNamedForm(SmcLemmas.LEMMA_SOMEONE)
      val obj = expectNamedForm(SmcLemmas.LEMMA_OBJECT)
      val propGender = expectSingleProperty(entity)
      propGender.name must be equalTo LEMMA_GENDER
      propGender.domain must be equalTo PROPERTY_OPEN_ENUM
      val genderValues = cosmos.getPropertyStateMap(propGender)
      genderValues.size must be equalTo 2
      genderValues must contain(LEMMA_MASCULINE -> LEMMA_MASCULINE)
      genderValues must contain(LEMMA_FEMININE -> LEMMA_FEMININE)
      cosmos.resolveIdealSynonym(LEMMA_WHO) must
        be equalTo SmcLemmas.LEMMA_SOMEONE
      cosmos.getFormHypernyms(entity) must be equalTo(
        Seq(entity))
      cosmos.getFormHypernyms(obj) must be equalTo(
        Seq(obj, entity))
      cosmos.getFormHypernyms(someone) must be equalTo(
        Seq(someone, obj, entity))
      cosmos.isHyponym(someone, entity) must beTrue
      cosmos.isHyponym(entity, someone) must beFalse
      cosmos.isHyponym(someone, someone) must beTrue
      cosmos.isHyponym(entity, entity) must beTrue
      // FIXME verify container/containee association
    }

    "support compound nouns" in new CosmosContext
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }

      addBelief("there is a steak knife")
      val form = expectNamedForm(cosmos.encodeName("steak knife"))
    }

    "elide redundant taxonomy edges" in new CosmosContext
    {
      SpcPrimordial.initCosmos(cosmos)
      addBelief("a firefighter is a kind of spc-someone")
      val entity = expectNamedForm(SpcMeta.ENTITY_METAFORM_NAME)
      val someone = expectNamedForm(SmcLemmas.LEMMA_SOMEONE)
      val obj = expectNamedForm(SmcLemmas.LEMMA_OBJECT)
      val firefighter = expectNamedForm("firefighter")
      cosmos.getFormHypernyms(entity) must be equalTo(
        Seq(entity))
      cosmos.getFormHypernyms(someone) must be equalTo(
        Seq(someone, obj, entity))
      cosmos.getFormHypernyms(firefighter) must be equalTo(
        Seq(firefighter, someone, obj, entity))
      cosmos.isHyponym(firefighter, someone) must beTrue
      cosmos.isHyponym(firefighter, obj) must beTrue
      cosmos.isHyponym(someone, firefighter) must beFalse
      cosmos.isHyponym(obj, firefighter) must beFalse
      cosmos.isHyponym(someone, obj) must beTrue
      cosmos.isHyponym(someone, entity) must beTrue
      cosmos.isHyponym(firefighter, entity) must beTrue
      cosmos.sanityCheck must beTrue
    }

    "load beliefs from a file" in new CosmosContext
    {
      val file = ResourceUtils.getResourceFile("/ontologies/bit.txt")
      val source = Source.fromFile(file)
      new SpcMind(cosmos).loadBeliefs(source)
      val form = expectNamedForm("bit")
      val property = expectSingleProperty(form)
      property.domain must be equalTo PROPERTY_CLOSED_ENUM
      val states = cosmos.getPropertyStateMap(property)
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
      addBelief("there is a big door")
      addBelief("there is a door") must
        throwA[AmbiguousBeliefExcn]

      addBelief("there is a red pig")
      addBelief("there is a green pig")
      addBelief("the pig is Charlotte's pet") must
        throwA[AmbiguousBeliefExcn]
    }

    "reject another ambiguous belief" in new CosmosContext
    {
      addBelief("there is a door")
      addBelief("there is a big door") must
        throwA[AmbiguousBeliefExcn]
    }

    "reject beliefs it cannot understand" in new CosmosContext
    {
      addBelief("he may be either open or closed") must
        throwA[IncomprehensibleBeliefExcn]
      addBelief("Daffy is a pig's duck") must
        throwA[IncomprehensibleBeliefExcn]
      addBelief("the wrench is an object")
      addBelief("the screwdriver is an object")
      addBelief("the screwdriver is proud of the wrench") must
        throwA[IncomprehensibleBeliefExcn]
    }

    "understand proper compound nouns" in new CosmosContext
    {
      skipped("not working yet")
      addBelief("Daffy is Porky Pig's duck") must
        throwA[IncomprehensibleBeliefExcn]
    }

    "reject beliefs it cannot implement" in new CosmosContext
    {
      addBelief("a green door must be either open or closed") must
        throwA[UnimplementedBeliefExcn]
    }

    "reject invalid beliefs" in new CosmosContext
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }
      addBelief(
        "if a person eats a pickle, " +
          "then the pickle is sandy"
      ) must throwA[InvalidBeliefExcn]
      addBelief(
        "if a person eats a pickle, " +
          "then equivalently the pickle becomes sandy"
      ) must throwA[InvalidBeliefExcn]
      addBelief(
        "if a person eats a pickle, " +
          "then equivalently the pickle is subsequently sandy"
      ) must throwA[InvalidBeliefExcn]
    }
  }
}
