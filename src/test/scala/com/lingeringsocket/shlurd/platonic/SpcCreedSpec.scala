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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

class SpcCreedSpec extends Specification
{
  trait CosmosContext extends Scope
  {
    protected val cosmos = new SpcCosmos

    private val refriedCosmos = new SpcCosmos

    protected val creed = new SpcCreed(cosmos)

    private val refriedCreed = new SpcCreed(refriedCosmos)

    protected def addBelief(input : String) =
    {
      val sentence = cosmos.newParser(input).parseOne
      val mind = new SpcMind(cosmos)
      val interpreter = new SpcInterpreter(mind)
      val resultCollector = SmcResultCollector[SpcEntity]()
      interpreter.resolveReferences(sentence, resultCollector)
      val beliefInterpreter = new SpcBeliefInterpreter(
        mind, false, resultCollector)
      beliefInterpreter.interpretBelief(sentence)
    }

    protected def expectPreserved(
      input : Iterable[String]) =
    {
      expectNormalized(input, input)
    }

    protected def expectNormalized(
      input : Iterable[String], expected : Iterable[String]) =
    {
      input.foreach(addBelief)
      val printer = new SilSentencePrinter
      val beliefStrings = creed.allBeliefs.map(s => printer.print(s))
      beliefStrings.map(SprUtils.capitalize) must be equalTo expected
      beliefStrings.foreach(beliefString => {
        val sentence = cosmos.newParser(beliefString).parseOne
        val refriedMind = new SpcMind(refriedCosmos)
        val refriedInterpreter =
          new SpcInterpreter(refriedMind)
        val resultCollector = SmcResultCollector[SpcEntity]()
        refriedInterpreter.resolveReferences(sentence, resultCollector)
        val refriedBeliefInterpreter =
          new SpcBeliefInterpreter(refriedMind, false, resultCollector)
        val refriedBeliefs =
          refriedBeliefInterpreter.recognizeBeliefs(sentence)
        refriedBeliefs.foreach(belief => {
          refriedBeliefInterpreter.applyBelief(belief)
        })
      })
      val refriedStrings = refriedCreed.allBeliefs.map(s => printer.print(s))
      refriedStrings.map(SprUtils.capitalize) must be equalTo expected
    }
  }

  private val stateMust = "A door must be open or closed."
  private val stateMay = "A window may be open or closed."
  private val stateAlias = "A lit light is on."
  private val stateNormalization = "A person at home is present."
  private val stateProperty = "A parakeet's mood may be happy or sad."
  private val formTaxonomy = "A duck is a kind of a bird."
  private val formSynonym = "An automobile is a car."
  private val formRole = "A mentor must be a person."
  private val formRole2 = "An owner must be a person."
  private val formRole3 = "A pet must be an animal."
  private val assocHas = "A dog has an owner."
  private val assocMust = "A dog must have one owner."
  private val assocMay = "A person may have one mentor."
  private val assocMayPlural = "A person may have pets."
  private val assocMayProperty = "A person may have one presence as a property."
  private val entityExists = "There is a parakeet."
  private val entityState = "The parakeet is happy."
  private val namedEntityExists = "Rapunzel is a dog."
  private val entityQualifiedExists = "There is an angry cat."
  private val personExists = "Yoda is a person."
  private val personExists2 = "Luke is a person."
  private val personAssoc = "Yoda is Luke's mentor."
  private val personAssocFlipped = "Luke's mentor is Yoda."
  private val mentorRole = "A mentor must be a monk."
  private val apprenticeRole = "An apprentice must be a monk."
  private val mentorApprentice = "A mentor may have apprentices."
  private val apprenticeMentor = "An apprentice may have one mentor."
  private val assocInverse1 = "A monk with an apprentice is a mentor."
  private val assocInverse2 = "A monk with a mentor is an apprentice."
  private val apprenticeMentors = "An apprentice may have mentors."
  private val nephew = "A man with an uncle or aunt is a nephew."
  private val auntNephews = "An aunt may have nephews."
  private val uncleNephews = "An uncle may have nephews."
  private val nephewAunts = "A nephew may have aunts."
  private val manAuntNephew = "A man with an aunt is a nephew."
  private val nephewUncles = "A nephew may have uncles."
  private val manUncleNephew = "A man with an uncle is a nephew."
  private val nephewMan = "A nephew must be a man."
  private val children = "A person may have sons or daughters."
  private val childrenSons = "A person may have sons."
  private val childrenDaughters = "A person may have daughters."
  private val moveTrigger = "If an spc-object moves to a location, " +
    "then the location is the object's spc-container."

  private val primordial = Seq(
    "An spc-class is an spc-form.",
    "An spc-type must be an spc-form.",
    "An spc-realization must be an spc-entity.",
    "An spc-attribute must be an spc-property.",
    "An spc-attributee must be an spc-form.",
    "An spc-superclass must be an spc-ideal.",
    "An spc-subclass must be an spc-ideal.",
    "An spc-container must be an spc-object.",
    "An spc-contained-object must be an spc-object.",
    "An spc-entity must have one spc-type.",
    "An spc-ideal is a kind of an spc-entity.",
    "An spc-ideal may have spc-superclasses.",
    "An spc-ideal may have spc-subclasses.",
    "An spc-form is a kind of an spc-ideal.",
    "An spc-form may have spc-realizations.",
    "An spc-form may have spc-attributes.",
    "An spc-role is a kind of an spc-ideal.",
    "An spc-property is a kind of an spc-entity.",
    "An spc-property must have one spc-attributee.",
    "An spc-someone's gender may be masculine or feminine.",
    "An spc-someone is a kind of an spc-object.",
    "An spc-object is a kind of an spc-entity.",
    "An spc-object may have spc-contained-objects.",
    "An spc-object must have one spc-container.",
    "An spc-form with an spc-realization is an spc-type.",
    "An spc-entity with an spc-type is an spc-realization.",
    "An spc-form with an spc-attribute is an spc-attributee.",
    "An spc-property with an spc-attributee is an spc-attribute.",
    "An spc-ideal with an spc-superclass is an spc-subclass.",
    "An spc-ideal with an spc-subclass is an spc-superclass.",
    "An spc-object with an spc-contained-object is an spc-container.",
    "An spc-object with an spc-container is an spc-contained-object.",
    "SPC-Form-spc-entity is an spc-form.",
    "SPC-Form-spc-ideal is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Role-spc-realization is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-property is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-object is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-entity's spc-type.",
    "SPC-Form-spc-ideal is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-ideal's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Form-spc-role is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Role-spc-superclass is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Role-spc-subclass is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-ideal's spc-type.",
    "SPC-Form-spc-form is an spc-form.",
    "SPC-Form-spc-ideal is SPC-Form-spc-form's spc-superclass.",
    "SPC-Role-spc-type is SPC-Form-spc-form's spc-subclass.",
    "SPC-Role-spc-attributee is SPC-Form-spc-form's spc-subclass.",
    "SPC-Form-spc-entity is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-ideal is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-form is SPC-Form-spc-form's spc-type.",
    "SPC-Form-spc-form is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-role is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-property is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-someone is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-object is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-role is an spc-form.",
    "SPC-Form-spc-ideal is SPC-Form-spc-role's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-role's spc-type.",
    "SPC-Role-spc-type is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-realization is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-attribute is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-attributee is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-superclass is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-subclass is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-container is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-contained-object is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-type is an spc-role.",
    "SPC-Form-spc-form is SPC-Role-spc-type's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-type's spc-type.",
    "SPC-Role-spc-realization is an spc-role.",
    "SPC-Form-spc-entity is SPC-Role-spc-realization's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-realization's spc-type.",
    "SPC-Form-spc-property is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-property's spc-superclass.",
    "SPC-Role-spc-attribute is SPC-Form-spc-property's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-property's spc-type.",
    "SPC-Role-spc-attribute is an spc-role.",
    "SPC-Form-spc-property is SPC-Role-spc-attribute's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-attribute's spc-type.",
    "SPC-Role-spc-attributee is an spc-role.",
    "SPC-Form-spc-form is SPC-Role-spc-attributee's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-attributee's spc-type.",
    "SPC-Role-spc-superclass is an spc-role.",
    "SPC-Form-spc-ideal is SPC-Role-spc-superclass' spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-superclass' spc-type.",
    "SPC-Role-spc-subclass is an spc-role.",
    "SPC-Form-spc-ideal is SPC-Role-spc-subclass' spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-subclass' spc-type.",
    "SPC-Form-spc-someone is an spc-form.",
    "SPC-Form-spc-object is SPC-Form-spc-someone's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-someone's spc-type.",
    "SPC-Form-spc-object is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-object's spc-superclass.",
    "SPC-Form-spc-someone is SPC-Form-spc-object's spc-subclass.",
    "SPC-Role-spc-container is SPC-Form-spc-object's spc-subclass.",
    "SPC-Role-spc-contained-object is SPC-Form-spc-object's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-object's spc-type.",
    "SPC-Role-spc-container is an spc-role.",
    "SPC-Form-spc-object is SPC-Role-spc-container's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-container's spc-type.",
    "SPC-Role-spc-contained-object is an spc-role.",
    "SPC-Form-spc-object is SPC-Role-spc-contained-object's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-contained-object's spc-type.",
    "SPC-Property-spc-someone-gender is " +
      "SPC-Form-spc-property's spc-realization.",
    "SPC-Property-spc-someone-gender is an spc-property.",
    "SPC-Form-spc-property is SPC-Property-spc-someone-gender's spc-type.",
    "SPC-Property-spc-someone-gender is SPC-Form-spc-someone's spc-attribute.",
    "SPC-Form-spc-someone is SPC-Property-spc-someone-gender's spc-attributee.",
    "An spc-valued-property must be an spc-property.",
    "An spc-property-value must be an spc-value.",
    "An spc-property may have spc-property-values.",
    "An spc-value is a kind of an spc-entity.",
    "An spc-value must have one spc-valued-property.",
    "An spc-property with an spc-property-value is an spc-valued-property.",
    "An spc-value with an spc-valued-property is an spc-property-value.",
    "SPC-Form-spc-value is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-value is SPC-Form-spc-form's spc-realization.",
    "SPC-Role-spc-valued-property is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-property-value is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-valued-property is SPC-Form-spc-property's spc-subclass.",
    "SPC-Role-spc-valued-property is an spc-role.",
    "SPC-Form-spc-property is SPC-Role-spc-valued-property's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-valued-property's spc-type.",
    "SPC-Form-spc-value is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-value's spc-superclass.",
    "SPC-Role-spc-property-value is SPC-Form-spc-value's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-value's spc-type.",
    "SPC-Role-spc-property-value is an spc-role.",
    "SPC-Form-spc-value is SPC-Role-spc-property-value's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-property-value's spc-type.",
    "SPC-Value-spc-someone-gender-masculine is " +
      "SPC-Form-spc-value's spc-realization.",
    "SPC-Value-spc-someone-gender-feminine is " +
      "SPC-Form-spc-value's spc-realization.",
    "SPC-Value-spc-someone-gender-masculine is " +
      "SPC-Property-spc-someone-gender's spc-property-value.",
    "SPC-Value-spc-someone-gender-feminine is " +
      "SPC-Property-spc-someone-gender's spc-property-value.",
    "SPC-Value-spc-someone-gender-masculine is an spc-value.",
    "SPC-Property-spc-someone-gender is " +
      "SPC-Value-spc-someone-gender-masculine's spc-valued-property.",
    "SPC-Form-spc-value is SPC-Value-spc-someone-gender-masculine's spc-type.",
    "SPC-Value-spc-someone-gender-feminine is an spc-value.",
    "SPC-Property-spc-someone-gender is " +
      "SPC-Value-spc-someone-gender-feminine's spc-valued-property.",
    "SPC-Form-spc-value is SPC-Value-spc-someone-gender-feminine's spc-type."
  )

  "SpcCreed" should
  {
    "preserve states" in new CosmosContext
    {
      expectPreserved(Seq(stateMust, stateMay))
    }

    "preserve state alias" in new CosmosContext
    {
      expectPreserved(Seq(stateAlias))
    }

    "preserve state normalizations" in new CosmosContext
    {
      expectPreserved(Seq(stateNormalization))
    }

    "preserve state property" in new CosmosContext
    {
      expectPreserved(Seq(stateProperty))
    }

    "preserve form synonyms" in new CosmosContext
    {
      expectPreserved(Seq(formSynonym))
    }

    "preserve form taxonomy" in new CosmosContext
    {
      expectPreserved(Seq(formTaxonomy))
    }

    "preserve form associations" in new CosmosContext
    {
      expectPreserved(Seq(
        formRole, formRole2, formRole3, assocMay, assocMayPlural,
        assocMayProperty, assocMust))
    }

    "normalize form associations" in new CosmosContext
    {
      expectNormalized(Seq(formRole2, assocHas), Seq(formRole2, assocMust))
    }

    "preserve entity existence" in new CosmosContext
    {
      expectPreserved(Seq(
        entityExists, entityQualifiedExists, personExists))
    }

    "preserve named entity existence" in new CosmosContext
    {
      expectPreserved(Seq(namedEntityExists))
    }

    "preserve entity state" in new CosmosContext
    {
      expectPreserved(Seq(
        stateProperty, entityExists, entityState))
    }

    "preserve entity associations" in new CosmosContext
    {
      expectPreserved(Seq(
        formRole, assocMay,
        personExists, personExists2, personAssoc))
    }

    "normalize entity associations" in new CosmosContext
    {
      expectNormalized(
        Seq(
          formRole, assocMay,
          personExists, personExists2, personAssocFlipped),
        Seq(
          formRole, assocMay,
          personExists, personExists2, personAssoc))
    }

    "preserve inverse associations" in new CosmosContext
    {
      expectPreserved(Seq(
        mentorRole, apprenticeRole,
        mentorApprentice, apprenticeMentor,
        assocInverse1, assocInverse2))
    }

    "normalize inverse associations" in new CosmosContext
    {
      expectNormalized(
        Seq(
          mentorRole, apprenticeRole,
          apprenticeMentor, assocInverse1),
        Seq(
          mentorRole, apprenticeRole,
          mentorApprentice, apprenticeMentor,
          assocInverse1, assocInverse2))
    }

    "normalize inverse associations with implied roles" in new CosmosContext
    {
      expectNormalized(
        Seq(assocInverse1),
        Seq(mentorRole, mentorApprentice, apprenticeMentors, assocInverse1))
    }

    "normalize associations with multiple roles" in new CosmosContext
    {
      expectNormalized(
        Seq(children),
        Seq(childrenSons, childrenDaughters))
    }

    "normalize inverse associations with multiple roles" in new CosmosContext
    {
      expectNormalized(
        Seq(nephew),
        Seq(nephewMan, nephewUncles, nephewAunts, uncleNephews,
          auntNephews, manUncleNephew, manAuntNephew))
    }

    "preserve triggers" in new CosmosContext
    {
      expectPreserved(Seq(moveTrigger))
    }

    "recite primordial beliefs" in
    {
      val cosmos = new SpcCosmos
      SpcPrimordial.initCosmos(cosmos)
      val creed = new SpcCreed(cosmos, true)
      val printer = new SilSentencePrinter
      val beliefStrings = creed.allBeliefs.map(s => printer.print(s) + "\n")
      beliefStrings.map(s => SprUtils.capitalize(s)) must
        containTheSameElementsAs(primordial.map(_ + "\n"))
    }
  }
}
