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

    protected val refriedCosmos = new SpcCosmos

    protected val creed = new SpcCreed(cosmos)

    protected val refriedCreed = new SpcCreed(refriedCosmos)

    protected def addBelief(input : String) =
    {
      val mind = new SpcMind(cosmos)
      val responder = new SpcResponder(mind)
      val sentence = responder.newParser(input).parseOne
      val resultCollector = SmcResultCollector[SpcEntity]()
      responder.resolveReferences(sentence, resultCollector)
      val beliefAccepter = SpcBeliefAccepter.forResponder(
        responder, SpcBeliefParams(), resultCollector)
      beliefAccepter.processBelief(sentence)
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
      val beliefStrings = creed.allBeliefs(printer).map(s => printer.print(s))
      beliefStrings.map(SprUtils.capitalize) must be equalTo expected
      beliefStrings.foreach(beliefString => {
        val refriedMind = new SpcMind(refriedCosmos)
        val refriedResponder =
          new SpcResponder(refriedMind)
        val sentence = refriedResponder.newParser(beliefString).parseOne
        val resultCollector = SmcResultCollector[SpcEntity]()
        refriedResponder.resolveReferences(sentence, resultCollector)
        val refriedBeliefAccepter =
          SpcBeliefAccepter.forResponder(
            refriedResponder, SpcBeliefParams(), resultCollector)
        val refriedBeliefs =
          refriedBeliefAccepter.recognizeBeliefs(sentence)
        refriedBeliefs.foreach(belief => {
          refriedBeliefAccepter.applyBelief(belief)
        })
      })
      val refriedStrings = refriedCreed.allBeliefs(printer).map(
        s => printer.print(s))
      refriedStrings.map(SprUtils.capitalize) must be equalTo expected
    }
  }

  private val stateMust = "A door must be open or closed."
  private val stateMay = "A window may be open or closed."
  private val stateMay2 = "A person may be smart or stupid."
  private val propertyStateEnum = "A parakeet's mood may be happy or sad."
  private val propertyQuotation = "A parakeet's phrase must be an spc-string."
  private val formTaxonomy = "A duck is a kind of a bird."
  private val formTaxonomy2 = "A monk is a kind of a person."
  private val formSynonym = "An automobile is the same as a car."
  private val formRole = "A person's mentor must be a person."
  private val formRole2 = "An animal's owner must be a person."
  private val formRole3 = "A person's pet must be an animal."
  private val animalOwners = "An animal may have owners."
  private val assocHas = "A dog has an owner."
  private val assocMust = "A dog must have one owner."
  private val assocMustPlural = "A shark must have teeth."
  private val assocMay = "A person may have one mentor."
  private val assocMayPlural = "A person may have pets."
  private val entityExists = "A parakeet exists."
  private val entityExists2 = "A door exists."
  private val entityPropertyState = "The parakeet's mood is happy."
  private val entityState = "The parakeet is happy."
  private val entityState2 = "The door is closed."
  private val entityQuotation =
    "The parakeet's phrase is \"Polly wanna crack 'er?\"."
  private val namedEntityExists = "Rapunzel is a dog."
  private val entityQualifiedExists = "An angry cat exists."
  private val personExists = "Yoda is a person."
  private val personExists2 = "Luke is a person."
  private val personExists3 = "Yoda is a monk."
  private val personState = "Yoda is smart."
  private val personAssoc = "Yoda is Luke's mentor."
  private val personAssocFlipped = "Luke's mentor is Yoda."
  private val personAssocExists = "Luke has a mentor."
  private val personAssocQualifiedExists = "Luke has a smart mentor."
  private val personAssocForm = "Luke's mentor is a monk."
  private val mentorRole = "A monk's mentor must be a monk."
  private val apprenticeRole = "A monk's apprentice must be a monk."
  private val monkApprentice = "A monk may have apprentices."
  private val assocInverse1 = "If a monk is another monk's apprentice, " +
    "equivalently the second monk is the first monk's mentor."
  private val assocInverse2 = "If a monk is another monk's mentor, " +
    "equivalently the second monk is the first monk's apprentice."
  private val assocInverse3 = "If a monk is another monk's apprentice, " +
    "equivalently the other monk is the first monk's mentor."
  private val assocInverse4 = "If a monk is another monk's apprentice, " +
    "equivalently the latter monk is the former monk's mentor."
  private val assocInverse5 = "If a monk is another monk's apprentice, " +
    "equivalently the latter is the former's mentor."
  private val monkMentor = "A monk may have one mentor."
  private val monkMentors = "A monk may have mentors."
  private val womanNephews = "A woman may have nephews."
  private val manNephews = "A man may have nephews."
  private val manAunts = "A man may have aunts."
  private val manAuntNephew = "If a woman is a man's aunt, " +
    "equivalently the man is the woman's nephew."
  private val manUncles = "A man may have uncles."
  private val manUncleNephew = "If a man is another man's uncle, " +
    "equivalently the second man is the first man's nephew."
  private val manNephewUncle = "If a man is another man's nephew, " +
    "equivalently the second man is the first man's uncle."
  private val womanNephewAunt = "If a man is a woman's nephew, " +
    "equivalently the woman is the man's aunt."
  private val manNephewMan = "A man's nephew must be a man."
  private val womanNephewMan = "A woman's nephew must be a man."
  private val uncleMan = "A man's uncle must be a man."
  private val auntWoman = "A man's aunt must be a woman."
  private val children = "A person may have sons or daughters."
  private val childrenSons = "A person may have sons."
  private val childrenDaughters = "A person may have daughters."
  private val moveTrigger = "If an spc-object moves to a location, " +
    "then the location becomes the spc-object's spc-container."
  private val positiveConstraintTrigger = "If a team wins a trophy, " +
    "then the team must be awesome."
  private val negativeConstraintTrigger = "If a team wins a trophy, " +
    "then the team must not drink."
  private val positiveTestTrigger = "If a team wins a medal, " +
    "then the team might be awesome."
  private val negativeTestTrigger = "If a team wins a medal, " +
    "then the team might not drink."
  private val alternativeTrigger = "If a person eats a candy, " +
    "then the candy must be small; the person chokes otherwise."
  private val additionalTrigger = "If a person eats a candy, " +
    "then the candy becomes small; the candy crunches also."
  private val positiveAssertion = "A person can kill a thief."
  private val negativeAssertion = "A person can not kill a thief."
  private val wordRule = "\"Happy\" may be a proper noun."
  private val conjunctiveBelief =
    "A duck is a kind of a bird and a monk is a kind of a person."

  private val primordial = Seq(
    "An spc-class is the same as an spc-form.",
    "An spc-entity's spc-type must be an spc-form.",
    "An spc-form's spc-realization must be an spc-entity.",
    "An spc-form's spc-attribute must be an spc-property.",
    "An spc-property's spc-attributee must be an spc-form.",
    "An spc-ideal's spc-superclass must be an spc-ideal.",
    "An spc-ideal's spc-subclass must be an spc-ideal.",
    "An spc-object's spc-container must be an spc-object.",
    "An spc-object's spc-contained-object must be an spc-object.",
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
    "An spc-entity's gender may be masculine or feminine.",
    "An spc-someone is a kind of an spc-object.",
    "An spc-object is a kind of an spc-entity.",
    "An spc-object may have spc-contained-objects.",
    "An spc-object must have one spc-container.",
    "If an spc-form is an spc-entity's spc-type, " +
      "equivalently the spc-entity is the spc-form's spc-realization.",
    "If an spc-entity is an spc-form's spc-realization, " +
      "equivalently the spc-form is the spc-entity's spc-type.",
    "SPC-Form-spc-entity is an spc-form.",
    "SPC-Form-spc-ideal is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Role-spc-form-spc-realization is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-property is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-object is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-entity's spc-type.",
    "SPC-Form-spc-ideal is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-ideal's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Form-spc-role is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Role-spc-ideal-spc-superclass is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Role-spc-ideal-spc-subclass is SPC-Form-spc-ideal's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-ideal's spc-type.",
    "SPC-Form-spc-form is an spc-form.",
    "SPC-Form-spc-ideal is SPC-Form-spc-form's spc-superclass.",
    "SPC-Role-spc-entity-spc-type is SPC-Form-spc-form's spc-subclass.",
    "SPC-Role-spc-property-spc-attributee is SPC-Form-spc-form's spc-subclass.",
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
    "SPC-Role-spc-entity-spc-type is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-form-spc-realization is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-form-spc-attribute is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-ideal-spc-superclass is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-ideal-spc-subclass is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-object-spc-container is SPC-Form-spc-role's spc-realization.",
    "SPC-Role-spc-entity-spc-type is an spc-role.",
    "SPC-Form-spc-form is SPC-Role-spc-entity-spc-type's spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-entity-spc-type's spc-type.",
    "SPC-Role-spc-form-spc-realization is an spc-role.",
    "SPC-Form-spc-role is SPC-Role-spc-form-spc-realization's spc-type.",
    "SPC-Form-spc-property is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-property's spc-superclass.",
    "SPC-Role-spc-form-spc-attribute is SPC-Form-spc-property's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-property's spc-type.",
    "SPC-Role-spc-form-spc-attribute is an spc-role.",
    "SPC-Form-spc-role is SPC-Role-spc-form-spc-attribute's spc-type.",
    "SPC-Role-spc-property-spc-attributee is an spc-role.",
    "SPC-Role-spc-ideal-spc-superclass is an spc-role.",
    "SPC-Form-spc-ideal is SPC-Role-spc-ideal-spc-superclass' spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-ideal-spc-superclass' spc-type.",
    "SPC-Role-spc-ideal-spc-subclass is an spc-role.",
    "SPC-Form-spc-ideal is SPC-Role-spc-ideal-spc-subclass' spc-superclass.",
    "SPC-Form-spc-role is SPC-Role-spc-ideal-spc-subclass' spc-type.",
    "SPC-Form-spc-someone is an spc-form.",
    "SPC-Form-spc-object is SPC-Form-spc-someone's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-someone's spc-type.",
    "SPC-Form-spc-object is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-object's spc-superclass.",
    "SPC-Form-spc-someone is SPC-Form-spc-object's spc-subclass.",
    "SPC-Role-spc-object-spc-container is SPC-Form-spc-object's spc-subclass.",
    "SPC-Form-spc-form is SPC-Form-spc-object's spc-type.",
    "SPC-Role-spc-object-spc-container is an spc-role.",
    "SPC-Form-spc-role is SPC-Role-spc-object-spc-container's spc-type.",
    "SPC-Role-spc-object-spc-contained-object is an spc-role.",
    "SPC-Form-spc-role is SPC-Role-spc-object-spc-contained-object's spc-type.",
    "SPC-Property-spc-entity-gender is an spc-property.",
    "SPC-Form-spc-property is SPC-Property-spc-entity-gender's spc-type.",
    "SPC-Property-spc-entity-gender is SPC-Form-spc-entity's spc-attribute.",
    "SPC-Form-spc-entity is SPC-Property-spc-entity-gender's spc-attributee.",
    "An spc-value's spc-valued-property must be an spc-property.",
    "An spc-property's spc-property-value must be an spc-value.",
    "An spc-property may have spc-property-values.",
    "An spc-value is a kind of an spc-entity.",
    "An spc-value must have one spc-valued-property.",
    "SPC-Form-spc-value is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-value is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-value is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-value's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-value's spc-type.",
    "SPC-Value-spc-entity-gender-masculine is " +
      "SPC-Form-spc-value's spc-realization.",
    "SPC-Value-spc-entity-gender-feminine is " +
      "SPC-Form-spc-value's spc-realization.",
    "SPC-Value-spc-entity-gender-masculine is " +
      "SPC-Property-spc-entity-gender's spc-property-value.",
    "SPC-Value-spc-entity-gender-feminine is " +
      "SPC-Property-spc-entity-gender's spc-property-value.",
    "SPC-Value-spc-entity-gender-masculine is an spc-value.",
    "SPC-Property-spc-entity-gender is " +
      "SPC-Value-spc-entity-gender-masculine's spc-valued-property.",
    "SPC-Form-spc-value is SPC-Value-spc-entity-gender-masculine's spc-type.",
    "SPC-Value-spc-entity-gender-feminine is an spc-value.",
    "SPC-Property-spc-entity-gender is " +
      "SPC-Value-spc-entity-gender-feminine's spc-valued-property.",
    "SPC-Form-spc-value is SPC-Value-spc-entity-gender-feminine's spc-type.",
    "An spc-open-enum is a kind of an spc-entity.",
    "An spc-closed-enum is a kind of an spc-entity.",
    "An spc-string is a kind of an spc-entity.",
    "SPC-Form-spc-open-enum is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-closed-enum is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-string is SPC-Form-spc-entity's spc-subclass.",
    "SPC-Form-spc-open-enum is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-closed-enum is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-string is SPC-Form-spc-form's spc-realization.",
    "SPC-Form-spc-open-enum is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-open-enum's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-open-enum's spc-type.",
    "SPC-Form-spc-closed-enum is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-closed-enum's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-closed-enum's spc-type.",
    "SPC-Form-spc-string is an spc-form.",
    "SPC-Form-spc-entity is SPC-Form-spc-string's spc-superclass.",
    "SPC-Form-spc-form is SPC-Form-spc-string's spc-type."
  )

  "SpcCreed" should
  {
    "preserve states" in new CosmosContext
    {
      expectPreserved(Seq(stateMust, stateMay))
    }

    "preserve state property" in new CosmosContext
    {
      expectPreserved(Seq(propertyStateEnum))
    }

    "preserve quotation property" in new CosmosContext
    {
      expectPreserved(Seq(propertyQuotation))
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
        animalOwners, assocMust, assocMustPlural))
    }

    "normalize form associations" in new CosmosContext
    {
      expectNormalized(
        Seq(formRole2, assocHas),
        Seq(formRole2, animalOwners, assocMust))
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
      expectPreserved(
        Seq(stateMust, entityExists2, entityState2))
    }

    "normalize entity state" in new CosmosContext
    {
      expectNormalized(
        Seq(propertyStateEnum, entityExists, entityState),
        Seq(propertyStateEnum, entityExists, entityPropertyState))
    }

    "preserve entity property state" in new CosmosContext
    {
      expectPreserved(Seq(
        propertyStateEnum, entityExists, entityPropertyState))
    }

    "preserve entity quotation" in new CosmosContext
    {
      expectPreserved(Seq(
        propertyQuotation, entityExists, entityQuotation))
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

    "normalize association existence" in new CosmosContext
    {
      expectNormalized(
        Seq(
          formRole, assocMay, formTaxonomy2, personExists2, personExists3,
          personAssocExists, personAssocForm, personAssoc),
        Seq(
          formRole, assocMay, formTaxonomy2, personExists2, personAssoc,
          personExists3)
      )
    }

    "normalize qualified association existence" in new CosmosContext
    {
      expectNormalized(
        Seq(
          formRole, stateMay2, assocMay, formTaxonomy2,
          personExists2, personExists3,
          personAssocQualifiedExists, personAssocForm, personAssoc),
        Seq(
          formRole, stateMay2, assocMay, formTaxonomy2,
          personExists2, personAssoc, personExists3,
          personState)
      )
    }

    "normalize association form" in new CosmosContext
    {
      skipped("not working yet")
      expectNormalized(
        Seq(
          formRole, assocMay, formTaxonomy2, personExists2, personExists3,
          personAssocForm, personAssoc),
        Seq(
          formRole, assocMay, formTaxonomy2, personExists2, personAssoc,
          personExists3)
      )
    }

    "preserve inverse associations" in new CosmosContext
    {
      expectPreserved(Seq(
        mentorRole, apprenticeRole,
        monkMentor, monkApprentice,
        assocInverse1, assocInverse2))
    }

    "normalize inverse associations" in new CosmosContext
    {
      val expected = Seq(
        mentorRole, apprenticeRole,
        monkMentor, monkApprentice,
        assocInverse1, assocInverse2
      )
      expectNormalized(
        Seq(
          mentorRole, apprenticeRole,
          monkMentor, assocInverse1),
        expected)
      expectNormalized(
        Seq(
          mentorRole, apprenticeRole,
          monkMentor, assocInverse3),
        expected)
      expectNormalized(
        Seq(
          mentorRole, apprenticeRole,
          monkMentor, assocInverse4),
        expected)
      expectNormalized(
        Seq(
          mentorRole, apprenticeRole,
          monkMentor, assocInverse5),
        expected)
    }

    "normalize inverse associations with implied roles" in new CosmosContext
    {
      expectNormalized(
        Seq(apprenticeRole, assocInverse1),
        Seq(apprenticeRole, mentorRole, monkApprentice, monkMentors,
          assocInverse1, assocInverse2))
    }

    "normalize associations with multiple roles" in new CosmosContext
    {
      expectNormalized(
        Seq(children),
        Seq(childrenSons, childrenDaughters))
    }

    "preserve inverse associations with multiple roles" in new CosmosContext
    {
      expectPreserved(
        Seq(manNephewMan, uncleMan, auntWoman, womanNephewMan,
          manNephews, manUncles, manAunts,
          womanNephews, manUncleNephew, manNephewUncle,
          manAuntNephew, womanNephewAunt))
    }

    "preserve triggers" in new CosmosContext
    {
      expectPreserved(Seq(moveTrigger, positiveConstraintTrigger,
        negativeConstraintTrigger, positiveTestTrigger,
        negativeTestTrigger, alternativeTrigger, additionalTrigger))
    }

    "preserve assertions" in new CosmosContext
    {
      expectPreserved(Seq(positiveAssertion, negativeAssertion, wordRule))
    }

    "preserve conjunctive beliefs" in new CosmosContext
    {
      expectNormalized(
        Seq(conjunctiveBelief),
        Seq(formTaxonomy, formTaxonomy2))
    }

    "recite primordial beliefs" in
    {
      val cosmos = new SpcCosmos
      SpcPrimordial.initCosmos(cosmos)
      val creed = new SpcCreed(cosmos, true)
      val printer = new SilSentencePrinter
      val beliefStrings = creed.allBeliefs(printer).map(
        s => printer.print(s) + "\n")
      beliefStrings.size must be equalTo 160
      beliefStrings.map(s => SprUtils.capitalize(s)) must
        contain(allOf(primordial.map(_ + "\n"):_*))
    }
  }
}
