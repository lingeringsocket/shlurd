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
import com.lingeringsocket.shlurd.mind._

import ShlurdExceptionCode._

class SpcAssertionSpec extends SpcProcessingSpecification
{
  private def fiatForm(form : String, hypernym : String = "object") =
    s"a $form is a kind of $hypernym"

  private def fiatExistence(form : String) =
    s"there is a $form"

  private def fiatState(form : String, state : String) =
    s"the $form is $state"

  private val formToaster = "toaster"
  private val formSlice = "slice"
  private val formPumpernickel = "pumpernickel"
  private val formRye = "rye"

  private val stateEmpty = "empty"
  private val stateCold = "cold"

  private val fiatToasterStates =
    "a toaster's state may be empty, toasting, or done"

  private val fiatSliceStates =
    "a slice may be cold, toasted, or burnt"

  private val abilityPersonCanPut =
    "a person can put a slice into a toaster"

  private val abilityPersonCannotPut =
    "A person can not put a slice into a toaster."

  private val abilityClockCanTick =
    "a clock can tick"

  private val abilityToasterCanGlow =
    "a toaster can glow"

  private val actionClockTicks =
    "the clock ticks"

  private val constraintToasterMustBeEmpty =
    "before a person puts a slice into a toaster, " +
      "the toaster must be empty"

  private val triggerToasterActivation =
    "whenever a person puts a slice into a toaster, " +
      "subsequently the toaster is toasting"

  private val actionToasterCooks =
    "the toaster cooks"

  private val triggerClockToasterCooks =
    "when a clock ticks, the toaster cooks"

  private val conditionalClockToasterCooks =
    "when a clock ticks, the toaster might not be toasting; " +
      "otherwise the toaster cooks"

  private val constraintToasterMustBeToasting =
    "before the toaster cooks, " +
      "the toaster must be toasting"

  private val triggerToasterCompletion =
    "after the toaster cooks, " +
      "the toaster is done"

  private val failedPrereqToasterNotEmpty =
    "But the toaster is not empty."

  private val failedPrereqToasterNotToasting =
    "But the toaster is not toasting."

  private val fiatWallace =
    "Wallace is a person"

  private val fiatGromit =
    "Gromit is a person"

  private val actionWallacePutsPumpernickel =
    "Wallace puts the pumpernickel into the toaster"

  private val actionWallacePutsRye =
    "Wallace puts the rye into the toaster"

  private val actionGlow =
    "the toaster glows"

  private val errorInterpretation =
    "I'm not sure how to interpret that."

  private def errorInvalid(belief : String) =
    s"I am unable to validate the belief that $belief."

  private val queryState =
    "what is the toaster's state"

  private val queryGlow =
    "does the toaster glow"

  private val responseStateEmpty =
    "Empty."

  private val responseStateToasting =
    "Toasting."

  private val responseStateComplete =
    "Done."

  private val responseYes =
    "Yes."

  private val responseNo =
    "No."

  private val responseDunno =
    "I don't know."

  private val equivalenceGlowToasting =
    "if a toaster glows, " +
      "then equivalently the toaster is toasting"
  private val equivalenceGlowToastingConverse =
    "if a toaster is toasting, " +
      "then equivalently the toaster glows"

  trait AssertionContext extends ProcessingContext
  {
    protected def verify(input : String, response : String) =
    {
      process(
        input,
        ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams(
          verbosity = RESPONSE_TERSE,
          reportExceptionCodes = true)
      ) must be equalTo response
    }

    protected def verifyOK(input : String) =
    {
      verify(input, "OK.")
    }

    protected def defineToaster()
    {
      verifyOK(fiatForm(formToaster))
      verifyOK(fiatToasterStates)
      verifyOK(fiatExistence(formToaster))
      verifyOK(abilityToasterCanGlow)
    }

    protected def defineSlice()
    {
      verifyOK(fiatForm(formSlice))
      verifyOK(fiatSliceStates)
    }

    protected def definePumpernickel()
    {
      verifyOK(fiatForm(formPumpernickel, formSlice))
      verifyOK(fiatExistence(formPumpernickel))
      verifyOK(fiatState(formPumpernickel, stateCold))
    }

    protected def defineRye()
    {
      verifyOK(fiatForm(formRye, formSlice))
      verifyOK(fiatExistence(formRye))
      verifyOK(fiatState(formRye, stateCold))
    }

    protected def defineToasterSlice()
    {
      SpcPrimordial.initCosmos(cosmos)
      loadBeliefs("/ontologies/containment.txt")

      defineToaster
      defineSlice
      definePumpernickel
      defineRye

      verifyOK(constraintToasterMustBeEmpty)
      verifyOK(constraintToasterMustBeToasting)

      verifyOK(triggerToasterCompletion)
    }

    protected def defineWallace()
    {
      verifyOK(fiatWallace)
    }

    protected def defineGromit()
    {
      verifyOK(fiatGromit)
    }

    protected def defineClock()
    {
      verifyOK(fiatForm("clock"))
      verifyOK(fiatExistence("clock"))
      verifyOK(abilityClockCanTick)
    }

    protected def verifyInvalid(
      assertion : String,
      code : ShlurdExceptionCode) =
    {
      val message = errorInvalid(assertion)
      verifyError(
        assertion,
        message,
        code)
    }

    protected def verifyError(
      sentence : String,
      message : String,
      code : ShlurdExceptionCode) =
    {
      val expected = s"$message\n\nFor more information see ${code.getUrl}"
      verify(
        sentence,
        expected)
    }

    protected def processWithClock(equivalenceGlow : String)
    {
      defineToasterSlice
      defineClock
      defineWallace

      verifyOK(equivalenceGlow)

      // toaster state is initially unknown, so we can't
      // put anything in
      verify(actionWallacePutsPumpernickel, failedPrereqToasterNotEmpty)
      verify(queryGlow, responseDunno)

      // now make the toaster empty by fiat
      verifyOK(fiatState(formToaster, stateEmpty))

      verify(queryGlow, responseNo)

      // the put verb is not yet enabled
      verify(actionWallacePutsPumpernickel, errorInterpretation)

      // declare ability to put
      verifyOK(abilityPersonCanPut)

      // now it should be fine, but it doesn't do anything useful
      verifyOK(actionWallacePutsPumpernickel)
      verify(queryState, responseStateEmpty)

      // use a conditional trigger to avoid errors
      verifyOK(conditionalClockToasterCooks)
      verifyOK(actionClockTicks)

      // now declare an actual effect of putting in a slice
      verifyOK(triggerToasterActivation)
      verifyOK(actionWallacePutsPumpernickel)
      verify(queryState, responseStateToasting)
      verify(queryGlow, responseYes)

      // now that the toaster is actually full, should
      // not be able to put anything else in
      verify(actionWallacePutsRye, failedPrereqToasterNotEmpty)

      // tick tock
      verifyOK(actionClockTicks)
      verify(queryState, responseStateComplete)
      verify(queryGlow, responseNo)

      // switch it back on by fiat
      verifyOK(actionGlow)
      verify(queryState, responseStateToasting)
      verify(queryGlow, responseYes)
    }

    def verifyEquivalenceStandardization(
      equiv : String, imp1 : String, imp2 : String) =
    {
      val responder = new SpcResponder(mind)
      val before = responder.getBiconditionalImplications
      verifyOK(equiv)
      val after = responder.getBiconditionalImplications
      after.size must be equalTo (before.size + 2)
      val imps = after.takeRight(2).map {
        case (cs, placeholderMap) => {
          responder.sentencePrinter.print(cs)
        }
      }
      imps.size must be equalTo 2
      imps.head must be equalTo imp1
      imps.last must be equalTo imp2
    }

    def verifyTriggerStandardization(
      trigger : String, imp : String) =
    {
      val responder = new SpcResponder(mind)
      val before = cosmos.getTriggers
      verifyOK(trigger)
      val after = cosmos.getTriggers
      after.size must be equalTo (before.size + 1)
      val imps = responder.getTriggerImplications(after.last).map {
        case (cs, placeholderMap) => {
          responder.sentencePrinter.print(cs)
        }
      }
      imps.size must be equalTo 1
      imps.head must be equalTo imp
    }
  }

  "SpcAssertion" should
  {
    "process with clock using glow equivalence" in new AssertionContext
    {
      processWithClock(equivalenceGlowToasting)
    }

    "process with clock using glow equivalence converse" in new AssertionContext
    {
      processWithClock(equivalenceGlowToastingConverse)
    }

    "process without clock" in new AssertionContext
    {
      defineToasterSlice
      defineWallace

      verifyOK(fiatState(formToaster, stateEmpty))
      verifyOK(abilityPersonCanPut)
      verifyOK(triggerToasterActivation)

      verifyOK(triggerToasterCompletion)
      verify(actionToasterCooks, failedPrereqToasterNotToasting)

      verifyOK(actionWallacePutsPumpernickel)
      verify(queryState, responseStateToasting)
      verifyOK(actionToasterCooks)
      verify(queryState, responseStateComplete)
    }

    "propagate errors upward" in  new AssertionContext
    {
      defineToasterSlice
      defineClock
      defineWallace

      verifyOK(fiatState(formToaster, stateEmpty))

      verifyOK(triggerClockToasterCooks)
      verify(actionClockTicks, failedPrereqToasterNotToasting)
    }

    "allow genitive property as subject" in new AssertionContext
    {
      defineToasterSlice
      defineWallace

      verifyOK(fiatState(formToaster, stateEmpty))
      verifyOK(abilityPersonCanPut)
      verifyOK("whenever a person puts a slice into a toaster, " +
        "subsequently the toaster's state is toasting")
      verifyOK(actionWallacePutsPumpernickel)
      verify(queryState, responseStateToasting)
    }

    "prevent disabled actions" in new AssertionContext
    {
      defineToasterSlice
      defineWallace

      // declare disability
      verifyOK(abilityPersonCannotPut)

      verifyOK(fiatState(formToaster, stateEmpty))
      verify(actionWallacePutsPumpernickel, abilityPersonCannotPut)
    }

    "prevent invalid variables in assertions" in new AssertionContext
    {
      defineToasterSlice

      // FIXME error should include the original belief
      verifyError(
        "if a slice becomes cold, the slick spreads",
        "Sorry, I don't know about any 'slick'.",
        UnknownForm)
      verifyError(
        "if a slice becomes burnt, then the first slice becomes cold",
        "Sorry, when you say 'first slice', I don't know which you mean.",
        MisqualifiedNoun)
      verifyError(
        "if a slice becomes burnt, then the second slice becomes cold",
        "Sorry, when you say 'second slice', I don't know which you mean.",
        MisqualifiedNoun)
      verifyError(
        "if a toaster gives a slice to a person, " +
          "then the person becomes its owner",
        "Sorry, when you say 'it', it's ambiguous.",
        AmbiguousPronoun)
      verifyError(
        "if a slice becomes cold, then the slice might be burnt; " +
          "otherwise the pig becomes toasted",
        "Sorry, I don't know about any 'pig'.",
        UnknownForm)

      verifyInvalid(
        "if a pickle becomes cold, then the pickle splits",
        UnknownForm)
      verifyInvalid(
        "if a slice touches a slice, then the slice becomes cold",
        AssertionInvalidVariable)
      verifyInvalid(
        "if another slice becomes burnt, then the slice becomes cold",
        AssertionInvalidVariable)
      verifyInvalid(
        "if another slice touches a slice, then the slice becomes cold",
        AssertionInvalidVariable)
      verifyInvalid(
        "if a slice extends from another slice to another slice, " +
          "then the slice becomes cold",
        AssertionInvalidVariable)
    }

    "prevent invalid assertions" in new AssertionContext
    {
      defineToasterSlice
      defineClock

      verifyError(
        "before a person puts a slice into a toaster, " +
          "subsequently the toaster's state must be toasting",
        errorInvalid("before a person puts a slice into a toaster, " +
          "then the toaster's state must be toasting subsequently"),
        AssertionModifiersIncompatible)
      verifyError("if a slice becomes cold, " +
        "otherwise also the toaster becomes done",
        errorInvalid("if a slice becomes cold, " +
          "then the toaster becomes done otherwise also"),
        AssertionModifierSequence)
      verifyInvalid("if a slice becomes cold, " +
        "equivalently the toaster must be done",
        AssertionModalProhibited)
      verifyInvalid("before a slice becomes cold, " +
        "then the toaster becomes done",
        ConsequentConstraintExpected)
      verifyInvalid("before a slice becomes cold, " +
        "equivalently the toaster must be done",
        AssertionModalProhibited)
      verifyInvalid("after a slice becomes cold, " +
        "equivalently the toaster becomes done",
        EquivalenceIfExpected)
      verifyInvalid("whenever a slice is cold, " +
        "equivalently the toaster is done",
        EquivalenceIfExpected)
      verifyInvalid("after a slice becomes cold, " +
        "then the toaster must be done",
        PostConstraintNotYetImplemented)
    }

    "prevent invalid inverse associations" in new AssertionContext
    {
      verifyOK(fiatForm("map-place"))
      verifyInvalid("if a map-place is another map-place's map-neighbor, " +
        "equivalently the first map-place is " +
        "the second map-place's map-neighbor",
        AssertionInvalidAssociation)
      verifyInvalid("if a map-place is a map-place's map-neighbor, " +
        "equivalently the second map-place is " +
        "the first map-place's map-neighbor",
        AssertionInvalidVariable)
      verifyInvalid("if another map-place is a map-place's map-neighbor, " +
        "equivalently the second map-place is " +
        "the first map-place's map-neighbor",
        AssertionInvalidVariable)
    }

    "prevent runaway triggers" in new AssertionContext
    {
      defineToasterSlice
      defineWallace

      verifyOK(fiatForm("heel", "slice"))
      verifyOK("a slice's predecessor must be a slice")
      verifyOK("a slice may have a predecessor")
      verifyOK("after a person cuts a slice, " +
        "the slice has a predecessor; " +
        "also the slice's predecessor becomes a heel; " +
        "also the person cuts the slice's predecessor")

      verifyError(
        "Wallace cuts the pumpernickel",
        "Trigger limit exceeded.",
        TriggerLimit)
    }

    "map genitives in equivalences" in new AssertionContext
    {
      defineToasterSlice
      defineWallace
      defineGromit
      verifyOK("a slice's devourer must be a person")
      verifyOK("a slice may have a devourer")
      verifyOK("if a person eats a slice, " +
        "equivalently the person is the slice's devourer")
      verifyOK("Wallace eats the pumpernickel")
      verifyOK("Gromit eats the rye")
      verify("which slice's devourer is Wallace",
        "The pumpernickel's devourer.")
      verify("what does Wallace eat", "The pumpernickel.")
    }

    "support pronouns in assertions" in new AssertionContext
    {
      defineToasterSlice

      verifyOK("if a slice explodes, then it becomes burnt")
      verifyOK("the rye explodes")
      verify("is the rye burnt", responseYes)
    }

    "preserve singletons in equivalences" in new AssertionContext
    {
      defineToasterSlice

      verifyOK(fiatForm("receptacle"))
      verifyOK(fiatForm("freezer", "receptacle"))
      verifyOK(fiatExistence("freezer"))
      verifyOK(fiatForm("trash", "receptacle"))
      verifyOK(fiatExistence("trash"))

      verifyOK("if a slice is cold, " +
        "then equivalently it belongs in the freezer")
      verifyOK("if a slice is burnt, " +
        "then equivalently it belongs in the trash")

      verifyOK("a slice can belong in a receptacle")

      verifyOK("the pumpernickel belongs in the freezer")
      verifyOK("the rye belongs in the trash")
      verify("is the pumpernickel cold", responseYes)
      verify("is the rye burnt", responseYes)
    }

    "unify genitives" in new AssertionContext
    {
      skipped("not working yet")

      verifyOK("A balloon's state must be empty, full, or broken.")
      verifyOK("A person's state must be energetic or tired.")
      verifyOK("A balloon's owner must be a person.")
      verifyOK("A balloon must have an owner.")
      verifyOK("If a balloon's owner is tired, " +
        "consequently the balloon is broken.")
      verifyOK("Pinkie is a person.")
      verifyOK("There is a red balloon.")
      verifyOK("Pinkie is the red balloon's owner.")
      verifyOK("Pinkie is tired.")
      verify("Is the red balloon broken?", "Yes.")
    }

    "map pronouns in implications" in new AssertionContext
    {
      SpcPrimordial.initCosmos(cosmos)
      verifyOK("A person is a kind of spc-someone.")
      verifyOK("A person must be happy or sad.")
      verifyOK("Percival is a person.")
      verifyOK("The coatroom is a map-place.")
      verifyOK("After a person relocates to a map-place, " +
        "the person surveys it.")
      verifyOK("After a person surveys a map-place, " +
        "the person is sad.")
      verifyOK("Percival relocates to the coatroom.")
      verify("Is Percival sad?", "Yes.")
    }

    "map pronouns in queries" in new AssertionContext
    {
      SpcPrimordial.initCosmos(cosmos)
      verifyOK("A person is a kind of spc-someone.")
      verifyOK("A balloon's owner must be a person.")
      verifyOK("A balloon must have an owner.")
      verifyOK("Pinkie is a person.")
      verifyOK("There is a balloon.")
      verifyOK("Pinkie is the balloon's owner.")
      verifyOK("If a person holds a balloon, " +
        "equivalently the person is its owner.")
      verify("Who holds the balloon?", "Pinkie.")
    }

    "map ordinal placeholders" in new AssertionContext
    {
      verifyOK("A person may be proud, happy, or silly.")
      verifyOK("If a person introduces another person to a third person, " +
        "then the first person becomes proud; " +
        "also the second person becomes happy; " +
        "also the third person becomes happy.")
      verifyOK("Bob, Ted, and Alice are persons.");
      verifyOK("Bob introduces Ted to Alice.");
      verify("is Bob proud", responseYes)
      verify("is Ted happy", responseYes)
      verify("is Alice happy", responseYes)
    }

    "standardize variables" in new AssertionContext
    {
      verifyOK("An object may be hot or cold.")
      verifyOK("There is a fire.")
      verifyTriggerStandardization(
        "if an object becomes hot, then the fire burns it and its neighbor.",
        "if an object becomes hot, " +
          "then the fire burns the object and the object's neighbor.")
      verifyEquivalenceStandardization(
        "if an object is hot, equivalently it is cold.",
        "if an object is hot, equivalently the object is cold.",
        "if an object is cold, equivalently the object is hot.")
      verifyEquivalenceStandardization(
        "if one object is shoving another object, " +
          "equivalently the former is pushing the latter.",
        "if an object is shoving another object, " +
          "equivalently the object is pushing the second object.",
        "if an object is pushing a second object, " +
          "equivalently the object is shoving the second object.")
      verifyEquivalenceStandardization(
        "if an object is shoving another object, " +
          "equivalently the first object is pushing the other object.",
        "if an object is shoving another object, " +
          "equivalently the object is pushing the second object.",
        "if an object is pushing a second object, " +
          "equivalently the object is shoving the second object.")
      verifyEquivalenceStandardization(
        "if an object (the shover) is shoving another object (the shovee), " +
          "equivalently the shover is pushing the shovee.",
        "if an object is shoving another object, " +
          "equivalently the object is pushing the second object.",
        "if an object is pushing a second object, " +
          "equivalently the object is shoving the second object.")
    }
  }
}
