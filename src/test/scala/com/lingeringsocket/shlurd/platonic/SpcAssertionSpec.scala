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
  private val stateToasting = "toasting"
  private val stateCold = "cold"
  private val stateBurnt = "burnt"

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
    s"The belief that $belief is not valid in the given context."

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
        SmcResponseParams(verbosity = RESPONSE_TERSE)
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
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }

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

    protected def verifyInvalid(assertion : String) =
    {
      verify(assertion, errorInvalid(assertion))
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

    "prevent invalid assertions" in new AssertionContext
    {
      defineToasterSlice
      defineClock

      verifyInvalid("if a slice becomes cold, " +
        "equivalently the toaster must be done")
      verifyInvalid("before a slice becomes cold, " +
        "then the toaster becomes done")
      verifyInvalid("before a slice is cold, " +
        "equivalently the toaster is done")
      verifyInvalid("after a slice is cold, " +
        "equivalently the toaster is done")
      verifyInvalid("whenever a slice is cold, " +
        "equivalently the toaster is done")
      verifyInvalid("after a slice becomes cold, " +
        "then the toaster must be done")
      verify(
        "before a slice becomes cold, " +
          "subsequently the toaster must be done",
        errorInvalid(
          "before a slice becomes cold, " +
            "then the toaster must be done subsequently"))
    }

    "prevent invalid inverse associations" in new AssertionContext
    {
      verifyInvalid("if a map-place is another map-place's map-neighbor, " +
        "equivalently the first map-place is " +
        "the second map-place's map-neighbor")
      verifyInvalid("if a map-place is a map-place's map-neighbor, " +
        "equivalently the second map-place is " +
        "the first map-place's map-neighbor")
      verifyInvalid("if another map-place is a map-place's map-neighbor, " +
        "equivalently the second map-place is " +
        "the first map-place's map-neighbor")
      verifyInvalid("if a map-place is another map-place's map-neighbor, " +
        "equivalently the second map-place is " +
        "the map-place's map-neighbor")
      verifyInvalid("if a map-place is another map-place's map-neighbor, " +
        "then the second map-place is " +
        "the first map-place's map-neighbor")
    }

    "prevent runaway triggers" in new AssertionContext
    {
      defineToasterSlice
      defineWallace

      verifyOK(fiatForm("heel", "slice"))
      verifyOK("a predecessor must be a slice")
      verifyOK("a slice may have a predecessor")
      verifyOK("after a person cuts a slice, " +
        "the slice has a predecessor; " +
        "also the slice's predecessor becomes a heel; " +
        "also the person cuts the slice's predecessor")

      verify(
        "Wallace cuts the pumpernickel",
        "Trigger limit exceeded.")
    }

    "map genitives in equivalences" in new AssertionContext
    {
      defineToasterSlice
      defineWallace
      defineGromit
      verifyOK("a devourer must be a person")
      verifyOK("a slice may have a devourer")
      verifyOK("if a person eats a slice, " +
        "equivalently the person is the slice's devourer")
      verifyOK("Wallace eats the pumpernickel")
      verifyOK("Gromit eats the rye")
      verify("which slice's devourer is Wallace",
        "The pumpernickel's devourer.")
      verify("what does Wallace eat", "The pumpernickel.")
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
        "then equivalently the slice belongs in the freezer")
      verifyOK("if a slice is burnt, " +
        "then equivalently the slice belongs in the trash")

      verifyOK("a slice can belong in a receptacle")

      verifyOK("the pumpernickel belongs in the freezer")
      verifyOK("the rye belongs in the trash")
      verify("is the pumpernickel cold", responseYes)
      verify("is the rye burnt", responseYes)
    }
  }
}
