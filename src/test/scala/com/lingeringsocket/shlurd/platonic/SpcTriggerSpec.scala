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

class SpcTriggerSpec extends SpcResponseSpecification
{
  "understand actions" in new ResponderContext(ACCEPT_MODIFIED_BELIEFS)
  {
    loadBeliefs("/ontologies/containment.txt")
    processBelief(
      "if one object moves to another object, " +
        "then subsequently the former is in the latter")
    processBelief(
      "if an object rolls into another object, " +
        "then the former moves to the latter")
    processBelief("Percy is an object")
    processBelief("Thomas is an object")
    processBelief("Fuji is an object")
    processBelief("Kilimanjaro is an object")
    processBelief("Denali is an object")
    processBelief("Percy moves to Fuji")
    process("where is Percy", "It is in Fuji.")
    processBelief("Percy rolls into Kilimanjaro")
    process("where is Percy", "It is in Kilimanjaro.")

    process("Percy rolls to Denali",
      "I'm not sure how to interpret that.")
    process("where is Percy", "It is in Kilimanjaro.")

    processBelief("Percy and Thomas move to Denali")
    process("where is Percy", "It is in Denali.")
    process("where is Thomas", "It is in Denali.")

    processBelief("if a person drops an object, " +
      "then subsequently the object is in the person's container")
    processBelief("Curtis is a person")
    processBelief("the boiler is an object")
    processBelief("the engine is an object")
    processBelief("the bomb is an object")
    process("where is the bomb", "I don't know.")
    processBelief("Curtis is in the boiler")
    processBelief("Curtis drops the bomb")
    processBelief("Curtis moves to the engine")
    processTerse("where is Curtis", "The engine.")
    process("where is the bomb", "It is in the boiler.")
  }

  "understand past actions" in new
    ResponderContext(ACCEPT_NEW_BELIEFS)
  {
    loadBeliefs("/ontologies/person.txt")
    loadBeliefs("/ontologies/containment.txt")
    mind.startConversation()
    mind.startNarrative()
    processBelief("Curtis is a man")
    processBelief("Andrea is a woman")
    processBelief("Thomas is a man")
    processBelief("the bomb is an object")
    processBelief("the wrench is an object")
    processBelief("the screwdriver is an object")
    processBelief("if a person receives an object, " +
      "then the object becomes the person's contained-object")
    processBelief("if a person gives an object " +
      "to another person (the recipient), " +
      "then the recipient receives the object")
    processBelief("if a person passes an object" +
      " to another person (the recipient), " +
      "then the person gives the object to the recipient")
    processBelief("Thomas passed the wrench to Andrea")
    // FIXME this used to work
    /*
     processBelief("Curtis passed the bomb to her")
     */
    processBelief("Curtis passed the bomb to Andrea")
    processBelief("Curtis passed the screwdriver to Thomas")
    // FIXME irregular forms
    processMatrix(
      "what did Curtis give to Andrea",
      "He gived the bomb to her.",
      "Curtis gived the bomb to Andrea.",
      "The bomb.",
      "The bomb.")
    processTerse("what did Curtis give to Thomas", "The screwdriver.")
    processTerse("what did Thomas give to Andrea", "The wrench.")
    processMatrix(
      "who did Curtis give the bomb to",
      "He gived it to Andrea.",
      "Curtis gived the bomb to Andrea.",
      "Andrea.",
      "Andrea.")
    processMatrix(
      "who received the bomb",
      "Andrea received it.",
      "Andrea received the bomb.",
      "Andrea.",
      "Andrea.")
  }

  "understand constraints in beliefs" in new
    ResponderContext(ACCEPT_NEW_BELIEFS)
  {
    processBelief("a wire must be red or blue")
    processBelief("there is an important wire")
    processBelief("the important wire is red")
    processBelief("MacGyver is a person")
    processBelief("if a person cuts a wire, then the wire must be blue")
    process(
      "MacGyver cuts the important wire",
      "But the important wire is not blue.")
  }

  "understand nested triggers" in new
    ResponderContext(ACCEPT_NEW_BELIEFS)
  {
    loadBeliefs("/ontologies/containment.txt")
    processBelief("a manager is a kind of person")
    processBelief("a worker is a kind of person")
    processBelief("a person's minion must be a worker")
    processBelief("a manager may have minions")
    processBelief("Scrooge is a manager")
    processBelief("Cratchit is a worker")
    processBelief("Cratchit is Scrooge's minion")
    processBelief("a person may be angry or sad")
    processBelief("if a manager becomes angry," +
      " then the manager strikes the manager's minions")
    processBelief("if a person (the bully) strikes " +
      "another person (the victim), then the victim becomes sad")
    processBelief("Scrooge is angry")
    processTerse("is Scrooge angry", "Yes.")
    processTerse("is Cratchit sad", "Yes.")
    processBelief("Cratchit becomes angry")
    processTerse("is Cratchit angry", "Yes.")
    processBelief("Scrooge becomes sad")
    processTerse("is Scrooge angry", "No.")
    processBelief("Scrooge becomes angry")
    processTerse("is Cratchit angry", "No.")
  }

  "understand subset matches" in new
    ResponderContext(ACCEPT_NEW_BELIEFS)
  {
    loadBeliefs("/ontologies/containment.txt")
    processBelief("a fruit is a kind of object")
    processBelief("a fruit must be green or red")
    processBelief("an apple is a kind of fruit")
    processBelief("a tomato is a kind of fruit")
    processBelief("if a person sees an apple, then the latter becomes red")
    processBelief("Pippin is an apple")
    processBelief("EarlyGirl is a tomato")
    processBelief("Merry is a person")
    processBelief("the box is an object")
    processBelief("Pippin is in the box")
    processBelief("EarlyGirl is in the box")
    processBelief("Merry sees the box's contained-objects")
    processTerse("is Pippin red", "Yes.")
    processTerse("is EarlyGirl red", "I don't know.")
  }

  "understand instantiation triggers" in new
    ResponderContext(ACCEPT_NEW_BELIEFS)
  {
    processBelief("fruits and trees are kinds of objects")
    processBelief("a tree's product must be a fruit")
    processBelief("apples and oranges are kinds of fruit")
    processBelief("a fruit must be green or red")
    processBelief("when a fruit instantiates, it becomes green")
    processBelief("before a fruit instantiates, a tree must exist")
    processExceptionExpected(
      "an apple exists",
      "But a tree does not exist.",
      ShlurdExceptionCode.InstantiationProhibited)
    processBelief("a tree exists")
    processBelief("an apple exists")
    processTerse("is the apple green", "Yes.")
    processBelief("the tree has a product")
    processTerse("is the tree's product green", "Yes.")
  }

  "detect causality violations" in new ResponderContext(
    ACCEPT_NEW_BELIEFS)
  {
    mind.startNarrative()
    processBelief("yesterday, Harvey was Elwood's pet")
    processExceptionExpected(
      "this afternoon, Elwood had no pets",
      "The belief that Elwood had no pets " +
        "contradicts the belief that Harvey is Elwood's pet.",
      ShlurdExceptionCode.AbsenceConstraint)
    processBelief("this afternoon, Calvin had no pets")
    processExceptionExpected(
      "yesterday, Hobbes was Calvin's pet",
      "The belief that Calvin has no pets " +
        "contradicts the belief that Hobbes is Calvin's pet.",
      ShlurdExceptionCode.AbsenceConstraint)
  }

  "validate constraints incrementally" in new ResponderContext(
    ACCEPT_NEW_BELIEFS)
  {
    loadBeliefs("/ontologies/person.txt")
    loadBeliefs("/ontologies/people.txt")

    val expected = "Previously I was told that a dog may have one owner and " +
        "Bart is Rapunzel's owner.  So it does not add up when I hear that " +
        "Amanda is Rapunzel's owner."
    processExceptionExpected(
      "Amanda is Rapunzel's owner",
      expected,
      ShlurdExceptionCode.CardinalityConstraint)

    // verify reporting from trigger as well
    processBelief(
      "when a person graduates, " +
        "then the person is subsequently Rapunzel's owner")
    processExceptionExpected(
      "Amanda graduates",
      expected,
      ShlurdExceptionCode.CardinalityConstraint)

    processExceptionExpected(
      "Scott is ROWDYTHREE's operative",
      "Previously I was told that a person may have one employer and " +
        "BLACKWING is Scott's employer.  So it does not add up when I " +
        "hear that Scott is ROWDYTHREE's operative.",
      ShlurdExceptionCode.CardinalityConstraint)
  }

  "report triggered errors" in new ResponderContext(
    ACCEPT_NEW_BELIEFS)
  {
    processBelief("a hobbit's friend must be a hobbit")
    processBelief("Frodo, Sam, Merry, and Pippin are hobbits")
    processBelief("when a hobbit befriends another hobbit, " +
      "the former becomes the latter's friend")
    processExceptionExpected(
      "Frodo befriends any hobbit",
      "I am unable to understand the belief that Frodo " +
        "becomes any hobbit's friend.",
      ShlurdExceptionCode.IncomprehensibleBelief)
  }

  "enforce assertions" in new ResponderContext(
    ACCEPT_NEW_BELIEFS)
  {
    processBelief("a hobbit is a kind of person")
    processBelief("a man is a kind of person")
    processBelief("a dwarf is a kind of person")
    processBelief("Bilbo is a hobbit")
    processBelief("Sigurd is a man")
    processBelief("Gimli is a dwarf")
    processBelief("Smaug is a dragon")
    processBelief("Fafnir is a dragon")
    processBelief("Glamdring is a sword")

    processExceptionExpected("Sigurd kills Gandalf",
      "Sorry, I don't know about any 'Gandalf'.",
      ShlurdExceptionCode.UnknownForm)
    process("Sigurd kills Fafnir", "I'm not sure how to interpret that.")
    processBelief("if a person kills a dragon, " +
      "then subsequently the dragon is dead")
    process("Sigurd kills Fafnir", "OK.")

    processBelief("a hobbit cannot kill a dragon")
    processBelief("generally a dwarf can't kill a dragon")
    processBelief("a dwarf can kill a dragon with a sword")

    process("Bilbo kills Smaug", "A hobbit can not kill a dragon.")
    process("Bilbo kills Smaug with Glamdring",
      "A hobbit can not kill a dragon.")
    process("Gimli kills Smaug", "One does not simply kill a dragon.")
    process("Gimli kills Smaug with Glamdring", "OK.")
    processExceptionExpected("Bilbo kills Gandalf",
      "Sorry, I don't know about any 'Gandalf'.",
      ShlurdExceptionCode.UnknownForm)
    processExceptionExpected("Gimli kills Gandalf with Glamdring",
      "Sorry, I don't know about any 'Gandalf'.",
      ShlurdExceptionCode.UnknownForm)
  }

  "understand funky genitives" in new ResponderContext(
    ACCEPT_MODIFIED_BELIEFS)
  {
    processBelief("A person is a kind of spc-someone.")
    processBelief("A supe is a kind of person.")
    processBelief("A supe's nemesis must be a supe.")
    processBelief("If a supe is another supe's nemesis, " +
      "then equivalently the latter is the former's nemesis.")
    processBelief("If a supe opposes another supe, " +
      "then equivalently the former is the latter's nemesis.")
    processBelief("Homelander is a supe.")
    processBelief("Starlight is a supe.")
    processBelief("Homelander is Starlight's nemesis.")
    processTerse("Whom does Homelander oppose?", "Starlight.")
  }

  "understand all" in new ResponderContext(ACCEPT_MODIFIED_BELIEFS)
  {
    loadBeliefs("/ontologies/containment.txt")
    processBelief("pigs and pens are kinds of objects")
    processBelief("a pig's color must be pink or red")
    processBelief("there is a farmer")
    processBelief("there is a pen")
    processBelief("if a farmer calls, then all pigs squeal")
    processBelief(
      "if a pig squeals, then subsequently the pig is in the pen;" +
        "also the pig's color becomes red")
    processBelief("there is a big pig")
    processBelief("there is a small pig")
    process("the farmer calls", "OK.")
    processTerse("is the big pig red", "Yes.")
    processTerse("is the small pig red", "Yes.")
    processTerse("is the big pig in the pen", "Yes.")
    processTerse("is the small pig in the pen", "Yes.")
  }
}
