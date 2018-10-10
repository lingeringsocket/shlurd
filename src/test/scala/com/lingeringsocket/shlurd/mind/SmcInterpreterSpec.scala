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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._
import scala.util._

import SprEnglishLemmas._

class SmcInterpreterSpec extends Specification
{
  type CosmosType = ZooCosmos
  type MindType = SmcMind[SmcEntity, SmcProperty, CosmosType]

  private val cosmos = new ZooCosmos

  type StateChangeInvocation = SmcStateChangeInvocation[SmcEntity]

  class ZooInterpreter(
    mind : SmcMind[SmcEntity, SmcProperty, CosmosType],
    params : SmcResponseParams,
    executor : SmcExecutor[SmcEntity]
  ) extends SmcInterpreter[
    SmcEntity, SmcProperty, CosmosType, MindType](mind, params, executor)
  {
    override protected def newPredicateEvaluator() =
    {
      new SmcPredicateEvaluator[SmcEntity, SmcProperty, CosmosType, MindType](
        mind, sentencePrinter, debugger)
      {
        private def normalizeState(
          state : SilState) : SilState =
        {
          state match {
            case SilAdpositionalState(
              SilAdposition.IN,
              SilNounReference(
                SilWord("dreamland", _),
                DETERMINER_UNSPECIFIED,
                COUNT_SINGULAR)) =>
              {
                SilPropertyState(SilWord("asleep"))
              }
            case _ => state
          }
        }

        override protected def normalizePredicate(
          predicate : SilPredicate,
          referenceMap : Map[SilReference, Set[SmcEntity]]) =
        {
          predicate match {
            case SilStatePredicate(subject, state, modifiers) => {
              SilStatePredicate(subject, normalizeState(state), modifiers)
            }
            case _ => predicate
          }
        }
      }
    }
  }

  abstract class InterpreterContext(
    responseParams : SmcResponseParams =
      SmcResponseParams(thirdPersonPronouns = false)
  ) extends Scope
  {
    protected val mind = new MindType(cosmos) {
      override def resolvePronoun(
        reference : SilPronounReference) : Try[Set[SmcEntity]] =
      {
        if (reference.count == COUNT_SINGULAR) {
          reference.person match {
            case PERSON_FIRST => return Success(Set(ZooVisitor))
            case PERSON_SECOND => return Success(Set(ZooKeeper))
            case _ =>
          }
        }
        super.resolvePronoun(reference)
      }
    }

    protected def interpret(
      input : String,
      params : SmcResponseParams = responseParams) =
    {
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation)
        {
          throw new RuntimeException("unexpected invocation")
        }
      }
      val interpreter =
        new ZooInterpreter(mind, params, executor)

      val sentence = SprParser(input).parseOne
      interpreter.interpret(sentence, input)
    }

    protected def interpretCommandExpected(
      input : String,
      invocation : StateChangeInvocation) =
    {
      val sentence = SprParser(input).parseOne
      var actualInvocation : Option[StateChangeInvocation] = None
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation)
        {
          actualInvocation = Some(invocation)
        }
      }
      val interpreter =
        new ZooInterpreter(mind, responseParams, executor)
      interpreter.interpret(sentence, input) must be equalTo("OK.")
      actualInvocation must be equalTo(Some(invocation))
    }
  }

  "SmcInterpreter" should
  {
    "deal with problem cases" in new InterpreterContext
    {
      skipped("maybe one day")
      // FIXME:  we don't deal with negated questions yet
      interpret("which goats are not awake") must be equalTo(
        "The domestic goat, the siberian goat, " +
          "and the mountain goat are not awake.")
      // FIXME:  we should use the same rules as for DETERMINER_UNIQUE
      interpret("is any tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      // FIXME:  ought to allow quantified existence queries
      interpret("how many bears are there") must be equalTo(
        "There are 2 bears.")
      interpret("how many bears are there in the small cage") must be equalTo(
        "There is 1 bear in the small cage.")
      // FIXME: this one is kinda cute
      interpret("is bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
      interpret("which goat is asleep in the farm") must be equalTo(
        "The domestic goat is asleep.")
    }

    "interpret questions" in new InterpreterContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      interpret("is the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      interpret("is the lion asleep", terse) must be equalTo(
        "Yes.")
      // FIXME:  better would be "Yes, the only lion is asleep."
      interpret("are the lions asleep") must be equalTo(
        "Yes, the lions are asleep.")
      interpret("is the lion awake") must be equalTo(
        "No, the lion is not awake.")
      interpret("is the lion awake", terse) must be equalTo(
        "No.")
      interpret("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      interpret("is the lion in dreamland") must be equalTo(
        "Yes, the lion is in dreamland.")
      interpret("is the tiger in dreamland") must be equalTo(
        "No, the tiger is not in dreamland.")
      interpret("is there a tiger") must be equalTo(
        "Yes, there is a tiger.")
      interpret("is there any tiger") must be equalTo(
        "Yes, there is a tiger.")
      interpret("are there any tigers") must be equalTo(
        "Yes, there is a tiger.")
      interpret("is there any goat") must be equalTo(
        "Yes, there are three of them.")
      interpret("is there a lion and a tiger") must be equalTo(
        "Yes, there is a lion and a tiger.")
      interpret("is there a lion or a peacock") must be equalTo(
        "Yes, there is a lion.")
      interpret("is there a peacock") must be equalTo(
        "No, there is not a peacock.")
      interpret("is there a hippogriff or a peacock") must be equalTo(
        "No, there is neither a hippogriff nor a peacock.")
      interpret("is there a hippogriff, a peacock, or a salamander") must
        be equalTo(
          "No, there is neither a hippogriff, a peacock, nor a salamander.")
      // FIXME:  improve response phrasing
      interpret("is there no peacock") must be equalTo(
        "Yes, there is no peacock.")
      interpret("is there a lion and a peacock") must be equalTo(
        "No, there is not a lion and a peacock.")
      interpret("is there a bengal tiger") must be equalTo(
        "No, there is not a bengal tiger.")
      interpret("is there a bear") must be equalTo(
        "Yes, there is a bear.")
      interpret("is there a grizzly bear") must be equalTo(
        "Yes, there is a grizzly bear.")
      interpret("is there a polar bear") must be equalTo(
        "Yes, there is a polar bear.")
      interpret("is there a kodiak bear") must be equalTo(
        "No, there is not a kodiak bear.")
      interpret("is the polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is the grizzly bear asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("is grizzly bear asleep") must be equalTo(
        "No, grizzly bear is not asleep.")
      interpret("is the kodiak bear asleep") must be equalTo(
        "But I don't know about any such bear.")
      interpret("is kodiak bear asleep") must be equalTo(
        "But I don't know about any such bear.")
      interpret("is the bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
      interpret("is any bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is any polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("is some bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("are the bears asleep") must be equalTo(
        "No, the bears are not asleep.")
      interpret("are any bears asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      interpret("are all bears asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("are all goats asleep") must be equalTo(
        "Yes, all goats are asleep.")
      interpret("are any goats asleep") must be equalTo(
        "Yes, all three of them are asleep.")
      val lowLimit = SmcResponseParams(listLimit = 1)
      interpret("are any goats asleep", lowLimit) must be equalTo(
        "Yes, all three of them are asleep.")
      interpret("are any goats awake") must be equalTo(
        "No, no goats are awake.")
      interpret("are all goats awake") must be equalTo(
        "No, none of them are awake.")
      interpret("is there an aardvark") must be equalTo(
        "Sorry, I don't know about any 'aardvark'.")
      interpret("is the sloth awake") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      interpret("is the lion or the polar bear awake") must be equalTo(
        "No, neither the lion nor the polar bear is awake.")
      interpret("is the grizzly bear or the tiger awake") must be equalTo(
        "Yes, both of them are awake.")
      interpret("is the sloth or the tiger asleep") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the lion awake") must be equalTo(
        "I don't know.")
      interpret("is the sloth or the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      interpret("are the tiger and the grizzly bear awake") must be equalTo(
        "Yes, the tiger and the grizzly bear are awake.")
      interpret("are the bears and the lion asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      interpret("are the bears and the lion awake") must be equalTo(
        "No, neither the polar bear nor the lion is awake.")
      interpret("are the bears and the lion awake", lowLimit) must be equalTo(
        "No, two of them are not awake.")
      interpret("are the tiger and the lion asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is the grizzly bear in any cage") must be equalTo(
        "No, the grizzly bear is in no cage.")
      interpret("is the tiger in the cage") must be equalTo(
        "Please be more specific about which cage you mean.")
      interpret("is the tiger in the big cage") must be equalTo(
        "Yes, the tiger is in the big cage.")
      interpret("is there a tiger in the big cage") must be equalTo(
        "Yes, there is a tiger in the big cage.")
      interpret("is the tiger in the small cage") must be equalTo(
        "No, the tiger is not in the small cage.")
      interpret("is there a tiger in the small cage") must be equalTo(
        "No, there is not a tiger in the small cage.")
      interpret("is the tiger in the big cage awake") must be equalTo(
        "Yes, the tiger in the big cage is awake.")
      interpret("is the tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      interpret("is the goat on the farm awake") must be equalTo(
        "No, the goat on the farm is not awake.")
      interpret("which goat is awake") must be equalTo(
        "No goat is awake.")
      interpret("which goats are awake") must be equalTo(
        "No goats are awake.")
      interpret("what goat is awake") must be equalTo(
        "No goat is awake.")
      val list = "The domestic goat, the siberian goat, " +
        "and the mountain goat are asleep."
      interpret("which goat is asleep") must be equalTo(list)
      interpret("which goats are asleep") must be equalTo(list)
      interpret("which goat in the farm is asleep") must be equalTo(
        "The domestic goat is asleep.")
      interpret("which goat is asleep on the farm") must be equalTo(
        "The domestic goat is asleep.")
      interpret("which goat in the farm is awake") must be equalTo(
        "No goat in the farm is awake.")
      interpret("how many goats are awake") must be equalTo(
        "No goats are awake.")
      interpret("how many goats are asleep") must be equalTo(
        "All three of them are asleep.")
      interpret("how many goats are asleep in the farm") must be equalTo(
        "One of them is asleep.")
      interpret("how many goats in the farm are asleep") must be equalTo(
        "One of them is asleep.")
      interpret("how many mountain goats are asleep") must be equalTo(
        "One of them is asleep.")
      interpret("how many lions or polar bears are asleep") must be equalTo(
        "Both of them are asleep.")
      interpret("am I in the big cage") must be equalTo(
        "No, you are not in the big cage.")
      interpret("are you in the big cage") must be equalTo(
        "Yes, I am in the big cage.")
      interpret("is he in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      interpret("is his tiger in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      interpret("are we in the big cage") must be equalTo(
        "Sorry, when you say 'we' I don't know who or what you mean.")
      interpret("are they in the big cage") must be equalTo(
        "Sorry, when you say 'they' I don't know who or what you mean.")
      interpret("is my tiger in the big cage") must be equalTo(
        "Yes, your tiger is in the big cage.")
      interpret("is your tiger in the big cage") must be equalTo(
        "But I don't know about any such tiger.")
      interpret("is my lion in the big cage") must be equalTo(
        "But I don't know about any such lion.")
      interpret("is your lion in the big cage") must be equalTo(
        "Yes, my lion is in the big cage.")
      interpret("who is in the big cage") must be equalTo(
        "Muldoon is in the big cage.")
      interpret("does Muldoon have a lion") must be equalTo(
        "Yes, Muldoon has a lion.")
      interpret("does Malcolm have a lion") must be equalTo(
        "No, Malcolm does not have a lion.")
      interpret("has Muldoon a tiger") must be equalTo(
        "No, Muldoon does not have a tiger.")
      interpret("does Malcolm have a tiger") must be equalTo(
        "Yes, Malcolm has a tiger.")
      interpret("do I have a lion") must be equalTo(
        "No, you do not have a lion.")
      interpret("do you have a lion") must be equalTo(
        "Yes, I have a lion.")
      interpret("do I have a tiger") must be equalTo(
        "Yes, you have a tiger.")
      interpret("do you have a tiger") must be equalTo(
        "No, I do not have a tiger.")
      interpret("is the grizzly bear a bear") must be equalTo(
        "Yes, the grizzly bear is a bear.")
      interpret("is the grizzly bear a lion") must be equalTo(
        "No, the grizzly bear is not a lion.")
      interpret("is the lion a lion") must be equalTo(
        "Yes, the lion is a lion.")
      interpret("who is Muldoon") must be equalTo(
        "I am Muldoon.")
      interpret("who am I") must be equalTo(
        "You are Malcolm.")
      interpret("who are you") must be equalTo(
        "I am Muldoon.")
      interpret("who is Malcolm") must be equalTo(
        "You are Malcolm.")
      interpret("is the lion an animal") must be equalTo(
        "Yes, the lion is an animal.")
      interpret("is the lion a person") must be equalTo(
        "No, the lion is not a person.")
      interpret("is Muldoon an animal") must be equalTo(
        "No, Muldoon is not an animal.")
      interpret("is Muldoon a person") must be equalTo(
        "Yes, Muldoon is a person.")
      interpret("which animals are in the big cage") must be equalTo(
        "The lion and the tiger are in the big cage.")
    }

    "interpret statements" in new InterpreterContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      interpret("the lion is asleep") must be equalTo(
        "Right, the lion is asleep.")
      interpret("the lion is asleep", terse) must be equalTo(
        "Right.")
      interpret("the lion is awake") must be equalTo(
        "Oh, really?")
      interpret("the tiger is asleep") must be equalTo(
        "Oh, really?")
      interpret("the tiger is awake") must be equalTo(
        "Right, the tiger is awake.")
      interpret("the lion or the polar bear is awake") must be equalTo(
        "Oh, really?")
    }

    "interpret commands" in new InterpreterContext
    {
      val awake = SilWord("awake", "awake")
      val asleep = SilWord("sleepify", "asleep")
      interpretCommandExpected(
        "awake the lion",
        SmcStateChangeInvocation(Set(ZooLion), awake))
      interpretCommandExpected(
        "awake the lion and the tiger",
        SmcStateChangeInvocation(Set(ZooLion), awake))
      interpretCommandExpected(
        "awake the polar bear and the lion",
        SmcStateChangeInvocation(Set(ZooLion, ZooPolarBear), awake))
      interpret("awake the tiger") must be equalTo(
        "But the tiger is awake already.")
      interpretCommandExpected(
        "sleep the tiger",
        SmcStateChangeInvocation(Set(ZooTiger), asleep))
      // FIXME error message should normalize to "asleep" instead of "sleep"
      interpret("sleep the goats") must be equalTo(
        "But the goats are sleep already.")
      interpret("sleep the lion") must be equalTo(
        "But the lion is sleep already.")
      interpret("sleep the lion and the goats") must be equalTo(
        "But the lion and the goats are sleep already.")
      interpretCommandExpected(
        "awake the goat on the farm.",
        SmcStateChangeInvocation(Set(ZooDomesticGoat), awake))
      interpretCommandExpected(
        "sleep the tiger in the big cage.",
        SmcStateChangeInvocation(Set(ZooTiger), asleep))
    }

    "respond to unrecognized phrases" in new InterpreterContext
    {
      if (!SprParser.isCoreNLP) {
        skipped("CoreNLP only")
      }
      interpret("My squeaking door is open.") must be equalTo(
        "I think you are saying that some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("My squeaking doors are open.") must be equalTo(
        "I think you are saying that some entities are open, but " +
          "I can't understand the phrase \"my squeaking doors\"")
      interpret("My squeaking door can be open.") must be equalTo(
        "I think you are saying that some entity can be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("My squeaking door may not be open.") must be equalTo(
        "I think you are saying that some entity may not be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Is my squeaking door open?") must be equalTo(
        "I think you are asking whether some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("The door is how I want it.") must be equalTo(
        "I think you are saying something about the door, but " +
          "I can't understand the phrase \"how I want it\"")
      interpret("Is the door how I want it?") must be equalTo(
        "I think you are asking something about the door, but " +
          "I can't understand the phrase \"how I want it\"")
      interpret("The cake is a lie running circles.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      interpret("A lie running circles is a cake.") must be equalTo(
        "I think you are saying that some entity is a cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      interpret("The cake has a lie running circles.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      interpret("A lie running circles has a cake.") must be equalTo(
        "I think you are saying that some entity has a cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      interpret(
        "Butterflies running circles are cakes.") must be equalTo(
        "I think you are saying that some entities are cakes, but I can't " +
          "understand the phrase \"butterflies running circles\"")
      interpret("There is my squeaking door.") must be equalTo(
        "I think you are saying that some entity exists, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Open my squeaking door.") must be equalTo(
        "I think you are telling me to open some entity, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Turn on my fracking light.") must be equalTo(
        "I think you are telling me to turn on some entity, but " +
          "I can't understand the phrase \"my fracking light\"")
      interpret("Which my squeaking door is open?") must be equalTo(
        "I think you are asking which entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Which door is how I want it?") must be equalTo(
        "I think you are asking which door is in some state, but " +
          "I can't understand the phrase \"how I want it\"")
      interpret("Who is how I want them?") must be equalTo(
        "I think you are asking who is in some state, but " +
          "I can't understand the phrase \"how I want them\"")
      // FIXME
      /*
      interpret("Which lie flunk circles is a cake?") must be equalTo(
        "I think you are asking which entity is a cake, but " +
          "I can't understand the phrase \"lie flunk circles\"")
       */
      interpret("Which cake is a lie flunk circles?") must be equalTo(
        "I think you are asking which cake is some entity, but " +
          "I can't understand the phrase \"a lie flunk circles\"")
      interpret("There is my squeaking door and a window") must
        be equalTo("I think you are saying that some " +
          "entities exist, but " +
          "I can't understand the phrase \"my squeaking door and a window\"")
      interpret("My squeaking door is open and sideways") must
        be equalTo("I think you are saying that some " +
          "entity is open and sideways, but " +
          "I can't understand the phrase \"my squeaking door\"")
      interpret("Are you how I want you?") must be equalTo(
        "I think you are asking something about me, but " +
          "I can't understand the phrase \"how I want you\"")
    }

    "remember conversation" in new InterpreterContext
    {
      mind.startConversation
      interpret("who are you") must be equalTo("I am Muldoon.")
      mind.getConversation.getUtterances must be equalTo Seq(
        SpeakerUtterance(
          SmcConversation.SPEAKER_NAME_PERSON,
          SilPredicateQuery(
            SilRelationshipPredicate(
              SilNounReference(SilWord(LEMMA_WHO)),
              SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR),
              REL_IDENTITY
            ),
            QUESTION_WHO,
            INFLECT_NOMINATIVE,
            SilTam.interrogative,
            SilFormality(FORCE_NEUTRAL)),
          "who are you",
          Map(
            SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR) ->
              Set(ZooKeeper))),
        SpeakerUtterance(
          SmcConversation.SPEAKER_NAME_SHLURD,
          SilPredicateSentence(
            SilRelationshipPredicate(
              SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR),
              SilNounReference(SilWord("Muldoon")),
              REL_IDENTITY),
            SilTam.indicative,
            SilFormality(FORCE_NEUTRAL)),
          "I am Muldoon.",
          Map(
            SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR) ->
              Set(ZooKeeper),
            SilNounReference(SilWord("Muldoon")) -> Set(ZooKeeper)
          )
        )
      )
    }

    "understand conversational singular pronoun references" in new
      InterpreterContext
    {
      mind.startConversation
      interpret("is it asleep") must be equalTo(
        "Sorry, when you say 'it' I don't know who or what you mean.")
      interpret("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      interpret("is it awake") must be equalTo(
        "Yes, the tiger is awake.")
    }

    "understand conversational plural pronoun references" in new
      InterpreterContext
    {
      mind.startConversation
      interpret("are they asleep") must be equalTo(
        "Sorry, when you say 'they' I don't know who or what you mean.")
      interpret("are the lion and the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      // FIXME:  the answer should be "No, the lion is not awake."; need to
      // split the pronoun reference somehow when evaluating it
      interpret("are they awake") must be equalTo(
        "No, the lion and the tiger are not awake.")
    }
  }
}
