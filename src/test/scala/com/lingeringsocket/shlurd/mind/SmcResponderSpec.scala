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

import SprEnglishLemmas._

class SmcResponderSpec extends Specification
{
  type CosmosType = ZooCosmos
  type MindType = ZooMind

  private val cosmos = new ZooCosmos

  type StateChangeInvocation = SmcStateChangeInvocation[SmcEntity]

  class ZooResponder(
    mind : ZooMind,
    params : SmcResponseParams,
    executor : SmcExecutor[SmcEntity],
    communicationContext : SmcCommunicationContext[SmcEntity]
  ) extends SmcResponder[
    SmcEntity, SmcProperty, CosmosType, MindType](
    mind, params, executor, communicationContext)
  {
    override protected def newPredicateEvaluator() =
    {
      new SmcPredicateEvaluator[SmcEntity, SmcProperty, CosmosType, MindType](
        mind, sentencePrinter, params.existenceAssumption,
        communicationContext, debugger)
      {
        private def normalizeState(
          state : SilState) : SilState =
        {
          state match {
            case SilAdpositionalState(
              SilAdposition.IN,
              SilNounReference(
                SilWordInflected("dreamland"),
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
            case SilStatePredicate(subject, verb, state, modifiers) => {
              SilStatePredicate(subject, verb, normalizeState(state), modifiers)
            }
            case _ => predicate
          }
        }
      }
    }
  }

  abstract class ResponderContext(
    responseParams : SmcResponseParams =
      SmcResponseParams(thirdPersonPronouns = false)
  ) extends Scope
  {
    protected val mind = new ZooMind(cosmos)

    protected val communicationContext = SmcCommunicationContext[SmcEntity](
      Some(ZooVisitor),
      Some(ZooKeeper)
    )

    protected def process(
      input : String,
      params : SmcResponseParams = responseParams) =
    {
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation,
          referenceMap : Map[SilReference, Set[SmcEntity]]) =
        {
          throw new RuntimeException("unexpected invocation")
        }
      }
      val responder =
        new ZooResponder(mind, params, executor, communicationContext)

      val sentence = responder.newParser(input).parseOne
      responder.process(sentence, input)
    }

    protected def processCommandExpected(
      input : String,
      invocation : StateChangeInvocation) =
    {
      val ok = "OK."
      var actualInvocation : Option[StateChangeInvocation] = None
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation,
          referenceMap : Map[SilReference, Set[SmcEntity]]) =
        {
          actualInvocation = Some(invocation)
          Some(ok)
        }
      }
      val responder =
        new ZooResponder(
          mind, responseParams, executor,
          communicationContext)
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence, input) must be equalTo(ok)
      actualInvocation must be equalTo(Some(invocation))
    }
  }

  "SmcResponder" should
  {
    "deal with problem cases" in new ResponderContext
    {
      skipped("maybe one day")
      // FIXME:  we don't deal with negated questions yet
      process("which goats are not awake") must be equalTo(
        "The domestic goat, the siberian goat, " +
          "and the mountain goat are not awake.")
      // FIXME:  we should use the same rules as for DETERMINER_UNIQUE
      process("is any tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      // FIXME: this one is kinda cute
      process("is bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
    }

    "handle CoreNLP excluded cases" in new ResponderContext
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not working")
      }
      process("which goat is asleep in the farm") must be equalTo(
        "The domestic goat is asleep.")
      process("how many bears are there in the small cage") must be equalTo(
        "There is one of them.")
      process("how many bears are there") must be equalTo(
        "There are two of them.")
      process("is there (a bear)") must be equalTo(
        "Yes, there is a bear.")
    }

    "process questions" in new ResponderContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      val ellipsis = SmcResponseParams(verbosity = RESPONSE_ELLIPSIS)
      process("is the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      process("is the lion asleep", terse) must be equalTo(
        "Yes.")
      // FIXME:  better would be "Yes, the only lion is asleep."
      process("are the lions asleep") must be equalTo(
        "Yes, the lions are asleep.")
      process("is the lion awake") must be equalTo(
        "No, the lion is not awake.")
      process("is the lion awake", terse) must be equalTo(
        "No.")
      process("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      process("is the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      process("is the lion in dreamland") must be equalTo(
        "Yes, the lion is in dreamland.")
      process("is the tiger in dreamland") must be equalTo(
        "No, the tiger is not in dreamland.")
      process("is there a tiger") must be equalTo(
        "Yes, there is a tiger.")
      process("is there any tiger") must be equalTo(
        "Yes, there is a tiger.")
      process("are there any tigers") must be equalTo(
        "Yes, there is a tiger.")
      process("is there any goat") must be equalTo(
        "Yes, there are three of them.")
      process("is there a lion and a tiger") must be equalTo(
        "Yes, there is a lion and a tiger.")
      process("is there a lion or a peacock") must be equalTo(
        "Yes, there is a lion.")
      process("is there a peacock") must be equalTo(
        "No, there is not a peacock.")
      process("is there a peacock", ellipsis) must be equalTo(
        "No, there is not a peacock.")
      process("is there any peacock") must be equalTo(
        "No, there is no peacock.")
      process("is there any peacock", ellipsis) must be equalTo(
        "No, there is no peacock.")
      process("are there any peacocks") must be equalTo(
        "No, there are no peacocks.")
      process("are there any peacocks", ellipsis) must be equalTo(
        "No, there are no peacocks.")
      process("is there a hippogriff or a peacock") must be equalTo(
        "No, there is neither a hippogriff nor a peacock.")
      process("is there a hippogriff, a peacock, or a salamander") must
        be equalTo(
          "No, there is neither a hippogriff, a peacock, nor a salamander.")
      // FIXME:  improve response phrasing
      process("is there no peacock") must be equalTo(
        "Yes, there is no peacock.")
      process("is there a lion and a peacock") must be equalTo(
        "No, there is not a lion and a peacock.")
      process("is there a siberian tiger") must be equalTo(
        "No, there is not a siberian tiger.")
      process("is there a bear") must be equalTo(
        "Yes, there is a bear.")
      process("is there a grizzly bear") must be equalTo(
        "Yes, there is a grizzly bear.")
      process("is there a polar bear") must be equalTo(
        "Yes, there is a polar bear.")
      process("is there a fuzzy bear") must be equalTo(
        "No, there is not a fuzzy bear.")
      process("is the polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is the grizzly bear asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("is grizzly bear asleep") must be equalTo(
        "No, grizzly bear is not asleep.")
      process("is the fuzzy bear asleep") must be equalTo(
        "But I don't know about any such bear.")
      process("is fuzzy bear asleep") must be equalTo(
        "But I don't know about any such bear.")
      process("is the bear asleep") must be equalTo(
        "Please be more specific about which bear you mean.")
      process("is any bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is any polar bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("is some bear asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("are the bears asleep") must be equalTo(
        "No, the bears are not asleep.")
      process("are any bears asleep") must be equalTo(
        "Yes, the polar bear is asleep.")
      process("are all bears asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("are all goats asleep") must be equalTo(
        "Yes, all goats are asleep.")
      process("are any goats asleep") must be equalTo(
        "Yes, all three of them are asleep.")
      val lowLimit = SmcResponseParams(listLimit = 1)
      process("are any goats asleep", lowLimit) must be equalTo(
        "Yes, all three of them are asleep.")
      process("are any goats awake") must be equalTo(
        "No, no goats are awake.")
      process("are all goats awake") must be equalTo(
        "No, none of them are awake.")
      process("is there an aardvark") must be equalTo(
        "Sorry, I don't know about any 'aardvark'.")
      process("is the sloth awake") must be equalTo(
        "I don't know.")
      process("is the sloth or the tiger awake") must be equalTo(
        "Yes, the tiger is awake.")
      process("is the lion or the polar bear awake") must be equalTo(
        "No, neither the lion nor the polar bear is awake.")
      process("is the grizzly bear or the tiger awake") must be equalTo(
        "Yes, both of them are awake.")
      process("is the sloth or the tiger asleep") must be equalTo(
        "I don't know.")
      process("is the sloth or the lion awake") must be equalTo(
        "I don't know.")
      process("is the sloth or the lion asleep") must be equalTo(
        "Yes, the lion is asleep.")
      process("are the tiger and the grizzly bear awake") must be equalTo(
        "Yes, the tiger and the grizzly bear are awake.")
      process("are the bears and the lion asleep") must be equalTo(
        "No, the grizzly bear is not asleep.")
      process("are the bears and the lion awake") must be equalTo(
        "No, neither the polar bear nor the lion is awake.")
      process("are the bears and the lion awake", lowLimit) must be equalTo(
        "No, two of them are not awake.")
      process("are the tiger and the lion asleep") must be equalTo(
        "No, the tiger is not asleep.")
      process("is the grizzly bear in any cage") must be equalTo(
        "No, the grizzly bear is in no cage.")
      process("is the tiger in the cage") must be equalTo(
        "Please be more specific about which cage you mean.")
      process("is the tiger in the big cage") must be equalTo(
        "Yes, the tiger is in the big cage.")
      process("is there a tiger in the big cage") must be equalTo(
        "Yes, there is a tiger in the big cage.")
      process("is the tiger in the small cage") must be equalTo(
        "No, the tiger is not in the small cage.")
      process("is there a tiger in the small cage") must be equalTo(
        "No, there is not a tiger in the small cage.")
      process("is the tiger in the big cage awake") must be equalTo(
        "Yes, the tiger in the big cage is awake.")
      process("is the tiger in the small cage awake") must be equalTo(
        "But I don't know about any such tiger.")
      process("is the goat on the farm awake") must be equalTo(
        "No, the goat on the farm is not awake.")
      process("which goat is awake") must be equalTo(
        "No goat is awake.")
      process("which goats are awake") must be equalTo(
        "No goats are awake.")
      process("what goat is awake") must be equalTo(
        "No goat is awake.")
      val list = "The domestic goat, the siberian goat, " +
        "and the mountain goat are asleep."
      process("which goat is asleep") must be equalTo(list)
      process("which goats are asleep") must be equalTo(list)
      process("which goat in the farm is asleep") must be equalTo(
        "The domestic goat is asleep.")
      process("which goat is asleep on the farm") must be equalTo(
        "The domestic goat is asleep.")
      process("which goat in the farm is awake") must be equalTo(
        "No goat in the farm is awake.")
      process("how many goats are awake") must be equalTo(
        "No goats are awake.")
      process("how many goats are asleep") must be equalTo(
        "All three of them are asleep.")
      process("how many goats are asleep in the farm") must be equalTo(
        "One of them is asleep.")
      process("how many goats in the farm are asleep") must be equalTo(
        "One of them is asleep.")
      process("how many mountain goats are asleep") must be equalTo(
        "One of them is asleep.")
      process("how many lions or polar bears are asleep") must be equalTo(
        "Both of them are asleep.")
      process("am I in the big cage") must be equalTo(
        "No, you are not in the big cage.")
      process("are you in the big cage") must be equalTo(
        "Yes, I am in the big cage.")
      process("is he in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      process("is his tiger in the big cage") must be equalTo(
        "Sorry, when you say 'he' I don't know who or what you mean.")
      process("are we in the big cage") must be equalTo(
        "Sorry, when you say 'we' I don't know who or what you mean.")
      process("are they in the big cage") must be equalTo(
        "Sorry, when you say 'they' I don't know who or what you mean.")
      process("is my tiger in the big cage") must be equalTo(
        "Yes, your tiger is in the big cage.")
      process("is your tiger in the big cage") must be equalTo(
        "But I don't know about any such tiger.")
      process("is my lion in the big cage") must be equalTo(
        "But I don't know about any such lion.")
      process("is your lion in the big cage") must be equalTo(
        "Yes, my lion is in the big cage.")
      process("who is in the big cage") must be equalTo(
        "Muldoon is in the big cage.")
      process("does Muldoon have a lion") must be equalTo(
        "Yes, Muldoon has a lion.")
      process("does Malcolm have a lion") must be equalTo(
        "No, Malcolm does not have a lion.")
      process("has Muldoon a tiger") must be equalTo(
        "No, Muldoon does not have a tiger.")
      process("does Malcolm have a tiger") must be equalTo(
        "Yes, Malcolm has a tiger.")
      process("do I have a lion") must be equalTo(
        "No, you do not have a lion.")
      process("do you have a lion") must be equalTo(
        "Yes, I have a lion.")
      process("do I have a tiger") must be equalTo(
        "Yes, you have a tiger.")
      process("do you have a tiger") must be equalTo(
        "No, I do not have a tiger.")
      process("is the grizzly bear a bear") must be equalTo(
        "Yes, the grizzly bear is a bear.")
      process("is the grizzly bear a lion") must be equalTo(
        "No, the grizzly bear is not a lion.")
      process("is the lion a lion") must be equalTo(
        "Yes, the lion is a lion.")
      process("who is Muldoon") must be equalTo(
        "I am Muldoon.")
      process("who am I") must be equalTo(
        "You are Malcolm.")
      process("who are you") must be equalTo(
        "I am Muldoon.")
      process("who is Malcolm") must be equalTo(
        "You are Malcolm.")
      process("is the lion an animal") must be equalTo(
        "Yes, the lion is an animal.")
      process("is the lion a person") must be equalTo(
        "No, the lion is not a person.")
      process("is Muldoon an animal") must be equalTo(
        "No, Muldoon is not an animal.")
      process("is Muldoon a person") must be equalTo(
        "Yes, Muldoon is a person.")
      process("which animals are in the big cage") must be equalTo(
        "The lion and the tiger are in the big cage.")
    }

    "process statements" in new ResponderContext
    {
      val terse = SmcResponseParams(verbosity = RESPONSE_TERSE)
      process("the lion is asleep") must be equalTo(
        "Right, the lion is asleep.")
      process("the lion is asleep", terse) must be equalTo(
        "Right.")
      process("the lion is awake") must be equalTo(
        "Oh, really?")
      process("the tiger is asleep") must be equalTo(
        "Oh, really?")
      process("the tiger is awake") must be equalTo(
        "Right, the tiger is awake.")
      process("the lion or the polar bear is awake") must be equalTo(
        "Oh, really?")
    }

    "process commands" in new ResponderContext
    {
      val awake = SilWord("awake")
      val asleep = SilWord("sleepify", "asleep")
      processCommandExpected(
        "awake the lion",
        SmcStateChangeInvocation(Set(ZooLion), awake))
      processCommandExpected(
        "awake the lion and the tiger",
        SmcStateChangeInvocation(Set(ZooLion), awake))
      processCommandExpected(
        "awake the polar bear and the lion",
        SmcStateChangeInvocation(Set(ZooLion, ZooPolarBear), awake))
      process("awake the tiger") must be equalTo(
        "But the tiger is awake already.")
      processCommandExpected(
        "sleep the tiger",
        SmcStateChangeInvocation(Set(ZooTiger), asleep))
      // FIXME error message should normalize to "asleep" instead of "sleep"
      process("sleep the goats") must be equalTo(
        "But the goats are sleep already.")
      process("sleep the lion") must be equalTo(
        "But the lion is sleep already.")
      process("sleep the lion and the goats") must be equalTo(
        "But the lion and the goats are sleep already.")
      processCommandExpected(
        "awake the goat on the farm.",
        SmcStateChangeInvocation(Set(ZooDomesticGoat), awake))
      processCommandExpected(
        "sleep the tiger in the big cage.",
        SmcStateChangeInvocation(Set(ZooTiger), asleep))
    }

    "respond to unrecognized phrases" in new ResponderContext
    {
      if (!SprParser.isCoreNLP) {
        skipped("CoreNLP only")
      }
      process("My squeaking door is open.") must be equalTo(
        "I think you are saying that some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("My squeaking doors are open.") must be equalTo(
        "I think you are saying that some entities are open, but " +
          "I can't understand the phrase \"my squeaking doors\"")
      process("My squeaking door can be open.") must be equalTo(
        "I think you are saying that some entity can be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("My squeaking door may not be open.") must be equalTo(
        "I think you are saying that some entity may not be open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("Is my squeaking door open?") must be equalTo(
        "I think you are asking whether some entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("The door is how I want it.") must be equalTo(
        "I think you are saying something about the door, but " +
          "I can't understand the phrase \"how I want it\"")
      process("Is the door how I want it?") must be equalTo(
        "I think you are asking something about the door, but " +
          "I can't understand the phrase \"how I want it\"")
      process("The cake is a lie running circles.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      process("A lie running circles is a cake.") must be equalTo(
        "I think you are saying that some entity is a cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      process("The cake has a lie running circles.") must be equalTo(
        "I think you are saying something about the cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      process("A lie running circles has a cake.") must be equalTo(
        "I think you are saying that some entity has a cake, but " +
          "I can't understand the phrase \"a lie running circles\"")
      process(
        "Butterflies running circles are cakes.") must be equalTo(
        "I think you are saying that some entities are cakes, but I can't " +
          "understand the phrase \"butterflies running circles\"")
      process("There is my squeaking door.") must be equalTo(
        "I think you are saying that some entity exists, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("Which my squeaking door is open?") must be equalTo(
        "I think you are asking which entity is open, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("Which door is how I want it?") must be equalTo(
        "I think you are asking which door is in some state, but " +
          "I can't understand the phrase \"how I want it\"")
      process("Who is how I want them?") must be equalTo(
        "I think you are asking who is in some state, but " +
          "I can't understand the phrase \"how I want them\"")
      // FIXME
      /*
      process("Which lie flunk circles is a cake?") must be equalTo(
        "I think you are asking which entity is a cake, but " +
          "I can't understand the phrase \"lie flunk circles\"")
       */
      process("Which cake is a lie flunk circles?") must be equalTo(
        "I think you are asking which cake is some entity, but " +
          "I can't understand the phrase \"a lie flunk circles\"")
      process("There is my squeaking door and a window") must
        be equalTo("I think you are saying that some " +
          "entities exist, but " +
          "I can't understand the phrase \"my squeaking door and a window\"")
      process("My squeaking door is open and sideways") must
        be equalTo("I think you are saying that some " +
          "entity is open and sideways, but " +
          "I can't understand the phrase \"my squeaking door\"")
      process("Are you how I want you?") must be equalTo(
        "I think you are asking something about me, but " +
          "I can't understand the phrase \"how I want you\"")
    }

    "remember conversation" in new ResponderContext
    {
      mind.startConversation
      process("who are you") must be equalTo("I am Muldoon.")
      mind.getConversation.getUtterances must be equalTo Seq(
        SpeakerUtterance(
          SmcConversation.SPEAKER_NAME_PERSON,
          SilPredicateQuery(
            SilRelationshipPredicate(
              SilNounReference(SilWord(LEMMA_WHO)),
              SilWord("are", LEMMA_BE),
              SilPronounReference(PERSON_SECOND, GENDER_N, COUNT_SINGULAR)
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
              SilWord.uninflected(LEMMA_BE),
              SilNounReference(SilWord("Muldoon"))
            ),
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
      ResponderContext
    {
      mind.startConversation
      process("is it asleep") must be equalTo(
        "Sorry, when you say 'it' I don't know who or what you mean.")
      process("is the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      process("is it awake") must be equalTo(
        "Yes, the tiger is awake.")
    }

    "understand conversational plural pronoun references" in new
      ResponderContext
    {
      mind.startConversation
      process("are they asleep") must be equalTo(
        "Sorry, when you say 'they' I don't know who or what you mean.")
      process("are the lion and the tiger asleep") must be equalTo(
        "No, the tiger is not asleep.")
      // FIXME:  the answer should be "No, the lion is not awake."; need to
      // split the pronoun reference somehow when evaluating it
      process("are they awake") must be equalTo(
        "No, the lion and the tiger are not awake.")
    }

    "allow for unknown existence" in new
      ResponderContext(SmcResponseParams(
        thirdPersonPronouns = false,
        existenceAssumption = EXISTENCE_ASSUME_UNKNOWN))
    {
      process("is there a peacock") must be equalTo(
        "I don't know.")
      process("do you have a tiger") must be equalTo(
        "I don't know.")
    }
  }
}
