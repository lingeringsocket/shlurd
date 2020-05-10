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
import com.lingeringsocket.shlurd.ilang._

class SpcResponderSpec extends SpcResponseSpecification
{
  "SpcResponder" should
  {
    "understand conversational pronoun references" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS,
      SmcResponseParams(
        thirdPersonPronouns = false,
        reportExceptionCodes = true))
    {
      loadBeliefs("/ontologies/containment.txt")
      loadBeliefs("/ontologies/person.txt")
      loadBeliefs("/ontologies/people.txt")

      mind.startConversation
      processExceptionExpected(
        "is she a dog",
        "Sorry, when you say 'she' I don't know who or what you mean.",
        ShlurdExceptionCode.UnresolvedPronoun)
      process("is Todd or Dirk a dog", "No, neither Todd nor Dirk is a dog.")
      processExceptionExpected(
        "is he a cat",
        "Sorry, when you say 'he', it's ambiguous.",
        ShlurdExceptionCode.AmbiguousPronoun)
      process("who is Todd", "Todd is Amanda's brother.")
      process("is she a dog", "No, Amanda is not a dog.")
      process("is he Dirk's friend", "Yes, Todd is Dirk's friend.")
      processBelief("the jail is an object")
      processBelief("if a person teleports, " +
        "then subsequently the person is in the jail")
      processBelief("Todd and Dirk teleport")
      process("are they in the jail", "Yes, Todd and Dirk are in the jail.")

      processBelief("a person may have a lover")
      processBelief("a person may have a muse")
      processBelief("Camille is a woman")
      processBelief("Auguste is a man")
      processBelief("She is his lover")
      mind.stopConversation

      mind.startConversation
      processBelief("Auguste's muse is his lover")
      processTerse("who is his muse", "Camille.")
    }

    "understand spatial deixis in conversation" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      loadBeliefs("/ontologies/person.txt")
      loadBeliefs("/ontologies/location.txt")

      mind.startConversation
      processTerse("Where is Janet", "Christine.")
      processTerse("is Chrissy there", "Yes.")
    }

    "understand spatial deixis" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      loadBeliefs("/ontologies/person.txt")
      loadBeliefs("/ontologies/location.txt")

      processTerse("is Janet in Christine", "Yes.")
      processExceptionExpected("is Janet here",
        "Sorry, I don't know what 'here' means in this context.",
        ShlurdExceptionCode.UnknownModifier)
      processExceptionExpected("is Janet there",
        "Sorry, I don't know what 'there' means in this context.",
        ShlurdExceptionCode.UnknownModifier)

      val distantContext = SmcCommunicationContext(
        Some(expectProperName("Janet")),
        Some(expectProperName("Jack"))
      )
      val closeContext = SmcCommunicationContext(
        Some(expectProperName("Janet")),
        Some(expectProperName("Chrissy"))
      )
      val distantResponderTerse = new SpcResponder(
        mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_TERSE),
        new SmcExecutor[SpcEntity],
        distantContext)
      val distantResponder = new SpcResponder(
        mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_COMPLETE),
        new SmcExecutor[SpcEntity],
        distantContext)
      val closeResponder = new SpcResponder(
        mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_COMPLETE),
        new SmcExecutor[SpcEntity],
        closeContext)
      def processDistant(input : String, expected : String) =
      {
        processWithResponder(
          distantResponder,
          input
        ) must be equalTo(expected)
      }
      def processClose(input : String, expected : String) =
      {
        processWithResponder(
          closeResponder,
          input
        ) must be equalTo(expected)
      }
      def processDistantTerse(input : String, expected : String) =
      {
        processWithResponder(
          distantResponderTerse,
          input
        ) must be equalTo(expected)
      }
      processDistant(
        "is Janet here",
        "Yes, Janet is there.")
      processClose(
        "is Janet here",
        "Yes, Janet is here.")
      processDistantTerse(
        "is Chrissy here",
        "Yes.")
      processDistant(
        "is Jack here",
        "No, Jack is not there.")
      processDistantTerse(
        "is Larry here",
        "No.")
      processDistant(
        "is Janet there",
        "No, Janet is not here.")
      processDistantTerse(
        "is Chrissy there",
        "No.")
      processDistant(
        "is Jack there",
        "Yes, Jack is here.")
      processDistantTerse(
        "is Larry there",
        "No.")
    }

    "understand sequential timeframes" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("if an object moves to another object (the location), " +
        "then the object is subsequently in the location")
      processBelief("the key is an object")
      processBelief("the pocket is an object")
      processBelief("the purse is an object")
      processBelief("the shoe is an object")
      processExceptionExpected(
        "where was the key before the pocket",
        "No narrative in progress.",
        ShlurdExceptionCode.NotYetImplemented)
      mind.startNarrative
      processBelief("the key was in the pocket")
      processTerse("where is the key", "The pocket.")
      processBelief("after that the key moved to the purse")
      processTerse("where is the key", "The purse.")
      processBelief("after that the key was in the shoe")
      processTerse("where is the key", "The shoe.")
      processExceptionExpected(
        "where was the key",
        "A timeframe must be specified.",
        ShlurdExceptionCode.NotYetImplemented)
      processTerse("where was the key before the purse", "The pocket.")
      processTerse("where was the key after the purse", "The shoe.")
      processTerse("where was the key before the shoe", "The purse.")
      processTerse("where was the key after the pocket", "The purse.")
      processExceptionExpected(
        "where was the key after the shoe",
        "No such timeframe and/or event in narrative.",
        ShlurdExceptionCode.NotYetImplemented)
      processExceptionExpected(
        "where was the key before the pocket",
        "No such timeframe and/or event in narrative.",
        ShlurdExceptionCode.NotYetImplemented)
    }

    "understand relative timeframes" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("the key is an object")
      processBelief("the pocket is an object")
      processBelief("the purse is an object")
      processBelief("the shoe is an object")
      processBelief("the card is an object")
      mind.startNarrative
      processBelief("the key was in the pocket this afternoon")
      processBelief("this morning, the card was in the purse")
      processBelief("yesterday, the card was in the shoe")
      processBelief("this evening, the key was in the shoe")
      processBelief("this afternoon the card was in the shoe")
      processTerse("where was the card before the shoe", "The purse.")
      processTerse("where was the card before the purse", "The shoe.")
      processTerse("where was the key before the shoe", "The pocket.")
    }

    "ignore new beliefs" in new ResponderContext(IGNORE_BELIEFS)
    {
      processExceptionExpected(
        "There is a big door",
        "Sorry, I don't know about any 'door'.",
        ShlurdExceptionCode.UnknownForm)
    }

    "prevent new beliefs" in new ResponderContext(ACCEPT_NO_BELIEFS)
    {
      processExceptionExpected(
        "There is a big door",
        "The belief that there is a big door " +
          "is prohibited in the given context.",
        ShlurdExceptionCode.NewBeliefsProhibited)
    }

    "accept new beliefs" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a door may be either open or closed")
      processBelief("there is a big door")
      process("is the big door open",
        "I don't know.")
    }

    "reject invalid new beliefs" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("there is a white door")
      processExceptionExpected(
        "there is a big white door",
        "Previously I was told that a white door exists.  " +
          "So there is an ambiguous reference in the belief that " +
          "there is a big white door.",
        ShlurdExceptionCode.AmbiguousInterpretation)
    }

    "reject cyclic taxonomy belief" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a bird is a kind of animal")
      processBelief("a duck is a kind of bird")
      processExceptionExpected(
        "an animal is a kind of duck",
        "The belief that an animal is a kind of duck contradicts " +
          "the belief that a duck is a kind of a bird and " +
          "a bird is a kind of an animal.",
        ShlurdExceptionCode.TaxonomyCycle)
    }

    "reject incompatible form for role" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("a person must have a lawyer")
      processBelief("a person's lawyer must be a weasel")
      processBelief("Donald is a person")
      processBelief("Michael is a snake")
      processExceptionExpected(
        "Michael is Donald's lawyer",
        "The belief that Michael is Donald's lawyer contradicts " +
          "the belief that a person's lawyer must be a weasel.",
        ShlurdExceptionCode.FormRoleIncompatible)

      cosmos.sanityCheck must beTrue
    }

    "reject unknown actions" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("Superman is a person")
      processBelief("the kite is an object")
      process("Superman flies the kite",
        "I'm not sure how to interpret that.")
    }

    "reject unknown subject" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      loadBeliefs("/ontologies/containment.txt")
      processBelief("a destroyer is a kind of object")
      processBelief(
        "if a destroyer destroys an object, then the object has no container")
      processBelief("the football is an object")
      processExceptionExpected(
        "Geoff destroys the football",
        "Sorry, I don't know about any 'Geoff'.",
        ShlurdExceptionCode.UnknownForm)
    }

    "ignore action cycles" in new ResponderContext(
      ACCEPT_NEW_BELIEFS)
    {
      processBelief("Curtis is a person")
      processBelief("Andrew is a person")
      processBelief("a message is a kind of object")
      processBelief("if a person (the sender) sends " +
        "another person (the recipient) a message, " +
        "then the sender conveys the recipient the message")
      processBelief("if a person (the sender) conveys " +
        "another person (the recipient) a message, " +
        "then the sender sends the recipient the message")
      processBelief("the signal is a message")
      process("Curtis sends Andrew the signal", "OK.")
    }

    "reify unknown person" in new ResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("a person is a kind of spc-someone")
      processBelief("a person must have a lawyer")
      processBelief("Donald is a person")
      process("who is Donald's lawyer", "I don't know.")

      cosmos.sanityCheck must beTrue
    }

    "accept updates with constraints" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/person.txt")
      loadBeliefs("/ontologies/people.txt")
      processBelief("Amanda is Rapunzel's owner")
      processTerse("who is Rapunzel's owner", "Amanda.")
      processBelief("Scott is ROWDYTHREE's operative")
      processTerse("who is ROWDYTHREE's operative", "Scott.")
    }

    "detect ambiguous pronouns" in new ResponderContext(
      ACCEPT_MODIFIED_BELIEFS)
    {
      loadBeliefs("/ontologies/person.txt")
      processBelief("Mike is a man")
      processBelief("Ike is a man")
      processBelief("A person's fan must be a person")
      processBelief("Ike is Mike's fan")
      processBelief("If a person likes another person, " +
        "equivalently the former is the latter's fan")
      mind.startConversation
      process("does Mike like himself", "No, he does not like himself.")
      // FIXME need a rule for introducing reflexive here
      if (false) {
        process("does Mike like Mike", "No, he does not like himself.")
      }
      process("who is Mike", "He is a man.")
      process("who likes Mike", "Ike likes him.")
      processExceptionExpected(
        "who is he",
        "Sorry, when you say 'he', it's ambiguous.",
        ShlurdExceptionCode.AmbiguousPronoun)
    }

    "derive types" >> new ResponderContext
    {
      loadBeliefs("/ontologies/person.txt")
      loadBeliefs("/ontologies/people.txt")
      Seq(
        ("the dog", "dog"),
        ("a dog", "dog"),
        ("a frog", "spc-entity"),
        ("the big dog", "dog"),
        ("an organization with a problem", "organization"),
        ("Amanda", "woman"),
        ("Todd", "man"),
        ("it", "spc-entity"),
        ("Amanda and Todd", "person"),
        ("Amanda and BLACKWING", "spc-entity"),
        ("Todd's sister", "woman"),
        ("Todd and his sister", "person"),
        ("Rapunzel's owner", "person")
      ).foreach {
        case (
          subject, expectedType
        ) => {
          val input = s"$subject is hungry"
          val parseResult = responder.newParser(input).parseOne
          val resultCollector =
            SpcResultCollector(
              SpcAnnotator(parseResult.annotator))
          val sentence = parseResult.sentence
          responder.resolveReferences(sentence, resultCollector)
          val subjectRef = sentence match {
            case SilPredicateSentence(
              SilStatePredicate(
                subject,
                _,
                _,
                _),
              _,
              _
            ) => {
              subject
            }
            case _ => {
              throw new RuntimeException(s"unexpected sentence $sentence")
            }
          }
          responder.deriveType(
            SpcAnnotator(parseResult.annotator),
            subjectRef, resultCollector.refMap
          ).name must be equalTo expectedType
        }
      }
    }
  }
}
