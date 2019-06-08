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
package com.lingeringsocket.shlurd.ilang

import org.specs2.mutable._

import com.lingeringsocket.shlurd.parser._

class SilSentencePrinterSpec extends Specification
{
  private val printer = new SilSentencePrinter

  private def normalize(s : String) : String =
  {
    val parsed = SprParser(s).parseOne
    val normalized = normalize(parsed)
    printer.print(normalized)
  }

  private def normalize(parsed : SilSentence) : SilSentence =
  {
    parsed match {
      case SilPredicateSentence(predicate, tam, formality) => {
        tam.mood match {
          case MOOD_IMPERATIVE => SilPredicateSentence(
            predicate, tam,
            formality.copy(force = FORCE_EXCLAMATION))
          case _ => parsed
        }
      }
      case _ : SilConditionalSentence => parsed
      case _ : SilPredicateQuery => parsed
      case SilAmbiguousSentence(alternatives, _) => {
        SilAmbiguousSentence(alternatives.map(normalize))
      }
      case _ : SilConjunctiveSentence => {
        // FIXME
        parsed
      }
      case _ : SilUnknownSentence => parsed
      case _ : SilUnparsedSentence => parsed
    }
  }

  private def expectPreserved(s : String) =
  {
    normalize(s) must be equalTo s
  }

  private def expectStatement(s : String) =
  {
    normalize(s) must be equalTo (s + ".")
  }

  private def expectCommand(s : String) =
  {
    normalize(s) must be equalTo (s + "!")
  }

  private def expectQuestion(s : String) =
  {
    normalize(s) must be equalTo (s + "?")
  }

  private def expectNormalized(s : String, normalized : String) =
  {
    normalize(s) must be equalTo(normalized)
  }

  "SilSentencePrinter" should
  {
    "deal with problem cases" in
    {
      skipped("maybe one day")
      expectNormalized("a body generally has a tail",
        "a body has a tail generally.")
      expectStatement("a body has a tail generally")
      expectStatement("Salieri sends Mozart a letter angrily")
      expectQuestion("does Salieri send Mozart a letter angrily")
      expectStatement("the king is running merrily through the woods")
    }

    "deal with cases which are problematic for CoreNLP" in
    {
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not working")
      }
      expectQuestion("is neither franny nor zooey speaking")
      expectStatement("a vehicle must be either moving or stopped")
      expectPreserved("does the mule kick the ball smugly at the vase?")
      expectQuestion("does the door close")
      expectNormalized("what is Brian afraid of",
        "of what is Brian afraid?")
      expectNormalized("what is the cave south of",
        "south of what is the cave?")
      expectNormalized("Franny says \"I love you\"",
        "Franny says, \"I love you\".")
      expectStatement(
        "John eats apples; Mary eats pears")
    }

    "preserve sentences" in
    {
      expectPreserved("the door is closed.")
      expectPreserved("the door is closed!")
      expectPreserved("is the door closed?")
      expectPreserved("the door can be closed.")
      expectPreserved("can the door be closed?")
      expectPreserved("can the door not be closed?")
      expectPreserved("does the mule have the ball?")
      expectPreserved("Fred is in the cinema.")
    }

    "normalize sentences" in
    {
      expectNormalized(
        "Sandra is no longer in the hallway",
        "Sandra is not in the hallway.")
      if (SprParser.isCoreNLP) {
        expectNormalized(
          "Merry picked up the apple there",
          "Merry picked the apple up there.")
        expectNormalized(
          "Mary put down the milk",
          "Mary put the milk down.")
      } else {
        expectStatement("Merry picked up the apple there")
        expectStatement(
          "Mary put down the milk")
        expectStatement(
          "(the gray vehicle)'s engine is broken"
        )
      }
      expectNormalized("the mule kicks the ball smugly at the vase?",
        "does the mule kick the ball smugly at the vase?")
      expectNormalized("the mule has the ball?",
        "does the mule have the ball?")
      expectStatement("the door is closed")
      expectQuestion("is the door closed")
      expectQuestion("is the door not closed")
      expectNormalized(
        "is not the door closed", "is the door not closed?")
      expectNormalized(
        "isn't the door closed", "is the door not closed?")
      expectNormalized(
        "the door can be closed?", "can the door be closed?")
      expectNormalized(
        "the door can't be closed?", "can the door not be closed?")
      expectCommand("close the door")
      expectStatement("the chickens are fat")
      expectStatement("a young man goes west")
      expectStatement("I am hungry")
      expectStatement("we are hungry")
      expectStatement("you are hungry")
      expectStatement("he is hungry")
      expectStatement("they are hungry")
      expectCommand("erase them")
      expectQuestion("is his granddaughter at home")
      expectQuestion("is the server up and running")
      expectQuestion("is the server down or failing")
      expectQuestion("is franny or zooey smart")
      expectQuestion("are franny and zooey smart")
      expectQuestion("is franny or zooey speaking")
      expectQuestion("is either franny or zooey speaking")
      expectQuestion("are franny, zooey and phoebe smart")
      expectQuestion("are franny, zooey, and phoebe smart")
      expectStatement("your friend and I are hungry")
      expectStatement("your friend, Stalin, and I are hungry")
      expectStatement("the red pig, Stalin, and I are hungry")
      expectStatement("the horse is healthy, strong, and hungry")
      expectNormalized(
        "the horse is healthy, and strong",
        "the horse is healthy and strong.")
      expectStatement("the door must be open")
      expectStatement("the door may be open")
      expectStatement("the door must not be open")
      expectStatement("a door must be either open or closed")
      expectStatement("a door must be neither open nor closed")
      expectStatement("there is a door")
      expectStatement("there is a front door")
      expectStatement("there is a front door and a back door")
      expectStatement("there exists a door")
      expectStatement("a door exists")
      expectStatement("there is a door and a window")
      expectStatement("there is a door and windows")
      expectStatement("there are doors and a window")
      expectStatement("there are doors and windows")
      expectStatement("there is not a door")
      expectStatement("there must be a door")
      expectQuestion("is there a door")
      expectQuestion("is there a front door")
      expectQuestion("is there a front door or a back door")
      expectQuestion("is there not a door")
      expectQuestion("must there be a door")
      expectQuestion("must there not be a door")
      expectStatement("the window in the bathroom is open")
      expectQuestion("is the window in the bathroom open")
      expectCommand("open the window in the bathroom")
      expectQuestion("is the tiger in the cage")
      expectQuestion("is the light in the bathroom on")
      expectStatement("the light in the bathroom is on")
      expectCommand("open all windows on the first floor")
      expectNormalized("is the window open in the bathroom",
        "is the window in the bathroom open?")
      expectNormalized("is the window closed in the bathroom",
        "is the window in the bathroom closed?")
      expectNormalized("is the light on in the bathroom",
        "is the light in the bathroom on?")
      expectNormalized("is the light off in the bathroom",
        "is the light in the bathroom off?")
      expectNormalized("awake the goat on the farm.",
        "awake the goat on the farm!")
      expectQuestion("is the tiger in the big cage")
      expectQuestion("are all lights on the first floor on")
      expectStatement("the tiger has a tail")
      expectStatement("the tigers have tails")
      expectStatement("Muldoon has a tiger")
      expectStatement("Muldoon does have a tiger")
      expectQuestion("does Muldoon have a tiger")
      expectNormalized(
        "how many lights are there on the first floor",
        "how many lights on the first floor are there?")
      expectQuestion("how many lights are there")
      expectQuestion("how many lights on the first floor are there")
      expectQuestion("how many lights are on the first floor")
      expectNormalized("a person that is at home is present",
        "a person at home is present.")
      expectStatement("a jackrabbit is a kind of animal")
      expectQuestion("does a vehicle exist")
      expectStatement("a vehicle exists")
      expectStatement("a vehicle does exist")
      expectQuestion("which vehicles exist")
      expectQuestion("how many vehicles exist")
      expectStatement("a vehicle with a propellor exists")
      expectQuestion("where are the enchanted isles")
      expectQuestion("where is my mother")
      expectQuestion("who is John Galt")
      expectQuestion("who are the twin sisters")
      expectQuestion("who is there")
      expectQuestion("who exists")
      expectStatement("the door closes")
      expectStatement("the door does close")
      expectStatement("my parents close the door")
      expectStatement("Marina closes the door")
      expectQuestion("does the door open")
      expectQuestion("do my parents open the door")
      expectQuestion("does Marina open the door")
      expectNormalized(
        "Salieri sends Mozart a letter",
        "Salieri sends a letter to Mozart.")
      expectNormalized(
        "in the morning Salieri sends Mozart a letter",
        "Salieri sends a letter to Mozart in the morning.")
      expectStatement("the mule kicks the ball smugly at the vase")
      expectQuestion("does the mule eat the grass very smugly in the field")
      expectStatement("Fred is in the cinema")
      expectQuestion("is Fred in the cinema")
      expectNormalized("Fred is in the cinema?", "is Fred in the cinema?")
      expectNormalized("in free fall does a body have mass",
        "does a body have mass in free fall?")
      expectNormalized("generally does a body have mass",
        "does a body have mass generally?")
      expectNormalized("generally a body is a hadron?",
        "is a body a hadron generally?")
      expectNormalized("a body is generally a hadron?",
        "is a body a hadron generally?")
      expectNormalized("generally a body is a hadron",
        "a body is a hadron generally.")
      expectNormalized("a body is generally a hadron",
        "a body is a hadron generally.")
      expectNormalized("generally a body has a tail",
        "a body has a tail generally.")
      expectNormalized("angrily Salieri sends Mozart a letter",
        "Salieri sends a letter to Mozart angrily.")
      expectNormalized("angrily Salieri sends Mozart a letter?",
        "does Salieri send a letter to Mozart angrily?")
      expectNormalized("usually a bulb is lit?",
        "is a bulb lit usually?")
      expectNormalized("a bulb is usually lit?",
        "is a bulb lit usually?")
      expectNormalized("usually a bulb is lit",
        "a bulb is lit usually.")
      expectNormalized("a bulb is usually lit",
        "a bulb is lit usually.")
      expectStatement("a person may have an ubiety as a property")
      expectStatement("if an object moves to a location, " +
        "then the location becomes the object's container")
      expectNormalized(
        "when an object moves to a location, " +
          "the location becomes the object's container",
        "when an object moves to a location, " +
          "then the location becomes the object's container."
      )
      expectNormalized(
        "whenever an object moves to a location, " +
          "the location becomes the object's container",
        "whenever an object moves to a location, " +
          "then the location becomes the object's container."
      )
      expectNormalized(
        "after an object moves to a location, " +
          "the location becomes the object's container",
        "after an object moves to a location, " +
          "then the location becomes the object's container."
      )
      expectNormalized(
        "before an object moves to a location, " +
          "the location must not be the object's container",
        "before an object moves to a location, " +
          "then the location must not be the object's container."
      )
      expectQuestion("what is south of the cave")
      expectNormalized("after that he travelled to the lake",
        "he travelled to the lake after that.")
      expectNormalized("following that she went back to the dorm",
        "she went back to the dorm following that.")
      expectQuestion("which door must Franny open")
      expectQuestion("who opens the door")
      expectQuestion("how many pigs is Franny carrying")
      expectQuestion("how many objects are filling the wallet")
      expectStatement("Franny is carrying the pigs")
      expectQuestion("who is floating")
      expectNormalized("Ramona went to the library?",
        "did Ramona go to the library?")
      expectQuestion("did Ramona go to the library")
      expectStatement("Mortimer went to the beach yesterday")
      expectStatement("Mortimer went to the beach this morning")
      expectStatement("Mortimer went back to the beach this morning")
      expectQuestion("what did Curtis give to Andrea")
      expectQuestion("who received the bomb")
      expectNormalized("who did Curtis give the bomb to",
        "to whom did Curtis give the bomb?")
      expectQuestion("to whom did Curtis give the bomb")
      expectQuestion("to which person did Curtis give the bomb")
      expectQuestion("to how many people did Curtis give the bomb")
      expectQuestion("for whom did Curtis detonate the bomb")
      expectNormalized(
        "who did Curtis detonate the bomb for",
        "for whom did Curtis detonate the bomb?")
      expectNormalized(
        "which person did Curtis give the bomb to",
        "to which person did Curtis give the bomb?")
      expectNormalized(
        "how many people did Curtis give the bomb to",
        "to how many people did Curtis give the bomb?")
      expectStatement("the kitchen is west of the garden")
      expectNormalized(
        "otherwise the player burps",
        "the player burps otherwise.")
    }
  }

  "print compound words" in
  {
    def print(style : SilCompoundStyle) =
    {
      val ref = SilNounReference(
        SilCompoundWord(
          Seq(
            SilWord("tea"),
            SilWord("shop")
          ),
          style
        )
      )
      printer.print(ref, INFLECT_NONE, SilConjoining.NONE)
    }
    print(COMPOUND_OPEN) must be equalTo "tea shop"
    print(COMPOUND_CLOSED) must be equalTo "teashop"
    print(COMPOUND_HYPHENATED) must be equalTo "tea-shop"
  }
}
