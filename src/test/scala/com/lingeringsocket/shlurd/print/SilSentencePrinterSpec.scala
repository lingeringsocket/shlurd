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
package com.lingeringsocket.shlurd.print

import org.specs2.mutable._

import com.lingeringsocket.shlurd.parser._

class SilSentencePrinterSpec extends Specification
{
  private val printer = new SilSentencePrinter

  private def normalize(s : String) : String =
  {
    val parsed = ShlurdParser(s).parseOne
    val normalized = normalize(parsed)
    printer.print(normalized)
  }

  private def normalize(parsed : SilSentence) : SilSentence =
  {
    parsed match {
      case SilPredicateSentence(predicate, mood, formality) => {
        mood match {
          case MOOD_IMPERATIVE => SilPredicateSentence(
            predicate, mood,
            formality.copy(force = FORCE_EXCLAMATION))
          case _ => parsed
        }
      }
      case SilStateChangeCommand(predicate, changeVerb, formality) => {
        SilStateChangeCommand(
          predicate,
          changeVerb,
          formality.copy(force = FORCE_EXCLAMATION))
      }
      case SilPredicateQuery(predicate, question, mood, formality) => parsed
      case SilAmbiguousSentence(alternatives, _) => {
        SilAmbiguousSentence(alternatives.map(normalize))
      }
      case _ : SilConjunctiveSentence => {
        // FIXME
        parsed
      }
      case _ : SilUnknownSentence => parsed
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
      expectQuestion("is neither franny nor zooey speaking")
      expectStatement("a vehicle must be either moving or stopped")
      expectQuestion("does the door close")
      expectPreserved("does the mule kick the ball smugly at the vase?")
      expectNormalized("a body generally has a tail",
        "a body has a tail generally.")
      expectStatement("a body has a tail generally")
      expectStatement("Scalieri sends Mozart a letter angrily")
      expectQuestion("does Scalieri send Mozart a letter angrily")
    }

    "preserve sentences" in
    {
      expectPreserved("the door is closed.")
      expectPreserved("the door is closed!")
      expectPreserved("is the door closed?")
      expectPreserved("the door can be closed.")
      expectPreserved("can the door be closed?")
      expectPreserved("can the door not be closed?")
      expectPreserved("the mule has the ball?")
      expectPreserved("the mule kicks the ball smugly at the vase?")
      expectPreserved("does the mule have the ball?")
      expectPreserved("Fred is in the cinema.")
    }

    "normalize sentences" in
    {
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
      expectNormalized("there exists a door", "there is a door.")
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
      expectNormalized("does a vehicle exist", "does there exist a vehicle?")
      expectNormalized("a vehicle exists", "there is a vehicle.")
      expectNormalized("a vehicle does exist", "there does exist a vehicle.")
      expectNormalized("which vehicles exist", "which vehicles are there?")
      expectNormalized("how many vehicles exist",
        "how many vehicles are there?")
      expectNormalized("a vehicle with a propellor exists",
        "there is a vehicle with a propellor.")
      expectNormalized("who exists", "who is there?")
      expectStatement("the door closes")
      expectStatement("the door does close")
      expectStatement("my parents close the door")
      expectStatement("Marina closes the door")
      expectQuestion("does the door open")
      expectQuestion("do my parents open the door")
      expectQuestion("does Marina open the door")
      expectNormalized(
        "Scalieri sends Mozart a letter",
        "Scalieri sends a letter to Mozart.")
      expectNormalized(
        "in the morning Scalieri sends Mozart a letter",
        "Scalieri sends a letter to Mozart in the morning.")
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
      expectNormalized("angrily Scalieri sends Mozart a letter",
        "Scalieri sends a letter to Mozart angrily.")
      expectNormalized("angrily Scalieri sends Mozart a letter?",
        "Scalieri sends a letter to Mozart angrily?")
      expectNormalized("usually a bulb is lit?",
        "is a bulb lit usually?")
      expectNormalized("a bulb is usually lit?",
        "is a bulb lit usually?")
      expectNormalized("usually a bulb is lit",
        "a bulb is lit usually.")
      expectNormalized("a bulb is usually lit",
        "a bulb is lit usually.")
      expectStatement("a person may have a personal_presence as a property")
    }
  }
}
