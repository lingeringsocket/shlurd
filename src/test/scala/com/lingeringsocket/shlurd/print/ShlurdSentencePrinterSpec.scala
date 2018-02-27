// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

class ShlurdSentencePrinterSpec extends Specification
{
  private val printer = new ShlurdSentencePrinter

  private def normalize(s : String) : String =
  {
    val parsed = ShlurdParser(s).parseOne
    val normalized = normalize(parsed)
    printer.print(normalized)
  }

  private def normalize(parsed : ShlurdSentence) : ShlurdSentence =
  {
    parsed match {
      case ShlurdPredicateSentence(predicate, mood, formality) => {
        mood match {
          case MOOD_IMPERATIVE => ShlurdPredicateSentence(
            predicate, mood,
            formality.copy(force = FORCE_EXCLAMATION))
          case _ => parsed
        }
      }
      case ShlurdStateChangeCommand(predicate, formality) => {
        ShlurdStateChangeCommand(
          predicate,
          formality.copy(force = FORCE_EXCLAMATION))
      }
      case ShlurdPredicateQuery(predicate, question, mood, formality) => parsed
      case ShlurdAmbiguousSentence(alternatives) => {
        ShlurdAmbiguousSentence(alternatives.map(normalize))
      }
      case ShlurdUnknownSentence => parsed
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

  "ShlurdSentencePrinter" should
  {
    "preserve sentences" in
    {
      expectPreserved("the door is closed.")
      expectPreserved("the door is closed!")
      expectPreserved("is the door closed?")
      expectPreserved("the door can be closed.")
      expectPreserved("can the door be closed?")
      expectPreserved("can the door not be closed?")
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
      // FIXME:  either/both etc don't work here
      expectQuestion("is franny or zooey speaking")
      expectQuestion("are franny and zooey speaking")
      expectQuestion("are franny, zooey, and phoebe speaking")
      expectQuestion("are franny, zooey and phoebe speaking")
      expectQuestion("is the server up and running")
      expectQuestion("is the server down or failing")
      expectStatement("the horse is healthy, strong, and hungry")
      expectNormalized(
        "the horse is healthy, and strong",
        "the horse is healthy and strong.")
      expectStatement("your friend and I are hungry")
      expectStatement("your friend, Stalin, and I are hungry")
      expectStatement("the red pig, Stalin, and I are hungry")
      expectStatement("the door must be open")
      expectStatement("the door may be open")
      expectStatement("the door must not be open")
      expectStatement("a door must be either open or closed")
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
      // FIXME:  negated "must there NOT be a door" fails
      expectQuestion("must there be a door")
      expectStatement("the window in the bathroom is open")
      expectQuestion("is the window in the bathroom open")
      expectCommand("open the window in the bathroom")
      expectQuestion("is the tiger in the cage")
      // FIXME:  "is the grizzly bear in the cage"
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
    }
  }
}
