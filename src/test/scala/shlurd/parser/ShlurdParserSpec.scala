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
package shlurd.parser

import org.specs2.mutable._

class ShlurdParserSpec extends Specification
{
  private val ENTITY_DOOR = ShlurdWord("door", "door")

  private val ENTITY_DOORS = ShlurdWord("doors", "door")

  private val ENTITY_FRONT_DOOR = ShlurdWord("front door", "front door")

  private val ENTITY_FRANNY = ShlurdWord("franny", "franny")

  private val ENTITY_HOME = ShlurdWord("home", "home")

  private val STATE_OPEN = ShlurdWord("open", "open")

  private val STATE_CLOSE = ShlurdWord("close", "close")

  private val STATE_CLOSED = ShlurdWord("closed", "close")

  private val STATE_SIDEWAYS = ShlurdWord("sideways", "sideways")

  private def pred(
    subject : ShlurdWord,
    state : ShlurdWord = STATE_OPEN,
    quantifier : ShlurdQuantifier = QUANT_SPECIFIC,
    count : ShlurdCount = COUNT_SINGULAR) =
  {
    ShlurdStatePredicate(
      ShlurdEntityReference(subject, quantifier, count),
      ShlurdPropertyState(state))
  }

  private def predDoor(
    state : ShlurdWord = STATE_OPEN,
    quantifier : ShlurdQuantifier = QUANT_SPECIFIC,
    count : ShlurdCount = COUNT_SINGULAR) =
  {
    pred(ENTITY_DOOR, state, quantifier, count)
  }

  "ShlurdParser" should
  {
    "parse a statement" in
    {
      val input = "the door is open"
      ShlurdParser(input).parse must be equalTo
        ShlurdPredicateStatement(predDoor())
      ShlurdParser(input + ".").parse must be equalTo
        ShlurdPredicateStatement(predDoor())
      ShlurdParser(input + "!").parse must be equalTo
        ShlurdPredicateStatement(predDoor())
      ShlurdParser(input + "?").parse must be equalTo
        ShlurdPredicateQuestion(predDoor())
    }

    "parse a question" in
    {
      val input = "is the door open"
      ShlurdParser(input).parse must be equalTo
        ShlurdPredicateQuestion(predDoor())
      ShlurdParser(input + "?").parse must be equalTo
        ShlurdPredicateQuestion(predDoor())
    }

    "parse a command" in
    {
      val input = "open the door"
      ShlurdParser(input).parse must be equalTo
        ShlurdStateChangeCommand(predDoor())
      ShlurdParser(input + ".").parse must be equalTo
        ShlurdStateChangeCommand(predDoor())
      ShlurdParser(input + "!").parse must be equalTo
        ShlurdStateChangeCommand(predDoor())
      ShlurdParser(input + "?").parse must be equalTo
        ShlurdStateChangeCommand(predDoor())
    }

    "lemmatize correctly" in
    {
      val command = "close the door"
      ShlurdParser(command).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_CLOSE))
      val question = "is the door closed"
      ShlurdParser(question).parse must be equalTo
        ShlurdPredicateQuestion(predDoor(STATE_CLOSED))
    }

    "parse adverbial state" in
    {
      val question = "is the door sideways"
      ShlurdParser(question).parse must be equalTo
        ShlurdPredicateQuestion(predDoor(STATE_SIDEWAYS))
    }

    "parse quantifiers" in
    {
      val inputThe = "open the door"
      ShlurdParser(inputThe).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, QUANT_SPECIFIC))
      val inputAny = "open any door"
      ShlurdParser(inputAny).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, QUANT_ANY))
      val inputA = "open a door"
      ShlurdParser(inputA).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, QUANT_ANY))
      val inputSome = "open some door"
      ShlurdParser(inputSome).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, QUANT_ANY))
      val inputAll = "open all doors"
      ShlurdParser(inputAll).parse must be equalTo
        ShlurdStateChangeCommand(
          pred(ENTITY_DOORS, STATE_OPEN, QUANT_ALL, COUNT_PLURAL))
      val inputNone = "open no door"
      ShlurdParser(inputNone).parse must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, QUANT_NONE))

      val inputAnyQ = "is any door open"
      ShlurdParser(inputAnyQ).parse must be equalTo
        ShlurdPredicateQuestion(predDoor(STATE_OPEN, QUANT_ANY))
    }

    "parse qualifiers" in
    {
      val inputFront = "open the front door"
      ShlurdParser(inputFront).parse must be equalTo
        ShlurdStateChangeCommand(
          pred(ENTITY_FRONT_DOOR, STATE_OPEN, QUANT_SPECIFIC))
    }

    "parse locatives" in
    {
      val input = "is franny at home"
      ShlurdParser(input).parse must be equalTo
        ShlurdPredicateQuestion(
          ShlurdStatePredicate(
            ShlurdEntityReference(ENTITY_FRANNY),
            ShlurdLocationState(
              LOC_AT,
              ShlurdEntityReference(ENTITY_HOME))))
    }

    "give up" in
    {
      val inputUnspecified = "open door"
      ShlurdParser(inputUnspecified).parse must be equalTo
      ShlurdUnknownSentence
    }
  }
}
