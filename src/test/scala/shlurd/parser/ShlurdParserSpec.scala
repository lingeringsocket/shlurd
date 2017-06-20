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
  private val ENTITY_DOOR = word("door")

  private val ENTITY_DOORS = ShlurdWord("doors", "door")

  private val ENTITY_FRANNY = word("franny")

  private val ENTITY_HOME = word("home")

  private val ENTITY_GRANDDAUGHTER = word("granddaughter")

  private val STATE_OPEN = word("open")

  private val STATE_CLOSE = word("close")

  private val STATE_CLOSED = ShlurdWord("closed", "close")

  private val STATE_SIDEWAYS = word("sideways")

  private val STATE_HUNGRY = word("hungry")

  private val QUALIFIER_FRONT = word("front")

  private def word(s : String) = ShlurdWord(s, s)

  private def pred(
    subject : ShlurdWord,
    state : ShlurdWord = STATE_OPEN,
    determiner : ShlurdDeterminer = DETERMINER_UNIQUE,
    count : ShlurdCount = COUNT_SINGULAR) =
  {
    ShlurdStatePredicate(
      ShlurdEntityReference(subject, determiner, count),
      ShlurdPropertyState(state))
  }

  private def predDoor(
    state : ShlurdWord = STATE_OPEN,
    determiner : ShlurdDeterminer = DETERMINER_UNIQUE,
    count : ShlurdCount = COUNT_SINGULAR) =
  {
    pred(ENTITY_DOOR, state, determiner, count)
  }

  private def parse(input : String) = ShlurdParser(input).parseOne

  "ShlurdParser" should
  {
    "parse a statement" in
    {
      val input = "the door is open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor())
      parse(input + ".") must be equalTo
        ShlurdPredicateSentence(predDoor())
      parse(input + "!") must be equalTo
        ShlurdPredicateSentence(predDoor())
      parse(input + "?") must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE)
    }

    "parse a negation" in
    {
      val input = "the door is not open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INDICATIVE_NEGATIVE)
      val contracted = "the door isn't open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INDICATIVE_NEGATIVE)
    }

    "parse a question" in
    {
      val input = "is the door open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE)
      parse(input + "?") must be equalTo
      ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE)
    }

    "parse a command" in
    {
      val input = "open the door"
      parse(input) must be equalTo
        ShlurdStateChangeCommand(predDoor())
      parse(input + ".") must be equalTo
        ShlurdStateChangeCommand(predDoor())
      parse(input + "!") must be equalTo
        ShlurdStateChangeCommand(predDoor())
      parse(input + "?") must be equalTo
        ShlurdStateChangeCommand(predDoor())
    }

    "lemmatize correctly" in
    {
      val command = "close the door"
      parse(command) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_CLOSE))
      val question = "is the door closed"
      parse(question) must be equalTo
        ShlurdPredicateSentence(predDoor(STATE_CLOSED), MOOD_INTERROGATIVE)
    }

    "parse adverbial state" in
    {
      val question = "is the door sideways"
      parse(question) must be equalTo
        ShlurdPredicateSentence(predDoor(STATE_SIDEWAYS), MOOD_INTERROGATIVE)
    }

    "parse determiners" in
    {
      val inputThe = "open the door"
      parse(inputThe) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_UNIQUE))
      val inputAny = "open any door"
      parse(inputAny) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_ANY))
      val inputA = "open a door"
      parse(inputA) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_NONSPECIFIC))
      val inputSome = "open some door"
      parse(inputSome) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_ANY))
      val inputAll = "open all doors"
      parse(inputAll) must be equalTo
        ShlurdStateChangeCommand(
          pred(ENTITY_DOORS, STATE_OPEN, DETERMINER_ALL, COUNT_PLURAL))
      val inputNone = "open no door"
      parse(inputNone) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_NONE))

      val inputAnyQ = "is any door open"
      parse(inputAnyQ) must be equalTo
        ShlurdPredicateSentence(
          predDoor(STATE_OPEN, DETERMINER_ANY), MOOD_INTERROGATIVE)
    }

    "parse qualifiers" in
    {
      val inputFront = "open the front door"
      parse(inputFront) must be equalTo
        ShlurdStateChangeCommand(
          ShlurdStatePredicate(
            ShlurdQualifiedReference(
              ShlurdEntityReference(ENTITY_DOOR, DETERMINER_UNIQUE),
              Seq(QUALIFIER_FRONT)),
            ShlurdPropertyState(STATE_OPEN)))
    }

    "parse locatives" in
    {
      val input = "is franny at home"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdEntityReference(ENTITY_FRANNY),
            ShlurdLocationState(
              LOC_AT,
              ShlurdEntityReference(ENTITY_HOME))),
          MOOD_INTERROGATIVE)
    }

    "parse pronouns" in
    {
      val input = "I am hungry"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR),
            ShlurdPropertyState(STATE_HUNGRY)))
    }

    "parse possessive pronouns" in
    {
      val input = "is his granddaughter at home"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdGenitiveReference(
              ShlurdPronounReference(PERSON_THIRD, GENDER_M, COUNT_SINGULAR),
              ShlurdEntityReference(ENTITY_GRANDDAUGHTER)),
            ShlurdLocationState(
              LOC_AT,
              ShlurdEntityReference(ENTITY_HOME))),
          MOOD_INTERROGATIVE)
    }

    "give up" in
    {
      val inputUnspecified = "open door"
      val result = parse(inputUnspecified)
      result must be equalTo ShlurdUnknownSentence
      result.hasUnknown must beTrue
    }

    "deal with unknowns" in
    {
      predDoor().hasUnknown must beFalse
      ShlurdUnknownPredicate.hasUnknown must beTrue
      ShlurdPredicateSentence(predDoor()).hasUnknown must beFalse
      ShlurdPredicateSentence(ShlurdUnknownPredicate).hasUnknown must beTrue
    }
  }
}
