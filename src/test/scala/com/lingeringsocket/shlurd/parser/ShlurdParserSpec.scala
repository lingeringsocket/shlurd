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
package com.lingeringsocket.shlurd.parser

import org.specs2.mutable._

import ShlurdEnglishLemmas._

class ShlurdParserSpec extends Specification
{
  private val ENTITY_DOOR = word("door")

  private val ENTITY_PORTAL = word("portal")

  private val ENTITY_WINDOW = word("window")

  private val ENTITY_BATHROOM = word("bathroom")

  private val ENTITY_DOORS = ShlurdWord("doors", "door")

  private val ENTITY_WHO = word(LEMMA_WHO)

  private val ENTITY_FRANNY = word("franny")

  private val ENTITY_ZOOEY = word("zooey")

  private val ENTITY_MOUSE = word("mouse")

  private val ENTITY_HOME = word("home")

  private val ENTITY_GRANDDAUGHTER = word("granddaughter")

  private val ENTITY_SISTER = word("sister")

  private val STATE_OPEN = word("open")

  private val STATE_CLOSE = word("close")

  private val STATE_SHUT = word("shut")

  private val STATE_CLOSED = ShlurdWord("closed", "close")

  private val STATE_SIDEWAYS = word("sideways")

  private val STATE_HUNGRY = word("hungry")

  private val STATE_ON = word("on")

  private val STATE_OFF = word("off")

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
    "parse a state predicate statement" in
    {
      val input = "the door is open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor())
      parse(input + ".") must be equalTo
        ShlurdPredicateSentence(predDoor())
      parse(input + "!") must be equalTo
        ShlurdPredicateSentence(predDoor(),
          MOOD_INDICATIVE_POSITIVE, ShlurdFormality(FORCE_EXCLAMATION))
      parse(input + "?") must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_POSITIVE)
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

    "parse a state predicate question" in
    {
      val input = "is the door open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_POSITIVE)
      parse(input + "?") must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse an enumeration question" in
    {
      val input = "which door is open"
      val expected = ShlurdPredicateQuery(
        predDoor(STATE_OPEN, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        QUESTION_WHICH, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a who question" in
    {
      val input = "who is at home"
      val expected = ShlurdPredicateQuery(
        ShlurdStatePredicate(
          ShlurdEntityReference(ENTITY_WHO),
          ShlurdLocationState(
            LOC_AT,
            ShlurdEntityReference(ENTITY_HOME))),
        QUESTION_WHO, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a quantity question" in
    {
      val input = "how many doors are open"
      val expected = ShlurdPredicateQuery(
        pred(ENTITY_DOORS, STATE_OPEN, DETERMINER_UNSPECIFIED, COUNT_PLURAL),
        QUESTION_HOW_MANY, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a negated question" in
    {
      val input = "is not the door open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_NEGATIVE)
      parse(input + "?") must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_NEGATIVE)
    }

    "parse a negated question with contraction" in
    {
      val input = "isn't the door open"
      parse(input) must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_NEGATIVE)
      parse(input + "?") must be equalTo
        ShlurdPredicateSentence(predDoor(), MOOD_INTERROGATIVE_NEGATIVE)
    }

    "parse a command" in
    {
      val input = "open the door"
      parse(input) must be equalTo
        ShlurdStateChangeCommand(predDoor())
      parse(input + ".") must be equalTo
        ShlurdStateChangeCommand(predDoor())
      parse(input + "!") must be equalTo
        ShlurdStateChangeCommand(predDoor(), ShlurdFormality(FORCE_EXCLAMATION))
      parse(input + "?") must be equalTo
        ShlurdStateChangeCommand(predDoor())
    }

    "parse an identity statement" in
    {
      val input = "a portal is a door"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdRelationshipPredicate(
            ShlurdEntityReference(
              ENTITY_PORTAL, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            ShlurdEntityReference(
              ENTITY_DOOR, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            REL_IDENTITY
          )
        )
    }

    "lemmatize correctly" in
    {
      val command = "close the door"
      parse(command) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_CLOSE))
      val question = "is the door closed"
      parse(question) must be equalTo
        ShlurdPredicateSentence(
          predDoor(STATE_CLOSED), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse prepositional verbs" in
    {
      parse("turn the door on") must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_ON))
      parse("turn on the door") must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_ON))
      parse("turn the door off") must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OFF))
      parse("turn off the door") must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OFF))
    }

    "parse adverbial state" in
    {
      val question = "is the door sideways"
      parse(question) must be equalTo
      ShlurdPredicateSentence(
        predDoor(STATE_SIDEWAYS), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse conjunctive state" in
    {
      val conjunction = "is the door open and sideways"
      parse(conjunction) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdEntityReference(
              ENTITY_DOOR, DETERMINER_UNIQUE, COUNT_SINGULAR),
            ShlurdConjunctiveState(
              DETERMINER_ALL,
              Seq(
                ShlurdPropertyState(STATE_OPEN),
                ShlurdPropertyState(STATE_SIDEWAYS)))),
          MOOD_INTERROGATIVE_POSITIVE)
      val disjunction = "is the door either open or closed"
      parse(disjunction) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdEntityReference(
              ENTITY_DOOR, DETERMINER_UNIQUE, COUNT_SINGULAR),
            ShlurdConjunctiveState(
              DETERMINER_UNIQUE,
              Seq(
                ShlurdPropertyState(STATE_OPEN),
                ShlurdPropertyState(STATE_CLOSED)))),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse determiners" in
    {
      val inputThe = "open the door"
      parse(inputThe) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_UNIQUE))
      val inputAny = "open any door"
      parse(inputAny) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_ANY))
      val inputEither = "open either door"
      parse(inputEither) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_UNIQUE))
      val inputA = "open a door"
      parse(inputA) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_NONSPECIFIC))
      val inputSome = "open some door"
      parse(inputSome) must be equalTo
        ShlurdStateChangeCommand(predDoor(STATE_OPEN, DETERMINER_SOME))
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
          predDoor(STATE_OPEN, DETERMINER_ANY), MOOD_INTERROGATIVE_POSITIVE)
      val inputAllQ = "are all doors open"
      parse(inputAllQ) must be equalTo
        ShlurdPredicateSentence(
          pred(ENTITY_DOORS, STATE_OPEN, DETERMINER_ALL, COUNT_PLURAL),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse qualifiers" in
    {
      val inputFront = "open the front door"
      parse(inputFront) must be equalTo
        ShlurdStateChangeCommand(
          ShlurdStatePredicate(
            ShlurdReference.qualified(
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
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse locative specifiers" in
    {
      val pred = ShlurdStatePredicate(
        ShlurdStateSpecifiedReference(
          ShlurdEntityReference(
            ENTITY_WINDOW, DETERMINER_UNIQUE, COUNT_SINGULAR),
          ShlurdLocationState(
            LOC_INSIDE,
            ShlurdEntityReference(
              ENTITY_BATHROOM, DETERMINER_UNIQUE, COUNT_SINGULAR)
          )
        ),
        ShlurdPropertyState(STATE_OPEN)
      )
      parse("the window in the bathroom is open") must be equalTo
        ShlurdPredicateSentence(
          pred,
          MOOD_INDICATIVE_POSITIVE)
      parse("is the window in the bathroom open") must be equalTo
        ShlurdPredicateSentence(
          pred,
          MOOD_INTERROGATIVE_POSITIVE)
      parse("open the window in the bathroom") must be equalTo
        ShlurdStateChangeCommand(
          pred)
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
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse possessive reference" in
    {
      val input = "is franny's mouse hungry"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdGenitiveReference(
              ShlurdEntityReference(ENTITY_FRANNY),
              ShlurdEntityReference(ENTITY_MOUSE)),
            ShlurdPropertyState(STATE_HUNGRY)),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse genitive reference" in
    {
      val input = "franny is zooey's sister"
      parse(input) must be equalTo
        ShlurdPredicateSentence(
          ShlurdRelationshipPredicate(
            ShlurdEntityReference(ENTITY_FRANNY),
            ShlurdGenitiveReference(
              ShlurdEntityReference(ENTITY_ZOOEY),
              ShlurdEntityReference(ENTITY_SISTER)),
            REL_IDENTITY
          ),
          MOOD_INDICATIVE_POSITIVE)
    }

    "parse conjunctive reference" in
    {
      val inputPositive = "franny and zooey are hungry"
      parse(inputPositive) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_ALL,
              Seq(
                ShlurdEntityReference(ENTITY_FRANNY),
                ShlurdEntityReference(ENTITY_ZOOEY))),
            ShlurdPropertyState(STATE_HUNGRY)))
      val inputNegative = "neither franny nor zooey is hungry"
      parse(inputNegative) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_NONE,
              Seq(
                ShlurdEntityReference(ENTITY_FRANNY),
                ShlurdEntityReference(ENTITY_ZOOEY))),
            ShlurdPropertyState(STATE_HUNGRY)))
    }

    "parse disjunctive reference" in
    {
      // this would be more natural as part of a conditional,
      // but whatever
      val inputInclusive = "franny or zooey is hungry"
      parse(inputInclusive) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_ANY,
              Seq(
                ShlurdEntityReference(ENTITY_FRANNY),
                ShlurdEntityReference(ENTITY_ZOOEY))),
            ShlurdPropertyState(STATE_HUNGRY)))

      // FIXME:  in this context, should really be DETERMINER_ANY
      // instead of DETERMINER_UNIQUE
      val inputExclusive = "either franny or zooey is hungry"
      parse(inputExclusive) must be equalTo
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_UNIQUE,
              Seq(
                ShlurdEntityReference(ENTITY_FRANNY),
                ShlurdEntityReference(ENTITY_ZOOEY))),
            ShlurdPropertyState(STATE_HUNGRY)))
    }

    "parse modals" in
    {
      parse("The door must be open") must be equalTo(
        ShlurdPredicateSentence(
          predDoor(), ShlurdIndicativeMood(true, MODAL_MUST)))
      parse("Must the door be open") must be equalTo(
        ShlurdPredicateSentence(
          predDoor(), ShlurdInterrogativeMood(true, MODAL_MUST)))
      parse("The door may be open") must be equalTo(
        ShlurdPredicateSentence(
          predDoor(), ShlurdIndicativeMood(true, MODAL_MAY)))
      parse("The door must not be open") must be equalTo(
        ShlurdPredicateSentence(
          predDoor(), ShlurdIndicativeMood(false, MODAL_MUST)))
      parse("Mustn't the door be open") must be equalTo(
        ShlurdPredicateSentence(
          predDoor(), ShlurdInterrogativeMood(false, MODAL_MUST)))
    }

    "parse existence" in
    {
      val doorExistencePred = ShlurdStatePredicate(
        ShlurdEntityReference(ENTITY_DOOR, DETERMINER_NONSPECIFIC),
        ShlurdExistenceState())

      parse("There is a door") must be equalTo(
        ShlurdPredicateSentence(doorExistencePred))
      parse("There exists a door") must be equalTo(
        ShlurdPredicateSentence(doorExistencePred))
      parse("There is not a door") must be equalTo(
        ShlurdPredicateSentence(
          doorExistencePred,
          ShlurdIndicativeMood(false)))
      parse("There must be a door") must be equalTo(
        ShlurdPredicateSentence(
          doorExistencePred,
          ShlurdIndicativeMood(true, MODAL_MUST)))
      parse("There is a door?") must be equalTo(
        ShlurdPredicateSentence(
          doorExistencePred,
          ShlurdInterrogativeMood(true)))
      parse("Is there a door") must be equalTo(
        ShlurdPredicateSentence(
          doorExistencePred,
          ShlurdInterrogativeMood(true)))
      parse("Must there be a door") must be equalTo(
        ShlurdPredicateSentence(
          doorExistencePred,
          ShlurdInterrogativeMood(true, MODAL_MUST)))

      parse("There is a front door") must be equalTo(
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdReference.qualified(
              ShlurdEntityReference(ENTITY_DOOR, DETERMINER_NONSPECIFIC),
              Seq(QUALIFIER_FRONT)),
            ShlurdExistenceState())))

      val doorPlusWindow = Seq(
        ShlurdEntityReference(ENTITY_DOOR, DETERMINER_NONSPECIFIC),
        ShlurdEntityReference(ENTITY_WINDOW, DETERMINER_NONSPECIFIC))
      parse("There is a door and a window") must be equalTo(
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_ALL,
              doorPlusWindow),
            ShlurdExistenceState())))
      parse("Is there a door or a window") must be equalTo(
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdConjunctiveReference(
              DETERMINER_ANY,
              doorPlusWindow),
            ShlurdExistenceState()),
          ShlurdInterrogativeMood(true)))
    }

    "parse relative clauses" in
    {
      parse("a door that is shut is closed") must be equalTo(
        ShlurdPredicateSentence(
          ShlurdStatePredicate(
            ShlurdReference.qualified(
              ShlurdEntityReference(ENTITY_DOOR, DETERMINER_NONSPECIFIC),
              Seq(STATE_SHUT)),
            ShlurdPropertyState(STATE_CLOSED))))
    }

    "give up" in
    {
      val inputUnspecified = "colorless green ideas slumber furiously"
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
