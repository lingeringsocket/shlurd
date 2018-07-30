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
package com.lingeringsocket.shlurd.parser

import org.specs2.mutable._

import ShlurdEnglishLemmas._
import ShlurdPennTreebankLabels._

class ShlurdParserSpec extends Specification
{
  private val NOUN_DOOR = SilWord("door")

  private val NOUN_PORTAL = SilWord("portal")

  private val NOUN_WINDOW = SilWord("window")

  private val NOUN_BATHROOM = SilWord("bathroom")

  private val NOUN_DOORS = SilWord("doors", "door")

  private val NOUN_PIGS = SilWord("pigs", "pig")

  private val NOUN_WHO = SilWord(LEMMA_WHO)

  private val NOUN_FRANNY = SilWord("Franny")

  private val NOUN_ZOOEY = SilWord("Zooey")

  private val NOUN_MOUSE = SilWord("mouse")

  private val NOUN_HOME = SilWord("home")

  private val NOUN_GRANDDAUGHTER = SilWord("granddaughter")

  private val NOUN_SISTER = SilWord("sister")

  private val STATE_OPEN = SilWord("open")

  private val ACTION_OPENS = SilWord("opens", "open")

  private val ACTION_OPEN = SilWord("open")

  private val STATE_CLOSE = SilWord("close")

  private val ACTION_CARRYING = SilWord("carrying", "carry")

  private val STATE_SHUT = SilWord("shut")

  private val STATE_CLOSED = SilWord("closed", "close")

  private val STATE_SIDEWAYS = SilWord("sideways")

  private val STATE_HUNGRY = SilWord("hungry")

  private val STATE_ON = SilWord("on")

  private val STATE_OFF = SilWord("off")

  private val QUALIFIER_FRONT = SilWord("front")

  private val VERB_TURN = SilWord("turn")

  private def predTransitiveAction(
    subject : SilWord,
    action : SilWord = ACTION_OPENS,
    directObject : SilWord = NOUN_DOOR,
    determiner : SilDeterminer = DETERMINER_UNIQUE,
    count : SilCount = COUNT_SINGULAR) =
  {
    SilActionPredicate(
      SilNounReference(subject),
      action,
      Some(SilNounReference(directObject, determiner, count)))
  }

  private def predIntransitiveAction(
    subject : SilWord,
    action : SilWord = ACTION_OPENS,
    determiner : SilDeterminer = DETERMINER_UNIQUE,
    count : SilCount = COUNT_SINGULAR) =
  {
    SilActionPredicate(
      SilNounReference(subject, determiner, count),
      action)
  }

  private def predState(
    subject : SilWord,
    state : SilWord = STATE_OPEN,
    determiner : SilDeterminer = DETERMINER_UNIQUE,
    count : SilCount = COUNT_SINGULAR) =
  {
    SilStatePredicate(
      SilNounReference(subject, determiner, count),
      SilPropertyState(state))
  }

  private def predStateDoor(
    state : SilWord = STATE_OPEN,
    determiner : SilDeterminer = DETERMINER_UNIQUE,
    count : SilCount = COUNT_SINGULAR) =
  {
    predState(NOUN_DOOR, state, determiner, count)
  }

  private def parse(input : String) = ShlurdParser(input).parseOne

  private def leaf(s : String) = ShlurdSyntaxLeaf(s, s, s)

  private def leafCapitalized(s : String) = ShlurdSyntaxLeaf(
    ShlurdParseUtils.capitalize(s), s, ShlurdParseUtils.capitalize(s))

  "ShlurdParser" should
  {
    "parse a state predicate statement" in
    {
      val input = "the door is open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor())
      parse(input + ".") must be equalTo
        SilPredicateSentence(predStateDoor())
      parse(input + "!") must be equalTo
        SilPredicateSentence(predStateDoor(),
          MOOD_INDICATIVE_POSITIVE, SilFormality(FORCE_EXCLAMATION))
      parse(input + "?") must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse a transitive action predicate statement" in
    {
      val input = "Franny opens the door"
      parse(input) must be equalTo
        SilPredicateSentence(predTransitiveAction(NOUN_FRANNY))
      parse(input + "?") must be equalTo
        SilPredicateSentence(predTransitiveAction(NOUN_FRANNY),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse an intransitive action predicate statement" in
    {
      val input = "The door opens"
      parse(input) must be equalTo
        SilPredicateSentence(predIntransitiveAction(NOUN_DOOR))
      parse(input + "?") must be equalTo
        SilPredicateSentence(predIntransitiveAction(NOUN_DOOR),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse a negation" in
    {
      val input = "the door is not open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INDICATIVE_NEGATIVE)
      val contracted = "the door isn't open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INDICATIVE_NEGATIVE)
    }

    "parse a state predicate question" in
    {
      val input = "is the door open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_POSITIVE)
      parse(input + "?") must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse a nominative enumeration question" in
    {
      val input = "which door is open"
      val expected = SilPredicateQuery(
        predStateDoor(STATE_OPEN, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        QUESTION_WHICH, INFLECT_NOMINATIVE, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse an accusative enumeration question" in
    {
      val input = "which door must Franny open"
      val expected = SilPredicateQuery(
        predTransitiveAction(
          NOUN_FRANNY, ACTION_OPEN, NOUN_DOOR, DETERMINER_UNSPECIFIED),
        QUESTION_WHICH, INFLECT_ACCUSATIVE,
        SilInterrogativeMood(true, MODAL_MUST))
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a who identity question" in
    {
      val input = "who is at home"
      val expected = SilPredicateQuery(
        SilStatePredicate(
          SilNounReference(NOUN_WHO),
          SilAdpositionalState(
            SilAdposition.AT,
            SilNounReference(NOUN_HOME))),
        QUESTION_WHO, INFLECT_NOMINATIVE, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a who nominative question" in
    {
      val input = "who opens the door"
      val expected = SilPredicateQuery(
        predTransitiveAction(NOUN_WHO),
        QUESTION_WHO, INFLECT_NOMINATIVE, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a quantity question" in
    {
      val input = "how many doors are open"
      val expected = SilPredicateQuery(
        predState(NOUN_DOORS, STATE_OPEN, DETERMINER_UNSPECIFIED, COUNT_PLURAL),
        QUESTION_HOW_MANY, INFLECT_NOMINATIVE, MOOD_INTERROGATIVE_POSITIVE)
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse an accusative progressive quantity question" in
    {
      val input = "how many pigs is Franny carrying"
      val expected = SilPredicateQuery(
        predTransitiveAction(
          NOUN_FRANNY, ACTION_CARRYING, NOUN_PIGS,
          DETERMINER_UNSPECIFIED, COUNT_PLURAL),
        QUESTION_HOW_MANY, INFLECT_ACCUSATIVE,
        SilInterrogativeMood(true, MODAL_PROGRESSIVE))
      parse(input) must be equalTo expected
      parse(input + "?") must be equalTo expected
    }

    "parse a negated question" in
    {
      val input = "is not the door open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_NEGATIVE)
      parse(input + "?") must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_NEGATIVE)
    }

    "parse a negated question with contraction" in
    {
      val input = "isn't the door open"
      parse(input) must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_NEGATIVE)
      parse(input + "?") must be equalTo
        SilPredicateSentence(predStateDoor(), MOOD_INTERROGATIVE_NEGATIVE)
    }

    "parse a command" in
    {
      val input = "open the door"
      parse(input) must be equalTo
        SilStateChangeCommand(predStateDoor())
      parse(input + ".") must be equalTo
        SilStateChangeCommand(predStateDoor())
      parse(input + "!") must be equalTo
        SilStateChangeCommand(predStateDoor(),
          None,
          SilFormality(FORCE_EXCLAMATION))
      parse(input + "?") must be equalTo
        SilStateChangeCommand(predStateDoor())
    }

    "parse an identity statement" in
    {
      val input = "a portal is a door"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilRelationshipPredicate(
            SilNounReference(
              NOUN_PORTAL, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            SilNounReference(
              NOUN_DOOR, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            REL_IDENTITY
          )
        )
    }

    "lemmatize correctly" in
    {
      val command = "close the door"
      parse(command) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_CLOSE))
      val question = "is the door closed"
      parse(question) must be equalTo
        SilPredicateSentence(
          predStateDoor(STATE_CLOSED), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse adpositional verbs" in
    {
      parse("turn the door on") must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_ON), Some(VERB_TURN))
      parse("turn on the door") must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_ON), Some(VERB_TURN))
      parse("turn the door off") must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OFF), Some(VERB_TURN))
      parse("turn off the door") must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OFF), Some(VERB_TURN))
    }

    "parse adverbial state" in
    {
      val question = "is the door sideways"
      parse(question) must be equalTo
      SilPredicateSentence(
        predStateDoor(STATE_SIDEWAYS), MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse conjunctive state" in
    {
      val conjunction = "is the door open and sideways"
      parse(conjunction) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilNounReference(
              NOUN_DOOR, DETERMINER_UNIQUE, COUNT_SINGULAR),
            SilConjunctiveState(
              DETERMINER_ALL,
              Seq(
                SilPropertyState(STATE_OPEN),
                SilPropertyState(STATE_SIDEWAYS)))),
          MOOD_INTERROGATIVE_POSITIVE)
      val disjunction = "is the door either open or closed"
      parse(disjunction) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilNounReference(
              NOUN_DOOR, DETERMINER_UNIQUE, COUNT_SINGULAR),
            SilConjunctiveState(
              DETERMINER_UNIQUE,
              Seq(
                SilPropertyState(STATE_OPEN),
                SilPropertyState(STATE_CLOSED)))),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse determiners" in
    {
      val inputThe = "open the door"
      parse(inputThe) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_UNIQUE))
      val inputAny = "open any door"
      parse(inputAny) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_ANY))
      val inputEither = "open either door"
      parse(inputEither) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_UNIQUE))
      val inputA = "open a door"
      parse(inputA) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_NONSPECIFIC))
      val inputSome = "open some door"
      parse(inputSome) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_SOME))
      val inputAll = "open all doors"
      parse(inputAll) must be equalTo
        SilStateChangeCommand(
          predState(NOUN_DOORS, STATE_OPEN, DETERMINER_ALL, COUNT_PLURAL))
      val inputNone = "open no door"
      parse(inputNone) must be equalTo
        SilStateChangeCommand(predStateDoor(STATE_OPEN, DETERMINER_NONE))

      val inputAnyQ = "is any door open"
      parse(inputAnyQ) must be equalTo
        SilPredicateSentence(
          predStateDoor(STATE_OPEN, DETERMINER_ANY),
          MOOD_INTERROGATIVE_POSITIVE)
      val inputAllQ = "are all doors open"
      parse(inputAllQ) must be equalTo
        SilPredicateSentence(
          predState(NOUN_DOORS, STATE_OPEN, DETERMINER_ALL, COUNT_PLURAL),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse qualifiers" in
    {
      val inputFront = "open the front door"
      parse(inputFront) must be equalTo
        SilStateChangeCommand(
          SilStatePredicate(
            SilReference.qualified(
              SilNounReference(NOUN_DOOR, DETERMINER_UNIQUE),
              Seq(QUALIFIER_FRONT)),
            SilPropertyState(STATE_OPEN)))
    }

    "parse adpositions" in
    {
      val input = "is Franny at home"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilNounReference(NOUN_FRANNY),
            SilAdpositionalState(
              SilAdposition.AT,
              SilNounReference(NOUN_HOME))),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse adposition specifiers" in
    {
      val pred = SilStatePredicate(
        SilStateSpecifiedReference(
          SilNounReference(
            NOUN_WINDOW, DETERMINER_UNIQUE, COUNT_SINGULAR),
          SilAdpositionalState(
            SilAdposition.IN,
            SilNounReference(
              NOUN_BATHROOM, DETERMINER_UNIQUE, COUNT_SINGULAR)
          )
        ),
        SilPropertyState(STATE_OPEN)
      )
      parse("the window in the bathroom is open") must be equalTo
        SilPredicateSentence(
          pred,
          MOOD_INDICATIVE_POSITIVE)
      parse("is the window in the bathroom open") must be equalTo
        SilPredicateSentence(
          pred,
          MOOD_INTERROGATIVE_POSITIVE)
      parse("open the window in the bathroom") must be equalTo
        SilStateChangeCommand(
          pred)
    }

    "parse pronouns" in
    {
      val input = "I am hungry"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilPronounReference(PERSON_FIRST, GENDER_N, COUNT_SINGULAR),
            SilPropertyState(STATE_HUNGRY)))
    }

    "parse possessive pronouns" in
    {
      val input = "is his granddaughter at home"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilGenitiveReference(
              SilPronounReference(PERSON_THIRD, GENDER_M, COUNT_SINGULAR),
              SilNounReference(NOUN_GRANDDAUGHTER)),
            SilAdpositionalState(
              SilAdposition.AT,
              SilNounReference(NOUN_HOME))),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse possessive reference" in
    {
      val input = "is Franny's mouse hungry"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilGenitiveReference(
              SilNounReference(NOUN_FRANNY),
              SilNounReference(NOUN_MOUSE)),
            SilPropertyState(STATE_HUNGRY)),
          MOOD_INTERROGATIVE_POSITIVE)
    }

    "parse genitive reference" in
    {
      val input = "Franny is Zooey's sister"
      parse(input) must be equalTo
        SilPredicateSentence(
          SilRelationshipPredicate(
            SilNounReference(NOUN_FRANNY),
            SilGenitiveReference(
              SilNounReference(NOUN_ZOOEY),
              SilNounReference(NOUN_SISTER)),
            REL_IDENTITY
          ),
          MOOD_INDICATIVE_POSITIVE)
    }

    "parse conjunctive reference" in
    {
      val inputPositive = "Franny and Zooey are hungry"
      parse(inputPositive) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_ALL,
              Seq(
                SilNounReference(NOUN_FRANNY),
                SilNounReference(NOUN_ZOOEY))),
            SilPropertyState(STATE_HUNGRY)))
      val inputNegative = "neither Franny nor Zooey is hungry"
      parse(inputNegative) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_NONE,
              Seq(
                SilNounReference(NOUN_FRANNY),
                SilNounReference(NOUN_ZOOEY))),
            SilPropertyState(STATE_HUNGRY)))
    }

    "parse disjunctive reference" in
    {
      // this would be more natural as part of a conditional,
      // but whatever
      val inputInclusive = "Franny or Zooey is hungry"
      parse(inputInclusive) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_ANY,
              Seq(
                SilNounReference(NOUN_FRANNY),
                SilNounReference(NOUN_ZOOEY))),
            SilPropertyState(STATE_HUNGRY)))

      // FIXME:  in this context, should really be DETERMINER_ANY
      // instead of DETERMINER_UNIQUE
      val inputExclusive = "either Franny or Zooey is hungry"
      parse(inputExclusive) must be equalTo
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_UNIQUE,
              Seq(
                SilNounReference(NOUN_FRANNY),
                SilNounReference(NOUN_ZOOEY))),
            SilPropertyState(STATE_HUNGRY)))
    }

    "parse modals" in
    {
      parse("The door must be open") must be equalTo(
        SilPredicateSentence(
          predStateDoor(), SilIndicativeMood(true, MODAL_MUST)))
      parse("Must the door be open") must be equalTo(
        SilPredicateSentence(
          predStateDoor(), SilInterrogativeMood(true, MODAL_MUST)))
      parse("The door may be open") must be equalTo(
        SilPredicateSentence(
          predStateDoor(), SilIndicativeMood(true, MODAL_MAY)))
      parse("The door must not be open") must be equalTo(
        SilPredicateSentence(
          predStateDoor(), SilIndicativeMood(false, MODAL_MUST)))
      parse("Mustn't the door be open") must be equalTo(
        SilPredicateSentence(
          predStateDoor(), SilInterrogativeMood(false, MODAL_MUST)))
    }

    "parse existence" in
    {
      val doorExistencePred = SilStatePredicate(
        SilNounReference(NOUN_DOOR, DETERMINER_NONSPECIFIC),
        SilExistenceState())

      parse("There is a door") must be equalTo(
        SilPredicateSentence(doorExistencePred))
      parse("There exists a door") must be equalTo(
        SilPredicateSentence(doorExistencePred))
      parse("There is not a door") must be equalTo(
        SilPredicateSentence(
          doorExistencePred,
          SilIndicativeMood(false)))
      parse("There must be a door") must be equalTo(
        SilPredicateSentence(
          doorExistencePred,
          SilIndicativeMood(true, MODAL_MUST)))
      parse("There is a door?") must be equalTo(
        SilPredicateSentence(
          doorExistencePred,
          SilInterrogativeMood(true)))
      parse("Is there a door") must be equalTo(
        SilPredicateSentence(
          doorExistencePred,
          SilInterrogativeMood(true)))
      parse("Must there be a door") must be equalTo(
        SilPredicateSentence(
          doorExistencePred,
          SilInterrogativeMood(true, MODAL_MUST)))

      parse("There is a front door") must be equalTo(
        SilPredicateSentence(
          SilStatePredicate(
            SilReference.qualified(
              SilNounReference(NOUN_DOOR, DETERMINER_NONSPECIFIC),
              Seq(QUALIFIER_FRONT)),
            SilExistenceState())))

      val doorPlusWindow = Seq(
        SilNounReference(NOUN_DOOR, DETERMINER_NONSPECIFIC),
        SilNounReference(NOUN_WINDOW, DETERMINER_NONSPECIFIC))
      parse("There is a door and a window") must be equalTo(
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_ALL,
              doorPlusWindow),
            SilExistenceState())))
      parse("Is there a door or a window") must be equalTo(
        SilPredicateSentence(
          SilStatePredicate(
            SilConjunctiveReference(
              DETERMINER_ANY,
              doorPlusWindow),
            SilExistenceState()),
          SilInterrogativeMood(true)))
    }

    "parse relative clauses" in
    {
      parse("a door that is shut is closed") must be equalTo(
        SilPredicateSentence(
          SilStatePredicate(
            SilReference.qualified(
              SilNounReference(NOUN_DOOR, DETERMINER_NONSPECIFIC),
              Seq(STATE_SHUT)),
            SilPropertyState(STATE_CLOSED))))
    }

    "give up" in
    {
      val inputUnspecified =
        "colorless green ideas slumber and fume furiously to see you"
      val result = parse(inputUnspecified)
      result.hasUnknown must beTrue
    }

    "preserve unrecognized sentence syntax" in
    {
      val input = "Close the door quickly."
      val result = parse(input)
      result match {
        case SilUnrecognizedSentence(syntaxTree) => {
          val dependencyStripped = ShlurdSyntaxRewrite.rewriteAbstract(
            syntaxTree,
            true)
          dependencyStripped must be equalTo(
            SptS(
              SptVP(
                SptVB(leafCapitalized("close")),
                SptNP(
                  SptDT(leaf("the")),
                  SptNN(leaf("door"))
                ),
                SptADVP(
                  SptRB(leaf("quickly")))
              ),
              leaf(LABEL_DOT)
            )
          )
        }
        case _ => {
          s"unexpected result $result" must beEmpty
        }
      }
    }

    "preserve unrecognized reference syntax" in
    {
      val input = "The big nor strong door is open."
      val result = parse(input)
      result match {
        case SilPredicateSentence(
          SilStatePredicate(
            SilUnrecognizedReference(syntaxTree),
            SilPropertyState(STATE_OPEN),
            Seq()),
          MOOD_INDICATIVE_POSITIVE,
          SilFormality.DEFAULT
        ) => {
          val dependencyStripped = ShlurdSyntaxRewrite.rewriteAbstract(
            syntaxTree,
            true)
          dependencyStripped must be equalTo(
            SptNP(
              SptDT(leafCapitalized("the")),
              SptADJP(
                SptJJ(leaf("big")),
                SptCC(leaf("nor")),
                SptJJ(leaf("strong"))
              ),
              SptNN(leaf("door"))))
        }
        case _ => {
          s"unexpected result $result" must beEmpty
        }
      }
    }

    "deal with unknowns" in
    {
      predStateDoor().hasUnknown must beFalse
      val tree = ShlurdSyntaxLeaf("", "", "")
      SilUnrecognizedPredicate(tree).hasUnknown must beTrue
      SilPredicateSentence(predStateDoor()).hasUnknown must beFalse
      SilPredicateSentence(
        SilUnrecognizedPredicate(tree)).hasUnknown must beTrue
    }
  }
}
