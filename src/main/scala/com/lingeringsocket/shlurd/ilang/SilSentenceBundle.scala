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

import scala.util._

case class SilConjoining(
  determiner : SilDeterminer,
  separator : SilSeparator,
  pos : Int,
  total : Int)
{
  def isLast() = ((pos + 1) == total)
}

object SilConjoining
{
  val NONE = SilConjoining(DETERMINER_ABSENT, SEPARATOR_CONJOINED, 0, 1)
}

sealed trait SilObjectPosition
case object OBJ_BEFORE_VERB extends SilObjectPosition
case object OBJ_AFTER_VERB extends SilObjectPosition

abstract class SilSentenceBundle
{
  def getTongue : SilTongue

  protected def concat(s : String*) =
    s.mkString("")

  protected def compose(s : String*) =
    s.filterNot(_.isEmpty).mkString(" ")

  def separate(item : String, conjoining : SilConjoining) =
  {
    if (conjoining.separator.needPunctuationAt(
      conjoining.pos, conjoining.total)
    ) {
      concat(item, conjoining.separator.punctuationMark)
    } else {
      item
    }
  }

  protected def terminationMark(
    tam : SilTam, formality : SilFormality) =
  {
    formality.force match {
      case FORCE_NEUTRAL => tam.mood match {
        case MOOD_INTERROGATIVE => "?"
        case _ => "."
      }
      case FORCE_EXCLAMATION => "!"
    }
  }

  def terminatedSentence(
    s : String, tam : SilTam, formality : SilFormality) : String =
  {
    concat(s, terminationMark(tam, formality))
  }

  def statePredicateStatement(
    subject : String,
    verbSeq : Seq[String],
    state : String,
    existentialPronoun : Option[SilWord],
    modifiers : Seq[String]) : String

  def actionPredicate(
    subject : String,
    verbSeq : Seq[String],
    directObject : Option[String],
    modifiers : Seq[String],
    tam : SilTam,
    answerInflection : SilInflection = INFLECT_NONE,
    objectPosition : SilObjectPosition = OBJ_AFTER_VERB) : String

  def relationshipPredicate(
    firstRef : String,
    verbSeq : Seq[String],
    secondRef : String,
    verb : SilWord,
    question : Option[SilQuestion],
    tam : SilTam,
    modifiers : Seq[String]) : String

  def statePredicateQuestion(
    subject : String,
    verbSeq : Seq[String],
    state : String,
    existentialPronoun : Option[SilWord],
    question : Option[SilQuestion],
    modifiers : Seq[String],
    answerInflection : SilInflection = INFLECT_NONE) : String

  def statePredicateCommand(
    subject : String,
    state : String,
    modifiers : Seq[String]) : String

  def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord, answerInflection : SilInflection) : Seq[String]

  def adpositionString(adposition : SilAdposition) : String

  def actionVerb(
    verb : SilWord) : String

  def existsVerb() : SilWord

  def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) : String

  def delemmatizeNoun(
    entity : SilWord,
    gender : SilGender,
    count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) : String

  def delemmatizeState(
    state : SilWord, tam : SilTam,
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    conjoining : SilConjoining) : String

  def delemmatizeQualifier(qualifier : SilWord) : String

  def conjoin(
    determiner : SilDeterminer,
    separator : SilSeparator,
    inflection : SilInflection,
    items : Seq[String]) : String

  def conditional(
    conjunction : SilWord,
    antecedent : String,
    consequent : String,
    biconditional : Boolean) : String

  def composeQualifiers(qualifiers : Seq[SilWord]) : String

  def query(noun : String, question : Option[SilQuestion],
    answerInflection : SilInflection = INFLECT_NONE) : String

  def qualifiedNoun(qualifiers : String, noun : String) : String =
  {
    getTongue.getAdjectivePosition match {
      case MOD_BEFORE_ALWAYS | MOD_BEFORE_DEFAULT => {
        compose(qualifiers, noun)
      }
      case _ => {
        compose(noun, qualifiers)
      }
    }
  }

  def specifiedNoun(specifier : String, noun : String) : String

  def determinedNoun(
    determiner : SilDeterminer, noun : String,
    person : SilPerson, gender : SilGender, count : SilCount) : String

  def adpositionedNoun(
    position : String, noun : String, conjoining : SilConjoining) : String

  def parenthetical(
    inside : String, inflection : SilInflection,
    conjoining : SilConjoining,
    bracket : SilBracket) : String =
  {
    val outside = concat(
      bracket.begin, separate(inside, conjoining), bracket.end)
    inflection match {
      // FIXME we should make this language-specific, but regardless of what
      // we do, it's still "unnatural language"
      case INFLECT_GENITIVE => concat(outside, "'s")
      case _ => outside
    }
  }

  def appositionedNoun(primary : String, secondary : String) : String =
  {
    compose(primary, secondary)
  }

  def respondToQuery(sentence : String) : String

  def respondToCounterfactual(sentence : String) : String

  def respondAmbiguous(entity : SilWord) : String

  def respondUnknown(word : SilWord) : String

  def respondUnknownModifier(word : SilWord) : String

  def respondUnknownState(subject : String, word : SilWord) : String

  def respondUnresolvedPronoun(pronoun : String) : String

  def respondAmbiguousPronoun(pronoun : String) : String

  def respondMisqualifiedNoun(
    noun : SilWord, qualifiers : Seq[String]) : String

  def respondNonexistent(entity : SilWord) : String

  def respondCannotUnderstand() : String

  def respondDontKnow() : String

  def respondNotUnderstood(
    tam : SilTam, predicate : String, errorPhrase : String) : String

  def respondUnable(action : String) : String

  def respondIrrelevant() : String

  def respondTriggerLimit() : String

  def predicateUnrecognizedSubject(
    tam : SilTam, complement : String, verbSeq : Seq[String],
    count : SilCount, changeVerb : Option[SilWord],
    question : Option[SilQuestion]) : String

  def predicateUnrecognizedComplement(
    tam : SilTam, subject : String,
    verbSeq : Seq[String],
    question : Option[SilQuestion],
    isRelationship : Boolean) : String

  def respondCompliance() : String

  def respondNoncommittal() : String

  def respondToAssumption(
    assumption : SilAssumption, truth : Boolean,
    sentence : String, strength : Boolean)
      : String =
  {
    assumption match {
      case ASSUMED_TRUE => {
        if (truth) {
          affirmAssumption(sentence, strength)
        } else {
          contradictAssumption(sentence, strength)
        }
      }
      case ASSUMED_FALSE => {
        if (truth) {
          contradictAssumption(sentence, strength)
        } else {
          affirmAssumption(sentence, strength)
        }
      }
      case _ => sentence
    }
  }

  def affirmAssumption(sentence : String, strength : Boolean) : String

  def contradictAssumption(sentence : String, strength : Boolean) : String

  def pronoun(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity, politeness : SilPoliteness,
    word : Option[SilWord],
    inflection : SilInflection,
    conjoining : SilConjoining) : String

  def applyInflection(
    base : String, count : SilCount, inflection : SilInflection) : String

  def genitivePhrase(
    genitive : String, head : String, isPronoun : Boolean) : String

  def cardinalNumber(
    num : Int, gender : SilGender, isModifier : Boolean) : String = num.toString

  def cardinalValue(s : String) : Try[Int] = Try(s.toInt)

  def ordinalNumber(num : Int, gender : SilGender) : String = num.toString

  def ordinalValue(s : String) : Try[Int] = Try(s.toInt)

  def unknownSentence() : String

  def unknownReference() : String

  def unknownState() : String

  def unknownVerbModifier() : String

  def unknownPredicateStatement() : String

  def unknownPredicateCommand() : String

  def unknownPredicateQuestion() : String

  // FIXME move to SilTongue
  def getObjectPosition(refOpt : Option[SilReference]) : SilObjectPosition =
  {
    OBJ_AFTER_VERB
  }
}
