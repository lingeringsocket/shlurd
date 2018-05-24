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

import com.lingeringsocket.shlurd.parser._

import ShlurdEnglishLemmas._

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
  val NONE = SilConjoining(DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, 0, 1)
}

object SilSentenceBundle
{
  def apply(parlance : ShlurdParlance) =
    parlance.newSentenceBundle
}

abstract class SilSentenceBundle
    extends ShlurdParlanceBundle
{
  protected def concat(s : String*) =
    s.mkString("")

  protected def compose(s : String*) =
    s.filterNot(_.isEmpty).mkString(" ")

  protected def separate(item : String, conjoining : SilConjoining) =
  {
    if (conjoining.separator.needComma(conjoining.pos, conjoining.total)) {
      concat(item, ",")
    } else {
      item
    }
  }

  protected def terminationMark(
    mood : SilMood, formality : SilFormality) =
  {
    formality.force match {
      case FORCE_NEUTRAL => mood match {
        case _ : SilInterrogativeMood => "?"
        case _ => "."
      }
      case FORCE_EXCLAMATION => "!"
    }
  }

  def terminatedSentence(
    s : String, mood : SilMood, formality : SilFormality) : String =
  {
    concat(s, terminationMark(mood, formality))
  }

  def statePredicateStatement(
    subject : String,
    copula : Seq[String],
    state : String) : String

  def relationshipPredicateStatement(
    firstRef : String,
    copula : Seq[String],
    secondRef : String) : String

  def statePredicateQuestion(
    subject : String,
    copula : Seq[String],
    state : String,
    question : Option[SilQuestion]) : String

  def relationshipPredicateQuestion(
    firstRef : String,
    coupla : Seq[String],
    secondRef : String) : String

  def statePredicateCommand(subject : String, state : String) : String

  def copula(
    person : SilPerson, gender : SilGender, count : SilCount,
    mood : SilMood, isExistential : Boolean,
    relationship : SilRelationship) : Seq[String] =
  {
    val verbLemma = relationship match {
      case REL_IDENTITY => LEMMA_BE
      case REL_ASSOCIATION => LEMMA_HAVE
    }
    copula(person, gender, count, mood, isExistential, verbLemma)
  }

  def copula(
    person : SilPerson, gender : SilGender, count : SilCount,
    mood : SilMood, isExistential : Boolean,
    verbLemma : String) : Seq[String]

  def position(locative : SilLocative) : String

  def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) : String

  def delemmatizeNoun(
    entity : SilWord, count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) : String

  def delemmatizeState(
    state : SilWord, mood : SilMood,
    conjoining : SilConjoining) : String

  def delemmatizeQualifier(qualifier : SilWord) : String

  def conjoin(
    determiner : SilDeterminer,
    separator : SilSeparator,
    inflection : SilInflection,
    items : Seq[String]) : String

  def composeQualifiers(qualifiers : Seq[SilWord]) : String

  def query(noun : String, question : Option[SilQuestion]) : String

  def qualifiedNoun(qualifiers : String, noun : String) : String

  def specifiedNoun(specifier : String, noun : String) : String

  def determinedNoun(determiner : SilDeterminer, noun : String) : String

  def locationalNoun(
    position : String, noun : String, conjoining : SilConjoining) : String

  def respondToQuery(sentence : String) : String

  def respondToCounterfactual(sentence : String) : String

  def respondAmbiguous(entity : SilWord) : String

  def respondUnknown(word : SilWord) : String

  def respondUnknownPronoun(pronoun : String) : String

  def respondNonexistent(entity : SilWord) : String

  def respondCannotUnderstand() : String

  def respondDontKnow() : String

  def respondNotUnderstood(
    mood : SilMood, predicate : String, errorPhrase : String) : String

  def predicateUnrecognizedSubject(
    mood : SilMood, complement : String, copula : Seq[String],
    count : SilCount, changeVerb : Option[SilWord],
    question : Option[SilQuestion]) : String

  def predicateUnrecognizedComplement(
    mood : SilMood, subject : String,
    copula : Seq[String],
    question : Option[SilQuestion],
    isRelationship : Boolean) : String

  def respondCompliance() : String

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
    inflection : SilInflection, conjoining : SilConjoining) : String

  def genitivePhrase(genitive : String, head : String) : String

  def unknownSentence() : String

  def unknownReference() : String

  def unknownState() : String

  def unknownCopula() : String

  def unknownPredicateStatement() : String

  def unknownPredicateCommand() : String

  def unknownPredicateQuestion() : String
}
