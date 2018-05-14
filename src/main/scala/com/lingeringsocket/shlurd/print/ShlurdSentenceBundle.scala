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

case class ShlurdConjoining(
  determiner : ShlurdDeterminer,
  separator : ShlurdSeparator,
  pos : Int,
  total : Int)
{
  def isLast() = ((pos + 1) == total)
}

object ShlurdConjoining
{
  val NONE = ShlurdConjoining(DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, 0, 1)
}

object ShlurdSentenceBundle
{
  def apply(parlance : ShlurdParlance) =
    parlance.newSentenceBundle
}

abstract class ShlurdSentenceBundle
    extends ShlurdParlanceBundle
{
  protected def concat(s : String*) =
    s.mkString("")

  protected def compose(s : String*) =
    s.filterNot(_.isEmpty).mkString(" ")

  protected def separate(item : String, conjoining : ShlurdConjoining) =
  {
    if (conjoining.separator.needComma(conjoining.pos, conjoining.total)) {
      concat(item, ",")
    } else {
      item
    }
  }

  protected def terminationMark(
    mood : ShlurdMood, formality : ShlurdFormality) =
  {
    formality.force match {
      case FORCE_NEUTRAL => mood match {
        case _ : ShlurdInterrogativeMood => "?"
        case _ => "."
      }
      case FORCE_EXCLAMATION => "!"
    }
  }

  def terminatedSentence(
    s : String, mood : ShlurdMood, formality : ShlurdFormality) : String =
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
    question : Option[ShlurdQuestion]) : String

  def relationshipPredicateQuestion(
    firstRef : String,
    coupla : Seq[String],
    secondRef : String) : String

  def statePredicateCommand(subject : String, state : String) : String

  def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood, isExistential : Boolean,
    relationship : ShlurdRelationship) : Seq[String] =
  {
    val verbLemma = relationship match {
      case REL_IDENTITY => LEMMA_BE
      case REL_ASSOCIATION => LEMMA_HAVE
    }
    copula(person, gender, count, mood, isExistential, verbLemma)
  }

  def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood, isExistential : Boolean,
    verbLemma : String) : Seq[String]

  def position(locative : ShlurdLocative) : String

  def changeStateVerb(
    state : ShlurdWord, changeVerb : Option[ShlurdWord]) : String

  def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount,
    inflection : ShlurdInflection,
    conjoining : ShlurdConjoining) : String

  def delemmatizeState(
    state : ShlurdWord, mood : ShlurdMood,
    conjoining : ShlurdConjoining) : String

  def delemmatizeQualifier(qualifier : ShlurdWord) : String

  def conjoin(
    determiner : ShlurdDeterminer,
    separator : ShlurdSeparator,
    inflection : ShlurdInflection,
    items : Seq[String]) : String

  def composeQualifiers(qualifiers : Seq[ShlurdWord]) : String

  def query(noun : String, question : Option[ShlurdQuestion]) : String

  def qualifiedNoun(qualifiers : String, noun : String) : String

  def specifiedNoun(specifier : String, noun : String) : String

  def determinedNoun(determiner : ShlurdDeterminer, noun : String) : String

  def locationalNoun(
    position : String, noun : String, conjoining : ShlurdConjoining) : String

  def respondToQuery(sentence : String) : String

  def respondToCounterfactual(sentence : String) : String

  def respondAmbiguous(entity : String) : String

  def respondUnknown(entity : String) : String

  def respondUnknownPronoun(entity : String) : String

  def respondNonexistent(entity : String) : String

  def respondCannotUnderstand() : String

  def respondDontKnow() : String

  def respondNotUnderstood(
    mood : ShlurdMood, predicate : String, errorPhrase : String) : String

  def predicateUnrecognizedSubject(
    mood : ShlurdMood, complement : String, copula : Seq[String],
    count : ShlurdCount, changeVerb : Option[ShlurdWord],
    question : Option[ShlurdQuestion]) : String

  def predicateUnrecognizedComplement(
    mood : ShlurdMood, subject : String,
    copula : Seq[String],
    question : Option[ShlurdQuestion],
    isRelationship : Boolean) : String

  def respondCompliance() : String

  def respondToAssumption(
    assumption : ShlurdAssumption, truth : Boolean,
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
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection, conjoining : ShlurdConjoining) : String

  def genitivePhrase(genitive : String, head : String) : String

  def unknownSentence() : String

  def unknownReference() : String

  def unknownState() : String

  def unknownCopula() : String

  def unknownPredicateStatement() : String

  def unknownPredicateCommand() : String

  def unknownPredicateQuestion() : String
}
