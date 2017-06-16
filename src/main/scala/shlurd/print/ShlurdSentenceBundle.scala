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
package shlurd.print

import shlurd.parser._

object ShlurdSentenceBundle
{
  def apply(parlance : ShlurdParlance) = parlance.newSentenceBundle
}

abstract class ShlurdSentenceBundle extends ShlurdParlanceBundle
{
  protected def concat(s : String*) =
    s.mkString("")

  protected def compose(s : String*) =
    s.filterNot(_.isEmpty).mkString(" ")

  def command(s : String) : String

  def statement(s : String) : String

  def question(s : String) : String

  def statePredicateStatement(
    subject : String, copula : String, state : String) : String

  def statePredicateQuestion(
    subject : String, copula : String, state : String) : String

  def statePredicateCommand(subject : String, state : String) : String

  def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood) : String

  def determine(determiner : ShlurdDeterminer) : String

  def position(locative : ShlurdLocative) : String

  def changeStateVerb(state : ShlurdWord) : String

  def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount,
    inflection : ShlurdInflection) : String

  def delemmatizeState(state : ShlurdWord, mood : ShlurdMood) : String

  def delemmatizeQualifier(qualifier : ShlurdWord) : String

  def composeQualifiers(qualifiers : Seq[ShlurdWord]) : String

  def qualifiedNoun(qualifiers : String, noun : String) : String

  def determinedNoun(determiner : String, noun : String) : String

  def locationalNoun(position : String, noun : String) : String

  def confirmAssumption(
    assumption : ShlurdAssumption, truth : Boolean, sentence : String)
      : String =
  {
    assumption match {
      case ASSUMED_TRUE => {
        if (truth) {
          affirmAssumption(sentence)
        } else {
          contradictAssumption(sentence)
        }
      }
      case ASSUMED_FALSE => {
        if (truth) {
          contradictAssumption(sentence)
        } else {
          affirmAssumption(sentence)
        }
      }
      case _ => sentence
    }
  }

  def affirmAssumption(sentence : String) : String

  def contradictAssumption(sentence : String) : String

  def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection) : String

  def genitivePhrase(genitive : String, head : String) : String

  def unknownSentence() : String

  def unknownReference() : String

  def unknownState() : String

  def unknownCopula() : String

  def unknownPredicateStatement() : String

  def unknownPredicateCommand() : String

  def unknownPredicateQuestion() : String
}
