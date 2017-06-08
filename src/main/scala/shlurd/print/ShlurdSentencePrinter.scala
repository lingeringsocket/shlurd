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

class ShlurdSentencePrinter(parlance : ShlurdParlance = ShlurdDefaultParlance)
{
  val sb = ShlurdSentenceBundle(parlance)

  def print(sentence : ShlurdSentence) : String =
  {
    sentence match {
      case ShlurdPredicateStatement(predicate) => {
        sb.statement(printPredicateStatement(predicate))
      }
      case ShlurdStateChangeCommand(predicate) => {
        sb.command(printPredicateCommand(predicate))
      }
      case ShlurdPredicateQuestion(predicate) => {
        sb.question(printPredicateQuestion(predicate))
      }
      case ShlurdUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(reference : ShlurdReference, mark : ShlurdMark) : String =
  {
    reference match {
      case ShlurdEntityReference(entity, quantifier, count) => {
        sb.determinedNoun(
          sb.determiner(quantifier),
          sb.delemmatizeNoun(entity, count, mark))
      }
      case ShlurdPronounReference(person, gender, count, _) => {
        sb.pronoun(person, gender, count, mark)
      }
      case ShlurdQualifiedReference(sub, qualifiers) => {
        val qualifierString = sb.composeQualifiers(qualifiers)
        sub match {
          case ShlurdEntityReference(entity, quantifier, count) => {
            sb.determinedNoun(
              sb.determiner(quantifier),
              sb.qualifiedNoun(
                qualifierString, sb.delemmatizeNoun(entity, count, mark)))
          }
          case _ => {
            sb.qualifiedNoun(qualifierString, print(sub, mark))
          }
        }
      }
      case ShlurdGenitiveReference(genitive, reference) => {
        val qualifierString = genitive match {
          case ShlurdPronounReference(person, gender, count, _) => {
            sb.genitivePronoun(person, gender, count)
          }
          case _ => {
            sb.genitiveNoun(print(genitive, MARK_NONE))
          }
        }
        sb.genitive(qualifierString, print(reference, mark))
      }
      case ShlurdUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(state : ShlurdState) : String =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.delemmatizeState(state)
      }
      case ShlurdLocationState(locative, location) => {
        sb.locationalNoun(
          sb.position(locative),
          print(location, MARK_NONE))
      }
      case ShlurdUnknownState => {
        sb.unknownState
      }
    }
  }

  def printChangeStateVerb(state : ShlurdState) =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.changeStateVerb(state)
      }
      case _ => print(state)
    }
  }

  def printPredicateStatement(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateStatement(
          print(subject, MARK_SUBJECT),
          printCopula(subject, state),
          print(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateStatement
      }
    }
  }

  def printPredicateCommand(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateCommand(
          print(subject, MARK_DIRECT_OBJECT),
          printChangeStateVerb(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateCommand
      }
    }
  }

  def printPredicateQuestion(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateQuestion(
          print(subject, MARK_SUBJECT),
          printCopula(subject, state),
          print(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  def printCopula(subject : ShlurdReference, state : ShlurdState) : String =
  {
    subject match {
      case ShlurdPronounReference(person, gender, count, reference) => {
        sb.copula(person, gender, count)
      }
      case ShlurdEntityReference(entity, quantifier, count) => {
        sb.copula(PERSON_THIRD, GENDER_N, count)
      }
      case ShlurdQualifiedReference(reference, qualifiers) => {
        printCopula(reference, state)
      }
      case ShlurdGenitiveReference(genitive, reference) => {
        printCopula(reference, state)
      }
      case ShlurdUnknownReference => {
        sb.unknownCopula
      }
    }
  }
}

case class ShlurdSentenceUnprintable() extends RuntimeException
{
}
