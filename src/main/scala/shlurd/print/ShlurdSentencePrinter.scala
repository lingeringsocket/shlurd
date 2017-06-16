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
      case ShlurdPredicateSentence(predicate, mood) => {
        mood match {
          case _ : ShlurdIndicativeMood =>  {
            sb.statement(printPredicateStatement(predicate, mood))
          }
          case MOOD_INTERROGATIVE => {
            sb.question(printPredicateQuestion(predicate))
          }
          case MOOD_IMPERATIVE => {
            sb.command(printPredicateCommand(predicate))
          }
        }
      }
      case ShlurdStateChangeCommand(predicate) => {
        sb.command(printPredicateCommand(predicate))
      }
      case ShlurdUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(reference : ShlurdReference,
    inflection : ShlurdInflection) : String =
  {
    reference match {
      case ShlurdEntityReference(entity, determiner, count) => {
        sb.determinedNoun(
          sb.determine(determiner),
          sb.delemmatizeNoun(entity, count, inflection))
      }
      case ShlurdPronounReference(person, gender, count, _) => {
        sb.pronoun(person, gender, count, inflection)
      }
      case ShlurdQualifiedReference(sub, qualifiers) => {
        val qualifierString = sb.composeQualifiers(qualifiers)
        sub match {
          case ShlurdEntityReference(entity, determiner, count) => {
            sb.determinedNoun(
              sb.determine(determiner),
              sb.qualifiedNoun(
                qualifierString, sb.delemmatizeNoun(entity, count, inflection)))
          }
          case _ => {
            sb.qualifiedNoun(qualifierString, print(sub, inflection))
          }
        }
      }
      case ShlurdGenitiveReference(genitive, reference) => {
        val qualifierString = genitive match {
          case ShlurdPronounReference(person, gender, count, _) => {
            sb.pronoun(person, gender, count, INFLECT_GENITIVE)
          }
          case _ => {
            print(genitive, INFLECT_GENITIVE)
          }
        }
        sb.genitivePhrase(qualifierString, print(reference, inflection))
      }
      case ShlurdUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(state : ShlurdState, mood : ShlurdMood) : String =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.delemmatizeState(state, mood)
      }
      case ShlurdLocationState(locative, location) => {
        sb.locationalNoun(
          sb.position(locative),
          print(location, INFLECT_NONE))
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
      case _ => print(state, MOOD_IMPERATIVE)
    }
  }

  def printPredicateStatement(predicate : ShlurdPredicate, mood : ShlurdMood) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE),
          printCopula(subject, state, mood),
          print(state, mood))
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
          print(subject, INFLECT_ACCUSATIVE),
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
          print(subject, INFLECT_NOMINATIVE),
          printCopula(subject, state, MOOD_INTERROGATIVE),
          print(state, MOOD_INTERROGATIVE))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  def printCopula(
    subject : ShlurdReference, state : ShlurdState, mood : ShlurdMood)
      : String =
  {
    subject match {
      case ShlurdPronounReference(person, gender, count, reference) => {
        sb.copula(person, gender, count, mood)
      }
      case ShlurdEntityReference(entity, determiner, count) => {
        sb.copula(PERSON_THIRD, GENDER_N, count, mood)
      }
      case ShlurdQualifiedReference(reference, qualifiers) => {
        printCopula(reference, state, mood)
      }
      case ShlurdGenitiveReference(genitive, reference) => {
        printCopula(reference, state, mood)
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
