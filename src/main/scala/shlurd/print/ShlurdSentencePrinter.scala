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
      case ShlurdPredicateSentence(predicate, mood, formality) => {
        mood match {
          case _ : ShlurdIndicativeMood =>  {
            sb.terminatedSentence(
              printPredicateStatement(predicate, mood), mood, formality)
          }
          case MOOD_INTERROGATIVE => {
            sb.terminatedSentence(
              printPredicateQuestion(predicate), mood, sentence.formality)
          }
          case MOOD_IMPERATIVE => {
            sb.terminatedSentence(
              printPredicateCommand(predicate), mood, sentence.formality)
          }
        }
      }
      case ShlurdStateChangeCommand(predicate, formality) => {
        sb.terminatedSentence(
          printPredicateCommand(predicate), sentence.mood, formality)
      }
      case ShlurdUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(
    reference : ShlurdReference,
    inflection : ShlurdInflection,
    conjoining : ShlurdConjoining) : String =
  {
    reference match {
      case ShlurdEntityReference(entity, determiner, count) => {
        sb.determinedNoun(
          sb.determine(determiner),
          sb.delemmatizeNoun(entity, count, inflection, conjoining))
      }
      case ShlurdPronounReference(person, gender, count, _) => {
        sb.pronoun(person, gender, count, inflection, conjoining)
      }
      case ShlurdConjunctiveReference(determiner, references, separator) => {
        sb.conjoin(
          determiner, separator, inflection,
          references.zipWithIndex.map {
            case (r, i) => print(
              r, inflection,
              ShlurdConjoining(determiner, separator, i, references.size))
          }
        )
      }
      case ShlurdQualifiedReference(sub, qualifiers) => {
        val qualifierString = sb.composeQualifiers(qualifiers)
        sub match {
          case ShlurdEntityReference(entity, determiner, count) => {
            sb.determinedNoun(
              sb.determine(determiner),
              sb.qualifiedNoun(
                qualifierString,
                sb.delemmatizeNoun(entity, count, inflection, conjoining)))
          }
          case _ => {
            sb.qualifiedNoun(
              qualifierString, print(sub, inflection, conjoining))
          }
        }
      }
      case ShlurdGenitiveReference(genitive, reference) => {
        val qualifierString = genitive match {
          case ShlurdPronounReference(person, gender, count, _) => {
            sb.pronoun(
              person, gender, count, INFLECT_GENITIVE, ShlurdConjoining.NONE)
          }
          case _ => {
            print(genitive, INFLECT_GENITIVE, ShlurdConjoining.NONE)
          }
        }
        sb.genitivePhrase(
          qualifierString, print(reference, inflection, conjoining))
      }
      case ShlurdUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(
    state : ShlurdState, mood : ShlurdMood, conjoining : ShlurdConjoining)
      : String =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.delemmatizeState(state, mood, conjoining)
      }
      case ShlurdLocationState(locative, location) => {
        sb.locationalNoun(
          sb.position(locative),
          print(location, INFLECT_NONE, ShlurdConjoining.NONE),
          conjoining)
      }
      case ShlurdConjunctiveState(determiner, states, separator) => {
        sb.conjoin(
          determiner, separator, INFLECT_NONE,
          states.zipWithIndex.map {
            case (s, i) => print(
              s, mood,
              ShlurdConjoining(determiner, separator, i, states.size))
          }
        )
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
      // FIXME:  conjoining, e.g. "close and lock the door"
      case _ => print(state, MOOD_IMPERATIVE, ShlurdConjoining.NONE)
    }
  }

  def printPredicateStatement(predicate : ShlurdPredicate, mood : ShlurdMood) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE, ShlurdConjoining.NONE),
          printCopula(subject, state, mood),
          print(state, mood, ShlurdConjoining.NONE))
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
          print(subject, INFLECT_ACCUSATIVE, ShlurdConjoining.NONE),
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
          print(subject, INFLECT_NOMINATIVE, ShlurdConjoining.NONE),
          printCopula(subject, state, MOOD_INTERROGATIVE),
          print(state, MOOD_INTERROGATIVE, ShlurdConjoining.NONE))
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
      case ShlurdConjunctiveReference(determiner, references, _) => {
        val count = determiner match {
          case DETERMINER_ALL => COUNT_PLURAL
          // DETERMINER_NONE is debatable
          case _ => COUNT_SINGULAR
        }
        // FIXME:  also derive person and gender from underlying references,
        // since it makes a difference in languages such as Spanish
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
