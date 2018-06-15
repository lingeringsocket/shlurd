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

object SilSentencePrinter
{
  private val ELLIPSIS_MARKER = "<...>"

  private val ELLIPSIS_REMOVAL = " " + ELLIPSIS_MARKER
}
import SilSentencePrinter._

class SilSentencePrinter(parlance : ShlurdParlance = ShlurdDefaultParlance)
{
  val sb = SilSentenceBundle(parlance)

  def print(
    sentence : SilSentence, ellipsis : Boolean = false) : String =
  {
    sb.terminatedSentence(
      printUnterminated(sentence, ellipsis),
      sentence.mood, sentence.formality)
  }

  def printUnterminated(
    sentence : SilSentence, ellipsis : Boolean = false) : String =
  {
    sentence match {
      case SilPredicateSentence(predicate, mood, _) => {
        mood match {
          case _ : SilIndicativeMood =>  {
            printPredicateStatement(predicate, mood, ellipsis).
              replaceAllLiterally(ELLIPSIS_REMOVAL, "")
          }
          case _ : SilInterrogativeMood => {
            printPredicateQuestion(predicate, mood)
          }
          case MOOD_IMPERATIVE => {
            printPredicateCommand(predicate)
          }
        }
      }
      case SilStateChangeCommand(predicate, changeVerb, _) => {
        printPredicateCommand(predicate, changeVerb)
      }
      case SilPredicateQuery(predicate, question, mood, _) => {
        printPredicateQuestion(
          predicate, mood, Some(question))
      }
      case SilConjunctiveSentence(determiner, sentences, separator) => {
        sb.conjoin(
          determiner, separator, INFLECT_NONE,
          sentences.map(s => printUnterminated(s, ellipsis)))
      }
      case SilAmbiguousSentence(alternatives, _) => {
        alternatives.map(s => printUnterminated(s, ellipsis)).mkString(" | ")
      }
      case _ : SilUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(
    reference : SilReference,
    inflection : SilInflection,
    conjoining : SilConjoining) : String =
  {
    reference match {
      case SilNounReference(noun, determiner, count) => {
        sb.determinedNoun(
          determiner,
          sb.delemmatizeNoun(noun, count, inflection, conjoining))
      }
      case SilPronounReference(person, gender, count) => {
        sb.pronoun(person, gender, count, inflection, conjoining)
      }
      case SilConjunctiveReference(determiner, references, separator) => {
        sb.conjoin(
          determiner, separator, inflection,
          references.zipWithIndex.map {
            case (r, i) => print(
              r, inflection,
              SilConjoining(determiner, separator, i, references.size))
          }
        )
      }
      case SilStateSpecifiedReference(sub, state) => {
        state match {
          case SilAdpositionalState(adposition, objRef) => {
            val specified = print(sub, inflection, SilConjoining.NONE)
            val specifier = sb.adpositionedNoun(
              sb.adpositionString(adposition),
              print(objRef, INFLECT_ACCUSATIVE, SilConjoining.NONE),
              conjoining)
            return sb.specifiedNoun(specifier, specified)
          }
          case _ => {

          }
        }
        val qualifierString = state match {
          case _ : SilUnknownState => sb.unknownState
          case _ =>  {
            sb.composeQualifiers(
              SilReference.extractQualifiers(state))
          }
        }
        sub match {
          case SilNounReference(noun, determiner, count) => {
            sb.determinedNoun(
              determiner,
              sb.qualifiedNoun(
                qualifierString,
                sb.delemmatizeNoun(noun, count, inflection, conjoining)))
          }
          case _ => {
            sb.qualifiedNoun(
              qualifierString, print(sub, inflection, conjoining))
          }
        }
      }
      case SilGenitiveReference(possessor, possessee) => {
        val qualifierString = possessor match {
          case SilPronounReference(person, gender, count) => {
            sb.pronoun(
              person, gender, count, INFLECT_GENITIVE, SilConjoining.NONE)
          }
          case _ => {
            print(possessor, INFLECT_GENITIVE, SilConjoining.NONE)
          }
        }
        sb.genitivePhrase(
          qualifierString, print(possessee, inflection, conjoining))
      }
      case _ : SilResolvedReference[_] => {
        // FIXME:  call to cosmos?
        sb.unknownReference
      }
      case _ : SilUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(
    state : SilState, mood : SilMood, conjoining : SilConjoining)
      : String =
  {
    state match {
      case SilExistenceState() => {
        ""
      }
      case SilPropertyState(state) => {
        sb.delemmatizeState(state, mood, conjoining)
      }
      case SilAdpositionalState(adposition, objRef) => {
        sb.adpositionedNoun(
          sb.adpositionString(adposition),
          print(objRef, INFLECT_NONE, SilConjoining.NONE),
          conjoining)
      }
      case SilConjunctiveState(determiner, states, separator) => {
        sb.conjoin(
          determiner, separator, INFLECT_NONE,
          states.zipWithIndex.map {
            case (s, i) => print(
              s, mood,
              SilConjoining(determiner, separator, i, states.size))
          }
        )
      }
      case SilNullState() | _ : SilUnknownState => {
        sb.unknownState
      }
    }
  }

  def printChangeStateVerb(
    state : SilState, changeVerb : Option[SilWord]) =
  {
    state match {
      case SilPropertyState(state) => {
        sb.changeStateVerb(state, changeVerb)
      }
      // FIXME:  conjoining, e.g. "close and lock the door"
      case _ => print(state, MOOD_IMPERATIVE, SilConjoining.NONE)
    }
  }

  def printPredicateStatement(
    predicate : SilPredicate, moodOriginal : SilMood,
    ellipsis : Boolean = false) : String =
  {
    predicate match {
      case SilStatePredicate(subject, state) => {
        val mood = moodOriginal
        val rhs = {
          if (ellipsis) {
            ELLIPSIS_MARKER
          } else {
            print(state, mood, SilConjoining.NONE)
          }
        }
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getCopula(subject, state, mood, REL_IDENTITY),
          rhs)
      }
      case SilRelationshipPredicate(subject, complement, relationship) => {
        val complementInflection = relationship match {
          case REL_IDENTITY => INFLECT_NOMINATIVE
          case REL_ASSOCIATION => INFLECT_ACCUSATIVE
        }
        val mood = {
          if (ellipsis && (relationship == REL_ASSOCIATION)) {
            moodOriginal match {
              case SilIndicativeMood(positive, _) => {
                SilIndicativeMood(positive, MODAL_ELLIPTICAL)
              }
              case _ => moodOriginal
            }
          } else {
            moodOriginal
          }
        }
        val rhs = {
          if (ellipsis) {
            ELLIPSIS_MARKER
          } else {
            print(complement, complementInflection, SilConjoining.NONE)
          }
        }
        sb.relationshipPredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getCopula(subject, SilNullState(), mood, relationship),
          rhs)
      }
      case _ : SilUnknownPredicate => {
        sb.unknownPredicateStatement
      }
    }
  }

  def printPredicateCommand(
    predicate : SilPredicate, changeVerb : Option[SilWord] = None) =
  {
    predicate match {
      case SilStatePredicate(subject, state) => {
        sb.statePredicateCommand(
          print(subject, INFLECT_ACCUSATIVE, SilConjoining.NONE),
          printChangeStateVerb(state, changeVerb))
      }
      case _ => {
        sb.unknownPredicateCommand
      }
    }
  }

  def printPredicateQuestion(
    predicate : SilPredicate, mood : SilMood,
    question : Option[SilQuestion] = None) : String =
  {
    predicate match {
      case SilStatePredicate(subject, state) => {
        sb.statePredicateQuestion(
          sb.query(
            print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
            question),
          getCopula(subject, state, mood, REL_IDENTITY),
          print(state, mood, SilConjoining.NONE),
          question)
      }
      case SilRelationshipPredicate(subject, complement, relationship) => {
        sb.relationshipPredicateQuestion(
          sb.query(
            print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
            question),
          getCopula(subject, SilNullState(), mood, relationship),
          print(complement, INFLECT_NOMINATIVE, SilConjoining.NONE))
      }
      case _ : SilUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  private def getCopula(
    subject : SilReference, state : SilState, mood : SilMood,
    relationship : SilRelationship)
      : Seq[String] =
  {
    val isExistential = state match {
      case SilExistenceState() => true
      case _ => false
    }
    subject match {
      case SilPronounReference(person, gender, count) => {
        sb.copula(person, gender, count, mood, isExistential, relationship)
      }
      case SilNounReference(_, _, count) => {
        sb.copula(
          PERSON_THIRD, GENDER_N, count, mood, isExistential, relationship)
      }
      case rr : SilResolvedReference[_] => {
        sb.copula(
          PERSON_THIRD, GENDER_N, SilReference.getCount(rr),
          mood, isExistential, relationship)
      }
      case SilConjunctiveReference(determiner, references, _) => {
        val count = if (isExistential) {
          // FIXME:  this is probably English-specific
          SilReference.getCount(references.head)
        } else {
          determiner match {
            case DETERMINER_ALL => COUNT_PLURAL
            // DETERMINER_NONE is debatable
            case _ => COUNT_SINGULAR
          }
        }
        // FIXME:  also derive person and gender from underlying references,
        // since it makes a difference in languages such as Spanish
        sb.copula(
          PERSON_THIRD, GENDER_N, count, mood, isExistential, relationship)
      }
      case SilStateSpecifiedReference(reference, _) => {
        getCopula(reference, state, mood, relationship)
      }
      case SilGenitiveReference(possessor, possessee) => {
        getCopula(possessee, state, mood, relationship)
      }
      case _ : SilUnknownReference => {
        Seq(sb.unknownCopula)
      }
    }
  }
}

case class SilSentenceUnprintable() extends RuntimeException
{
}
