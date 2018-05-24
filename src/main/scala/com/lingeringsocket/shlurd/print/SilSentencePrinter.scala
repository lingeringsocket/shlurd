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

class SilSentencePrinter(parlance : ShlurdParlance = ShlurdDefaultParlance)
{
  val sb = SilSentenceBundle(parlance)

  def print(sentence : SilSentence) : String =
  {
    sentence match {
      case SilPredicateSentence(predicate, mood, formality) => {
        mood match {
          case _ : SilIndicativeMood =>  {
            sb.terminatedSentence(
              printPredicateStatement(predicate, mood), mood, formality)
          }
          case _ : SilInterrogativeMood => {
            sb.terminatedSentence(
              printPredicateQuestion(predicate, mood), mood, sentence.formality)
          }
          case MOOD_IMPERATIVE => {
            sb.terminatedSentence(
              printPredicateCommand(predicate), mood, sentence.formality)
          }
        }
      }
      case SilStateChangeCommand(predicate, changeVerb, formality) => {
        sb.terminatedSentence(
          printPredicateCommand(predicate, changeVerb),
          sentence.mood, formality)
      }
      case SilPredicateQuery(predicate, question, mood, formality) => {
        sb.terminatedSentence(
          printPredicateQuestion(
            predicate, mood, Some(question)), mood, formality)
      }
      case SilAmbiguousSentence(alternatives, _) => {
        alternatives.map(print(_)).mkString(" | ")
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
          case SilLocationState(locative, location) => {
            val specified = print(sub, inflection, SilConjoining.NONE)
            val specifier = sb.locationalNoun(
              sb.position(locative),
              print(location, INFLECT_NONE, SilConjoining.NONE),
              conjoining)
            return sb.specifiedNoun(specifier, specified)
          }
          case _ => {
          }
        }
        val qualifierString = sb.composeQualifiers(
          SilReference.extractQualifiers(state))
        sub match {
          case SilNounReference(noun, determiner, count) => {
            sb.determinedNoun(
              determiner,
              sb.qualifiedNoun(
                qualifierString,
                sb.delemmatizeNoun(noun, count, inflection, conjoining)))
          }
          case SilPronounReference(person, gender, count) => {
            // kludged representation for "three of them"
            sb.locationalNoun(
              sb.qualifiedNoun(qualifierString, "of"),
              sb.pronoun(
                person, gender, COUNT_PLURAL, INFLECT_ACCUSATIVE,
                SilConjoining.NONE),
              conjoining)
          }
          case _ => {
            sb.qualifiedNoun(
              qualifierString, print(sub, inflection, conjoining))
          }
        }
      }
      case SilGenitiveReference(genitive, reference) => {
        val qualifierString = genitive match {
          case SilPronounReference(person, gender, count) => {
            sb.pronoun(
              person, gender, count, INFLECT_GENITIVE, SilConjoining.NONE)
          }
          case _ => {
            print(genitive, INFLECT_GENITIVE, SilConjoining.NONE)
          }
        }
        sb.genitivePhrase(
          qualifierString, print(reference, inflection, conjoining))
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
      case SilLocationState(locative, location) => {
        sb.locationalNoun(
          sb.position(locative),
          print(location, INFLECT_NONE, SilConjoining.NONE),
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
    predicate : SilPredicate, mood : SilMood) : String =
  {
    predicate match {
      case SilStatePredicate(subject, state) => {
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getCopula(subject, state, mood, REL_IDENTITY),
          print(state, mood, SilConjoining.NONE))
      }
      case SilRelationshipPredicate(subject, complement, relationship) => {
        sb.relationshipPredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getCopula(subject, SilNullState(), mood, relationship),
          print(complement, INFLECT_NOMINATIVE, SilConjoining.NONE))
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
      case SilGenitiveReference(genitive, reference) => {
        getCopula(reference, state, mood, relationship)
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
