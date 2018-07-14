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
          case adpositionalState : SilAdpositionalState => {
            val specified = print(sub, inflection, SilConjoining.NONE)
            val specifier = printAdpositionalPhrase(
              adpositionalState, conjoining)
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
      case adpositionalState : SilAdpositionalState => {
        printAdpositionalPhrase(adpositionalState, conjoining)
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
      case SilStatePredicate(subject, state, modifiers) => {
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
          getVerbSeq(
            subject, state, mood, REL_IDENTITY, predicate.getInflectedCount),
          rhs,
          modifiers.map(printVerbModifier(_))
        )
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
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
        sb.relationshipPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, SilNullState(), mood, relationship,
            predicate.getInflectedCount),
          rhs,
          relationship,
          None,
          mood,
          modifiers.map(printVerbModifier(_)))
      }
      case SilActionPredicate(
        subject, action, directObject, indirectObject, modifiers
      ) => {
        val count = SilReference.getCount(subject)
        sb.actionPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, action, moodOriginal, predicate.getInflectedCount),
          directObject.map(
            ref => print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE)),
          indirectObject.map(
            ref => print(ref, INFLECT_DATIVE, SilConjoining.NONE)),
          modifiers.map(printVerbModifier(_)),
          moodOriginal)
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
      case SilStatePredicate(subject, state, modifiers) => {
        sb.statePredicateCommand(
          print(subject, INFLECT_ACCUSATIVE, SilConjoining.NONE),
          printChangeStateVerb(state, changeVerb),
          modifiers.map(printVerbModifier(_)))
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
      case SilStatePredicate(subject, state, modifiers) => {
        sb.statePredicateQuestion(
          sb.query(
            print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
            question),
          getVerbSeq(
            subject, state, mood, REL_IDENTITY, predicate.getInflectedCount),
          print(state, mood, SilConjoining.NONE),
          question,
          modifiers.map(printVerbModifier(_)))
      }
      case SilActionPredicate(
        subject, action, directObject, indirectObject, modifiers
      ) => {
        sb.actionPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(subject, action, mood, predicate.getInflectedCount),
          directObject.map(
            ref => print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE)),
          indirectObject.map(
            ref => print(ref, INFLECT_DATIVE, SilConjoining.NONE)),
          modifiers.map(printVerbModifier(_)),
          mood)
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
        sb.relationshipPredicate(
          sb.query(
            print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
            question),
          getVerbSeq(
            subject, SilNullState(), mood, relationship,
            predicate.getInflectedCount),
          print(complement, INFLECT_NOMINATIVE, SilConjoining.NONE),
          relationship,
          question,
          mood,
          modifiers.map(printVerbModifier(_)))
      }
      case _ : SilUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  private def getVerbSeq(
    person : SilPerson, gender : SilGender, count : SilCount,
    mood : SilMood, isExistential : Boolean,
    relationship : SilRelationship) : Seq[String] =
  {
    val verbLemma = relationship match {
      case REL_IDENTITY => {
        if (isExistential && (mood.getModality == MODAL_EMPHATIC)) {
          LEMMA_EXIST
        } else {
          LEMMA_BE
        }
      }
      case REL_ASSOCIATION => LEMMA_HAVE
    }
    sb.delemmatizeVerb(
      person, gender, count, mood, isExistential, SilWord(verbLemma))
  }

  private def getVerbSeq(
    subject : SilReference,
    action : SilWord,
    mood : SilMood,
    predicateCount : SilCount) : Seq[String] =
  {
    subject match {
      case _ : SilUnknownReference => {
        Seq(sb.unknownCopula)
      }
      case _ => {
        val (person, gender, count) = getSubjectAttributes(subject)
        sb.delemmatizeVerb(
          person, gender, combineCounts(count, predicateCount),
          mood, false, action)
      }
    }
  }

  private def getSubjectAttributes(
    subject : SilReference, isExistential : Boolean = false)
      : (SilPerson, SilGender, SilCount) =
  {
    subject match {
      case SilPronounReference(person, gender, count) => {
        (person, gender, count)
      }
      case SilNounReference(_, _, count) => {
        (PERSON_THIRD, GENDER_N, count)
      }
      case rr : SilResolvedReference[_] => {
        (PERSON_THIRD, GENDER_N, SilReference.getCount(rr))
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
        (PERSON_THIRD, GENDER_N, count)
      }
      case SilStateSpecifiedReference(reference, _) => {
        getSubjectAttributes(reference, isExistential)
      }
      case SilGenitiveReference(possessor, possessee) => {
        getSubjectAttributes(possessee, isExistential)
      }
      case _ : SilUnknownReference => {
        (PERSON_THIRD, GENDER_N, COUNT_SINGULAR)
      }
    }
  }

  private def combineCounts(count1 : SilCount, count2 : SilCount) : SilCount =
  {
    (count1, count2) match {
      case (COUNT_SINGULAR, COUNT_SINGULAR) => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
  }

  private def getVerbSeq(
    subject : SilReference, state : SilState, mood : SilMood,
    relationship : SilRelationship, predicateCount : SilCount)
      : Seq[String] =
  {
    subject match {
      case _ : SilUnknownReference => {
        Seq(sb.unknownCopula)
      }
      case _ => {
        val isExistential = state match {
          case SilExistenceState() => true
          case _ => false
        }
        val (person, gender, count) =
          getSubjectAttributes(subject, isExistential)
        getVerbSeq(
          person, gender, combineCounts(count, predicateCount),
          mood, isExistential, relationship)
      }
    }
  }

  def printVerbModifier(modifier : SilVerbModifier) : String =
  {
    modifier match {
      case SilBasicVerbModifier(words) => {
        sb.composeQualifiers(words)
      }
      case adpositionalPhrase : SilAdpositionalVerbModifier => {
        printAdpositionalPhrase(adpositionalPhrase, SilConjoining.NONE)
      }
      case _ : SilUnknownVerbModifier => {
        sb.unknownVerbModifier
      }
    }
  }

  def printAdpositionalPhrase(
    phrase : SilAdpositionalPhrase,
    conjoining : SilConjoining) : String =
  {
    sb.adpositionedNoun(
      sb.adpositionString(phrase.adposition),
      print(phrase.objRef, INFLECT_ACCUSATIVE, SilConjoining.NONE),
      conjoining)
  }
}

case class SilSentenceUnprintable() extends RuntimeException
{
}
