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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import ShlurdEnglishLemmas._

class ShlurdUnrecognizedResponder(sentencePrinter : SilSentencePrinter)
{
  private val sb = sentencePrinter.sb

  def respond(unrecognized : SilSentence) : String =
  {
    assert(unrecognized.isUninterpretable)
    unrecognized match {
      case SilPredicateSentence(predicate, mood, _) => {
        predicate match {
          case SilStatePredicate(subject, state, modifiers) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None, modifiers)
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(
            subject, complement, rel, modifiers
          ) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, mood, count, Some(rel), modifiers)
            if (!response.isEmpty) {
              return response
            }
          }
          case _ =>
        }
      }
      case SilStateChangeCommand(
        predicate : SilStatePredicate, changeVerb, _) =>
      {
        val count = computeMaxCount(
          predicate.subject,
          predicate.getInflectedCount)
        val response = respondToUnresolvedPredicate(
          predicate.subject, predicate.state,
          MOOD_IMPERATIVE, count, None, predicate.modifiers, changeVerb)
        if (!response.isEmpty) {
          return response
        }
      }
      case SilPredicateQuery(
        predicate, question, answerInflection, mood, _
      ) => {
        predicate match {
          case SilStatePredicate(subject, state, modifiers) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None, modifiers, None,
              Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(
            subject, complement, rel, modifiers
          ) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, mood, count,
              Some(rel), modifiers, None, Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case _ =>
        }
      }
      case SilConjunctiveSentence(determiner, sentences, _) => {
        // FIXME:  divide and conquer if possible
      }
      case _ : SilConditionalSentence => ;
      case _ : SilStateChangeCommand => ;
      case _ : SilAmbiguousSentence => ;
      case _ : SilUnknownSentence => ;
      case _ : SilUnparsedSentence => ;
    }
    sb.respondCannotUnderstand()
  }

  private def respondToUnresolvedPredicate(
    subject : SilReference,
    complement : SilPhrase,
    mood : SilMood,
    count : SilCount,
    rel : Option[SilRelationship],
    modifiers : Seq[SilVerbModifier],
    changeVerb : Option[SilWord] = None,
    question : Option[SilQuestion] = None) : String =
  {
    if (!modifiers.isEmpty) {
      // FIXME get real
      return sb.respondNotUnderstood(
        mood, "something",
        modifiers.map(_.toWordString).mkString(" "))
    }
    val verbLemma = {
      complement match {
        case SilExistenceState() => {
          LEMMA_EXIST
        }
        case _ => {
          rel match {
            case Some(REL_ASSOCIATION) => LEMMA_HAVE
            case _ => LEMMA_BE
          }
        }
      }
    }
    val verbSeq = sb.delemmatizeVerb(
      PERSON_THIRD, GENDER_N, count,
      mood, false, SilWord(verbLemma))
    if (!subject.hasUnknown) {
      assert(complement.hasUnknown)
      sb.respondNotUnderstood(
        mood,
        sb.predicateUnrecognizedComplement(
          mood,
          sentencePrinter.print(
            subject,
            if (question.isEmpty) INFLECT_ACCUSATIVE else INFLECT_NOMINATIVE,
            SilConjoining.NONE),
          verbSeq, question, !rel.isEmpty),
        complement.toWordString)
    } else if (!complement.hasUnknown) {
      assert(subject.hasUnknown)
      sb.respondNotUnderstood(
        mood,
        sb.predicateUnrecognizedSubject(
          mood,
          complement match {
            case reference : SilReference => {
              sentencePrinter.print(
                reference,
                INFLECT_NOMINATIVE,
                SilConjoining.NONE)
            }
            case state : SilState => {
              sentencePrinter.print(
                state,
                mood,
                SilConjoining.NONE)
            }
            case _ => complement.toWordString
          },
          verbSeq, count, changeVerb, question),
        subject.toWordString)
    } else {
      ""
    }
  }
  private def findKnownCount(
    ref1 : SilReference, ref2 : SilReference) : SilCount =
  {
    if (ref1.hasUnknown) {
      SilReference.getCount(ref2)
    } else {
      SilReference.getCount(ref1)
    }
  }

  private def computeMaxCount(
    ref : SilReference, count : SilCount) : SilCount =
  {
    if (SilReference.getCount(ref) == COUNT_PLURAL) {
      COUNT_PLURAL
    } else if (count == COUNT_PLURAL) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }
}
