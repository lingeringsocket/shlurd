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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import ShlurdEnglishLemmas._

class ShlurdUnrecognizedResponder(sentencePrinter : SilSentencePrinter)
{
  private val sb = sentencePrinter.sb

  def respond(unrecognized : SilSentence) : String =
  {
    assert(unrecognized.hasUnknown)
    unrecognized match {
      case SilPredicateSentence(predicate, mood, _) => {
        predicate match {
          case SilStatePredicate(subject, state) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None)
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(subject, complement, rel) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, mood, count, Some(rel))
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
          MOOD_IMPERATIVE, count, None, changeVerb)
        if (!response.isEmpty) {
          return response
        }
      }
      case SilPredicateQuery(predicate, question, mood, _) => {
        predicate match {
          case SilStatePredicate(subject, state) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None, None, Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(subject, complement, rel) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, mood, count,
              Some(rel), None, Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case _ =>
        }
      }
      case _ : SilStateChangeCommand => ;
      case _ : SilAmbiguousSentence => ;
      case _ : SilUnknownSentence => ;
    }
    sb.respondCannotUnderstand()
  }

  private def respondToUnresolvedPredicate(
    subject : SilReference,
    complement : SilPhrase,
    mood : SilMood,
    count : SilCount,
    rel : Option[SilRelationship],
    changeVerb : Option[SilWord] = None,
    question : Option[SilQuestion] = None) : String =
  {
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
    val copula = sb.copula(
      PERSON_THIRD, GENDER_N, count,
      mood, false, verbLemma)
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
          copula, question, !rel.isEmpty),
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
          copula, count, changeVerb, question),
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
