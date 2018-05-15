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

class ShlurdUnrecognizedResponder(sentencePrinter : ShlurdSentencePrinter)
{
  private val sb = sentencePrinter.sb

  def respond(unrecognized : ShlurdSentence) : String =
  {
    assert(unrecognized.hasUnknown)
    unrecognized match {
      case ShlurdPredicateSentence(predicate, mood, _) => {
        predicate match {
          case ShlurdStatePredicate(subject, state) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None)
            if (!response.isEmpty) {
              return response
            }
          }
          case ShlurdRelationshipPredicate(subject, complement, rel) => {
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
      case ShlurdStateChangeCommand(
        predicate : ShlurdStatePredicate, changeVerb, _) =>
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
      case ShlurdPredicateQuery(predicate, question, mood, _) => {
        predicate match {
          case ShlurdStatePredicate(subject, state) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, mood, count, None, None, Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case ShlurdRelationshipPredicate(subject, complement, rel) => {
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
      case _ : ShlurdStateChangeCommand => ;
      case _ : ShlurdAmbiguousSentence => ;
      case _ : ShlurdUnknownSentence => ;
    }
    sb.respondCannotUnderstand()
  }

  private def respondToUnresolvedPredicate(
    subject : ShlurdReference,
    complement : ShlurdPhrase,
    mood : ShlurdMood,
    count : ShlurdCount,
    rel : Option[ShlurdRelationship],
    changeVerb : Option[ShlurdWord] = None,
    question : Option[ShlurdQuestion] = None) : String =
  {
    val verbLemma = {
      complement match {
        case ShlurdExistenceState() => {
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
            ShlurdConjoining.NONE),
          copula, question, !rel.isEmpty),
        complement.toWordString)
    } else if (!complement.hasUnknown) {
      assert(subject.hasUnknown)
      sb.respondNotUnderstood(
        mood,
        sb.predicateUnrecognizedSubject(
          mood,
          complement match {
            case reference : ShlurdReference => {
              sentencePrinter.print(
                reference,
                INFLECT_NOMINATIVE,
                ShlurdConjoining.NONE)
            }
            case state : ShlurdState => {
              sentencePrinter.print(
                state,
                mood,
                ShlurdConjoining.NONE)
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
    ref1 : ShlurdReference, ref2 : ShlurdReference) : ShlurdCount =
  {
    if (ref1.hasUnknown) {
      ShlurdReference.getCount(ref2)
    } else {
      ShlurdReference.getCount(ref1)
    }
  }

  private def computeMaxCount(
    ref : ShlurdReference, count : ShlurdCount) : ShlurdCount =
  {
    if (ShlurdReference.getCount(ref) == COUNT_PLURAL) {
      COUNT_PLURAL
    } else if (count == COUNT_PLURAL) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }
}
