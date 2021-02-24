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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

class SmcUnrecognizedResponder(
  tongueIn : SprTongue,
  sentencePrinter : SilSentencePrinter)
{
  private val sb = sentencePrinter.responseBundle

  private implicit val tongue = tongueIn

  def respond(unrecognized : SilSentence) : String =
  {
    assert(unrecognized.isUninterpretable)
    unrecognized match {
      case SilPredicateSentence(predicate, tam, _) => {
        predicate matchPartial {
          case SilStatePredicate(subject, verb, state, modifiers) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, tam, count, None, modifiers)
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(
            subject, verb, complement, modifiers
          ) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, tam, count, Some(verb), modifiers)
            if (!response.isEmpty) {
              return response
            }
          }
        }
      }
      case SilPredicateQuery(
        predicate, question, answerInflection, tam, _
      ) => {
        predicate matchPartial {
          case SilStatePredicate(subject, verb, state, modifiers) => {
            val count = computeMaxCount(
              subject,
              predicate.getInflectedCount)
            val response = respondToUnresolvedPredicate(
              subject, state, tam, count, None, modifiers, None,
              Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
          case SilRelationshipPredicate(
            subject, verb, complement, modifiers
          ) => {
            val count = findKnownCount(subject, complement)
            val response = respondToUnresolvedPredicate(
              subject, complement, tam, count,
              Some(verb), modifiers, None, Some(question))
            if (!response.isEmpty) {
              return response
            }
          }
        }
      }
      case SilConjunctiveSentence(determiner, sentences, _) => {
        // FIXME:  divide and conquer if possible
      }
      case _ : SilConditionalSentence => ;
      case _ : SilAmbiguousSentence => ;
      case _ : SilUnknownSentence => ;
      case _ : SilUnparsedSentence => ;
    }
    sb.respondCannotUnderstand
  }

  private def respondToUnresolvedPredicate(
    subject : SilReference,
    complement : SilPhrase,
    tam : SilTam,
    count : SilCount,
    relationshipVerb : Option[SilWord],
    modifiers : Seq[SilVerbModifier],
    changeVerb : Option[SilWord] = None,
    question : Option[SilQuestion] = None) : String =
  {
    if (!modifiers.isEmpty) {
      // FIXME get real
      return sb.respondNotUnderstood(
        tam, "something",
        modifiers.map(_.toWordString).mkString(" "))
    }
    val verbLemma = {
      complement match {
        case SilExistenceState(_) => {
          PD_EXIST.toLemma
        }
        case _ => {
          relationshipVerb.map(_.toLemma).getOrElse(PD_BE.toLemma)
        }
      }
    }
    val verbSeq = sb.delemmatizeVerb(
      PERSON_THIRD, GENDER_NEUTER, count,
      tam, None, SilWord.uninflected(verbLemma), INFLECT_NONE)
    if (!subject.hasUnknown) {
      assert(complement.hasUnknown)
      sb.respondNotUnderstood(
        tam,
        sb.predicateUnrecognizedComplement(
          tam,
          sentencePrinter.print(
            subject,
            if (question.isEmpty) INFLECT_ACCUSATIVE else INFLECT_NOMINATIVE,
            SilConjoining.NONE),
          verbSeq, question, !relationshipVerb.isEmpty),
        complement.toWordString)
    } else if (!complement.hasUnknown) {
      assert(subject.hasUnknown)
      sb.respondNotUnderstood(
        tam,
        sb.predicateUnrecognizedSubject(
          tam,
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
                tam,
                Some(subject),
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
      SilUtils.getCount(ref2)
    } else {
      SilUtils.getCount(ref1)
    }
  }

  private def computeMaxCount(
    ref : SilReference, count : SilCount) : SilCount =
  {
    if (SilUtils.getCount(ref) == COUNT_PLURAL) {
      COUNT_PLURAL
    } else if (count == COUNT_PLURAL) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }
}
