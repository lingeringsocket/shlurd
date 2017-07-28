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
package shlurd.world

import shlurd.parser._
import shlurd.print._

import scala.util._

class ShlurdInterpreter[E<:ShlurdEntity, P<:ShlurdProperty](
  world : ShlurdWorld[E,P])
{
  private val sentencePrinter = new ShlurdSentencePrinter

  def fail(msg : String) = world.fail(msg)

  def interpret(sentence : ShlurdSentence) : String =
  {
    sentence match {
      case ShlurdStateChangeCommand(predicate, formality) => {
        evaluatePredicate(predicate) match {
          case Success(true) => {
            // FIXME:  use proper rephrasing
            "But it already is."
          }
          case Success(false) => {
            // FIXME:  carry out action
            "Okay, I will get right on that."
          }
          case Failure(e) => {
            e.getMessage
          }
        }
      }
      case ShlurdPredicateSentence(predicate, mood, formality) => {
        mood match {
          // FIXME deal with positive, modality
          case ShlurdInterrogativeMood(positive, modality) => {
            evaluatePredicate(predicate) match {
              case Success(truth) => {
                val responseMood = ShlurdIndicativeMood(truth)
                sentencePrinter.sb.respondToAssumption(
                  ASSUMED_TRUE, truth,
                  sentencePrinter.print(
                    ShlurdPredicateSentence(predicate, responseMood)),
                  false)
              }
              case Failure(e) => {
                e.getMessage
              }
            }
          }
          case _ : ShlurdIndicativeMood => {
            // FIXME deal with mood.getModality
            val positivity = mood.isPositive
            val predicateTruth = evaluatePredicate(predicate)
            val responseMood = {
              predicateTruth match {
                case Success(false) => {
                  MOOD_INDICATIVE_NEGATIVE
                }
                case _ => {
                  // FIXME:  deal with uncertainty
                  MOOD_INDICATIVE_POSITIVE
                }
              }
            }
            predicateTruth match {
              case Success(truth) => {
                if (truth == positivity) {
                  sentencePrinter.sb.respondToAssumption(
                    ASSUMED_TRUE, true,
                    sentencePrinter.print(
                      ShlurdPredicateSentence(predicate, responseMood)),
                    true)
                } else {
                  // FIXME:  add details on inconsistency, and maybe try
                  // to update state?
                  "Oh, really?"
                }
              }
              case Failure(e) => {
                // FIXME:  try to update state?
                e.getMessage
              }
            }
          }
          case _ => {
            "I am not sure what you mean by that."
          }
        }
      }
      case ShlurdUnknownSentence => {
        "Sorry, I do not understand."
      }
    }
  }

  private def evaluatePredicate(
    predicate : ShlurdPredicate) : Try[Boolean] =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        state match {
          case ShlurdConjunctiveState(determiner, states, _) => {
            val tries = states.map(
              s => evaluatePredicate(ShlurdStatePredicate(subject, s)))
            tries.find(_.isFailure) match {
              case Some(failed) => failed
              case _ => {
                val results = tries.map(_.get)
                determiner match {
                  case DETERMINER_NONE => Success(!results.exists(r => r))
                  case DETERMINER_UNIQUE =>
                    Success(results.count(r => r) == 1)
                  case DETERMINER_ALL => Success(results.forall(r => r))
                  case DETERMINER_ANY => Success(results.exists(r => r))
                  case _ => fail("I don't know about this determiner")
                }
              }
            }
          }
          case ShlurdExistenceState() => {
            evaluateExistencePredicate(subject)
          }
          case ShlurdPropertyState(word) => {
            evaluatePropertyStatePredicate(subject, word)
          }
          case ShlurdLocationState(locative, location) => {
            evaluateLocationStatePredicate(subject, locative, location)
          }
          case ShlurdUnknownState => fail(
            "I don't know about this kind of state")
        }
      }
      case _ => fail(
        "I don't know about this kind of predicate")
    }
  }

  private def evaluateExistencePredicate(subjectRef : ShlurdReference)
      : Try[Boolean] =
  {
    // FIXME:  ambiguity etc
    world.resolveReference(subjectRef, REF_SUBJECT) match {
      case Success(entity) => Success(true)
      case Failure(e) => Failure(e)
    }
  }

  private def evaluatePropertyStatePredicate(
    subjectRef : ShlurdReference,
    state : ShlurdWord) : Try[Boolean] =
  {
    world.resolveReference(subjectRef, REF_SUBJECT) match {
      case Success(entity) => {
        world.resolveProperty(entity, state.lemma) match {
          case Success(property) => {
            world.evaluateEntityPropertyPredicate(
              entity, property, state.lemma)
          }
          case Failure(e) => Failure(e)
        }
      }
      case Failure(e) => Failure(e)
    }
  }

  private def evaluateLocationStatePredicate(
    subjectRef : ShlurdReference, locative : ShlurdLocative,
    locationRef : ShlurdReference) : Try[Boolean] =
  {
    world.resolveReference(subjectRef, REF_LOCATED) match {
      case Success(entity) => {
        world.resolveReference(locationRef, REF_LOCATION) match {
          case Success(location) => {
            world.evaluateEntityLocationPredicate(entity, location, locative)
          }
          case Failure(e) => Failure(e)
        }
      }
      case Failure(e) => Failure(e)
    }
  }
}
