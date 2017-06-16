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

class ShlurdInterpreter
{
  private val sentencePrinter = new ShlurdSentencePrinter

  def interpret(sentence : ShlurdSentence) : String =
  {
    sentence match {
      case ShlurdStateChangeCommand(predicate) => {
        evaluatePredicate(predicate) match {
          case Some(true) => {
            // FIXME:  use proper rephrasing
            "But it already is."
          }
          case Some(false) => {
            // FIXME:  carry out action
            "Okay, I will get right on that."
          }
          case _ => {
            "I don't know how to do that."
          }
        }
      }
      case ShlurdPredicateSentence(predicate, mood) => {
        mood match {
          case MOOD_INTERROGATIVE => {
            evaluatePredicate(predicate) match {
              case Some(truth) => {
                val responseMood = ShlurdIndicativeMood(truth)
                sentencePrinter.sb.confirmAssumption(
                  ASSUMED_TRUE, truth,
                  sentencePrinter.print(
                    ShlurdPredicateSentence(predicate, responseMood)))
              }
              case _ => {
                "I don't know."
              }
            }
          }
          case _ : ShlurdIndicativeMood => {
            val positivity = mood.isPositive
            val predicateTruth = evaluatePredicate(predicate)
            val responseMood = {
              predicateTruth match {
                case Some(false) => {
                  MOOD_INDICATIVE_NEGATIVE
                }
                case _ => {
                  // FIXME:  deal with uncertainty
                  MOOD_INDICATIVE_POSITIVE
                }
              }
            }
            predicateTruth match {
              case Some(truth) => {
                if (truth == positivity) {
                  // FIXME:  use strength like "That's right" instead of "Yes"
                  sentencePrinter.sb.confirmAssumption(
                    ASSUMED_TRUE, true,
                    sentencePrinter.print(
                      ShlurdPredicateSentence(predicate, responseMood)))
                } else {
                  // FIXME:  add details on inconsistency, and maybe try
                  // to update state?
                  "Oh, really?"
                }
              }
              case _ => {
                // FIXME:  try to update state?
                "Oh, I wasn't sure."
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

  private def evaluatePredicate(predicate : ShlurdPredicate) : Option[Boolean] =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        state match {
          case ShlurdPropertyState(word) => {
            evaluatePropertyStatePredicate(subject, word)
          }
          case ShlurdLocationState(locative, location) => {
            evaluateLocationStatePredicate(subject, locative, location)
          }
          case ShlurdUnknownState => None
        }
      }
      case ShlurdUnknownPredicate => None
    }
  }

  private def evaluatePropertyStatePredicate(
    subject : ShlurdReference,
    state : ShlurdWord) =
  {
    // FIXME
    None
  }

  private def evaluateLocationStatePredicate(
    subject : ShlurdReference, locative : ShlurdLocative,
    location : ShlurdReference) =
  {
    // FIXME
    None
  }
}
