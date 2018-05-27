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

class ShlurdPlatonicInterpreter(
  cosmos : ShlurdPlatonicCosmos,
  acceptNewBeliefs : Boolean = false)
    extends ShlurdInterpreter(cosmos)
{
  private val beliefInterpreter = new ShlurdPlatonicBeliefInterpreter(cosmos)

  override protected def interpretImpl(sentence : SilSentence) : String =
  {
    if (acceptNewBeliefs && sentence.mood.isIndicative) {
      beliefInterpreter.recognizeBelief(sentence) match {
        case Some(belief) => {
          debug(s"APPLYING NEW BELIEF : $belief")
          try {
            beliefInterpreter.applyBelief(belief)
          } catch {
            case ex : RejectedBelief => {
              debug("NEW BELIEF REJECTED", ex)
              return respondContradiction(ex)
            }
          }
          debug("NEW BELIEF ACCEPTED")
          return sentencePrinter.sb.respondCompliance
        }
        case _ =>
      }
    }
    super.interpretImpl(sentence)
  }

  // FIXME:  i18n
  private def respondContradiction(ex : RejectedBelief) : String =
  {
    val beliefString = printBelief(ex.belief)
    ex match {
      case IncomprehensibleBelief(belief) => {
        s"I am unable to understand the belief that ${beliefString}."
      }
      case ContradictoryBelief(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}." +
          s"  So I am unable to accept that ${beliefString}."
      }
      case AmbiguousBelief(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}." +
          s"  So I am unclear how to interpret the belief that ${beliefString}."
      }
      case IncrementalCardinalityViolation(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}." +
          s"  So it does not add up when I hear that ${beliefString}."
      }
    }
  }

  private def printBelief(belief : SilSentence) : String =
  {
    val punctuated = belief.maybeSyntaxTree match {
      case Some(syntaxTree) => syntaxTree.toWordString
      case _ => sentencePrinter.print(belief)
    }
    // FIXME:  need a cleaner way to omit full stop
    punctuated.dropRight(1).trim
  }
}
