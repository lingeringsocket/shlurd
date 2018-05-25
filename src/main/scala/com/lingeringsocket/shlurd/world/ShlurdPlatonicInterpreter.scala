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

import ShlurdPlatonicWorld._

class ShlurdPlatonicInterpreter(
  world : ShlurdPlatonicWorld,
  acceptNewBeliefs : Boolean = false)
    extends ShlurdInterpreter(world)
{
  private val beliefInterpreter = new ShlurdPlatonicBeliefInterpreter(world)

  override protected def interpretImpl(sentence : SilSentence) : String =
  {
    if (acceptNewBeliefs && sentence.mood.isIndicative) {
      try {
        beliefInterpreter.interpretBelief(sentence)
        debug("NEW BELIEF ACCEPTED")
        return sentencePrinter.sb.respondCompliance
      } catch {
        case _ : IncomprehensibleBelief => {
          // fall through
        }
        case ex : ContradictedBelief => {
          debug("NEW BELIEF REJECTED", ex)
          return ex.getMessage
        }
      }
    }
    super.interpretImpl(sentence)
  }
}
