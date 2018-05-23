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

import org.specs2.mutable._
class ShlurdPlatonicCreedSpec extends Specification
{
  trait WorldContext extends NameSpace
  {
    protected val world = new ShlurdPlatonicWorld

    protected val creed = new ShlurdPlatonicCreed(world)

    protected def addBelief(input : String) =
    {
      val sentence = ShlurdParser(input).parseOne
      world.addBelief(sentence)
    }
  }

  private def expectBeliefs(
    expected : Iterable[String], actual : Iterable[ShlurdSentence]) =
  {
    val printer = new ShlurdSentencePrinter
    val beliefs = actual.map(printer.print)
    beliefs.map(ShlurdParseUtils.capitalize) must be equalTo expected
  }

  "ShlurdPlatonicCreed" should
  {
    "preserve states" in new WorldContext
    {
      val originalMust = "A door must be open or closed."
      val originalMay = "A window may be open or closed."
      addBelief(originalMust)
      addBelief(originalMay)
      expectBeliefs(Seq(originalMust, originalMay), creed.allBeliefs)
    }

    "preserve state alias" in new WorldContext
    {
      val original = "A lit light is on."
      addBelief(original)
      expectBeliefs(Seq(original), creed.allBeliefs)
    }


    "preserve state normalizations" in new WorldContext
    {
      val original = "A person at home is present."
      addBelief(original)
      expectBeliefs(Seq(original), creed.allBeliefs)
    }
  }
}
