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
package com.lingeringsocket.shlurd.cli

import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._
import org.specs2.specification._

class ShlurdPrimordialWordnetSpec extends Specification
{
  abstract class ResponderContext extends Scope
  {
    protected val cosmos = ShlurdPrimordialWordnet.newMutableCosmos
    protected val mind = new SpcWordnetMind(cosmos)
    protected val responder =
      new SpcResponder(
        mind, SpcBeliefParams(ACCEPT_NEW_BELIEFS), SmcResponseParams())

    protected def processBelief(input : String) =
    {
      process(input, "OK.")
    }

    protected def process(input : String, expected : String) =
    {
      val parseResult = responder.newParser(input).parseOne
      s"pass:  $input" ==> (
        responder.process(parseResult, input) === expected)
    }
  }

  "ShlurdPrimordialWordnet" should
  {
    "load forms" in
    {
      val cosmos = ShlurdPrimordialWordnet.frozenCosmos
      val dogOpt = cosmos.resolveForm("wnf-dog-1")
      dogOpt must beSome
      val dogForm = dogOpt.get
      val androsOpt = cosmos.resolveForm("wnf-man-1")
      androsOpt must beSome
      val androsForm = androsOpt.get
      val anthroposOpt = cosmos.resolveForm("wnf-man-4")
      anthroposOpt must beSome.which(_ != androsForm)
      val anthroposForm = anthroposOpt.get
      cosmos.resolveForm("wnf-human-1") must beSome.which(_ == anthroposForm)
      cosmos.resolveForm("wnf-xyzzy-1") must beEmpty
      val puppyOpt = cosmos.resolveForm("wnf-puppy-1")
      puppyOpt must beSome
      val puppyForm = puppyOpt.get
      val someoneOpt = cosmos.resolveForm(SmcLemmas.LEMMA_SOMEONE)
      someoneOpt must beSome
      val someoneForm = someoneOpt.get
      cosmos.isHyponym(puppyForm, dogForm) must beTrue
      cosmos.isHyponym(dogForm, puppyForm) must beFalse
      cosmos.isHyponym(puppyForm, anthroposForm) must beFalse
      cosmos.isHyponym(puppyForm, someoneForm) must beFalse
      cosmos.isHyponym(anthroposForm, someoneForm) must beFalse
      cosmos.isHyponym(androsForm, someoneForm) must beTrue
      cosmos.isHyponym(someoneForm, androsForm) must beFalse
    }

    "provide ontology to parser" in new ResponderContext
    {
      process(
        "which animals exist",
        "No animals exist.")
      processBelief("a pokemon is a kind of animal")
      processBelief("Pikachu is a pokemon")
      process(
        "which animals exist",
        "Pikachu exists.")
      process(
        "which organisms exist",
        "Pikachu exists.")
    }
  }
}
