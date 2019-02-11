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
package com.lingeringsocket.shlurd.platonic

import org.specs2.mutable._

class SpcWordnetSpec extends Specification
{
  "SpcWordnet" should
  {
    "load forms" in
    {
      val cosmos = new SpcCosmos
      // FIXME:  speed this up
      // SpcPrimordial.initCosmos(cosmos)
      val wordnet = new SpcWordnet(cosmos)
      cosmos.resolveForm("dog") must beEmpty
      wordnet.loadAll
      val dogOpt = cosmos.resolveForm("dog")
      dogOpt must beSome
      val dogForm = dogOpt.get
      val manOpt = cosmos.resolveForm("man")
      manOpt must beSome
      val manForm = manOpt.get
      cosmos.resolveForm("human") must beSome.which(_ == manForm)
      cosmos.resolveForm("xyzzy") must beEmpty
      val puppyOpt = cosmos.resolveForm("puppy")
      puppyOpt must beSome
      val puppyForm = puppyOpt.get
      val graph = cosmos.getGraph
      graph.isHyponym(puppyForm, dogForm) must beTrue
      graph.isHyponym(dogForm, puppyForm) must beFalse
      graph.isHyponym(puppyForm, manForm) must beFalse
    }
  }
}
