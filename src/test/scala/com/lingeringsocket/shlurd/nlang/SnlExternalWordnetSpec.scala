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
package com.lingeringsocket.shlurd.nlang

import org.specs2.mutable._

object SnlExternalWordnetSpec
{
  private val wordnet = new SnlExternalWordnet("/spanish_net.xml")
}

class SnlExternalWordnetSpec extends Specification
{
  import SnlExternalWordnetSpec._

  "SnlExternalWordnet" should
  {
    "support multiple languages" in
    {
      val senses = wordnet.getVerbSenses("caminar")
      senses.size must be equalTo 1
      val sense = senses.head
      val senseId = wordnet.getSenseId(sense)
      senseId must be equalTo "v:0"
      val found = wordnet.findSense(senseId)
      found must be equalTo sense
      wordnet.findSenses(senseId) must be equalTo Seq(sense)
    }

    "support plurals" in
    {
      // regular singular noun declared in wordnet
      wordnet.isPotentialPlural("cabra") must beFalse

      // regular plural noun with singular declared in wordnet
      wordnet.isPotentialPlural("cabras") must beTrue

      // noun not declared in wordnet
      wordnet.isPotentialPlural("crisis") must beFalse

      // noun with exceptional form declared in wordnet
      wordnet.isPotentialPlural("leones") must beTrue
    }
  }
}
