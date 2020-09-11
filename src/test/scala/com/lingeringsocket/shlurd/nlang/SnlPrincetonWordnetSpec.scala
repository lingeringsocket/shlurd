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

import net.sf.extjwnl.data._

import org.specs2.mutable._

object SnlPrincetonWordnetSpec
{
  private val wordnet = SnlPrincetonWordnet
}

class SnlPrincetonWordnetSpec extends Specification
{
  import SnlPrincetonWordnetSpec._

  "SnlPrincetonWordnet" should
  {
    "detect potential adverbs" in
    {
      wordnet.isPotentialAdverb("quickly") must beTrue
      wordnet.isPotentialAdverb("slow") must beTrue
      wordnet.isPotentialAdverb("smile") must beFalse
    }

    "detect potential verbs" in
    {
      wordnet.isPotentialVerb("run") must beTrue
      wordnet.isPotentialVerb("stir fry") must beTrue
      wordnet.isPotentialVerb("frogmarch") must beTrue
      wordnet.isPotentialVerb("bump off") must beTrue
      wordnet.isPotentialVerb("highchair") must beFalse
    }

    "detect potential nouns" in
    {
      wordnet.isPotentialNoun("defenestrate") must beFalse
      wordnet.isPotentialNoun("kill") must beTrue
      wordnet.isPotentialNoun("smile") must beTrue
      wordnet.isPotentialNoun("steak knife") must beTrue
    }

    "detect potential gerunds" in
    {
      wordnet.isPotentialGerund("running") must beTrue
      wordnet.isPotentialGerund("king") must beFalse
      wordnet.isPotentialGerund("run") must beFalse
    }

    "detect potential plurals" in
    {
      wordnet.isPotentialPlural("horses") must beTrue
      wordnet.isPotentialPlural("mice") must beTrue
      wordnet.isPotentialPlural("horse") must beFalse
    }

    "detect plain words" in
    {
      wordnet.isPlainWord("mouse") must beTrue
      wordnet.isPlainWord("boss") must beTrue
      wordnet.isPlainWord("NYC") must beFalse
    }

    "get verb frames" in
    {
      wordnet.getVerbFrames("defenestrate") must be equalTo Seq(
        "Somebody ----s something",
        "Somebody ----s somebody"
      )
      wordnet.getVerbFrames("distinguish") must be equalTo Seq(
        "Somebody ----s something",
        "Somebody ----s somebody",
        "Somebody ----s something from somebody",
        "Something ----s somebody",
        "Something ----s something"
      )
    }

    "get verb frame flags" in
    {
      val flags = wordnet.getVerbFrameFlags("defenestrate")
      flags.size must be equalTo 2
      flags(7) must beFalse
      flags(8) must beTrue
      flags(9) must beTrue
    }

    "get gloss definitions" in
    {
      wordnet.getVerbSenses("pester").flatMap(
        wordnet.getGlossDefinitions
      ) must be equalTo Seq(
        "annoy persistently"
      )
      wordnet.getNounSenses("gallop").flatMap(
        wordnet.getGlossDefinitions
      ) must be equalTo Seq(
        "a fast gait of a horse",
        "a two-beat stride during which all four legs " +
          "are off the ground simultaneously"
      )
    }

    "get gloss examples" in
    {
      wordnet.getVerbSenses("pester").flatMap(
        wordnet.getGlossExamples
      ) must be equalTo Seq(
        "The children teased the boy because of his stammer"
      )
      wordnet.getNounSenses("pickle").flatMap(
        wordnet.getGlossExamples
      ) must be equalTo Seq(
        "he got into a terrible fix",
        "he made a muddle of his marriage")
      wordnet.getVerbSenses("pickle").flatMap(
        wordnet.getGlossExamples
      ) must be equalTo Seq()
    }

    "use simple sense keys" in
    {
      val senses = wordnet.getVerbSenses("defenestrate")
      senses.size must be equalTo 1
      val sense = senses.head
      val senseId = wordnet.getSenseId(sense)
      senseId must be equalTo "v:1511516"
      val found = wordnet.findSense(senseId)
      found must be equalTo sense
      wordnet.findSenses(senseId) must be equalTo Seq(sense)
    }

    "use compound sense keys" in
    {
      val senses = wordnet.getVerbSenses("smile")
      senses.size must be equalTo 2
      val senseId = wordnet.getSenseId(senses)
      senseId must be equalTo "v:28558|v:1069534"
      val found = wordnet.findSenses(senseId)
      found must be equalTo senses
      wordnet.findSenses("") must beEmpty
    }

    "get lex file names" in
    {
      val senses = wordnet.getNounSenses("firefighter")
      senses.head.getLexFileName must be equalTo "noun.person"
    }

    "compute usage scores" in
    {
      wordnet.getUsageScore("small", POS.ADJECTIVE) must be equalTo 42
      wordnet.getUsageScore("small", POS.NOUN) must be equalTo 0
      wordnet.getUsageScore("small", POS.ADVERB) must be equalTo 0
      wordnet.getUsageScore("small", POS.VERB) must be equalTo -1
      wordnet.getUsageScore("red", POS.ADJECTIVE) must be equalTo 8
      wordnet.getUsageScore("red", POS.NOUN) must be equalTo 1
      wordnet.getUsageScore("young", POS.NOUN) must be equalTo 1
      wordnet.getUsageScore("young", POS.ADJECTIVE) must be equalTo 21
      wordnet.getUsageScore("man", POS.NOUN) must be equalTo 149
      wordnet.getUsageScore("man", POS.VERB) must be equalTo 0
      wordnet.getUsageScore("back", POS.ADVERB) must be equalTo 18
    }
  }
}
