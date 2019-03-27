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
package com.lingeringsocket.shlurd

import org.specs2.mutable._

class ShlurdWordnetSpec extends Specification
{
  "ShlurdWordnet" should
  {
    "detect potential adverbs" in
    {
      ShlurdWordnet.isPotentialAdverb("quickly") must beTrue
      ShlurdWordnet.isPotentialAdverb("slow") must beTrue
      ShlurdWordnet.isPotentialAdverb("smile") must beFalse
    }

    "detect potential nouns" in
    {
      ShlurdWordnet.isPotentialNoun("defenestrate") must beFalse
      ShlurdWordnet.isPotentialNoun("kill") must beTrue
      ShlurdWordnet.isPotentialNoun("smile") must beTrue
      ShlurdWordnet.isPotentialNoun("steak knife") must beTrue
    }

    "detect potential gerunds" in
    {
      ShlurdWordnet.isPotentialGerund("running") must beTrue
      ShlurdWordnet.isPotentialGerund("king") must beFalse
      ShlurdWordnet.isPotentialGerund("run") must beFalse
    }

    "detect potential plurals" in
    {
      ShlurdWordnet.isPotentialPlural("horses") must beTrue
      ShlurdWordnet.isPotentialPlural("mice") must beTrue
      ShlurdWordnet.isPotentialPlural("horse") must beFalse
    }

    "detect plain words" in
    {
      ShlurdWordnet.isPlainWord("mouse") must beTrue
      ShlurdWordnet.isPlainWord("NYC") must beFalse
    }

    "get verb frames" in
    {
      ShlurdWordnet.getVerbFrames("defenestrate") must be equalTo Seq(
        "Somebody ----s something",
        "Somebody ----s somebody"
      )
      ShlurdWordnet.getVerbFrames("distinguish") must be equalTo Seq(
        "Somebody ----s something",
        "Somebody ----s somebody",
        "Somebody ----s something from somebody",
        "Something ----s somebody",
        "Something ----s something"
      )
    }

    "get verb frame flags" in
    {
      val flags = ShlurdWordnet.getVerbFrameFlags("defenestrate")
      flags.size must be equalTo 2
      flags(7) must beFalse
      flags(8) must beTrue
      flags(9) must beTrue
    }

    "get gloss definitions" in
    {
      ShlurdWordnet.getVerbSenses("pester").flatMap(
        ShlurdWordnet.getGlossDefinitions
      ) must be equalTo Seq(
        "annoy persistently"
      )
      ShlurdWordnet.getNounSenses("gallop").flatMap(
        ShlurdWordnet.getGlossDefinitions
      ) must be equalTo Seq(
        "a fast gait of a horse",
        "a two-beat stride during which all four legs " +
          "are off the ground simultaneously"
      )
    }

    "get gloss examples" in
    {
      ShlurdWordnet.getVerbSenses("pester").flatMap(
        ShlurdWordnet.getGlossExamples
      ) must be equalTo Seq(
        "The children teased the boy because of his stammer"
      )
      ShlurdWordnet.getNounSenses("pickle").flatMap(
        ShlurdWordnet.getGlossExamples
      ) must be equalTo Seq(
        "he got into a terrible fix",
        "he made a muddle of his marriage")
      ShlurdWordnet.getVerbSenses("pickle").flatMap(
        ShlurdWordnet.getGlossExamples
      ) must be equalTo Seq()
    }

    "use simple sense keys" in
    {
      val senses = ShlurdWordnet.getVerbSenses("defenestrate")
      senses.size must be equalTo 1
      val sense = senses.head
      val senseId = ShlurdWordnet.getSenseId(sense)
      senseId must be equalTo "v:1511516"
      val found = ShlurdWordnet.findSense(senseId)
      found must be equalTo sense
      ShlurdWordnet.findSenses(senseId) must be equalTo Seq(sense)
    }

    "use compound sense keys" in
    {
      val senses = ShlurdWordnet.getVerbSenses("smile")
      senses.size must be equalTo 2
      val senseId = ShlurdWordnet.getSenseId(senses)
      senseId must be equalTo "v:28558|v:1069534"
      val found = ShlurdWordnet.findSenses(senseId)
      found must be equalTo senses
      ShlurdWordnet.findSenses("") must beEmpty
    }

    "get lex file names" in
    {
      val senses = ShlurdWordnet.getNounSenses("firefighter")
      senses.head.getLexFileName must be equalTo "noun.person"
    }
  }
}
