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
package com.lingeringsocket.shlurd.parser

import org.specs2.mutable._

import scala.collection._

object SprPhrasePatternMatcherSpec
{
  val leaf = SprSyntaxLeaf("", "", "")
  val jj = SptJJ(leaf)
  val nn = SptNN(leaf)

  val np = SptNP(jj, nn)
  val vp = SptVP()
  val tmod = SptTMOD(np)

  val sNpVp = SptS(np, vp)
  val sTmodNpVp = SptS(tmod, np, vp)
  val sinv = SptSINV(vp, np)

  val JJ = jj.label
  val NN = nn.label

  val NP = np.label
  val VP = vp.label
  val TMOD = tmod.label

  val S = sNpVp.label
  val SINV = sinv.label

  val OPTIONAL = "?"
  val REPEAT = "+"
  val KLEENE = "*"

  val sentence = "sentence"
}

class SprPhrasePatternMatcherSpec extends Specification
{
  import SprPhrasePatternMatcherSpec._

  private def simpleSeq(trees : SprSyntaxTree*) : Seq[Set[SprSyntaxTree]] =
  {
    trees.map(Set(_))
  }

  "SprPhrasePatternMatcher" should
  {
    "match simple patterns" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(NP, VP), S)
      matcher.addPattern(Seq(VP, NP), SINV)
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matcher.matchPatterns(simpleSeq(np, np), 0) must beEmpty
      matcher.matchPatterns(simpleSeq(vp, np), 0) must be equalTo(Map(
        2 -> Set(sinv)
      ))
    }

    "match middle of pattern" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(NP, VP), S)
      matcher.matchPatterns(simpleSeq(tmod, np, vp), 1) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
    }

    "match ambiguous patterns" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(NP, VP), S)
      matcher.addPattern(Seq(NP, VP), SINV)
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp, SptSINV(np, vp))
      ))
    }

    "match patterns with optional constituents" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(TMOD, OPTIONAL, NP, VP), S)
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matcher.matchPatterns(simpleSeq(tmod, np, vp), 0) must be equalTo(Map(
        3 -> Set(sTmodNpVp)
      ))
      matcher.matchPatterns(simpleSeq(tmod, tmod, np, vp), 0) must be beEmpty
    }

    "match patterns with Kleene star" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(JJ, KLEENE, NN), NP)
      matcher.matchPatterns(simpleSeq(nn), 0) must be equalTo(Map(
        1 -> Set(SptNP(nn))
      ))
      matcher.matchPatterns(simpleSeq(jj, nn), 0) must be equalTo(Map(
        2 -> Set(np)
      ))
      matcher.matchPatterns(simpleSeq(jj, jj, nn), 0) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns with repeat constituents" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(JJ, REPEAT, NN), NP)
      matcher.matchPatterns(simpleSeq(nn), 0) must beEmpty
      matcher.matchPatterns(simpleSeq(jj, nn), 0) must be equalTo(Map(
        2 -> Set(np)
      ))
      matcher.matchPatterns(simpleSeq(jj, jj, nn), 0) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns defined via symbols" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addSymbol(sentence, Seq(Seq(NP, VP), Seq(VP)))
      matcher.addPattern(Seq(sentence), S)
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matcher.matchPatterns(simpleSeq(vp), 0) must be equalTo(Map(
        1 -> Set(SptS(vp))
      ))
    }

    "prevent unknown labels" in
    {
      val unknown = "unknown"
      val matcher = new SprPhrasePatternMatcher
      matcher.addPattern(Seq(unknown), S) must
        throwA[IllegalArgumentException]
      matcher.addPattern(Seq(NP, VP), unknown) must
        throwA[IllegalArgumentException]
      matcher.addSymbol(sentence, Seq(Seq(unknown))) must
        throwA[IllegalArgumentException]
    }
  }
}
