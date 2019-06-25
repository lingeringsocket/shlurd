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
  val cc = SptCC(leaf)
  val comma = SptCOMMA(leaf)

  val np = SptNP(jj, nn)
  val vp = SptVP()
  val tmod = SptTMOD(np)

  val sNpVp = SptS(np, vp)
  val sTmodNpVp = SptS(tmod, np, vp)
  val sinv = SptSINV(vp, np)

  val JJ = jj.label
  val NN = nn.label
  val CC = cc.label
  val COMMA = comma.label

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
      matcher.addRule(S, Seq(NP, VP))
      matcher.addRule(SINV, Seq(VP, NP))
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
      matcher.addRule(S, Seq(NP, VP))
      matcher.matchPatterns(simpleSeq(tmod, np, vp), 1) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
    }

    "match ambiguous patterns" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(S, Seq(NP, VP))
      matcher.addRule(SINV, Seq(NP, VP))
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp, SptSINV(np, vp))
      ))
    }

    "match patterns with optional constituents" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(S, Seq(TMOD, OPTIONAL, NP, VP))
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
      matcher.addRule(NP, Seq(JJ, KLEENE, NN))
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

    "match patterns with repeat elements" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(NP, Seq(JJ, REPEAT, NN))
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
      matcher.addRule(S, Seq(sentence))
      matcher.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matcher.matchPatterns(simpleSeq(vp), 0) must be equalTo(Map(
        1 -> Set(SptS(vp))
      ))
    }

    "match patterns with nested repeats" in
    {
      val modifiedNoun = "modifiedNoun"
      val listElement = "listElement"
      val matcher = new SprPhrasePatternMatcher
      matcher.addSymbol(modifiedNoun, Seq(Seq(JJ, REPEAT, NN)))
      matcher.addSymbol(listElement, Seq(Seq(modifiedNoun, COMMA)))
      matcher.addRule(
        NP, Seq(listElement, listElement, REPEAT, CC, modifiedNoun))
      matcher.matchPatterns(
        simpleSeq(jj, nn, comma, jj, nn, comma,
          jj, jj, jj, nn, comma, cc, jj, jj, nn),
        0
      ) must be equalTo(Map(
        15 -> Set(SptNP(jj, nn, comma, jj, nn, comma,
          jj, jj, jj, nn, comma, cc, jj, jj, nn))
      ))
    }

    "match patterns with overlapping repeats" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(NP, Seq(JJ, REPEAT, NN))
      matcher.addRule(VP, Seq(JJ, REPEAT, CC))
      matcher.matchPatterns(simpleSeq(jj, jj, nn), 0) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
      matcher.matchPatterns(simpleSeq(jj, cc), 0) must be equalTo(Map(
        2 -> Set(SptVP(jj, cc))
      ))
    }

    "match patterns with optionals in symbols" in
    {
      val npSimple = "npSimple"
      val matcher = new SprPhrasePatternMatcher
      matcher.addSymbol(npSimple, Seq(Seq(JJ, OPTIONAL, NN)))
      matcher.addRule(NP, Seq(npSimple))
      matcher.matchPatterns(simpleSeq(nn), 0) must be equalTo(Map(
        1 -> Set(SptNP(nn))
      ))
    }

    "do not match cycles spuriously" in
    {
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(S, Seq(NP, REPEAT))
      matcher.addRule(TMOD, Seq(NP))
      matcher.matchPatterns(simpleSeq(np, np, np), 0) must be equalTo(Map(
        1 -> Set(tmod, SptS(np)),
        2 -> Set(SptS(np, np)),
        3 -> Set(SptS(np, np, np))
      ))
    }

    "prevent unknown labels" in
    {
      val unknown = "unknown"
      val matcher = new SprPhrasePatternMatcher
      matcher.addRule(S, Seq(unknown)) must
        throwA[IllegalArgumentException]
      matcher.addRule(unknown, Seq(NP, VP)) must
        throwA[IllegalArgumentException]
      matcher.addSymbol(sentence, Seq(Seq(unknown))) must
        throwA[IllegalArgumentException]
    }
  }
}
