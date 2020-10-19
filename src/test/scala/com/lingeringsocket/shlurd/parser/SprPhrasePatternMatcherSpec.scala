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
import org.specs2.specification._

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

  trait MatcherContext extends Scope
  {
    protected val matcher = new SprPhrasePatternMatcher

    protected def matchPatterns(
      seq : Seq[Set[SprSyntaxTree]], start : Int = 0) =
    {
      matcher.matchPatterns(seq.drop(start).to(LazyList))
    }
  }

  private def simpleSeq(trees : SprSyntaxTree*) : Seq[Set[SprSyntaxTree]] =
  {
    trees.map(Set(_))
  }

  "SprPhrasePatternMatcher" should
  {
    "match simple patterns" in new MatcherContext
    {
      matcher.addRule(S, Seq(NP, VP))
      matcher.addRule(SINV, Seq(VP, NP))
      matchPatterns(simpleSeq(np, vp)) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matchPatterns(simpleSeq(np, np)) must be equalTo(Map(
        1 -> Set.empty
      ))
      matchPatterns(simpleSeq(vp, np)) must be equalTo(Map(
        2 -> Set(sinv)
      ))
    }

    "match middle of pattern" in new MatcherContext
    {
      matcher.addRule(S, Seq(NP, VP))
      matchPatterns(simpleSeq(tmod, np, vp), 1) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
    }

    "match ambiguous patterns" in new MatcherContext
    {
      matcher.addRule(S, Seq(NP, VP))
      matcher.addRule(SINV, Seq(NP, VP))
      matchPatterns(simpleSeq(np, vp)) must be equalTo(Map(
        2 -> Set(sNpVp, SptSINV(np, vp))
      ))
    }

    "match patterns with optional constituents" in new MatcherContext
    {
      matcher.addRule(S, Seq(TMOD, OPTIONAL, NP, VP))
      matchPatterns(simpleSeq(np, vp)) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matchPatterns(simpleSeq(tmod, np, vp)) must be equalTo(Map(
        3 -> Set(sTmodNpVp)
      ))
      matchPatterns(simpleSeq(tmod, tmod, np, vp)) must be equalTo(Map(
        1 -> Set.empty
      ))
    }

    "match patterns with Kleene star" in new MatcherContext
    {
      matcher.addRule(NP, Seq(JJ, KLEENE, NN))
      matchPatterns(simpleSeq(nn)) must be equalTo(Map(
        1 -> Set(SptNP(nn))
      ))
      matchPatterns(simpleSeq(jj, nn)) must be equalTo(Map(
        2 -> Set(np)
      ))
      matchPatterns(simpleSeq(jj, jj, nn)) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns with repeat elements" in new MatcherContext
    {
      matcher.addRule(NP, Seq(JJ, REPEAT, NN))
      matchPatterns(simpleSeq(nn)) must be equalTo(Map(
        0 -> Set.empty
      ))
      matchPatterns(simpleSeq(jj, nn)) must be equalTo(Map(
        2 -> Set(np)
      ))
      matchPatterns(simpleSeq(jj, jj, nn)) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns defined via symbols" in new MatcherContext
    {
      matcher.addSymbol(sentence, Seq(Seq(NP, VP), Seq(VP)))
      matcher.addRule(S, Seq(sentence))
      matchPatterns(simpleSeq(np, vp)) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      matchPatterns(simpleSeq(vp)) must be equalTo(Map(
        1 -> Set(SptS(vp))
      ))
    }

    "match patterns with nested repeats" in new MatcherContext
    {
      val modifiedNoun = "modifiedNoun"
      val listElement = "listElement"
      matcher.addSymbol(modifiedNoun, Seq(Seq(JJ, REPEAT, NN)))
      matcher.addSymbol(listElement, Seq(Seq(modifiedNoun, COMMA)))
      matcher.addRule(
        NP, Seq(listElement, listElement, REPEAT, CC, modifiedNoun))
      matchPatterns(
        simpleSeq(jj, nn, comma, jj, nn, comma,
          jj, jj, jj, nn, comma, cc, jj, jj, nn)
      ) must be equalTo(Map(
        15 -> Set(SptNP(jj, nn, comma, jj, nn, comma,
          jj, jj, jj, nn, comma, cc, jj, jj, nn))
      ))
    }

    "match patterns with overlapping repeats" in new MatcherContext
    {
      matcher.addRule(NP, Seq(JJ, REPEAT, NN))
      matcher.addRule(VP, Seq(JJ, REPEAT, CC))
      matchPatterns(simpleSeq(jj, jj, nn)) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
      matchPatterns(simpleSeq(jj, cc)) must be equalTo(Map(
        2 -> Set(SptVP(jj, cc))
      ))
    }

    "match patterns with optionals in symbols" in new MatcherContext
    {
      val npSimple = "npSimple"
      matcher.addSymbol(npSimple, Seq(Seq(JJ, OPTIONAL, NN)))
      matcher.addRule(NP, Seq(npSimple))
      matchPatterns(simpleSeq(nn)) must be equalTo(Map(
        1 -> Set(SptNP(nn))
      ))
    }

    "do not match cycles spuriously" in new MatcherContext
    {
      matcher.addRule(S, Seq(NP, REPEAT))
      matcher.addRule(TMOD, Seq(NP))
      matchPatterns(simpleSeq(np, np, np)) must be equalTo(Map(
        1 -> Set(tmod, SptS(np)),
        2 -> Set(SptS(np, np)),
        3 -> Set(SptS(np, np, np))
      ))
    }

    "prevent unknown labels" in new MatcherContext
    {
      val unknown = "unknown"
      matcher.addRule(S, Seq(unknown)) must
        throwA[IllegalArgumentException]
      matcher.addRule(unknown, Seq(NP, VP)) must
        throwA[IllegalArgumentException]
      matcher.addSymbol(sentence, Seq(Seq(unknown))) must
        throwA[IllegalArgumentException]
    }
  }
}
