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

object SprPhrasePatternTrieSpec
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

class SprPhrasePatternTrieSpec extends Specification
{
  import SprPhrasePatternTrieSpec._

  private def simpleSeq(trees : SprSyntaxTree*) : Seq[Set[SprSyntaxTree]] =
  {
    trees.map(Set(_))
  }

  "SprPhrasePatternTrie" should
  {
    "match simple patterns" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(NP, VP), S)
      trie.addPattern(Seq(VP, NP), SINV)
      trie.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      trie.matchPatterns(simpleSeq(np, np), 0) must beEmpty
      trie.matchPatterns(simpleSeq(vp, np), 0) must be equalTo(Map(
        2 -> Set(sinv)
      ))
    }

    "match middle of pattern" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(NP, VP), S)
      trie.matchPatterns(simpleSeq(tmod, np, vp), 1) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
    }

    "match ambiguous patterns" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(NP, VP), S)
      trie.addPattern(Seq(NP, VP), SINV)
      trie.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp, SptSINV(np, vp))
      ))
    }

    "match patterns with optional constituents" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(TMOD, OPTIONAL, NP, VP), S)
      trie.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      trie.matchPatterns(simpleSeq(tmod, np, vp), 0) must be equalTo(Map(
        3 -> Set(sTmodNpVp)
      ))
      trie.matchPatterns(simpleSeq(tmod, tmod, np, vp), 0) must be beEmpty
    }

    "match patterns with Kleene star" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(JJ, KLEENE, NN), NP)
      trie.matchPatterns(simpleSeq(nn), 0) must be equalTo(Map(
        1 -> Set(SptNP(nn))
      ))
      trie.matchPatterns(simpleSeq(jj, nn), 0) must be equalTo(Map(
        2 -> Set(np)
      ))
      trie.matchPatterns(simpleSeq(jj, jj, nn), 0) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns with repeat constituents" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(JJ, REPEAT, NN), NP)
      trie.matchPatterns(simpleSeq(nn), 0) must beEmpty
      trie.matchPatterns(simpleSeq(jj, nn), 0) must be equalTo(Map(
        2 -> Set(np)
      ))
      trie.matchPatterns(simpleSeq(jj, jj, nn), 0) must be equalTo(Map(
        3 -> Set(SptNP(jj, jj, nn))
      ))
    }

    "match patterns defined via symbols" in
    {
      val trie = new SprPhrasePatternTrie
      trie.addSymbol(sentence, Seq(Seq(NP, VP), Seq(VP)))
      trie.addPattern(Seq(sentence), S)
      trie.matchPatterns(simpleSeq(np, vp), 0) must be equalTo(Map(
        2 -> Set(sNpVp)
      ))
      trie.matchPatterns(simpleSeq(vp), 0) must be equalTo(Map(
        1 -> Set(SptS(vp))
      ))
    }

    "prevent unknown labels" in
    {
      val unknown = "unknown"
      val trie = new SprPhrasePatternTrie
      trie.addPattern(Seq(unknown), S) must
        throwA[IllegalArgumentException]
      trie.addPattern(Seq(NP, VP), unknown) must
        throwA[IllegalArgumentException]
      trie.addSymbol(sentence, Seq(Seq(unknown))) must
        throwA[IllegalArgumentException]
    }
  }
}
