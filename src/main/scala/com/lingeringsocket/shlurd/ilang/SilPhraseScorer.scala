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
package com.lingeringsocket.shlurd.ilang

case class SilPhraseScore(pro : Int, con : Int) extends Ordered[SilPhraseScore]
{
  def +(that : SilPhraseScore) =
    SilPhraseScore(this.pro + that.pro, this.con + that.con)

  def combine(that : SilPhraseScore) =
    SilPhraseScore(
      Math.max(this.pro, that.pro), Math.max(this.con, that.con))

  override def compare(that : SilPhraseScore) =
  {
    if (this == that) {
      0
    } else if (this.con != that.con) {
      that.con - this.con
    } else {
      this.pro - that.pro
    }
  }
}

object SilPhraseScore
{
  def neutral = SilPhraseScore(0, 0)

  def pro(n : Int) = {
    assert(n > 0)
    SilPhraseScore(n, 0)
  }

  def con(n : Int) = {
    assert(n > 0)
    SilPhraseScore(0, n)
  }

  def numeric(n : Int) = {
    if (n < 0) {
      con(-n)
    } else if (n > 0) {
      pro(n)
    } else {
      neutral
    }
  }

  def proBig = pro(100)

  def proSmall = pro(1)

  def conBig = con(100)

  def conSmall = con(1)
}

trait SilPhraseScorer
{
  def computeLocalScore(phrase : SilPhrase) : SilPhraseScore

  def computeGlobalScore(phrase : SilPhrase) : SilPhraseScore =
  {
    var score = SilPhraseScore.neutral
    val querier = new SilPhraseQuerier
    def scorer = querier.queryMatcher {
      case phrase : SilPhrase => {
        score = score + computeLocalScore(phrase)
      }
    }
    querier.query(scorer, phrase)
    if (false) {
      println("PHRASE = " + phrase)
      println("SCORE = " + score)
      println()
    }
    score
  }
}

object SilNeutralPhraseScorer extends SilPhraseScorer
{
  override def computeLocalScore(phrase : SilPhrase) =
    SilPhraseScore.neutral
}
