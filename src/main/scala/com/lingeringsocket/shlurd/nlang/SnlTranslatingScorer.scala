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

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.data._

class SnlTranslatingScorer(
  companion : SprWordnetScorer,
  sourceTongue : SprTongue,
  targetTongue : SprTongue,
  alignment : SnlWordnetAlignment,
  direction : SnlTranslationDirection
) extends SilPhraseScorer
{
  private def phraseScorers() = Seq(
    scoreActionFrames
  )

  override def computeLocalScore(phrase : SilPhrase) =
  {
    companion.computeLocalScore(phrase) + phraseScorers().map(
      _.lift(phrase).getOrElse(SilPhraseScore.neutral)
    ).reduceLeft(_ + _)
  }

  private def scoreActionFrames = SprWordnetScorer.phraseScorer {
    case SilActionPredicate(
      subject,
      SilWordLemma(lemma),
      directObject,
      modifiers
    ) => {
      if (sourceTongue.isBeingLemma(lemma)) {
        SilPhraseScore.conSmall
      } else {
        sourceTongue.getWordnet.getWordSenses(POS.VERB, lemma).map(sense => {
          alignment.mapSense(sense, direction).map(targetSense => {
            val frameFlags = targetTongue.getWordnet.getVerbFrameFlags(
              targetSense)
            val matched = SprWordnetScorer.matchAction(
              sourceTongue, frameFlags,
              subject, directObject, modifiers)
            if (matched > 0) {
              SilPhraseScore.pro(matched)
            } else {
              SilPhraseScore.neutral
            }
          }).getOrElse(SilPhraseScore.neutral)
        }).reduceLeftOption(_ + _).getOrElse(SilPhraseScore.neutral)
      }
    }
  }
}
