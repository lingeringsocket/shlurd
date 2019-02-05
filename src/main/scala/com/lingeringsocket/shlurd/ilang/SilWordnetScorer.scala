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

import com.lingeringsocket.shlurd._

class SilWordnetScorer extends SilPhraseScorer
{
  type PhraseScorer = PartialFunction[SilPhrase, SilPhraseScore]

  private val dictionary = ShlurdWordnet.dictionary

  private val morphology = ShlurdWordnet.morphology

  private def phraseScorer(s : PhraseScorer)
      : PhraseScorer = s

  private def phraseScorers() = Seq(
    scoreGenitives,
    scoreVerbTransitivity
  )

  private def scoreGenitives = phraseScorer {
    case SilGenitiveReference(
      _,
      _ : SilPronounReference
    ) => {
      SilPhraseScore.conBig
    }
  }

  private def scoreVerbTransitivity = phraseScorer {
    case SilActionPredicate(
      _,
      action,
      Some(_),
      _
    ) => {
      if (ShlurdWordnet.isTransitiveVerb(action.lemma)) {
        SilPhraseScore.proSmall
      } else {
        SilPhraseScore.conBig
      }
    }
  }

  override def computeLocalScore(phrase : SilPhrase) =
  {
    phraseScorers().map(_.lift(phrase).getOrElse(SilPhraseScore.neutral)).
      reduceLeft(_ + _)
  }
}
