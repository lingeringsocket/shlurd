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
import com.lingeringsocket.shlurd.parser._

import SprEnglishLemmas._

class SilWordnetScorer extends SilPhraseScorer with SprEnglishWordAnalyzer
{
  type PhraseScorer = PartialFunction[SilPhrase, SilPhraseScore]

  private val dictionary = ShlurdWordnet.dictionary

  private val morphology = ShlurdWordnet.morphology

  private def phraseScorer(s : PhraseScorer)
      : PhraseScorer = s

  private def phraseScorers() = Seq(
    scoreGenitives,
    scoreVerbTransitivity,
    scoreVerbModifiers,
    scoreAdverbStates,
    scoreCompoundAdpositions,
    scoreNounStates,
    scoreLonger,
    scoreGerundNouns,
    scoreNestedAdpositions,
    scoreConjunctiveNouns,
    scoreSpecialAdpositions
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

  private def scoreVerbModifiers = phraseScorer {
    case SilBasicVerbModifier(_, score) => {
      if (score < 0) {
        SilPhraseScore.conSmall
      } else if (score > 0) {
        SilPhraseScore.proSmall
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  private def scoreAdverbStates = phraseScorer {
    case SilStateSpecifiedReference(
      _,
      SilPropertyState(word)
    ) if (ShlurdWordnet.isPotentialAdverb(word.inflected)) => {
      SilPhraseScore.conSmall
    }
    case SilStateSpecifiedReference(
      SilNounReference(noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
      _ : SilAdpositionalState
    ) if (ShlurdWordnet.isPotentialAdverb(noun.inflected)) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreCompoundAdpositions = phraseScorer {
    case ap : SilAdpositionalPhrase if (ap.adposition.words.size > 1) => {
      if (ap.adposition.words.forall(word => isAdposition(word.lemma))) {
        SilPhraseScore.conSmall
      } else {
        SilPhraseScore.proBig
      }
    }
  }

  private def scoreNounStates = phraseScorer {
    case SilRelationshipPredicate(
      _,
      SilNounReference(_, DETERMINER_UNSPECIFIED, COUNT_SINGULAR) |
        SilStateSpecifiedReference(
          SilNounReference(_, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
          _
        ),
      _,
      _
    ) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreLonger = phraseScorer {
    case SilPropertyState(SilWord("longer", _)) => {
      SilPhraseScore.conBig
    }
  }

  private def scoreGerundNouns = phraseScorer {
    case SilNounReference(
      word : SilWord, _, _
    ) if (ShlurdWordnet.isPotentialGerund(word.inflected)) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreNestedAdpositions = phraseScorer {
    case SilAdpositionalState(
      _,
      SilStateSpecifiedReference(
        _,
        _ : SilAdpositionalState)
    ) => {
      SilPhraseScore.conBig
    }
    case SilAdpositionalVerbModifier(
      _,
      SilStateSpecifiedReference(
        _,
        _ : SilAdpositionalState)
    ) => {
      SilPhraseScore.conBig
    }
  }

  private def scoreConjunctiveNouns = phraseScorer {
    case SilConjunctiveReference(
      _, refs, _
    ) if (refs.exists(!_.isInstanceOf[SilNounReference])) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreSpecialAdpositions = phraseScorer {
    case ap : SilAdpositionalPhrase => {
      val words = ap.adposition.words
      if ((words.size > 1) && words.exists(_.lemma == LEMMA_THERE)) {
        SilPhraseScore.conBig
      } else if (words.exists(_.inflected == LEMMA_ADVERBIAL_TMP)) {
        SilPhraseScore.proSmall
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  override def computeLocalScore(phrase : SilPhrase) =
  {
    phraseScorers().map(_.lift(phrase).getOrElse(SilPhraseScore.neutral)).
      reduceLeft(_ + _)
  }
}
