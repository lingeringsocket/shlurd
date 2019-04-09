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

import net.sf.extjwnl.data._

import scala.collection._

class SilWordnetScorer extends SilPhraseScorer with SprEnglishWordAnalyzer
{
  type PhraseScorer = PartialFunction[SilPhrase, SilPhraseScore]

  private val dictionary = ShlurdWordnet.dictionary

  private val morphology = ShlurdWordnet.morphology

  private def phraseScorer(s : PhraseScorer)
      : PhraseScorer = s

  private def phraseScorers() = Seq(
    scoreGenitives,
    scoreVerbModifiers,
    scoreUsage,
    scoreCompoundNouns,
    scoreCompoundAdpositions,
    scoreNounStates,
    scoreNestedAdpositions,
    scoreConjunctiveNouns,
    scoreSpecialAdpositions,
    scoreTam,
    scoreVerbFrames
  )

  private def scoreTam = phraseScorer {
    case s : SilSentence if (
      (s.tam.modality != MODAL_NEUTRAL) || s.tam.isNegative
    ) => {
      SilPhraseScore.proSmall
    }
  }

  private def scoreGenitives = phraseScorer {
    case SilGenitiveReference(
      _,
      _ : SilPronounReference
    ) => {
      SilPhraseScore.conBig
    }
  }

  private def scoreVerbModifiers = phraseScorer {
    case SilBasicVerbModifier(word, score) => {
      if (score < 0) {
        SilPhraseScore.conSmall
      } else if (score > 0) {
        SilPhraseScore.proSmall
      } else {
        if (word.toLemma == LEMMA_NO) {
          SilPhraseScore.conBig
        } else {
          SilPhraseScore.neutral
        }
      }
    }
  }

  private def usageScore(lemma : String, pos : POS) : SilPhraseScore =
  {
    val score = ShlurdWordnet.getUsageScore(lemma, pos)
    if (score == 0) {
      SilPhraseScore.neutral
    } else if (score < 0) {
      if (pos == POS.ADJECTIVE) {
        SilPhraseScore.neutral
      } else {
        SilPhraseScore.con(-score)
      }
    } else {
      SilPhraseScore.pro(score)
    }
  }

  private def scoreUsage = phraseScorer {
    case SilNounReference(noun, _, _) => {
      usageScore(noun.toLemma, POS.NOUN)
    }
    case SilPropertyState(sw : SilSimpleWord) => {
      usageScore(sw.toLemma, POS.ADJECTIVE)
    }
    case SilActionPredicate(_, sw : SilSimpleWord, _, _) => {
      usageScore(sw.toLemma, POS.VERB)
    }
    case SilBasicVerbModifier(sw : SilSimpleWord, _) => {
      val lemma = sw.toLemma
      if (lemma.toLowerCase == "yesterday") {
        SilPhraseScore.pro(10)
      } else {
        usageScore(lemma, POS.ADVERB)
      }
    }
  }

  private def scoreCompoundNouns = phraseScorer {
    case SilNounReference(
      noun, _, _
    ) if (noun.decomposed.size > 1) => {
      val matchCount = noun.decomposed.count(
        word => (word.inflected == word.lemma))
      if (matchCount > 0) {
        SilPhraseScore.proBig + SilPhraseScore.pro(matchCount)
      } else {
        SilPhraseScore.proBig
      }
    }
  }

  private def scoreCompoundAdpositions = phraseScorer {
    case ap : SilAdpositionalPhrase if (
      ap.adposition.word.decomposed.size > 1
    ) => {
      val decomposed = ap.adposition.word.decomposed
      if ((decomposed.last.lemma == LEMMA_OF) ||
        (decomposed.last.lemma == LEMMA_TO))
      {
        SilPhraseScore.proBig
      } else {
        SilPhraseScore.conSmall
      }
    }
  }

  private def scoreCompoundAdverbs = phraseScorer {
    case SilBasicVerbModifier(word, _) if (
      word.decomposed.size > 1
    ) => {
      SilPhraseScore.proBig
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
      val words = ap.adposition.word.decomposed
      if ((words.size > 1) && words.exists(
        _.lemma == LEMMA_THERE)
      ) {
        SilPhraseScore.conBig
      } else if (words.exists(_.inflected == LEMMA_ADVERBIAL_TMP)) {
        SilPhraseScore.proSmall
      } else {
        SilPhraseScore.neutral
      }
    }
  }

  private def scoreVerbFrames = phraseScorer {
    case SilActionPredicate(
      subject,
      SilWordLemma(lemma),
      directObject,
      modifiers
    ) => {
      val frameFlags = ShlurdWordnet.getVerbFrameFlags(lemma)
      val matched = SilWordnetScorer.matchAction(
        frameFlags, subject, directObject, modifiers)
      if (matched > 0) {
        SilPhraseScore.pro(matched)
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

object SilWordnetScorer extends SprEnglishWordAnalyzer
{
  def matchAction(
    frameFlags : BitSet,
    subject : SilReference,
    directObject : Option[SilReference],
    modifiers : Seq[SilVerbModifier]) : Int =
  {
    frameFlags.count(_ match {
      // see https://wordnet.princeton.edu/documentation/wninput5wn
      case 1 => {
        // Something ----s
        directObject.isEmpty
      }
      case 2 => {
        // Somebody ----s
        directObject.isEmpty
      }
      case 3 => {
        // It is ----ing
        false
      }
      case 4 => {
        // Something is ----ing PP
        // FIXME:  check tam
        directObject.isEmpty && modifiers.nonEmpty
      }
      case 5 => {
        // Something ----s something Adjective/Noun
        false
      }
      case 6 => {
        // Something ----s Adjective/Noun
        false
      }
      case 7 => {
        // Somebody ----s Adjective
        false
      }
      case 8 => {
        // Somebody ----s something
        directObject.nonEmpty && !hasAdposition(modifiers, SilAdposition.TO)
      }
      case 9 => {
        // Somebody ----s somebody
        directObject.nonEmpty && !hasAdposition(modifiers, SilAdposition.TO)
      }
      case 10 => {
        // Something ----s somebody
        directObject.nonEmpty && !hasAdposition(modifiers, SilAdposition.TO)
      }
      case 11 => {
        // Something ----s something
        directObject.nonEmpty && !hasAdposition(modifiers, SilAdposition.TO)
      }
      case 12 => {
        // Something ----s to somebody
        directObject.isEmpty && hasAdposition(modifiers, SilAdposition.TO)
      }
      case 13 => {
        //  Somebody ----s on something
        directObject.isEmpty && hasAdposition(modifiers, SilAdposition.ON)
      }
      case 14 => {
        // Somebody ----s somebody something
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.TO)
      }
      case 15 => {
        // Somebody ----s something to somebody
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.TO)
      }
      case 16 => {
        // Somebody ----s something from somebody
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.FROM)
      }
      case 17 => {
        // Somebody ----s somebody with something
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.WITH)
      }
      case 18 => {
        // Somebody ----s somebody of something
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.OF)
      }
      case 19 => {
        // Somebody ----s something on somebody
        directObject.nonEmpty && hasAdposition(modifiers, SilAdposition.ON)
      }
      case 20 => {
        // Somebody ----s somebody PP
        directObject.nonEmpty && hasAdposition(modifiers) &&
        !hasAdposition(modifiers, SilAdposition.TO)
      }
      case 21 => {
        // Somebody ----s something PP &&
        !hasAdposition(modifiers, SilAdposition.TO)
        directObject.nonEmpty && hasAdposition(modifiers)
      }
      case 22 => {
        // Somebody ----s PP
        directObject.isEmpty && hasAdposition(modifiers)
      }
      case 23 => {
        // Somebody's (body part) ----s
        false
      }
      case 24 => {
        // Somebody ----s somebody to INFINITIVE
        false
      }
      case 25 => {
        // Somebody ----s somebody INFINITIVE
        false
      }
      case 26 => {
        // Somebody ----s that CLAUSE
        false
      }
      case 27 => {
        // Somebody ----s to somebody
        directObject.isEmpty && hasAdposition(modifiers,SilAdposition.TO)
      }
      case 28 => {
        // Somebody ----s to INFINITIVE
        false
      }
      case 29 => {
        // Somebody ----s whether INFINITIVE
        false
      }
      case 30 => {
        // Somebody ----s somebody into V-ing something
        false
      }
      case 31 => {
        // Somebody ----s something with something
        directObject.nonEmpty && hasAdposition(modifiers,SilAdposition.WITH)
      }
      case 32 => {
        // Somebody ----s INFINITIVE
        false
      }
      case 33 => {
        // Somebody ----s VERB-ing
        false
      }
      case 34 => {
        // It ----s that CLAUSE
        false
      }
      case 35 => {
        // Something ----s INFINITIVE
        false
      }
      case _ => false
    })
  }

  private def hasAdposition(
    modifiers : Seq[SilVerbModifier],
    adp : SilAdposition) : Boolean =
  {
    modifiers.exists(_ match {
      case SilAdpositionalVerbModifier(adpFound, _) => (adp == adpFound)
      case _ => false
    })
  }

  private def hasAdposition(
    modifiers : Seq[SilVerbModifier]) : Boolean =
  {
    modifiers.exists(_ match {
      case _ : SilAdpositionalVerbModifier => true
      case _ => false
    })
  }
}
