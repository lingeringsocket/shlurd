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

import com.lingeringsocket.shlurd.ilang._

import scala.collection._

object SprWordnetScorer
{
  type PhraseScorer = PartialFunction[SilPhrase, SilPhraseScore]

  def phraseScorer(s : PhraseScorer)
      : PhraseScorer = s

  def matchAction(
    tongueIn : SprTongue,
    frameFlags : BitSet,
    subject : SilReference,
    directObject : Option[SilReference],
    modifiers : Seq[SilVerbModifier]) : Int =
  {
    implicit val tongue = tongueIn

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
        directObject.nonEmpty && !hasAdposition(
          modifiers, SprPredefAdposition(PD_TO))
      }
      case 9 => {
        // Somebody ----s somebody
        directObject.nonEmpty && !hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 10 => {
        // Something ----s somebody
        directObject.nonEmpty && !hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 11 => {
        // Something ----s something
        directObject.nonEmpty && !hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 12 => {
        // Something ----s to somebody
        directObject.isEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 13 => {
        //  Somebody ----s on something
        directObject.isEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_ON))
      }
      case 14 => {
        // Somebody ----s somebody something
        directObject.nonEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 15 => {
        // Somebody ----s something to somebody
        directObject.nonEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_TO))
      }
      case 16 => {
        // Somebody ----s something from somebody
        directObject.nonEmpty &&
          hasAdposition(modifiers, SprPredefAdposition(PD_FROM))
      }
      case 17 => {
        // Somebody ----s somebody with something
        directObject.nonEmpty &&
          hasAdposition(modifiers, SprPredefAdposition(PD_WITH))
      }
      case 18 => {
        // Somebody ----s somebody of something
        directObject.nonEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_OF))
      }
      case 19 => {
        // Somebody ----s something on somebody
        directObject.nonEmpty && hasAdposition(modifiers,
          SprPredefAdposition(PD_ON))
      }
      case 20 => {
        // Somebody ----s somebody PP
        directObject.nonEmpty && hasAdposition(modifiers) &&
        !hasAdposition(modifiers, SprPredefAdposition(PD_TO))
      }
      case 21 => {
        // Somebody ----s something PP &&
        !hasAdposition(modifiers, SprPredefAdposition(PD_TO))
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
        directObject.isEmpty && hasAdposition(
          modifiers,SprPredefAdposition(PD_TO))
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
        directObject.nonEmpty && hasAdposition(
          modifiers,SprPredefAdposition(PD_WITH))
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

  def matchState(
    frameFlags : BitSet,
    subject : SilReference,
    state : SilState,
    modifiers : Seq[SilVerbModifier]) : Int =
  {
    frameFlags.count(_ match {
      case 6 => {
        // Something ----s Adjective/Noun
        true
      }
      case 7 => {
        // Somebody ----s Adjective
        true
      }
      case _ => {
        false
      }
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

class SprWordnetScorer(
  tongueIn : SprTongue
) extends SilPhraseScorer
{
  import SprWordnetScorer._

  private implicit val tongue = tongueIn

  private val wordnet = tongue.getWordnet

  private def phraseScorers() = Seq(
    scoreGenitives,
    scoreCompoundNouns,
    scoreCompoundAdpositions,
    scoreCompoundVerbs,
    scoreCompoundAdverbs,
    scoreNounStates,
    scoreNestedAdpositions,
    scoreConjunctiveNouns,
    scoreConjunctiveSentences,
    scoreAppositions,
    scoreTam,
    scoreVerbFrames,
    scoreAgreement
  ) ++ tongue.getPhraseScorers

  private def scoreTam = phraseScorer {
    case s : SilSentence if (
      (s.tam.modality != MODAL_NEUTRAL) || s.tam.isNegative
    ) => {
      val tests = Seq(
        (s.tam.modality != MODAL_NEUTRAL),
        s.tam.isNegative)
      val boost = 2 * tests.count(truth => truth)
      SilPhraseScore.numeric(boost * SilPhraseScore.proBig.pro)
    }
    case s : SilSentence if (
      s.tam.isProgressive
    ) => {
      SilPhraseScore.proSmall
    }
  }

  private def scoreGenitives = phraseScorer {
    case SilGenitiveReference(
      _,
      _ : SilPronounReference | _ : SilStateSpecifiedReference |
        _ : SilQuotationReference
    ) => {
      SilPhraseScore.conBig
    }
  }

  private def scoreCompoundNouns = phraseScorer {
    case SilNounReference(
      noun
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
      if ((decomposed.last.lemma == PD_OF.toLemma) ||
        (decomposed.last.lemma == PD_TO.toLemma))
      {
        SilPhraseScore.proBig
      } else {
        SilPhraseScore.conSmall
      }
    }
  }

  private def scoreCompoundAdverbs = phraseScorer {
    case SilBasicVerbModifier(word) if (
      word.decomposed.size > 1
    ) => {
      SilPhraseScore.proBig
    }
  }

  private def scoreCompoundVerbs = phraseScorer {
    case SilActionPredicate(_, word, _, _) if (
      word.decomposed.size > 1
    ) => {
      SilPhraseScore.proBig
    }
  }

  private def scoreNounStates = phraseScorer {
    case SilRelationshipPredicate(
      _,
      _,
      SilMandatorySingular(_) |
        SilStateSpecifiedReference(
          SilMandatorySingular(_),
          _
        ),
      _
    ) => {
      SilPhraseScore.conSmall
    }
    case _ : SilAdpositionalVerbModifier => {
      SilPhraseScore.proSmall
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
    ) if (
      refs.exists(_ match {
        case _ : SilNounReference => false
        case _ : SilDeterminedReference => false
        case _ => true
      })
    ) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreConjunctiveSentences = phraseScorer {
    case SilConjunctiveSentence(
      _, subs, separator
    ) if (separator != SEPARATOR_SEMICOLON) => {
      SilPhraseScore.conSmall
    }
  }

  private def scoreAppositions = phraseScorer {
    case SilAppositionalReference(primary, secondary) => {
      primary match {
        case SilOptionallyDeterminedReference(
          SilStackedStateReference(nr : SilNounReference, states),
          _
        ) => {
          if (states.exists(_.isInstanceOf[SilAdpositionalState])) {
            SilPhraseScore.conBig
          } else {
            SilPhraseScore.proBig
          }
        }
        case _ => {
          SilPhraseScore.conBig
        }
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
      val frameFlags = wordnet.getVerbFrameFlags(lemma)
      val matched = SprWordnetScorer.matchAction(
        tongue, frameFlags, subject, directObject, modifiers)
      if (matched > 0) {
        SilPhraseScore.pro(matched)
      } else {
        SilPhraseScore.neutral
      }
    }
    case SilStatePredicate(
      subject,
      SilWordLemma(lemma),
      state,
      modifiers
    ) => {
      state match {
        case SilPropertyState(
          SilWordInflected(inflected)
        ) if (
          tongue.isPotentialGerund(inflected)
        ) => {
          SilPhraseScore.neutral
        }
        case _ => {
          val frameFlags = wordnet.getVerbFrameFlags(lemma)
          val matched = SprWordnetScorer.matchState(
            frameFlags, subject, state, modifiers)
          if (matched > 0) {
            SilPhraseScore.proBig
          } else {
            SilPhraseScore.neutral
          }
        }
      }
    }
  }

  private def scoreAgreement = phraseScorer {
    case pr : SilPredicate => {
      // for now we just use person; probably should use
      // gender and count as well
      val subject = pr.getSubject
      val verbPerson = pr.getInflectedPerson
      val person = subject match {
        case pr : SilPronounReference => {
          if (pr.isElided) {
            verbPerson
          } else {
            pr.person
          }
        }
        case _ => PERSON_THIRD
      }
      if (person != verbPerson) {
        SilPhraseScore.conSmall
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
