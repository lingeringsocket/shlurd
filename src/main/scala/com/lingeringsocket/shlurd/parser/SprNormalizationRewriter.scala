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

import SprEnglishLemmas._

private[parser] class SprNormalizationRewriter
  extends SilPhraseRewriter
{
  def normalize(sentence : SilSentence) : SilSentence =
  {
    rewrite(normalizeAllPhrases, sentence, SilRewriteOptions(repeat = true))
  }

  private def normalizeAllPhrases = combineRules(
    normalizeEmphatic,
    normalizeDanglingAdpositions,
    normalizeAdpositionalPhrases)

  private def normalizeEmphatic = replacementMatcher {
    case SilPredicateSentence(
      predicate,
      tam,
      formality
    ) if (
      (tam.isInterrogative || tam.isNegative) &&
        (tam.modality == MODAL_EMPHATIC)
    ) => {
      SilPredicateSentence(
        predicate,
        tam.withModality(MODAL_NEUTRAL),
        formality)
    }
    case SilPredicateQuery(
      predicate,
      question,
      answerInflection,
      tam,
      formality
    ) if (tam.modality == MODAL_EMPHATIC) => {
      SilPredicateQuery(
        predicate,
        question,
        answerInflection,
        tam.withModality(MODAL_NEUTRAL),
        formality
      )
    }
  }

  private def normalizeDanglingAdpositions = replacementMatcher {
    case SilPredicateQuery(
      SilActionPredicate(subject, action, directObject, modifiers),
      question,
      INFLECT_ACCUSATIVE,
      tam,
      formality
    ) if (modifiers.exists(isDanglingDative)) => {
      SilPredicateQuery(
        SilActionPredicate(
          subject, action, directObject,
          modifiers.filterNot(isDanglingDative)),
        question,
        INFLECT_DATIVE,
        tam,
        formality)
    }
  }

  private def isDanglingDative(modifier : SilVerbModifier) =
  {
    modifier match {
      case SilDanglingVerbModifier(
        SilAdposition(Seq(SilWord(LEMMA_TO, LEMMA_TO)))
      ) => true
      case _ => false
    }
  }

  private def normalizeAdpositionalPhrases = replacementMatcher {
    case SilStatePredicate(
      subject,
      state,
      modifiers
    ) => {
      val (subjectExtracted, subjectModifiers) =
        extractVerbModifier(subject)
      SilStatePredicate(
        subjectExtracted,
        state,
        subjectModifiers ++ modifiers
      )
    }
    case SilRelationshipPredicate(
      subject,
      complement,
      relationship,
      modifiers
    ) => {
      val (subjectExtracted, subjectModifiers) =
        extractVerbModifier(subject)
      val (complementExtracted, complementModifiers) =
        extractVerbModifier(complement)
      SilRelationshipPredicate(
        subjectExtracted,
        complementExtracted,
        relationship,
        subjectModifiers ++ complementModifiers ++ modifiers)
    }
    case SilActionPredicate(
      subject,
      action,
      directObject,
      modifiers
    ) => {
      val (subjectExtracted, subjectModifiers) =
        extractVerbModifier(subject)
      val (directObjectExtracted, directObjectModifiers) =
        directObject match {
          case Some(obj) => {
            val (r, m) = extractVerbModifier(obj)
            (Some(r), m)
          }
          case _ => (None, Seq.empty)
        }
      SilActionPredicate(
        subject,
        action,
        directObjectExtracted,
        subjectModifiers ++ directObjectModifiers ++ modifiers)
    }
  }

  private def extractVerbModifier(ref : SilReference)
      : (SilReference, Seq[SilVerbModifier]) =
  {
    ref match {
      case SilStateSpecifiedReference(
        sub,
        SilAdpositionalState(adposition, objRef)
      ) if (isAdverbialAdposition(sub, adposition, objRef)) => {
        (sub, Seq(SilAdpositionalVerbModifier(adposition, objRef)))
      }
      case _ => (ref, Seq.empty)
    }
  }

  private def isAdverbialAdposition(
    ref : SilReference, adposition : SilAdposition,
    objRef : SilReference) : Boolean =
  {
    // FIXME the real thing has to be sophisticated enough to understand
    // that "what happened the day before the fight" involves a state specifier
    // whereas "where was the football before the kitchen" and
    // "bow before the throne" involve adverbial phrases.  And in some cases,
    // we should leave it ambiguous and try it both ways.
    adposition.words match {
      case Seq(word) => word.lemma match {
        case LEMMA_BEFORE | LEMMA_AFTER | LEMMA_TO => true
        case _ => false
      }
      case _ => false
    }
  }
}

