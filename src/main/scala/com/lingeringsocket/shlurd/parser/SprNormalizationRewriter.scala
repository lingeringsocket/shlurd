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
    rewrite(normalizeAllPhrases, sentence)
  }

  private def normalizeAllPhrases = combineRules(
    normalizeInterrogativeEmphatic,
    normalizeAdpositionalPhrases)

  private def normalizeInterrogativeEmphatic = replacementMatcher {
    case SilPredicateSentence(
      predicate,
      tam,
      formality
    ) if (tam.isInterrogative && (tam.modality == MODAL_EMPHATIC)) => {
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

  private def normalizeAdpositionalPhrases = replacementMatcher {
    case SilRelationshipPredicate(
      subject,
      SilStateSpecifiedReference(
        complement,
        SilAdpositionalState(adposition, objRef)),
      relationship,
      modifiers
    ) if (isAdverbialAdposition(complement, adposition, objRef)) => {
      SilRelationshipPredicate(
        subject,
        complement,
        relationship,
        SilAdpositionalVerbModifier(adposition, objRef) +: modifiers)
    }
    case SilActionPredicate(
      subject,
      action,
      Some(SilStateSpecifiedReference(
        complement,
        SilAdpositionalState(adposition, objRef))),
      modifiers
    ) if (isAdverbialAdposition(complement, adposition, objRef)) => {
      SilActionPredicate(
        subject,
        action,
        Some(complement),
        SilAdpositionalVerbModifier(adposition, objRef) +: modifiers)
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

