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

import scala.collection._

object SilUtils
{
  def collectReferences(
    phrase : SilPhrase, topDown : Boolean = false) : Seq[SilReference] =
  {
    val refs = new mutable.ArrayBuffer[SilReference]
    val phraseQuerier = new SilPhraseQuerier
    val rule = phraseQuerier.queryMatcher {
      case ref : SilReference => {
        refs += ref
      }
    }
    phraseQuerier.query(rule, phrase, SilRewriteOptions(topDown = topDown))
    refs
  }

  def isCountCoercible(reference : SilReference) : Boolean =
  {
    reference match {
      case pr : SilPronounReference =>
        (pr.proximity == PROXIMITY_ELIDED)
      case _ : SilNounReference =>
        false
      case _ : SilConjunctiveReference =>
        false
      case SilStateSpecifiedReference(reference, _) =>
        isCountCoercible(reference)
      case SilDeterminedReference(reference, determiner) => {
        determiner match {
          case DETERMINER_NONE | DETERMINER_ABSENT |
              DETERMINER_DEFINITE => false
          case _ => true
        }
      }
      case SilAppositionalReference(primary, _) =>
        isCountCoercible(primary)
      case SilParenthesizedReference(reference, _) =>
        isCountCoercible(reference)
      case _ : SilGenitiveReference => true
      case _ : SilQuotationReference => true
      case _ : SilUnknownReference => false
    }
  }

  def getCount(reference : SilReference) : SilCount =
  {
    reference match {
      // FIXME this should work for any SilAnnotatedReference, but
      // only once we can correctly trigger recomputation when
      // a descendant is updated
      case annotatedRef : SilNounReference if (
        annotatedRef.hasAnnotation
      ) => {
        annotatedRef.getAnnotator.getBasicNote(annotatedRef).getCount
      }
      case _ => {
        deriveCount(reference)
      }
    }
  }

  def getGender(
    reference : SilReference,
    genderAnalyzer : SilGenderAnalyzer) : SilGender =
  {
    val genderOpt = reference match {
      // FIXME this should work for any SilAnnotatedReference, but
      // only once we can correctly trigger recomputation when
      // a descendant is updated
      case annotatedRef : SilAnnotatedReference if (
        annotatedRef.hasAnnotation
      ) => {
        annotatedRef match {
          case (_ : SilNounReference | _ : SilMappedReference) => {
            annotatedRef.getAnnotator.getBasicNote(annotatedRef).maybeGender
          }
          case _ => None
        }
      }
      case _ => None
    }
    genderOpt.getOrElse(genderAnalyzer.deriveGender(reference, genderAnalyzer))
  }

  def deriveCount(reference : SilReference) : SilCount =
  {
    reference match {
      case pr : SilPronounReference =>
        pr.count
      case SilConjunctiveReference(determiner, _, _) => {
        determiner match {
          case DETERMINER_ALL => COUNT_PLURAL
          // DETERMINER_NONE is debatable
          case _ => COUNT_SINGULAR
        }
      }
      case SilParenthesizedReference(r, _) =>
        getCount(r)
      case SilAppositionalReference(r, _) =>
        getCount(r)
      case SilStateSpecifiedReference(r, _) =>
        getCount(r)
      case SilDeterminedReference(_, DETERMINER_NONSPECIFIC) =>
        COUNT_SINGULAR
      case SilDeterminedReference(r, _) =>
        getCount(r)
      case SilGenitiveReference(_, possessee) =>
        getCount(possessee)
      case _ : SilNounReference => COUNT_SINGULAR
      case _ : SilQuotationReference => COUNT_SINGULAR
      case _ : SilUnknownReference => COUNT_SINGULAR
    }
  }

  def extractAdpositionSpecifiers(state : SilState)
      : Seq[SilAdpositionalState] =
  {
    state match {
      case SilConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractAdpositionSpecifiers)
      case adp : SilAdpositionalState => Seq(adp)
      case SilNullState() | SilPropertyState(_) |
          SilExistenceState(_) => Seq.empty
      case _ => {
        assert(false)
        Seq.empty
      }
    }
  }

  def extractQualifiers(state : SilState) : Seq[SilWord] =
  {
    state match {
      case SilConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractQualifiers)
      case SilPropertyState(state) => Seq(state)
      case SilNullState() | SilAdpositionalState(_, _) |
          SilExistenceState(_) => Seq.empty
      case _ => {
        assert(false, state)
        Seq.empty
      }
    }
  }

  def getDanglingAdposition(modifier : SilVerbModifier) =
  {
    modifier match {
      case SilDanglingVerbModifier(adposition) => {
        Some(adposition)
      }
      case _ => None
    }
  }

  def derivePoliteness(tongue : SilTongue, phrase : SilPhrase) : SilPoliteness =
  {
    val entries = new mutable.ArrayBuffer[SilPoliteness]
    val phraseQuerier = new SilPhraseQuerier
    val rule = phraseQuerier.queryMatcher {
      case sentence : SilSentence => {
        entries += sentence.formality.politeness
      }
      case pronoun : SilPronounReference => {
        entries += pronoun.politeness
      }
    }
    phraseQuerier.query(rule, phrase)
    entries.reduceOption(tongue.combinePoliteness).getOrElse {
      SilFormality.DEFAULT.politeness
    }
  }
}
