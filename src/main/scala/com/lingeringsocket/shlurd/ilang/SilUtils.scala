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
  def collectReferences(phrase : SilPhrase) : Seq[SilReference] =
  {
    val refs = new mutable.ArrayBuffer[SilReference]
    val phraseQuerier = new SilPhraseRewriter
    val rule = phraseQuerier.queryMatcher {
      case ref : SilReference => {
        refs += ref
      }
    }
    phraseQuerier.query(rule, phrase)
    refs
  }

  def isCountCoercible(reference : SilReference) : Boolean =
  {
    reference match {
      case _ : SilPronounReference =>
        false
      case SilNounReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_NONE => false
          case DETERMINER_UNSPECIFIED => false
          case DETERMINER_UNIQUE => false
          case _ => true
        }
      }
      case _ : SilConjunctiveReference =>
        false
      case SilStateSpecifiedReference(reference, _) =>
        isCountCoercible(reference)
      case SilParenthesizedReference(reference) =>
        isCountCoercible(reference)
      case _ : SilGenitiveReference => true
      case _ : SilQuotationReference => true
      case _ : SilUnknownReference => false
    }
  }

  def getCount(reference : SilReference) : SilCount =
  {
    reference match {
      case SilPronounReference(_, _, count, _) =>
        count
      case SilNounReference(_, _, count) =>
        count
      case SilConjunctiveReference(determiner, _, _) => {
        determiner match {
          case DETERMINER_ALL => COUNT_PLURAL
          // DETERMINER_NONE is debatable
          case _ => COUNT_SINGULAR
        }
      }
      case SilParenthesizedReference(reference) =>
        getCount(reference)
      case SilStateSpecifiedReference(reference, _) =>
        getCount(reference)
      case SilGenitiveReference(_, possessee) =>
        getCount(possessee)
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
}
