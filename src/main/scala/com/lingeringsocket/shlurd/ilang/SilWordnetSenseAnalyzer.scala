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

import net.sf.extjwnl.data._

import scala.collection._

class SilWordnetSenseAnalyzer(annotator : SilAnnotator)
    extends SilPhraseRewriter(annotator)
{
  def analyze[PhraseType <: SilPhrase](phrase : PhraseType) : PhraseType =
  {
    def matcher = replacementMatcher(
      "SilWordnetSenseAnalyzer", {
        case SilActionPredicate(subject, verb, directObject, modifiers) => {
          val senseId = filterSenses(verb, POS.VERB, sense => {
            isCompatibleAction(sense, subject, directObject, modifiers)
          })
          SilActionPredicate(
            subject,
            verb.withSense(senseId),
            directObject,
            modifiers)
        }
        case SilCountedNounReference(noun, count) => {
          val senseId = filterSenses(noun, POS.NOUN, sense => true)
          annotator.nounRef(noun.withSense(senseId), count)
        }
      }
    )
    rewrite(matcher, phrase)
  }

  private def filterSenses(
    word : SilWord,
    pos : POS,
    filter : Synset => Boolean) : String =
  {
    val inputSenses = ShlurdWordnet.findSenses(word.senseId)
    val candidateSenses = {
      if (inputSenses.isEmpty) {
        val lemma = {
          if (pos == POS.NOUN) {
            word.toNounLemma
          } else {
            word.toLemma
          }
        }
        ShlurdWordnet.getWordSenses(pos, lemma)
      } else {
        inputSenses
      }
    }
    val filteredSenses = candidateSenses.filter(filter)
    ShlurdWordnet.getSenseId(filteredSenses)
  }

  private def isCompatibleAction(
    sense : Synset,
    subject : SilReference,
    directObject : Option[SilReference],
    modifiers : Seq[SilVerbModifier]) : Boolean =
  {
    val frameFlags = BitSet(sense.getVerbFrameIndices:_*)
    val matched = SilWordnetScorer.matchAction(
      frameFlags, subject, directObject, modifiers)
    (matched > 0)
  }
}
