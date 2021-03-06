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

import net.sf.extjwnl.data._

import scala.collection._

class SprWordnetSenseAnalyzer(
  tongue : SprTongue,
  annotator : SilAnnotator)
    extends SilPhraseRewriter(annotator)
{
  import SilPhraseRewriter._

  private val wordnet = tongue.getWordnet

  def analyze[PhraseType <: SilPhrase](phrase : PhraseType) : PhraseType =
  {
    def matcher = replacementMatcher(
      "SprWordnetSenseAnalyzer", {
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
        case pred : SilPredicate => {
          val verb = pred.getVerb
          val senseId = filterSenses(verb, POS.VERB, sense => true)
          pred.withNewWord(verb.withSense(senseId))
        }
        case SilCountedNounReference(noun, count) => {
          val senseId = filterSenses(noun, POS.NOUN, sense => true)
          annotator.nounRef(noun.withSense(senseId), count)
        }
        case ps @ SilPropertyState(word) => {
          val senseId = filterSenses(word, POS.ADJECTIVE, sense => true)
          ps.withNewWord(word.withSense(senseId))
        }
        case vm @ SilBasicVerbModifier(word) => {
          val senseId = filterSenses(word, POS.ADVERB, sense => true)
          vm.withNewWord(word.withSense(senseId))
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
    val inputSenses = wordnet.findSenses(word.senseId)
    val candidateSenses = {
      if (inputSenses.isEmpty) {
        val lemma = {
          if (pos == POS.NOUN) {
            word.toNounLemma
          } else {
            word.toLemma
          }
        }
        wordnet.getWordSenses(pos, lemma)
      } else {
        inputSenses
      }
    }
    val filteredSenses = candidateSenses.filter(filter)
    wordnet.getSenseId(filteredSenses)
  }

  private def isCompatibleAction(
    sense : Synset,
    subject : SilReference,
    directObject : Option[SilReference],
    modifiers : Seq[SilVerbModifier]) : Boolean =
  {
    val frameFlags = BitSet(sense.getVerbFrameIndices.toIndexedSeq:_*)
    val matched = SprWordnetScorer.matchNonSpanishAction(
      tongue, frameFlags, subject, directObject, modifiers)
    (matched > 0)
  }
}
