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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd.ilang._

import scala.jdk.CollectionConverters._

sealed trait SnlTranslationDirection
case object TRANSLATE_FIRST_TO_SECOND extends SnlTranslationDirection
case object TRANSLATE_SECOND_TO_FIRST extends SnlTranslationDirection

class SnlTranslator(
  annotator : SilAnnotator,
  alignment : SnlWordnetAlignment
)
{
  def translate(
    input : SilSentence,
    direction : SnlTranslationDirection) : SilSentence =
  {
    // FIXME special handling for pronouns, adpositions, conjunctions, etc
    val rewriter = new SilPhraseRewriter(annotator)
    val sourceWordnet = direction match {
      case TRANSLATE_FIRST_TO_SECOND => alignment.getFirstWordnet
      case TRANSLATE_SECOND_TO_FIRST => alignment.getSecondWordnet
    }
    def translateWords = rewriter.replacementMatcher(
      "translateWords", {
        case phrase : SilPhrase if (
          phrase.maybeWord.map(_.senseId).getOrElse("").nonEmpty
        ) => {
          val word = phrase.maybeWord.get
          val senses = sourceWordnet.findSenses(word.senseId)
          val translatedSenses = senses.flatMap(synset => {
            alignment.mapSense(synset, direction)
          })
          val translatedSenseId = sourceWordnet.getSenseId(translatedSenses)
          // FIXME what about compound words?  Also should maybe
          // only preserve senses with the same lemma?
          val lemma = translatedSenses.head.getWords.asScala.head.getLemma
          val translatedWord = SilWord("", lemma, translatedSenseId)
          phrase.withNewWord(translatedWord)
        }
      }
    )
    rewriter.rewrite(translateWords, input)
  }
}
