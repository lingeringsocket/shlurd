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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import net.sf.extjwnl.data._

import scala.collection.JavaConverters._

object SprWordnetScorer extends SilPhraseRewriter
{
  private val dictionary = ShlurdWordnet.dictionary

  def adjustScores(sentence : SilSentence) : SilSentence =
  {
    rewrite(adjustAllScores, sentence)
  }

  private def adjustAllScores = combineRules(
    adjustVerbModifierScores
  )

  private def adjustVerbModifierScores = replacementMatcher {
    case SilActionPredicate(
      subject,
      action,
      directObject,
      modifiers
    ) => {
      // FIXME there's got to be a better way; if not, we should at least
      // cache
      val senses = dictionary.getIndexWordIterator(
        POS.VERB, action.toLemma).asScala.flatMap(indexWord => {
          indexWord.getSenses.asScala.flatMap(
            _.getWords.asScala.map(_.getLemma).filter(
              _.startsWith(s"${action.toLemma} ")).
              map(_.split(" ").tail.toSeq))
        }
      ).toStream.force.toSet
      val adjustedModifiers = modifiers.map(
        vm => adjustVerbModifier(vm, senses))
      SilActionPredicate(subject, action, directObject, adjustedModifiers)
    }
  }

  private def adjustVerbModifier(
    modifier : SilVerbModifier, senses : Set[Seq[String]]) : SilVerbModifier =
  {
    modifier match {
      case SilBasicVerbModifier(word, score) => {
        if (senses.contains(word.decomposed.map(_.lemma))) {
          SilBasicVerbModifier(word, 1)
        } else {
          modifier
        }
      }
      case _ => modifier
    }
  }
}
