// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

import org.kiama.rewriting._

class ShlurdPhraseRewrite(parser : ShlurdSingleParser)
{
  def completeSentence(
    tree : ShlurdSyntaxTree, phrase : ShlurdPhrase) : ShlurdSentence =
  {
    if (phrase.hasUnknown) {
      ShlurdUnrecognizedSentence(tree)
    } else {
      phrase match {
        case sentence : ShlurdSentence => sentence
        case _ => ShlurdUnrecognizedSentence(tree)
      }
    }
  }

  def rewrite(
    rule : PartialFunction[ShlurdPhrase, ShlurdPhrase])
      : (ShlurdPhrase) => ShlurdPhrase =
  {
    val strategy = Rewriter.rule[ShlurdPhrase](rule)
    Rewriter.rewrite(
      Rewriter.repeat(
        "rewriteRepeat",
        Rewriter.manybu("rewriteEverywhere", strategy)))
  }

  def rewriteSentence = rewrite {
    case ShlurdExpectedSentence(sentence : SptS, forceSQ) => {
      if (forceSQ) {
        parser.parseSQ(sentence, forceSQ)
      } else {
        parser.parseSentence(sentence)
      }
    }
    case ShlurdExpectedSentence(sq : SptSQ, forceSQ) => {
      parser.parseSQ(sq, forceSQ)
    }
    case ShlurdExpectedSentence(sbarq : SptSBARQ, _) => {
      parser.parseSBARQ(sbarq)
    }
    case ShlurdExpectedReference(np : ShlurdSyntaxTree) => {
      parser.parseReference(np)
    }
    case ambiguous : ShlurdAmbiguousSentence if (!ambiguous.hasUnresolved) => {
      val alternatives = ambiguous.alternatives
      assert(!alternatives.isEmpty)
      val dedup = alternatives.distinct
      if (dedup.size == 1) {
        dedup.head
      } else {
        val clean = dedup.filterNot(_.hasUnknown)
        if (clean.isEmpty) {
          ShlurdAmbiguousSentence(dedup)
        } else {
          if (clean.size == 1) {
            clean.head
          } else {
            ShlurdAmbiguousSentence(clean)
          }
        }
      }
    }
  }
}
