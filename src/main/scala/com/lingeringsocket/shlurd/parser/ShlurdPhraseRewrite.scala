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
import org.kiama.util._

import org.slf4j._

class ShlurdPhraseRewrite(parser : ShlurdSingleParser)
{
  type ShlurdPhrasePartialFunction = PartialFunction[ShlurdPhrase, ShlurdPhrase]

  private val logger = LoggerFactory.getLogger(classOf[ShlurdPhraseRewrite])

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
    rule : ShlurdPhrasePartialFunction)
      : (ShlurdPhrase) => ShlurdPhrase =
  {
    val strategy =
      Rewriter.manybu(
        "rewriteEverywhere",
        Rewriter.rule[ShlurdPhrase](rule))
    val maybeLogging = if (logger.isDebugEnabled) {
      Rewriter.log(
        "rewriteLog",
        strategy,
        "REWRITE ",
        new ErrorEmitter)
    } else {
      strategy
    }
    Rewriter.rewrite(
      Rewriter.repeat(
        "rewriteRepeat",
        maybeLogging))
  }

  def rewriteAllPhrases = rewrite {
    Seq(
      rewriteExpectedSentence,
      rewriteExpectedSBARQ,
      rewriteExpectedSQ,
      rewriteAmbiguousSentence,
      rewriteExpectedReference).reduceLeft(_ orElse _)
  }

  def phraseMatcher(f : ShlurdPhrasePartialFunction)
      : ShlurdPhrasePartialFunction = f

  def rewriteExpectedSentence() = phraseMatcher {
    case ShlurdExpectedSentence(sentence : SptS, forceSQ) => {
      if (forceSQ) {
        parser.parseSQ(sentence, forceSQ)
      } else {
        parser.parseSentence(sentence)
      }
    }
  }

  def rewriteExpectedReference = phraseMatcher {
    case ShlurdExpectedReference(SptNP(noun)) => {
      ShlurdExpectedReference(noun)
    }
    case ShlurdExpectedReference(np : SptNP) => {
      parser.parseNounPhraseReference(np)
    }
    case ShlurdExpectedReference(noun : ShlurdSyntaxNoun) => {
      ShlurdEntityReference(
        parser.getWord(noun.child),
        DETERMINER_UNSPECIFIED,
        parser.getCount(noun))
    }
    case ShlurdExpectedReference(pronoun : ShlurdSyntaxPronoun) => {
      parser.pronounFor(pronoun.child.lemma)
    }
  }

  def rewriteExpectedSBARQ = phraseMatcher {
    case ShlurdExpectedSentence(sbarq : SptSBARQ, _) => {
      parser.parseSBARQ(sbarq)
    }
  }

  def rewriteExpectedSQ = phraseMatcher {
    case ShlurdExpectedSentence(sq : SptSQ, forceSQ) => {
      parser.parseSQ(sq, forceSQ)
    }
  }

  def rewriteAmbiguousSentence = phraseMatcher {
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
