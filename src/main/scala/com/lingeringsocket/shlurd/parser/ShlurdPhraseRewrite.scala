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

import org.kiama.rewriting._
import org.kiama.util._

import org.slf4j._

class ShlurdPhraseRewrite(analyzer : ShlurdSyntaxAnalyzer)
{
  type ShlurdPhrasePartialFunction = PartialFunction[ShlurdPhrase, ShlurdPhrase]

  private val logger = LoggerFactory.getLogger(classOf[ShlurdPhraseRewrite])

  def completeSentence(
    tree : ShlurdSyntaxTree, phrase : ShlurdPhrase) : ShlurdSentence =
  {
    val completed = rewrite(rewriteExpectedToUnrecognized)(phrase)
    assert(!completed.hasUnresolved)
    completed match {
      case sentence : ShlurdSentence => sentence
      case _ => ShlurdUnrecognizedSentence(tree)
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
      rewriteUnresolvedPredicate,
      rewriteExpectedState,
      rewriteExpectedReference).reduceLeft(_ orElse _)
  }

  def phraseMatcher(f : ShlurdPhrasePartialFunction)
      : ShlurdPhrasePartialFunction = f

  def rewriteExpectedSentence() = phraseMatcher {
    case ShlurdExpectedSentence(sentence : SptS, forceSQ) => {
      if (forceSQ) {
        analyzer.analyzeSQ(sentence, forceSQ)
      } else {
        analyzer.analyzeSentence(sentence)
      }
    }
  }

  def rewriteExpectedReference = phraseMatcher {
    case ShlurdExpectedReference(SptNP(noun)) => {
      ShlurdExpectedReference(noun)
    }
    case ShlurdExpectedReference(np : SptNP) => {
      analyzer.analyzeNounPhrase(np)
    }
    case ShlurdExpectedReference(noun : ShlurdSyntaxNoun) => {
      ShlurdEntityReference(
        analyzer.getWord(noun.child),
        DETERMINER_UNSPECIFIED,
        analyzer.getCount(noun))
    }
    case ShlurdExpectedReference(pronoun : ShlurdSyntaxPronoun) => {
      analyzer.recognizePronounReference(pronoun.child)
    }
    case ShlurdUnresolvedRelativeReference(
      syntaxTree, reference, ShlurdPropertyState(qualifier)) =>
      {
        ShlurdReference.qualified(reference, Seq(qualifier))
      }
  }

  def rewriteExpectedState = phraseMatcher {
    case ShlurdExpectedPrepositionalState(tree) => {
      analyzer.requirePrepositionalState(tree)
    }
    case ShlurdExpectedComplementState(SptADJP(children @ _*)) => {
      analyzer.requirePropertyComplementState(children)
    }
    case ShlurdExpectedComplementState(
      phrase @ (_ : SptADVP | _ : SptPP)) =>
    {
      val seq = phrase.children
      if ((seq.head.isPreposition || seq.head.isAdverb) && (seq.size > 1) &&
        (!seq.exists(_.isPrepositionalPhrase)))
      {
        ShlurdExpectedPrepositionalState(phrase)
      } else {
        analyzer.requirePropertyComplementState(seq)
      }
    }
    case ShlurdExpectedComplementState(SptVP(children @ _*)) => {
      // TODO:  ambiguity for action (passive construction) vs
      // state (participial adjective)
      analyzer.requirePropertyComplementState(children)
    }
    case ShlurdExpectedComplementState(SptPRT(particle)) => {
      analyzer.requirePropertyState(particle)
    }
  }

  def rewriteUnresolvedPredicate = phraseMatcher {
    case ShlurdUnresolvedPredicate(
      _, subject, state, specifiedState)
        if (!state.hasUnresolved) =>
      {
        state match {
          case ShlurdConjunctiveState(DETERMINER_UNSPECIFIED, states, _) => {
            val propertyState = states.head
            val fullySpecifiedState = {
              if (specifiedState == ShlurdNullState()) {
                if (states.size == 2) {
                  states.last
                } else {
                  ShlurdConjunctiveState(DETERMINER_ALL, states.tail)
                }
              } else {
                ShlurdConjunctiveState(
                  DETERMINER_ALL, Seq(specifiedState) ++ states.tail)
              }
            }
            val specifiedSubject = analyzer.specifyReference(
              subject, fullySpecifiedState)
            ShlurdStatePredicate(specifiedSubject, propertyState)
          }
          case _ => {
            val specifiedSubject = analyzer.specifyReference(
              subject, specifiedState)
            ShlurdStatePredicate(specifiedSubject, state)
          }
        }
      }
  }

  def rewriteExpectedSBARQ = phraseMatcher {
    case ShlurdExpectedSentence(sbarq : SptSBARQ, _) => {
      analyzer.analyzeSBARQ(sbarq)
    }
  }

  def rewriteExpectedSQ = phraseMatcher {
    case ShlurdExpectedSentence(sq : SptSQ, forceSQ) => {
      analyzer.analyzeSQ(sq, forceSQ)
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

  def rewriteExpectedToUnrecognized = phraseMatcher {
    case ShlurdExpectedSentence(syntaxTree, _) => {
      ShlurdUnrecognizedSentence(syntaxTree)
    }
    case ShlurdExpectedPredicate(syntaxTree) => {
      ShlurdUnrecognizedPredicate(syntaxTree)
    }
    case ShlurdExpectedReference(syntaxTree) => {
      ShlurdUnrecognizedReference(syntaxTree)
    }
    case ShlurdExpectedComplementState(syntaxTree) => {
      ShlurdUnrecognizedState(syntaxTree)
    }
    case ShlurdExpectedPrepositionalState(syntaxTree) => {
      ShlurdUnrecognizedState(syntaxTree)
    }
    case ShlurdUnresolvedRelativeReference(syntaxTree, _, _) => {
      ShlurdUnrecognizedReference(syntaxTree)
    }
    case ShlurdUnresolvedPredicate(syntaxTree, _, _, _) => {
      ShlurdUnrecognizedPredicate(syntaxTree)
    }
  }
}
