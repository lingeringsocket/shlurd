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

class ShlurdPhraseRewriter
{
  type ShlurdPhraseReplacement = PartialFunction[ShlurdPhrase, ShlurdPhrase]

  type ShlurdPhraseQuery = PartialFunction[ShlurdPhrase, Unit]

  private val logger = LoggerFactory.getLogger(classOf[ShlurdPhraseRewriter])

  object SyntaxPreservingRewriter extends CallbackRewriter
  {
    override def rewriting[PhraseType](
      oldPhrase : PhraseType, newPhrase : PhraseType) : PhraseType =
    {
      (oldPhrase, newPhrase) match {
        case (oldMaybeTree : ShlurdPhrase,
          newTransformed : ShlurdTransformedPhrase) =>
          {
            oldMaybeTree.maybeSyntaxTree match {
              case Some(syntaxTree) => {
                rememberTransformation(syntaxTree, newTransformed)
              }
              case _ =>
            }
          }
        case _ =>
      }
      (oldPhrase, newPhrase) match {
        case (oldPredicate : ShlurdPredicate,
          newPredicate : ShlurdPredicate) =>
          {
            newPredicate.setInflectedCount(oldPredicate.getInflectedCount)
          }
        case _ =>
      }
      newPhrase
    }
  }

  def rewrite[PhraseType <: ShlurdPhrase](
    rule : ShlurdPhraseReplacement,
    phrase : PhraseType,
    repeat : Boolean = false)
      : PhraseType =
  {
    val strategy =
      SyntaxPreservingRewriter.manybu(
        "rewriteEverywhere",
        SyntaxPreservingRewriter.rule(rule))
    val maybeLogging = {
      if (logger.isDebugEnabled) {
        SyntaxPreservingRewriter.log(
          "rewriteLog",
          strategy,
          "REWRITE ",
          new ErrorEmitter)
      } else {
        strategy
      }
    }
    val finalStrategy = {
      if (repeat) {
        SyntaxPreservingRewriter.rewrite[PhraseType](
          SyntaxPreservingRewriter.repeat(
            "rewriteRepeat",
            maybeLogging)) _
      } else {
        SyntaxPreservingRewriter.rewrite[PhraseType](maybeLogging) _
      }
    }
    finalStrategy(phrase)
  }

  def query(
    rule : ShlurdPhraseQuery,
    phrase : ShlurdPhrase)
  {
    val strategy =
      Rewriter.manybu(
        "rewriteEverywhere",
        Rewriter.query[ShlurdPhrase](rule))
    Rewriter.rewrite(strategy)(phrase)
  }

  def replacementMatcher(f : ShlurdPhraseReplacement)
      : ShlurdPhraseReplacement = f

  def queryMatcher(f : ShlurdPhraseQuery)
      : ShlurdPhraseQuery = f

  def combineRules(rules : ShlurdPhraseReplacement*)
      : ShlurdPhraseReplacement =
  {
    rules.reduceLeft(_ orElse _)
  }

  private def rememberTransformation(
    syntaxTree : ShlurdSyntaxTree,
    phrase : ShlurdPhrase) =
  {
    phrase match {
      case transformedPhrase : ShlurdTransformedPhrase => {
        transformedPhrase.rememberSyntaxTree(syntaxTree)
      }
      case _ => assert(phrase.isInstanceOf[ShlurdUnknownPhrase])
    }
    phrase
  }
}
