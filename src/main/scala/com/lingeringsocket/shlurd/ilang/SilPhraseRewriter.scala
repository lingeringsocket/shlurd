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

case class SilRewriteOptions(
  repeat : Boolean = false,
  topDown : Boolean = false
)
{
}

class FixpointStrategy(s : Strategy) extends Strategy("fixpoint")
{
  override val body = {
    input : Any => {
      val output = s.apply(input)
      if (Some(input) == output) {
        None
      } else {
        output
      }
    }
  }
}

class SilPhraseRewriter
{
  type SilPhraseReplacement = PartialFunction[SilPhrase, SilPhrase]

  type SilPhraseQuery = PartialFunction[SilPhrase, Unit]

  private val logger = LoggerFactory.getLogger(classOf[SilPhraseRewriter])

  object SyntaxPreservingRewriter extends CallbackRewriter
  {
    override def rewriting[PhraseType](
      oldPhrase : PhraseType, newPhrase : PhraseType) : PhraseType =
    {
      (oldPhrase, newPhrase) match {
        case (oldMaybeTree : SilPhrase,
          newTransformed : SilTransformedPhrase) =>
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
        case (oldPredicate : SilPredicate,
          newPredicate : SilPredicate) =>
          {
            newPredicate.setInflectedCount(oldPredicate.getInflectedCount)
          }
        case _ =>
      }
      newPhrase
    }
  }

  def rewrite[PhraseType <: SilPhrase](
    rule : SilPhraseReplacement,
    phrase : PhraseType,
    options : SilRewriteOptions = SilRewriteOptions())
      : PhraseType =
  {
    val strategy = {
      val ruleStrategy = SyntaxPreservingRewriter.rule(rule)
      if (options.topDown) {
        SyntaxPreservingRewriter.manytd(
          "rewriteEverywhere",
          ruleStrategy)
      } else {
        SyntaxPreservingRewriter.manybu(
          "rewriteEverywhere",
          ruleStrategy)
      }
    }
    val maybeLogging = {
      if (logger.isTraceEnabled) {
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
      if (options.repeat) {
        SyntaxPreservingRewriter.rewrite[PhraseType](
          SyntaxPreservingRewriter.repeat(
            "rewriteRepeat",
            new FixpointStrategy(maybeLogging))) _
      } else {
        SyntaxPreservingRewriter.rewrite[PhraseType](maybeLogging) _
      }
    }
    finalStrategy(phrase)
  }

  def query(
    rule : SilPhraseQuery,
    phrase : SilPhrase,
    options : SilRewriteOptions = SilRewriteOptions())
  {
    assert(!options.repeat)
    val ruleStrategy = Rewriter.query[SilPhrase](rule)
    val strategy = {
      if (options.topDown) {
        Rewriter.manytd(
          "rewriteEverywhere",
          ruleStrategy)
      } else {
        Rewriter.manybu(
          "rewriteEverywhere",
          ruleStrategy)
      }
    }
    Rewriter.rewrite(strategy)(phrase)
  }

  def replacementMatcher(f : SilPhraseReplacement)
      : SilPhraseReplacement = f

  def queryMatcher(f : SilPhraseQuery)
      : SilPhraseQuery = f

  def combineRules(rules : SilPhraseReplacement*)
      : SilPhraseReplacement =
  {
    rules.reduceLeft(_ orElse _)
  }

  private def rememberTransformation(
    syntaxTree : SprSyntaxTree,
    phrase : SilPhrase) =
  {
    phrase match {
      case transformedPhrase : SilTransformedPhrase => {
        transformedPhrase.rememberSyntaxTree(syntaxTree)
      }
      case _ => assert(phrase.isInstanceOf[SilUnknownPhrase])
    }
    phrase
  }
}
