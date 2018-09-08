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

class TraceEmitter(logger : Logger) extends Emitter
{
  override def emit(any : Any)
  {
    logger.trace(any.toString)
  }

  override def emitln()
  {
    logger.trace("\n")
  }

  override def emitln(any : Any)
  {
    emit(any)
    emitln
  }
}

object SilPhraseRewriter
{
  def onPhraseTransformation(
    oldPhrase : SilPhrase, newPhrase : SilTransformedPhrase)
  {
    oldPhrase.maybeSyntaxTree match {
      case Some(syntaxTree) => {
        newPhrase.rememberSyntaxTree(syntaxTree)
      }
      case _ =>
    }
  }
}

class SilPhraseRewriter
{
  import SilPhraseRewriter._

  type SilPhraseReplacement = PartialFunction[SilPhrase, SilPhrase]

  type SilPhraseQuery = PartialFunction[SilPhrase, Unit]

  private val logger = LoggerFactory.getLogger(classOf[SilPhraseRewriter])

  object SyntaxPreservingRewriter extends CallbackRewriter
  {
    override def rewriting[PhraseType](
      oldPhrase : PhraseType, newPhrase : PhraseType) : PhraseType =
    {
      tupleN((oldPhrase, newPhrase)) match {
        case (oldTransformed : SilPhrase,
          newTransformed : SilTransformedPhrase) =>
          {
            onPhraseTransformation(oldTransformed, newTransformed)
          }
        case _ =>
      }
      tupleN((oldPhrase, newPhrase)) match {
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
          new TraceEmitter(logger))
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
}
