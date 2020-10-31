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

import scala.collection._

import org.bitbucket.inkytonik.kiama.rewriting._
import org.bitbucket.inkytonik.kiama.util._

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
  override def emit(any : Any) : Unit =
  {
    logger.trace(any.toString)
  }

  override def emitln() : Unit =
  {
    logger.trace("\n")
  }

  override def emitln(any : Any) : Unit =
  {
    emit(any)
    emitln()
  }
}

case class SilPhraseCopyOptions(
  preserveBasicNotes : Boolean = false,
  preserveNotes : Boolean = false,
  preserveIds : Boolean = false
)

object SilPhraseRewriter
{
  def onPhraseTransformation(
    annotator : SilAnnotator,
    oldPhrase : SilPhrase,
    newPhrase : SilTransformedPhrase,
    copyOptions : SilPhraseCopyOptions = SilPhraseCopyOptions()) : Unit =
  {
    oldPhrase.maybeSyntaxTree.foreach(syntaxTree => {
      newPhrase.rememberSyntaxTree(syntaxTree)
    })
    tupleN(oldPhrase, newPhrase) matchPartial {
      case (
        oldRef : SilAnnotatedReference,
        newRef : SilAnnotatedReference
      ) => {
        annotator.transform(oldRef, newRef, copyOptions)
      }
    }
  }
}

case class SilPhraseReplacementMatcher(
  name : String,
  replacement : PartialFunction[SilPhrase, SilPhrase])
    extends PartialFunction[SilPhrase, SilPhrase]
{
  private val logger = LoggerFactory.getLogger(classOf[SilPhraseRewriter])

  override def isDefinedAt(phrase : SilPhrase) : Boolean =
  {
    val r = replacement.isDefinedAt(phrase)
    if (logger.isTraceEnabled) {
      if (r) {
        logger.trace(s"match succeeded on $name : $phrase")
      } else {
        logger.trace(s"match failed on $name : $phrase")
      }
    }
    r
  }

  override def apply(phrase : SilPhrase) = replacement.apply(phrase)
}

class SilPhraseQuerier
{
  type SilPhraseQuery = PartialFunction[SilPhrase, Unit]

  def query(
    rule : SilPhraseQuery,
    phrase : SilPhrase,
    options : SilRewriteOptions = SilRewriteOptions()) : Unit =
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

  def queryMatcher(f : SilPhraseQuery)
      : SilPhraseQuery = f
}

class SilPhraseRewriter(
  annotator : SilAnnotator
) extends SilPhraseQuerier
{
  import SilPhraseRewriter._

  type SilPhraseReplacement = PartialFunction[SilPhrase, SilPhrase]

  private val logger = LoggerFactory.getLogger(classOf[SilPhraseRewriter])

  class SyntaxPreservingRewriter(
    copyOptions : SilPhraseCopyOptions
  ) extends CallbackRewriter
  {
    override def rewriting[PhraseType](
      oldPhrase : PhraseType, newPhrase : PhraseType) : PhraseType =
    {
      tupleN(oldPhrase, newPhrase) matchPartial {
        case (
          oldTransformed : SilPhrase,
          newTransformed : SilTransformedPhrase
        ) => {
          onPhraseTransformation(
            annotator, oldTransformed, newTransformed, copyOptions)
        }
      }
      tupleN(oldPhrase, newPhrase) matchPartial {
        case (
          oldPredicate : SilPredicate,
          newPredicate : SilPredicate
        ) => {
          newPredicate.setInflectedAttributes(
            oldPredicate.getInflectedAttributes)
        }
      }
      newPhrase
    }

    def deepclone[T <: Product](t : T) : T = {
      val cloner = everywherebu(rule[T] {
        case n => copy(n)
      })
      rewrite(cloner)(t)
    }
  }

  def combineRules(rules : SilPhraseReplacement*)
      : SilPhraseReplacement =
  {
    rules.reduceLeft(_ orElse _)
  }

  def deepclone[PhraseType <: SilPhrase](
    phrase : PhraseType,
    copyOptions : SilPhraseCopyOptions = SilPhraseCopyOptions()) : PhraseType =
  {
    if (copyOptions.preserveIds) {
      // FIXME also need to resync ID generator
      val refs = SilUtils.collectReferences(phrase)
      if (refs.exists(_ match {
        case annotatedRef : SilAnnotatedReference => {
          annotatedRef.getAnnotator == annotator
        }
        case _ => false
      })) {
        throw new IllegalArgumentException(
          "can't preserve annotation ID when copying within same annotator")
      }
    }
    phrase match {
      case p : Product => {
        val syntaxPreservingRewriter = new SyntaxPreservingRewriter(copyOptions)
        val newPhrase =
          syntaxPreservingRewriter.deepclone(p).asInstanceOf[PhraseType]
        newPhrase
      }
      case _ => throw new IllegalArgumentException(phrase.toString)
    }
  }

  def rewriteCombined[PhraseType <: SilPhrase](
    rules : Seq[SilPhraseReplacement],
    phrase : PhraseType,
    options : SilRewriteOptions = SilRewriteOptions())
      : PhraseType =
  {
    if (rules.isEmpty) {
      phrase
    } else {
      rewrite(
        combineRules(rules.toSeq:_*),
        phrase,
        options)
    }
  }

  def rewrite[PhraseType <: SilPhrase](
    rule : SilPhraseReplacement,
    phrase : PhraseType,
    options : SilRewriteOptions = SilRewriteOptions())
      : PhraseType =
  {
    val syntaxPreservingRewriter = new SyntaxPreservingRewriter(
      SilPhraseCopyOptions(preserveNotes = true))
    val strategy = {
      val ruleStrategy = syntaxPreservingRewriter.rule(rule)
      if (options.topDown) {
        syntaxPreservingRewriter.manytd(
          "rewriteEverywhere",
          ruleStrategy)
      } else {
        syntaxPreservingRewriter.manybu(
          "rewriteEverywhere",
          ruleStrategy)
      }
    }
    val maybeLogging = {
      if (logger.isTraceEnabled) {
        syntaxPreservingRewriter.log(
          "rewriteLog",
          strategy,
          s"REWRITE",
          new TraceEmitter(logger))
      } else {
        strategy
      }
    }
    val finalStrategy = {
      if (options.repeat) {
        syntaxPreservingRewriter.rewrite[PhraseType](
          syntaxPreservingRewriter.repeat(
            "rewriteRepeat",
            new FixpointStrategy(maybeLogging))) _
      } else {
        syntaxPreservingRewriter.rewrite[PhraseType](maybeLogging) _
      }
    }
    finalStrategy(phrase)
  }

  def replacementMatcher(name : String, f : SilPhraseReplacement)
      : SilPhraseReplacement = SilPhraseReplacementMatcher(name, f)
}
