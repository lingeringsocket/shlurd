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

import ShlurdParseUtils._
import ShlurdEnglishLemmas._

class ShlurdPhraseRewrite(analyzer : ShlurdSyntaxAnalyzer)
{
  type ShlurdPhraseTransformation = PartialFunction[ShlurdPhrase, ShlurdPhrase]

  type ShlurdPhraseQuery = PartialFunction[ShlurdPhrase, Unit]

  private val logger = LoggerFactory.getLogger(classOf[ShlurdPhraseRewrite])

  def completeSentence(
    tree : ShlurdSyntaxTree, phrase : ShlurdPhrase) : ShlurdSentence =
  {
    val completed = rewrite(rewriteUnresolvedToUnrecognized)(phrase)
    if (!completed.hasUnknown) {
      query(validateResult)(completed)
    }
    completed match {
      case sentence : ShlurdSentence => sentence
      case _ => ShlurdUnrecognizedSentence(tree)
    }
  }

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

  def rewrite(
    rule : ShlurdPhraseTransformation)
      : (ShlurdPhrase) => ShlurdPhrase =
  {
    val strategy =
      SyntaxPreservingRewriter.manybu(
        "rewriteEverywhere",
        SyntaxPreservingRewriter.rule[ShlurdPhrase](rule))
    val maybeLogging = if (logger.isDebugEnabled) {
      SyntaxPreservingRewriter.log(
        "rewriteLog",
        strategy,
        "REWRITE ",
        new ErrorEmitter)
    } else {
      strategy
    }
    SyntaxPreservingRewriter.rewrite[ShlurdPhrase](
      SyntaxPreservingRewriter.repeat(
        "rewriteRepeat",
        maybeLogging))
  }

  def query(
    rule : ShlurdPhraseQuery)
      : (ShlurdPhrase) => Unit =
  {
    val strategy =
      Rewriter.manybu(
        "rewriteEverywhere",
        Rewriter.query[ShlurdPhrase](rule))
    Rewriter.rewrite(strategy)
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

  def transformationMatcher(f : ShlurdPhraseTransformation)
      : ShlurdPhraseTransformation = f

  def queryMatcher(f : ShlurdPhraseQuery)
      : ShlurdPhraseQuery = f

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

  def validateResult = queryMatcher {
    case transformedPhrase : ShlurdTransformedPhrase => {
      if (!transformedPhrase.hasSyntaxTree) {
        throw new AssertionError("Syntax lost for " + transformedPhrase)
      }
    }
  }

  def rewriteExpectedSentence() = transformationMatcher {
    case ShlurdExpectedSentence(sentence : SptS, forceSQ) => {
      if (forceSQ) {
        analyzer.analyzeSQ(sentence, forceSQ)
      } else {
        analyzer.analyzeSentence(sentence)
      }
    }
  }

  def rewriteExpectedReference = transformationMatcher {
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
    case ShlurdExpectedNounlikeReference(
      syntaxTree, nounlike : ShlurdSyntaxPreTerminal, determiner)
        if (nounlike.isNoun || nounlike.isAdjectival) =>
    {
      // we allow mislabeled adjectives to handle
      // cases like "roll up the blind"
      ShlurdEntityReference(
        analyzer.getWord(nounlike.child),
        determiner,
        analyzer.getCount(nounlike))
    }
    case ShlurdExpectedReference(pronoun : ShlurdSyntaxPronoun) => {
      recognizePronounReference(pronoun.child)
    }
    case ShlurdUnresolvedRelativeReference(
      _, reference, qualifier : ShlurdPropertyState) =>
      {
        ShlurdReference.qualifiedByProperties(reference, Seq(qualifier))
      }
  }

  def rewriteExpectedState = transformationMatcher {
    case ShlurdExpectedPrepositionalState(syntaxTree) => {
      analyzer.expectPrepositionalState(syntaxTree)
    }
    case ShlurdExpectedPropertyState(preTerminal : ShlurdSyntaxPreTerminal) => {
      ShlurdPropertyState(analyzer.getWord(preTerminal.child))
    }
    case ShlurdExpectedExistenceState(_) => {
      ShlurdExistenceState()
    }
    case ShlurdExpectedComplementState(adjp : SptADJP) => {
      analyzer.expectPropertyComplementState(adjp.children)
    }
    case ShlurdExpectedComplementState(
      syntaxTree @ (_ : SptADVP | _ : SptPP)) =>
    {
      val seq = syntaxTree.children
      if ((seq.head.isPreposition || seq.head.isAdverb) && (seq.size > 1) &&
        (!seq.exists(_.isPrepositionalPhrase)))
      {
        ShlurdExpectedPrepositionalState(syntaxTree)
      } else {
        analyzer.expectPropertyComplementState(seq)
      }
    }
    case ShlurdExpectedComplementState(vp : SptVP) => {
      // TODO:  ambiguity for action (passive construction) vs
      // state (participial adjective)
      analyzer.expectPropertyComplementState(vp.children)
    }
    case ShlurdExpectedComplementState(syntaxTree : SptPRT) => {
      analyzer.expectPropertyState(requireUnique(syntaxTree.children))
    }
  }

  def rewriteUnresolvedPredicate = transformationMatcher {
    case predicate : ShlurdUnresolvedStatePredicate
        if (!predicate.state.hasUnresolved) =>
      {
        resolveStatePredicate(predicate)
      }
    case predicate : ShlurdUnresolvedRelationshipPredicate =>
      {
        resolveRelationshipPredicate(predicate)
      }
  }

  def rewriteExpectedSBARQ = transformationMatcher {
    case ShlurdExpectedSentence(sbarq : SptSBARQ, _) => {
      analyzer.analyzeSBARQ(sbarq)
    }
  }

  def rewriteExpectedSQ = transformationMatcher {
    case ShlurdExpectedSentence(sq : SptSQ, forceSQ) => {
      analyzer.analyzeSQ(sq, forceSQ)
    }
  }

  def rewriteAmbiguousSentence = transformationMatcher {
    case ambiguous : ShlurdAmbiguousSentence if (ambiguous.isRipe) => {
      val alternatives = ambiguous.alternatives
      assert(!alternatives.isEmpty)
      val dedup = alternatives.distinct
      if (dedup.size == 1) {
        dedup.head
      } else {
        val clean = dedup.filterNot(_.hasUnknown)
        if (clean.isEmpty) {
          // if all alternatives still contain unknowns, then
          // pick the one with the minimum number of unparsed leaves
          dedup.minBy(_.countUnknownSyntaxLeaves)
        } else {
          if (clean.size == 1) {
            clean.head
          } else {
            ShlurdAmbiguousSentence(clean, true)
          }
        }
      }
    }
  }

  def rewriteUnresolvedToUnrecognized = transformationMatcher {
    case ShlurdUnresolvedRelativeReference(syntaxTree, _, _) => {
      ShlurdUnrecognizedReference(syntaxTree)
    }
    case predicate : ShlurdUnresolvedStatePredicate => {
      resolveStatePredicate(predicate)
    }
    case predicate : ShlurdUnresolvedRelationshipPredicate => {
      resolveRelationshipPredicate(predicate)
    }
  }

  private def resolveRelationshipPredicate(
    predicate : ShlurdUnresolvedRelationshipPredicate) =
  {
    ShlurdRelationshipPredicate(
      predicate.reference,
      predicate.complement,
      predicate.relationship)
  }

  private def resolveStatePredicate(
    predicate : ShlurdUnresolvedStatePredicate) =
  {
    predicate.state match {
      case ShlurdConjunctiveState(DETERMINER_UNSPECIFIED, states, _) => {
        val propertyState = states.head
        val fullySpecifiedState = {
          if (predicate.specifiedState == ShlurdNullState()) {
            if (states.size == 2) {
              states.last
            } else {
              ShlurdConjunctiveState(DETERMINER_ALL, states.tail)
            }
          } else {
            ShlurdConjunctiveState(
              DETERMINER_ALL, Seq(predicate.specifiedState) ++ states.tail)
          }
        }
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, fullySpecifiedState)
        ShlurdStatePredicate(specifiedSubject, propertyState)
      }
      case _ => {
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, predicate.specifiedState)
        ShlurdStatePredicate(specifiedSubject, predicate.state)
      }
    }
  }

  private def recognizePronounReference(leaf : ShlurdSyntaxLeaf)
      : ShlurdPronounReference=
  {
    val lemma = leaf.lemma
    val person = lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_MY |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS => PERSON_FIRST
      case LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case LEMMA_WE | LEMMA_US | LEMMA_THEY |
          LEMMA_OUR | LEMMA_THEIR => COUNT_PLURAL
      case _ => COUNT_SINGULAR
    }
    val gender = lemma match {
      case LEMMA_HE | LEMMA_HIM | LEMMA_HIS => GENDER_M
      case LEMMA_SHE | LEMMA_HER | LEMMA_HERS => GENDER_F
      case _ => GENDER_N
    }
    ShlurdPronounReference(person, gender, count)
  }
}
