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

import SprUtils._

object SprPhraseRewriter extends SprEnglishWordAnalyzer
{
  def resolveAmbiguousSentence(ambiguous : SilAmbiguousSentence)
      : SilSentence =
  {
    val alternatives = ambiguous.alternatives
    assert(!alternatives.isEmpty)
    val dedup = alternatives.distinct
    if (dedup.size == 1) {
      dedup.head
    } else {
      def leastUnknown = dedup.minBy(_.countUnknownSyntaxLeaves)
      val clean = dedup.filterNot(_.hasUnknown)
      if (clean.isEmpty) {
        // if all alternatives still contain unknowns, then
        // pick the one with the minimum number of unparsed leaves
        leastUnknown
      } else {
        val scorer = new SilWordnetScorer
        val scores = clean.map(scorer.computeGlobalScore)
        val maxScore = scores.max
        val bestScore = {
          if (maxScore <= SilPhraseScore.conBig) {
            SilPhraseScore.neutral
          } else {
            maxScore
          }
        }
        val candidates =
          clean.zip(scores).filter(_._2 == bestScore).map(_._1)
        if (candidates.isEmpty) {
          leastUnknown
        } else if (candidates.size == 1) {
          candidates.head
        } else if (candidates.size < 20) {
          resolveAmbiguousSeq(candidates)
        } else {
          SilAmbiguousSentence(candidates, true)
        }
      }
    }
  }

  private def resolveAmbiguousSeq(seq : Seq[SilSentence]) : SilSentence =
  {
    assert(seq.nonEmpty)
    if (seq.size == 1) {
      seq.head
    } else {
      range(0 until seq.size).combinations(2).foreach(sub => {
        val iFirst = sub.head
        val iSecond = sub.last
        val first = seq(iFirst)
        val second = seq(iSecond)
        resolveAmbiguousPair(first, second) match {
          case Some(resolved) => {
            return resolveAmbiguousSeq(
              seq.patch(iFirst, Seq(resolved), 1).patch(iSecond, Seq.empty, 1))
          }
          case _ =>
        }
      })
      SilAmbiguousSentence(seq, true)
    }
  }

  private def resolveAmbiguousPair(
    first : SilSentence, second : SilSentence) : Option[SilSentence] =
  {
    if (ambiguousEquivalent(first, second)) {
      Some(first)
    } else if (ambiguousEquivalent(second, first)) {
      Some(second)
    } else {
      None
    }
  }

  private def ambiguousEquivalent(
    s1 : SilSentence, s2 : SilSentence) : Boolean =
  {
    val sn1 = normalizeCandidate(s1)
    val sn2 = normalizeCandidate(s2)
    tupleN((sn1, sn2)) match {
      case (
        SilPredicateSentence(p1, t1, f1),
        SilPredicateSentence(p2, t2, f2)
      ) if (f1 == f2) => {
        ambiguousEquivalent(p1, t1, p2, t2)
      }
      case (
        SilPredicateQuery(p1, q1, a1, t1, f1),
        SilPredicateQuery(p2, q2, a2, t2, f2)
      ) if (tupleN((q1, a1, f1)) == tupleN((q2, a2, f2))) => {
        ambiguousEquivalent(p1, t1, p2, t2)
      }
      case (
        SilConditionalSentence(a1, c1, ta1, tc1, b1, f1),
        SilConditionalSentence(a2, c2, ta2, tc2, b2, f2)
      ) => {
        ambiguousEquivalent(a1, ta1, a2, ta2) &&
          ambiguousEquivalent(c1, tc1, c2, tc2) &&
          tupleN((b1, f1)) == tupleN((b2, f2))
      }
      case _ => {
        (sn1 == sn2)
      }
    }
  }

  private def normalizeCandidate(s : SilSentence) : SilSentence =
  {
    val rewriter = new SilPhraseRewriter
    def normalizePropertyState = rewriter.replacementMatcher {
      case SilPropertyState(SilWord(inflected, lemma, senseId)) => {
        SilPropertyState(SilWord(inflected, inflected, senseId))
      }
      case SilAdpositionalVerbModifier(SilAdposition(words), objRef) => {
        SilAdpositionalVerbModifier(
          SilAdposition(
            words.map(word => SilWord(word.inflected, word.inflected))),
          objRef
        )
      }
      case SilNounReference(noun, determiner, count) => {
        SilNounReference(
          SilWord(noun.inflected.toLowerCase, noun.lemma), determiner, count)
      }
    }
    rewriter.rewrite(normalizePropertyState, s)
  }

  private def ambiguousEquivalent(
    p1 : SilPredicate, t1 : SilTam, p2 : SilPredicate, t2 : SilTam) : Boolean =
  {
    tupleN((p1, p2)) match {
      case (
        SilStatePredicate(
          s1,
          SilPropertyState(w1),
          m1
        ),
        SilRelationshipPredicate(
          s2,
          SilNounReference(w2, DETERMINER_UNSPECIFIED, _),
          REL_IDENTITY,
          m2
        )
      ) => {
        tupleN((t1, s1, m1)) == tupleN((t2, s2, m2)) &&
          ((w1.lemma == w2.lemma) || (w1.inflected == w2.inflected))
      }
      // FIXME this is way too loose, and should be replace by disambiguation
      // at the semantic level
      case (
        SilStatePredicate(
          s1,
          _ : SilConjunctiveState,
          m1
        ),
        SilRelationshipPredicate(
          s2,
          _ : SilConjunctiveReference,
          REL_IDENTITY,
          m2
        )
      ) if (t1.modality != MODAL_NEUTRAL) => {
        tupleN((t1, s1, m1)) == tupleN((t2, s2, m2))
      }
      case (
        SilStatePredicate(
          s1,
          SilAdpositionalState(
            SilAdposition(Seq(w1a, w1b)),
            o1),
          m1
        ),
        SilRelationshipPredicate(
          s2,
          SilStateSpecifiedReference(
            SilNounReference(w2a, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
            SilAdpositionalState(
              SilAdposition(Seq(w2b)),
              o2)
          ),
          REL_IDENTITY,
          m2
        )
      ) => {
        tupleN((t1, s1, w1a, w1b, m1)) == tupleN((t2, s2, w2a, w2b, m2))
      }
      case (
        SilStatePredicate(
          SilStateSpecifiedReference(
            s1,
            a1 : SilAdpositionalState),
          SilPropertyState(p1),
          m1),
        SilRelationshipPredicate(
          s2,
          SilStateSpecifiedReference(
            SilNounReference(p2, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
            a2 : SilAdpositionalState),
          REL_IDENTITY,
          m2)
      ) => {
        tupleN((s1, a1, p1, m1)) == tupleN((s2, a2, p2, m2))
      }
      case (
        SilStatePredicate(
          s1,
          SilPropertyState(w1),
          m1
        ),
        SilActionPredicate(
          s2,
          w2,
          None,
          m2
        )
      ) => {
        tupleN((s1, w1.inflected, m1)) == tupleN((s2, w2.inflected, m2)) &&
        t2.isProgressive &&
        t1.withAspect(ASPECT_PROGRESSIVE) == t2
      }
      case _ => {
        tupleN((p1, t1)) == tupleN((p2, t2))
      }
    }
  }
}

class SprPhraseRewriter(val analyzer : SprSyntaxAnalyzer)
  extends SilPhraseRewriter
{
  import SprPhraseRewriter._
  import SilPhraseRewriter._

  def parseSentence(sentenceSyntaxTree : SprSyntaxTree) : SilSentence =
  {
    val forceSQ = sentenceSyntaxTree.firstChild.firstChild.isBeingVerb
    val expected = SilExpectedSentence(sentenceSyntaxTree, forceSQ)
    val transformed = rewritePhrase(expected)
    val completed = rewrite(replaceUnresolvedWithUnrecognized, transformed)
    if (!completed.hasUnknown) {
      query(validateResult, completed)
    }
    completed match {
      case sentence : SilSentence => sentence
      case _ => SilUnrecognizedSentence(sentenceSyntaxTree)
    }
  }

  def rewritePhrase[PhraseType <: SilPhrase](phrase : PhraseType) : SilPhrase =
  {
    rewrite(
      replaceAllPhrases, phrase, SilRewriteOptions(repeat = true))
  }

  private def validateResult = queryMatcher {
    case transformedPhrase : SilTransformedPhrase => {
      if (!transformedPhrase.hasSyntaxTree) {
        throw new AssertionError("Syntax lost for " + transformedPhrase)
      }
    }
  }

  protected def replaceAllPhrases = combineRules(
    replaceExpectedSentence,
    replaceExpectedSBARQ,
    replaceExpectedSQ,
    replaceAmbiguousSentence,
    replaceUnresolvedPredicate,
    replaceExpectedState,
    replaceExpectedVerbModifier,
    replaceExpectedReference)

  private def replaceExpectedSentence = replacementMatcher {
    case SilExpectedSentence(sentence : SptS, forceSQ) => {
      if (forceSQ) {
        analyzer.analyzeSQ(sentence, forceSQ)
      } else {
        analyzer.analyzeSentence(sentence)
      }
    }
  }

  private def replaceExpectedReference = replacementMatcher {
    case SilExpectedReference(SptNP(noun)) => {
      SilExpectedReference(noun)
    }
    case SilExpectedReference(np : SptNP) => {
      analyzer.analyzeNounPhrase(np)
    }
    case SilExpectedReference(SptNNQ(quotation)) => {
      SilQuotationReference(quotation.token)
    }
    case SilExpectedReference(noun : SprSyntaxNoun) => {
      SilNounReference(
        analyzer.getWord(noun.child),
        DETERMINER_UNSPECIFIED,
        analyzer.getCount(noun))
    }
    case SilExpectedNounlikeReference(
      syntaxTree, nounlike : SprSyntaxPreTerminal, determiner)
        if (analyzer.isNounPhraseHead(nounlike)) =>
    {
      // we allow mislabeled adjectives to handle
      // cases like "roll up the blind"
      SilNounReference(
        analyzer.getWord(nounlike.child),
        determiner,
        analyzer.getCount(nounlike))
    }
    case SilExpectedReference(pronoun : SprSyntaxPronoun) => {
      analyzer.analyzePronounReference(pronoun.child)
    }
    case SilExpectedReference(
      determiner : SprSyntaxDeterminer
    ) if (determiner.isDemonstrative) => {
      analyzer.analyzePronounReference(determiner.child)
    }
  }

  private def replaceExpectedVerbModifier = replacementMatcher {
    case SilExpectedVerbModifier(advp : SptADVP, successor) => {
      analyzer.expectVerbModifierPhrase(advp, successor)
    }
    case SilExpectedVerbModifier(prt : SptPRT, successor) => {
      analyzer.expectVerbModifierPhrase(prt, successor)
    }
    case SilExpectedVerbModifier(tmod : SptTMOD, _) => {
      analyzer.expectTemporalVerbModifier(tmod)
    }
    case SilExpectedVerbModifier(adv : SprSyntaxAdverb, successor) => {
      analyzer.expectBasicVerbModifier(adv, successor)
    }
    case SilExpectedVerbModifier(particle : SptRP, successor) => {
      analyzer.expectBasicVerbModifier(particle, successor)
    }
    case SilExpectedVerbModifier(pp : SptPP, _) => {
      analyzer.expectAdpositionalVerbModifier(pp)
    }
  }

  private def replaceExpectedState = replacementMatcher {
    case SilExpectedAdpositionalState(syntaxTree, extracted) => {
      analyzer.expectAdpositionalState(syntaxTree, extracted)
    }
    case SilExpectedPropertyState(
      preTerminal : SprSyntaxPreTerminal
    ) if (!analyzer.isProhibitedPropertyState(preTerminal)) => {
      SilPropertyState(analyzer.getWord(preTerminal.child))
    }
    case SilExpectedExistenceState(_) => {
      SilExistenceState()
    }
    case SilExpectedComplementState(adjp : SptADJP) => {
      analyzer.expectPropertyComplementState(adjp)
    }
    case SilExpectedComplementState(
      syntaxTree @ (_ : SptADVP | _ : SptPP)) =>
    {
      val seq = syntaxTree.children
      if ((seq.head.isAdposition || seq.head.isAdverb) && (seq.size > 1) &&
        ((seq.head.isAdverb || !seq.exists(_.isAdpositionalPhrase))))
      {
        SilExpectedAdpositionalState(syntaxTree, false)
      } else {
        analyzer.expectPropertyComplementState(syntaxTree)
      }
    }
    case SilExpectedComplementState(vp : SptVP) => {
      // FIXME:  ambiguity for action (passive construction) vs
      // state (participial adjective)
      analyzer.expectPropertyComplementState(vp)
    }
    case SilExpectedComplementState(syntaxTree : SptPRT) if (
      syntaxTree.numChildren == 1
    ) => {
      analyzer.expectPropertyState(requireUnique(syntaxTree.children))
    }
    case SilExpectedComplementState(SptNP(noun)) => {
      analyzer.expectPropertyState(noun)
    }
  }

  private def replaceUnresolvedPredicate = replacementMatcher {
    case predicate : SilUnresolvedStatePredicate
        if (!predicate.hasUnresolvedChildren) =>
      {
        resolveStatePredicate(predicate)
      }
    case predicate : SilUnresolvedActionPredicate
        if (!predicate.hasUnresolvedChildren) =>
      {
        resolveActionPredicate(predicate)
      }
    case predicate : SilUnresolvedRelationshipPredicate
        if (!predicate.hasUnresolvedChildren) =>
      {
        resolveRelationshipPredicate(predicate)
      }
  }

  private def replaceExpectedSBARQ = replacementMatcher {
    case SilExpectedSentence(sbarq : SptSBARQ, _) => {
      analyzer.analyzeSBARQ(sbarq)
    }
  }

  private def replaceExpectedSQ = replacementMatcher {
    case SilExpectedSentence(sinv : SptSINV, forceSQ) => {
      analyzer.analyzeSQ(sinv, forceSQ)
    }
    case SilExpectedSentence(sq : SptSQ, forceSQ) => {
      analyzer.analyzeSQ(sq, forceSQ)
    }
  }

  private def replaceAmbiguousSentence = replacementMatcher {
    case ambiguous : SilAmbiguousSentence if (ambiguous.isRipe) => {
      resolveAmbiguousSentence(ambiguous)
    }
  }

  private def replaceUnresolvedWithUnrecognized = replacementMatcher {
    case predicate : SilUnresolvedStatePredicate => {
      resolveStatePredicate(predicate)
    }
    case predicate : SilUnresolvedActionPredicate => {
      resolveActionPredicate(predicate)
    }
    case predicate : SilUnresolvedRelationshipPredicate => {
      resolveRelationshipPredicate(predicate)
    }
  }

  private def resolveRelationshipPredicate(
    predicate : SilUnresolvedRelationshipPredicate) =
  {
    SilRelationshipPredicate(
      predicate.subject,
      predicate.complement,
      predicate.relationship,
      predicate.modifiers)
  }

  private def resolveActionPredicate(
    predicate : SilUnresolvedActionPredicate) =
  {
    val modifiers = {
      if (predicate.modifiers.exists(
        m => !SilReference.getDanglingAdposition(m).isEmpty))
      {
        predicate.adpositionObject match {
          case Some(adpositionObject) => {
            predicate.modifiers.flatMap(modifier => {
              SilReference.getDanglingAdposition(modifier) match {
                case Some(adposition) => {
                  val newModifier = SilAdpositionalVerbModifier(
                    adposition,
                    adpositionObject)
                  onPhraseTransformation(modifier, newModifier)
                  // FIXME this is gross--we leave the dangling adposition
                  // around just so that later we can rememmber to
                  // convert from INFLECT_ACCUSATIVE to INFLECT_ADPOSITIONED
                  Seq(newModifier, modifier)
                }
                case _ => {
                  Seq(modifier)
                }
              }
            })
          }
          case _ => {
            predicate.modifiers
          }
        }
      } else {
        assert(predicate.adpositionObject.isEmpty)
        predicate.modifiers
      }
    }
    SilActionPredicate(
      predicate.subject,
      predicate.action,
      predicate.directObject,
      modifiers)
  }

  private def resolveStatePredicate(
    predicate : SilUnresolvedStatePredicate) =
  {
    predicate.state match {
      case cs @ SilConjunctiveState(DETERMINER_UNSPECIFIED, states, _) => {
        val propertyState = states.head
        val fullySpecifiedState = {
          if (predicate.specifiedState == SilNullState()) {
            if (states.size == 2) {
              states.last
            } else {
              SilConjunctiveState(DETERMINER_ALL, states.tail)
            }
          } else {
            SilConjunctiveState(
              DETERMINER_ALL, Seq(predicate.specifiedState) ++ states.tail)
          }
        }
        fullySpecifiedState match {
          case tp : SilTransformedPhrase => {
            SilPhraseRewriter.onPhraseTransformation(cs, tp)
          }
          case _ =>
        }
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, fullySpecifiedState)
        SilStatePredicate(specifiedSubject, propertyState, predicate.modifiers)
      }
      case _ => {
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, predicate.specifiedState)
        SilStatePredicate(
          specifiedSubject, predicate.state, predicate.modifiers)
      }
    }
  }
}
