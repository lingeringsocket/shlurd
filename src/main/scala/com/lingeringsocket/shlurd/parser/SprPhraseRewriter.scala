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

import scala.collection._

class SprPhraseRewriter(
  context : SprContext,
  val analyzer : SprSyntaxAnalyzer)
    extends SilPhraseRewriter(context.annotator)
{
  import SilPhraseRewriter._

  private def annotator = context.annotator

  private implicit val tongue = context.getTongue

  def parseSentence(sentenceSyntaxTree : SprSyntaxTree) : SilSentence =
  {
    val forceSQ = tongue.shouldForceSQ(sentenceSyntaxTree)
    val expected = SipExpectedSentence(sentenceSyntaxTree, forceSQ)
    val transformed = rewritePhrase(expected)
    val completed = rewrite(replaceUnresolvedWithUnrecognized, transformed)
    SilAnnotator.sanityCheck(annotator, completed)
    if (!completed.hasUnknown) {
      query(
        validateTransformations,
        completed)
    }
    completed match {
      case sentence : SilSentence => sentence
      case _ => SilUnrecognizedSentence(sentenceSyntaxTree)
    }
  }

  def rewritePhrase[PhraseType <: SilPhrase](phrase : PhraseType) : SilPhrase =
  {
    val rewritten = rewrite(
      replaceAllPhrases, phrase, SilRewriteOptions(repeat = true))
    rewrite(resolveAmbiguousSentence, rewritten)
  }

  private def validateTransformations = queryMatcher {
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

  private def replaceExpectedSentence = replacementMatcher(
    "replaceExpectedSentence", {
      case SipExpectedSentence(sentence : SptS, forceSQ) => {
        if (forceSQ) {
          analyzer.analyzeSQ(sentence, forceSQ)
        } else {
          analyzer.analyzeSentence(sentence)
        }
      }
      case SipExpectedConditionalSentence(
        syntaxTree,
        conjunction,
        antecedent,
        consequent,
        biconditional,
        formality
      ) if (!antecedent.hasUnresolvedChildren &&
        !consequent.hasUnresolvedChildren
      ) => {
        analyzer.analyzeConditionalSentence(
          syntaxTree,
          conjunction,
          antecedent,
          consequent,
          biconditional,
          formality
        )
      }
    }
  )

  private def replaceExpectedReference = replacementMatcher(
    "replaceExpectedReference", {
      case SipExpectedReference(SptNP(noun)) => {
        SipExpectedReference(noun)
      }
      case SipExpectedReference(np : SptNP) => {
        analyzer.analyzeNounPhrase(np)
      }
      case SipExpectedReference(SptNNQ(quotation)) => {
        annotator.quotationRef(quotation.token)
      }
      case SipExpectedReference(SptNNE()) => {
        createElidedReference
      }
      case SipExpectedReference(noun : SprSyntaxSimpleNoun) => {
        createNounReference(noun, DETERMINER_ABSENT)
      }
      case SipExpectedNounlikeReference(
        syntaxTree, nounlike : SprSyntaxPreTerminal, determiner)
          if (analyzer.isNounPhraseHead(nounlike)) =>
        {
          // we allow mislabeled adjectives to handle
          // cases like "roll up the blind"
          createNounReference(nounlike, determiner)
        }
      case SipExpectedNounlikeReference(
        syntaxTree, noun : SptNNC, determiner
      ) => {
        createNounReference(noun, determiner)
      }
      case SipExpectedReference(noun : SptNNC) => {
        createNounReference(noun, DETERMINER_ABSENT)
      }
      case SipExpectedReference(pronoun : SptPRP) => {
        analyzer.analyzePronounReference(pronoun.child)
      }
      case SipExpectedPossessiveReference(pronoun : SptPRP_POS) => {
        analyzer.analyzePronounReference(pronoun.child)
      }
      case SipExpectedReference(
        determiner : SprSyntaxDeterminer
      ) if (determiner.isDemonstrative) => {
        analyzer.analyzePronounReference(determiner.child)
      }
    }
  )

  private def createElidedReference =
  {
    // for an elided noun phrase, we make up a corresponding pronoun
    // reference and normalize it downstream with the correct
    // person/gender/count derived from the verb
    annotator.pronounRef(
      PERSON_THIRD,
      GENDER_NEUTER,
      COUNT_SINGULAR,
      context.genderAnalyzer,
      PROXIMITY_ELIDED
    )
  }

  private def createNounReference(
    nounlike : SprSyntaxPreTerminal,
    determiner : SilDeterminer) =
  {
    val count = analyzer.getCount(nounlike)
    val nounRef = annotator.nounRef(
      analyzer.getWord(nounlike.child),
      count)
    rememberDetermined(
      annotator.determinedRef(nounRef, determiner),
      nounlike)
  }

  private def rememberDetermined(
    ref : SilReference,
    tree : SprSyntaxTree) : SilReference =
  {
    ref matchPartial {
      case SilDeterminedReference(sub : SilNounReference, _) => {
        sub.rememberSyntaxTree(tree)
      }
    }
    ref
  }

  private def createNounReference(
    compound : SptNNC,
    determiner : SilDeterminer) =
  {
    val count = analyzer.getCount(compound.children.last)
    val nounRef = annotator.nounRef(
      analyzer.getCompoundWord(compound),
      count)
    rememberDetermined(
      annotator.determinedRef(nounRef, determiner),
        compound)
  }

  private def replaceExpectedVerbModifier = replacementMatcher(
    "replaceExpectedVerbModifier", {
      case SipExpectedVerbModifier(advp : SptADVP) => {
        analyzer.expectVerbModifierPhrase(advp)
      }
      case SipExpectedVerbModifier(prt : SptPRT) => {
        analyzer.expectVerbModifierPhrase(prt)
      }
      case SipExpectedVerbModifier(tmod : SptTMOD) => {
        analyzer.expectTemporalVerbModifier(tmod)
      }
      case SipExpectedVerbModifier(adv : SprSyntaxSimpleAdverb) => {
        analyzer.expectBasicVerbModifier(adv)
      }
      case SipExpectedVerbModifier(compound : SptRBC) => {
        analyzer.expectBasicVerbModifier(compound)
      }
      case SipExpectedVerbModifier(particle : SptRP) => {
        analyzer.expectBasicVerbModifier(particle)
      }
      case SipExpectedVerbModifier(pp : SptPP) => {
        analyzer.expectAdpositionalVerbModifier(pp)
      }
    }
  )

  private def replaceExpectedState = replacementMatcher(
    "replaceExpectedState", {
      case SipExpectedAdpositionalState(syntaxTree, extracted) => {
        analyzer.expectAdpositionalState(syntaxTree, extracted)
      }
      case SipExpectedPropertyState(
        preTerminal : SprSyntaxPreTerminal
      ) if (!analyzer.isProhibitedPropertyState(preTerminal)) => {
        SilPropertyState(analyzer.getWord(preTerminal.child))
      }
      case SipExpectedExistenceState(SptNP(SptEX(leaf))) => {
        SilExistenceState(Some(analyzer.getWord(leaf)))
      }
      case SipExpectedExistenceState(_) => {
        SilExistenceState()
      }
      case SipExpectedComplementState(adjp : SptADJP) => {
        analyzer.expectPropertyComplementState(adjp)
      }
      case SipExpectedComplementState(
        syntaxTree @ (_ : SptADVP | _ : SptPP)) =>
        {
          val seq = syntaxTree.children
          if ((seq.head.isAdposition || seq.head.isAdverb) && (seq.size > 1) &&
            ((seq.head.isAdverb || !seq.exists(_.isAdpositionalPhrase))))
          {
            SipExpectedAdpositionalState(syntaxTree, false)
          } else {
            analyzer.expectPropertyComplementState(syntaxTree)
          }
        }
      case SipExpectedComplementState(vp : SptVP) => {
        // FIXME:  ambiguity for action (passive construction) vs
        // state (participial adjective)
        analyzer.expectPropertyComplementState(vp)
      }
      case SipExpectedComplementState(syntaxTree : SptPRT) if (
        syntaxTree.numChildren == 1
      ) => {
        analyzer.expectPropertyState(requireUnique(syntaxTree.children))
      }
      case SipExpectedComplementState(SptNP(noun)) => {
        analyzer.expectPropertyState(noun)
      }
    }
  )

  private def replaceUnresolvedPredicate = replacementMatcher(
    "replaceUnresolvedPredicate", {
      case predicate : SipUnresolvedStatePredicate
          if (!predicate.hasUnresolvedChildren) =>
        {
          resolveStatePredicate(predicate)
        }
      case predicate : SipUnresolvedActionPredicate
          if (!predicate.hasUnresolvedChildren) =>
        {
          resolveActionPredicate(predicate)
        }
      case predicate : SipUnresolvedRelationshipPredicate
          if (!predicate.hasUnresolvedChildren) =>
        {
          resolveRelationshipPredicate(predicate)
        }
    }
  )

  private def replaceExpectedSBARQ = replacementMatcher(
    "replaceExpectedSBARQ", {
      case SipExpectedSentence(sbarq : SptSBARQ, _) => {
        analyzer.analyzeSBARQ(sbarq)
      }
    }
  )

  private def replaceExpectedSQ = replacementMatcher(
    "replaceExpectedSQ", {
      case SipExpectedSentence(sinv : SptSINV, forceSQ) => {
        analyzer.analyzeSQ(sinv, forceSQ)
      }
      case SipExpectedSentence(sq : SptSQ, forceSQ) => {
        analyzer.analyzeSQ(sq, forceSQ)
      }
    }
  )

  private def replaceAmbiguousSentence = replacementMatcher(
    "replaceAmbiguousSentence", {
      case ambiguous : SilAmbiguousSentence if (ambiguous.isRipe) => {
        val resolver = new SprAmbiguityResolver(context)
        resolver.resolveAmbiguousSentence(ambiguous)
      }
    }
  )

  private def resolveAmbiguousSentence = replacementMatcher(
    "resolveAmbiguousSentence", {
      case ambiguous : SilAmbiguousSentence if (!ambiguous.done) => {
        val resolver = new SprAmbiguityResolver(context)
        resolver.resolveAmbiguousSentence(ambiguous)
      }
    }
  )

  private def replaceUnresolvedWithUnrecognized = replacementMatcher(
    "relaceUnresolvedWithUnrecognized", {
      case predicate : SipUnresolvedStatePredicate => {
        resolveStatePredicate(predicate)
      }
      case predicate : SipUnresolvedActionPredicate => {
        resolveActionPredicate(predicate)
      }
      case predicate : SipUnresolvedRelationshipPredicate => {
        resolveRelationshipPredicate(predicate)
      }
    }
  )

  private def resolveRelationshipPredicate(
    predicate : SipUnresolvedRelationshipPredicate) =
  {
    SilRelationshipPredicate(
      predicate.subject,
      predicate.verb,
      predicate.complement,
      predicate.modifiers)
  }

  private def resolveActionPredicate(
    predicate : SipUnresolvedActionPredicate) =
  {
    val modifiers = {
      if (predicate.modifiers.exists(
        m => !SilUtils.getDanglingAdposition(m).isEmpty))
      {
        predicate.adpositionObject match {
          case Some(adpositionObject) => {
            predicate.modifiers.flatMap(modifier => {
              SilUtils.getDanglingAdposition(modifier) match {
                case Some(adposition) => {
                  val newModifier = SilAdpositionalVerbModifier(
                    adposition,
                    adpositionObject)
                  onPhraseTransformation(
                    annotator, modifier, newModifier)
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
      predicate.verb,
      predicate.directObject,
      modifiers)
  }

  private def resolveStatePredicate(
    predicate : SipUnresolvedStatePredicate) =
  {
    predicate.state match {
      case cs @ SilConjunctiveState(DETERMINER_ABSENT, states, _) => {
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
        fullySpecifiedState matchPartial {
          case tp : SilTransformedPhrase => {
            SilPhraseRewriter.onPhraseTransformation(
              annotator, cs, tp)
          }
        }
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, fullySpecifiedState)
        SilStatePredicate(
          specifiedSubject, predicate.verb, propertyState, predicate.modifiers)
      }
      case _ => {
        val specifiedSubject = analyzer.specifyReference(
          predicate.subject, predicate.specifiedState)
        SilStatePredicate(
          specifiedSubject, predicate.verb,
          predicate.state, predicate.modifiers)
      }
    }
  }
}

class SprAmbiguityResolver(context : SprContext)
{
  private def annotator = context.annotator

  private implicit val tongue = context.getTongue

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
        val scores = clean.map(context.scorer.computeGlobalScore)
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
          candidates.head
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
        resolveAmbiguousPair(first, second).foreach(resolved => {
          return resolveAmbiguousSeq(
            seq.patch(iFirst, Seq(resolved), 1).patch(iSecond, Seq.empty, 1))
        })
      })
      seq.head
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
    tupleN(sn1, sn2) match {
      case (
        SilPredicateSentence(p1, t1, f1),
        SilPredicateSentence(p2, t2, f2)
      ) if (f1 == f2) => {
        ambiguousEquivalent(p1, t1, p2, t2)
      }
      case (
        SilPredicateQuery(p1, q1, a1, t1, f1),
        SilPredicateQuery(p2, q2, a2, t2, f2)
      ) if (tupleN(q1, a1, f1) == tupleN(q2, a2, f2)) => {
        ambiguousEquivalent(p1, t1, p2, t2)
      }
      case _ => {
        (sn1 == sn2)
      }
    }
  }

  private def normalizeCandidate(s : SilSentence) : SilSentence =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    def normalizer = SilPhraseRewriter.replacementMatcher(
      "normalizeAmbiguousCandidate", {
        case SilPropertyState(SilSimpleWord(inflected, lemma, senseId)) => {
          SilPropertyState(SilSimpleWord(inflected, inflected, senseId))
        }
        case SilStatePredicate(
          subject, SprStatePredefVerb(STATE_PREDEF_BE), state, modifiers
        ) => {
          SilStatePredicate(subject, STATE_PREDEF_BE.toVerb, state, modifiers)
        }
      }
    )
    rewriter.rewrite(normalizer, s)
  }

  private def ambiguousEquivalent(
    p1 : SilPredicate, t1 : SilTam, p2 : SilPredicate, t2 : SilTam) : Boolean =
  {
    tupleN(p1, p2) match {
      // FIXME this is way too loose, and should be replace by disambiguation
      // at the semantic level
      case (
        SilStatePredicate(
          s1,
          SprStatePredefVerb(STATE_PREDEF_BE),
          _ : SilConjunctiveState,
          m1
        ),
        SilRelationshipPredicate(
          s2,
          SprRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          _ : SilConjunctiveReference,
          m2
        )
      ) if (t1.modality != MODAL_NEUTRAL) => {
        tupleN(t1, s1, m1) == tupleN(t2, s2, m2)
      }
      case _ => {
        tupleN(p1, t1) == tupleN(p2, t2)
      }
    }
  }
}
