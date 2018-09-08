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

import com.lingeringsocket.shlurd.ilang._

import SprUtils._

class SprPhraseRewriter(analyzer : SprSyntaxAnalyzer)
  extends SilPhraseRewriter
{
  import SilPhraseRewriter._

  def parseSentence(sentenceSyntaxTree : SprSyntaxTree) : SilSentence =
  {
    val forceSQ = sentenceSyntaxTree.firstChild.firstChild.isBeingVerb
    val expected = SilExpectedSentence(sentenceSyntaxTree, forceSQ)
    val transformed = rewrite[SilSentence](
      replaceAllPhrases, expected, SilRewriteOptions(repeat = true))
    val completed = rewrite(replaceUnresolvedWithUnrecognized, transformed)
    if (!completed.hasUnknown) {
      query(validateResult, completed)
    }
    completed match {
      case sentence : SilSentence => sentence
      case _ => SilUnrecognizedSentence(sentenceSyntaxTree)
    }
  }

  private def validateResult = queryMatcher {
    case transformedPhrase : SilTransformedPhrase => {
      if (!transformedPhrase.hasSyntaxTree) {
        throw new AssertionError("Syntax lost for " + transformedPhrase)
      }
    }
  }

  private def replaceAllPhrases = combineRules(
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
    case SilExpectedReference(noun : SprSyntaxNoun) => {
      SilNounReference(
        analyzer.getWord(noun.child),
        DETERMINER_UNSPECIFIED,
        analyzer.getCount(noun))
    }
    case SilExpectedNounlikeReference(
      syntaxTree, nounlike : SprSyntaxPreTerminal, determiner)
        if (nounlike.isNoun || nounlike.isAdjectival) =>
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
    case SilExpectedVerbModifier(advp : SptADVP) => {
      analyzer.expectVerbModifierPhrase(advp)
    }
    case SilExpectedVerbModifier(prt : SptPRT) => {
      analyzer.expectVerbModifierPhrase(prt)
    }
    case SilExpectedVerbModifier(tmod : SptTMOD) => {
      analyzer.expectTemporalVerbModifier(tmod)
    }
    case SilExpectedVerbModifier(adv : SprSyntaxAdverb) => {
      analyzer.expectBasicVerbModifier(adv)
    }
    case SilExpectedVerbModifier(particle : SptRP) => {
      analyzer.expectBasicVerbModifier(particle)
    }
    case SilExpectedVerbModifier(pp : SptPP) => {
      analyzer.expectAdpositionalVerbModifier(pp)
    }
  }

  private def replaceExpectedState = replacementMatcher {
    case SilExpectedAdpositionalState(syntaxTree) => {
      analyzer.expectAdpositionalState(syntaxTree)
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
        SilExpectedAdpositionalState(syntaxTree)
      } else {
        analyzer.expectPropertyComplementState(syntaxTree)
      }
    }
    case SilExpectedComplementState(vp : SptVP) => {
      // FIXME:  ambiguity for action (passive construction) vs
      // state (participial adjective)
      analyzer.expectPropertyComplementState(vp)
    }
    case SilExpectedComplementState(syntaxTree : SptPRT) => {
      analyzer.expectPropertyState(requireUnique(syntaxTree.children))
    }
    case SilExpectedComplementState(SptNP(noun)) => {
      analyzer.expectPropertyState(noun)
    }
  }

  private def replaceUnresolvedPredicate = replacementMatcher {
    case predicate : SilUnresolvedStatePredicate
        if (!predicate.state.hasUnresolved) =>
      {
        resolveStatePredicate(predicate)
      }
    case predicate : SilUnresolvedActionPredicate
        if (!predicate.hasUnresolved) =>
      {
        resolveActionPredicate(predicate)
      }
    case predicate : SilUnresolvedRelationshipPredicate =>
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
            SilAmbiguousSentence(clean, true)
          }
        }
      }
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
        assert(!predicate.adpositionObject.isEmpty)
        predicate.modifiers.flatMap(modifier => {
          SilReference.getDanglingAdposition(modifier) match {
            case Some(adposition) => {
              val newModifier = SilAdpositionalVerbModifier(
                adposition,
                predicate.adpositionObject.get)
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
      case SilConjunctiveState(DETERMINER_UNSPECIFIED, states, _) => {
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
