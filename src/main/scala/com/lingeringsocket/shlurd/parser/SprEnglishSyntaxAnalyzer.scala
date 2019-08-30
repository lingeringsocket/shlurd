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

import SprPennTreebankLabels._
import SprEnglishLemmas._
import SprUtils._

class SprEnglishSyntaxAnalyzer(
  guessedQuestion : Boolean, strictness : SprStrictness = SPR_STRICTNESS_LOOSE,
  enforceTransitive : Boolean = true)
    extends SprAbstractSyntaxAnalyzer(strictness) with SprEnglishWordAnalyzer
{
  override def analyzeSentence(tree : SptS)
      : SilSentence =
  {
    val hasQuestionMark =
      tree.children.last.hasTerminalLabel(LABEL_DOT, LABEL_QUESTION_MARK)
    val isQuestion =
      hasQuestionMark && !guessedQuestion
    val force = {
      if (tree.children.last.hasTerminalLabel(
        LABEL_DOT, LABEL_EXCLAMATION_MARK))
      {
        FORCE_EXCLAMATION
      } else {
        FORCE_NEUTRAL
      }
    }
    val (semiSplits, semiSeparator) = splitSemicolons(tree.children)
    if (semiSplits.size > 1) {
      if (!semiSplits.forall(_.size == 1)) {
        return SilUnrecognizedSentence(tree)
      }
      return SilConjunctiveSentence(
        DETERMINER_UNSPECIFIED,
        semiSplits.map(split => SilExpectedSentence(split.head)),
        semiSeparator)
    }
    splitCoordinatingConjunction(tree.children) match {
      case (DETERMINER_UNSPECIFIED, _, _) => ;
      case (determiner, separator, splits) => {
        val subs = splits.map(split => {
          split match {
            case Seq(s : SptS) => Some(s)
            case _ => None
          }
        })
        if (subs.forall(_.nonEmpty)) {
          return SilConjunctiveSentence(
            determiner,
            subs.flatten.map(s => SilExpectedSentence(s)),
            separator)
        }
      }
    }
    val children = stripPauses(tree)
    extractAntecedent(children) match {
      case Some((conjunction, antecedent)) => {
        expectConditionalSentence(
          tree,
          conjunction,
          antecedent,
          SptS(children.tail.filterNot(c => (c.isThen || c.isEquivalently)):_*),
          children.tail.exists(c =>
            (c.isEquivalently || c.children.exists(_.isEquivalently))),
          SilFormality(force))
      }
      case _ if (isImperative(children)) => {
        expectCommand(tree, children.head, SilFormality(force))
      }
      case _ if (children.size >= 2) => {
        val tail = children.takeRight(2)
        val np = tail.head
        val vp = tail.last
        val verbModifiers = children.dropRight(2)
        if (np.isNounNode && vp.isVerbPhrase &&
          verbModifiers.forall(_.isAdverbialPhrase))
        {
          val tam = if (isQuestion) {
            SilTam.interrogative
          } else {
            SilTam.indicative
          }
          expectPredicateSentence(
            tree, np, vp,
            expectVerbModifiers(verbModifiers :+ np).dropRight(1),
            force, tam, COUNT_SINGULAR, false)
        } else {
          SilUnrecognizedSentence(tree)
        }
      }
      case _ => {
        SilUnrecognizedSentence(tree)
      }
    }
  }

  private def expectVerbModifiers(seq : Seq[SprSyntaxTree]) =
  {
    seq.map(expectVerbModifier)
  }

  override def analyzeSQ(
    tree : SprSyntaxTree, forceSQ : Boolean)
      : SilSentence =
  {
    val punctless = stripPauses(tree)
    val (specifiedState, children) = {
      val unwrapped = {
        if (forceSQ && isSinglePhrase(punctless)) {
          punctless.head.children
        } else {
          punctless
        }
      }
      val (s, c) = extractAdpositionalState(unwrapped)
      if (c.size < 3) {
        tupleN((SilNullState(), unwrapped))
      } else {
        tupleN((s, c))
      }
    }
    if (!forceSQ && isImperative(punctless)) {
      assert(specifiedState == SilNullState())
      return expectCommand(tree, children.head, SilFormality.DEFAULT)
    }
    if (children.size > 2) {
      analyzeSubQueryChildren(tree, children, specifiedState) match {
        case Some((predicate, tam)) => {
          SilPredicateSentence(predicate, tam)
        }
        case _ => SilUnrecognizedSentence(tree)
      }
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private def detectProgressive(
    seq : Seq[SprSyntaxTree]) : (Boolean, Int) =
  {
    val iFirstVerb = seq.indexWhere(_.isVerbNode)
    if (iFirstVerb < 0) {
      return tupleN((false, -1))
    }
    val iNextVerb = seq.indexWhere(_.isVerbNode, iFirstVerb + 1)
    if (iNextVerb < 0) {
      return tupleN((false, iFirstVerb))
    }
    val nextVerb = seq(iNextVerb) match {
      case vp : SptVP => {
        vp.children.find(_.isVerbNode) match {
          case Some(vb) => vb
          case _ => vp
        }
      }
      case vb => vb
    }
    if (seq(iFirstVerb).unwrapPhrase.isRelationshipVerb &&
      nextVerb.unwrapPhrase.isProgressiveVerb)
    {
      tupleN((true, iNextVerb))
    } else {
      tupleN((false, iFirstVerb))
    }
  }

  private def analyzeSubQueryChildren(
    tree : SprSyntaxTree,
    children : Seq[SprSyntaxTree],
    specifiedState : SilState,
    specifiedDirectObject : Option[SilReference] = None,
    extraModifiers : Seq[SilExpectedVerbModifier] = Seq.empty)
      : Option[(SilPredicate, SilTam)] =
  {
    val (tam, auxless, auxCount) = extractAux(children)
    val (negativeSuper, seq) = extractNegative(auxless)
    val iVerb = seq.indexWhere(_.isVerbNode)
    if (iVerb < 0) {
      return None
    }

    val (verbHead, np, vp, rhs, negative, verbModifiers, rhsLoss) = {
      if (seq.head.unwrapPhrase.isRelationshipVerb &&
        (tam.modality == MODAL_NEUTRAL))
      {
        // "is Larry smart?"
        val expectedSize = 3
        if (seq.size < (iVerb + expectedSize)) {
          return None
        }
        val fromVerbSlice = seq.slice(iVerb, iVerb + expectedSize)
        val vp = SptVP(fromVerbSlice(0), fromVerbSlice(2))
        tupleN((fromVerbSlice(0), fromVerbSlice(1), vp,
          fromVerbSlice(2), negativeSuper,
          expectVerbModifiers(seq).patch(iVerb, Seq.empty, expectedSize),
          false))
      } else {
        if (iVerb == 0) {
          return None
        }
        // "(can) Larry [be [smart]]?"
        val expectedSize = 2
        val iNoun = iVerb - 1
        if (seq.view(0, iNoun).exists(
          c => isNounPhraseHead(c) || isNounPhraseModifier(c, c) ||
            c.isDeterminer))
        {
          return None
        }
        if (seq.size < (iNoun + expectedSize)) {
          return None
        }
        val fromNounSlice = seq.slice(iNoun, iNoun + expectedSize)
        val nounSuccessor = fromNounSlice(1)
        val vp = nounSuccessor match {
          case _ : SptVP => nounSuccessor
          case _ => SptVP(seq.drop(iNoun + 1):_*)
        }
        val (negativeSub, sub) = extractNegative(vp.children)
        tupleN((sub.head, fromNounSlice(0), SptVP(sub:_*),
          sub.last, (negativeSub ^ negativeSuper),
          expectVerbModifiers(seq).patch(iNoun, Seq.empty, expectedSize),
          (sub.size > 2)))
      }
    }
    if (np.children.isEmpty) {
      return None
    }
    val tamTensed = extractTense(verbHead, tam)
    if (verbHead.isRelationshipVerb && specifiedDirectObject.isEmpty) {
      if (rhsLoss) {
        return None
      }
      val (negativeSub, predicate) =
        expectPredicate(tree, np, rhs, specifiedState,
          relationshipVerb(verbHead), verbModifiers ++ extraModifiers)
      val polarity = !(negative ^ negativeSub)
      rememberPredicateCount(predicate, verbHead, tam, auxCount)
      Some((predicate,
        tamTensed.withMood(MOOD_INTERROGATIVE).withPolarity(polarity)))
    } else {
      val (negativeSub, predicate) = analyzeActionPredicate(
        tree, np, vp, specifiedDirectObject, extraModifiers)
      val polarity = !(negative ^ negativeSub)
      rememberPredicateCount(predicate, verbHead, tam, auxCount)
      Some((predicate,
        tamTensed.withMood(MOOD_INTERROGATIVE).withPolarity(polarity)))
    }
  }

  override def analyzeSBARQ(tree : SptSBARQ)
      : SilSentence =
  {
    val children = stripPauses(tree)
    if (children.size != 2) {
      return SilUnrecognizedSentence(tree)
    }
    val first = children.head
    val second = children(1)
    val secondUnwrapped = {
      if ((second.numChildren == 1) && second.firstChild.isVerbPhrase) {
        second.firstChild.children
      } else {
        second.children
      }
    }
    val (negativeSuper, secondSub) = extractNegative(secondUnwrapped)
    if (!first.isQueryPhrase || !second.isSubQuestion) {
      return SilUnrecognizedSentence(tree)
    }
    val (specifiedState, whpc) = extractAdpositionalState(first.children)
    val seq = {
      if ((whpc.size == 1) && whpc.head.isQueryPhrase) {
        whpc.head.children
      } else {
        whpc
      }
    }
    val (question, adpositionOpt, questionChildren) =
      maybeQuestionFor(seq) match {
        case Some((q, a, qc)) => (q, a, qc)
        case _ => return SilUnrecognizedSentence(tree)
      }
    val verbHead = secondSub.head
    if (!verbHead.isVerbNode && !verbHead.isModal) {
      return SilUnrecognizedSentence(tree)
    }
    if ((question == QUESTION_WHERE) && !verbHead.isBeingVerb) {
      return SilUnrecognizedSentence(tree)
    }
    if (questionChildren.isEmpty) {
      return SilUnrecognizedSentence(tree)
    }
    // FIXME for QUESTION_WHERE, it shouldn't be a noun phrase at all;
    // it should be either a state or verb modifier
    val np = question match {
      // FIXME for QUESTION_WHAT, there are two flavors (plain "what
      // do you want?" and also "what beer is most delicious?")
      case QUESTION_WHERE  | QUESTION_WHAT => {
        SptNP(SptNN(requireLeaf(questionChildren)))
      }
      case QUESTION_WHO => {
        if ((questionChildren.size == 1) && questionChildren.head.isLeaf) {
          SptNP(SptNN(requireLeaf(questionChildren)))
        } else {
          SptNP(questionChildren:_*)
        }
      }
      case QUESTION_WHICH | QUESTION_HOW_MANY => {
        // FIXME likewise, these have two flavors "which do you want?"
        // and "which flavor do you want?"; "how many trees are there"
        // and "how many are still alive"
        SptNP(questionChildren:_*)
      }
    }
    val (progressive, iVerb) = detectProgressive(secondSub)
    assert(iVerb >= 0, secondSub)
    if (verbHead.isRelationshipVerb && !progressive) {
      // FIXME find a way to represent this
      if (!adpositionOpt.isEmpty) {
        return SilUnrecognizedSentence(tree)
      }
      val complement = secondSub.tail
      val (combinedState, complementRemainder) = {
        if (specifiedState == SilNullState()) {
          val (s, r) = extractAdpositionalState(complement)
          if (r.isEmpty) {
            tupleN((specifiedState, complement))
          } else {
            tupleN((s, r))
          }
        } else {
          tupleN((specifiedState, complement))
        }
      }
      val (subj, recomposedComplement, answerInflection, modifiers) = {
        if (complement.isEmpty) {
          tupleN((np, verbHead, INFLECT_NOMINATIVE, Seq.empty))
        } else {
          if (complement.head.isPreTerminal) {
            return SilUnrecognizedSentence(tree)
          }
          complement.last match {
            case SptPP(pt : SprSyntaxPreTerminal) => {
              if (!isAdposition(pt.child.lemma) || (complement.size < 3)) {
                return SilUnrecognizedSentence(tree)
              }
              tupleN((
                complement.head,
                SprSyntaxRewriter.recompose(
                  complement(1),
                  complement.drop(1).dropRight(1)),
                INFLECT_ADPOSITIONED,
                expectVerbModifiers(Seq(SptPP(pt, np)))
              ))
            }
            case _ => {
              tupleN((
                np,
                SprSyntaxRewriter.recompose(
                  complement.head, complementRemainder),
                INFLECT_NOMINATIVE,
                Seq.empty))
            }
          }
        }
      }
      val (negativeSub, predicate) = expectPredicate(
        tree,
        subj,
        recomposedComplement,
        combinedState,
        relationshipVerb(verbHead),
        modifiers)
      rememberPredicateCount(predicate, verbHead)
      val tam = SilTam.interrogative.
        withPolarity(!(negativeSuper ^ negativeSub))
      val tamTensed = extractTense(verbHead, tam)
      SilPredicateQuery(
        predicate, question, answerInflection, tamTensed)
    } else {
      // maybe we should use dependency info (plus WHO vs WHOM) too
      val (specifiedDirectObject, answerInflection,
        sqChildren, extraModifiers) =
      {
        val accusativePattern = {
          if (secondSub.size > 1) {
            !secondSub(1).isVerbNode
          } else {
            false
          }
        }
        if ((verbHead.isModal || progressive) && accusativePattern) {
          // I think you mean "whom", Chief!
          adpositionOpt match {
            case Some(adposition) => {
              tupleN((None, INFLECT_ADPOSITIONED, secondUnwrapped,
                expectVerbModifiers(Seq(SptPP(adpositionOpt.get, np)))))
            }
            case _ =>  {
              tupleN((Some(expectReference(np)), INFLECT_ACCUSATIVE,
                secondUnwrapped, Seq.empty))
            }
          }
        } else {
          assert(adpositionOpt.isEmpty)
          tupleN((None, INFLECT_NOMINATIVE,
            Seq(np) ++ secondUnwrapped, Seq.empty))
        }
      }
      analyzeSubQueryChildren(
        tree, sqChildren, specifiedState,
        specifiedDirectObject, extraModifiers) match
      {
        case Some((predicate, tam)) => {
          SilPredicateQuery(
            predicate, question, answerInflection, tam)
        }
        case _ => {
          SilUnrecognizedSentence(tree)
        }
      }
    }
  }

  override def analyzeNounPhrase(
    tree : SptNP) : SilReference =
  {
    val seqIn = tree.children
    if (seqIn.isEmpty) {
      return SilUnrecognizedReference(tree)
    }
    val seq = seqIn.map(_.unwrapPhrase)
    if (seq.size == 1) {
      return expectReference(seq.head)
    }
    if (seq(1).hasLabel(LABEL_LPAREN) && seq.last.hasLabel(LABEL_RPAREN)) {
      return SilAppositionalReference(
        expectReference(seq.head),
        expectReference(seq.tail)
      )
    }
    if (seq.head.hasLabel(LABEL_LPAREN) && seq.last.hasLabel(LABEL_RPAREN)) {
      return SilParenthesizedReference(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_PAREN
      )
    }
    if (seq.head.hasLabel(LABEL_LCURLY) && seq.last.hasLabel(LABEL_RCURLY)) {
      return SilParenthesizedReference(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_CURLY
      )
    }
    if (seq.head.lastChild.isPossessiveClitic) {
      return SilGenitiveReference(
        expectReference(seq.head.children.dropRight(1)),
        expectReference(seq.tail))
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_UNSPECIFIED, _, _) => {
      }
      case (determiner, separator, split) => {
        return SilConjunctiveReference(
          determiner, split.map(expectReference), separator)
      }
    }
    val (determiner, components) = {
      val first = seq.head.unwrapPhrase
      first match {
        // FIXME we should allow coordinating determiners in the absence
        // of conjunctions, e.g. "both lions are asleep"
        case pt @ (
          _ : SptDT | _ : SptCD
        ) if (
          !pt.isDemonstrative &&
            !isCoordinatingDeterminer(pt.firstChild.lemma)
        ) => {
          tupleN((determinerFor(requireLeaf(pt.children)), seq.drop(1)))
        }
        case _ => {
          tupleN((DETERMINER_UNSPECIFIED, seq))
        }
      }
    }
    if ((components.size == 2) &&
      ((components.head.isPronoun &&
        isPossessiveAdjective(components.head.firstChild.token)) ||
        components.head.isDemonstrative))
    {
      val pronounReference = SilExpectedPossessiveReference(components.head)
      val entityReference = expectNounReference(
        tree, components.last, determiner)
      SilGenitiveReference(pronounReference, entityReference)
    } else if (components.last.isCompoundAdpositionalPhrase) {
      SilStateSpecifiedReference(
        expectReference(seqIn.dropRight(1)),
        SilExpectedAdpositionalState(components.last, false))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = expectReference(components.head)
      expectRelativeReference(tree, entityReference, components.last)
    } else if (isNounPhraseHead(components.last) &&
      components.dropRight(1).forall(
        c => isNounPhraseModifier(c, components.last)))
    {
      val qr = {
        val entityReference = expectNounReference(
          tree, components.last, DETERMINER_UNSPECIFIED)
        if (components.size > 1) {
          val adjComponents = components.dropRight(1)
          val qualifiedReference = SilReference.qualifiedByProperties(
            entityReference,
            adjComponents.map(expectPropertyState))
          qualifiedReference matchPartial {
            case sr @ SilStateSpecifiedReference(
              _, state
            ) => {
              sr.rememberSyntaxTree(tree)
              state matchPartial {
                case cs : SilConjunctiveState => {
                  rememberSyntheticADJP(cs, adjComponents)
                }
              }
            }
          }
          qualifiedReference
        } else {
          entityReference
        }
      }
      SilReference.determined(qr, determiner)
    } else {
      SilUnrecognizedReference(tree)
    }
  }

  private def expectPredicateSentence(
    tree : SprSyntaxTree,
    np : SprSyntaxTree, vp : SprSyntaxTree,
    verbModifiers : Seq[SilExpectedVerbModifier],
    force : SilForce, tam : SilTam, auxCount : SilCount,
    negativeSuper : Boolean) : SilSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    // FIXME:  representation for double negatives?
    val negative = negativeSuper ^ negativeSub
    val verbHead = vpChildren.head
    if (!verbHead.isVerbNode && !verbHead.isModal) {
      return SilUnrecognizedSentence(tree)
    }
    val tamTensed = extractTense(verbHead, tam)
    val (progressive, iVerb) = detectProgressive(vpChildren)
    if ((verbHead.isModal || progressive)) {
      val vSub = {
        if (progressive) {
          vpChildren(iVerb)
        } else {
          // FIXME we should be able to use something similar
          // to progressive here
          vpChildren.last
        }
      }
      if ((vpChildren.size >= 2) && vSub.isVerbNode) {
        val vpSub = {
          if (vSub.isVerbPhrase) {
            vSub
          } else {
            SptVP(vSub)
          }
        }
        val extraModifiers = expectVerbModifiers(vpChildren.tail).
          filterNot(_.syntaxTree == vSub)
        expectPredicateSentence(
          tree, np, vpSub, verbModifiers ++ extraModifiers,
          force,
          tamForAux(requireLeaf(verbHead.children)).withMood(tam.mood).
            withTense(tamTensed.tense),
          getVerbCount(verbHead),
          negative)
      } else {
        SilUnrecognizedSentence(tree)
      }
    } else if (verbHead.isRelationshipVerb) {
      val (maybeSpecifiedState, vpRemainder) =
        extractAdpositionalState(vpChildren)
      val (complement, specifiedState) = {
        if (vpRemainder.size > 1) {
          tupleN((vpRemainder.last, maybeSpecifiedState))
        } else {
          tupleN((vpChildren.last, SilNullState()))
        }
      }
      val extraModifiers = expectVerbModifiers(vpRemainder).filterNot(
        vm => Seq(verbHead, complement).contains(vm.syntaxTree))
      val (negativeComplement, predicate) = expectPredicate(
        tree, np, complement, specifiedState,
        relationshipVerb(verbHead), verbModifiers ++ extraModifiers)
      val polarity = !(negative ^ negativeComplement)
      rememberPredicateCount(predicate, verbHead, tam, auxCount)
      SilPredicateSentence(
        predicate, tamTensed.withPolarity(polarity), SilFormality(force))
    } else {
      val (negativeVerb, predicate) = analyzeActionPredicate(
        tree, np, vp, None, verbModifiers)
      assert(negativeVerb == negativeSub)
      val polarity = !negative
      rememberPredicateCount(
        predicate, verbHead, tam, auxCount)
      SilPredicateSentence(
        predicate,
        tamTensed.withPolarity(polarity),
        SilFormality(force))
    }
  }

  private def expectCommand(
    tree : SprSyntaxTree,
    vp : SprSyntaxTree, formality : SilFormality) : SilSentence =
  {
    val np = SptNP(SptPRP(makeLeaf(LEMMA_YOU)))
    val (negativeVerb, predicate) = analyzeActionPredicate(
      tree, np, vp, None, Seq.empty, true)
    if (negativeVerb) {
      return SilUnrecognizedSentence(tree)
    }
    SilPredicateSentence(
      predicate,
      SilTam.imperative,
      formality)
  }

  private def expectRelativeReference(
    syntaxTree : SprSyntaxTree,
    reference : SilReference,
    relativeTree : SprSyntaxTree) : SilReference =
  {
    relativeTree match {
      case SptSBAR(
        SptWHNP(SptWDT(_)),
        SptS(SptVP(verb, complement))
      ) if (verb.isBeingVerb) => {
        val state = expectComplementState(SptVP(complement))
        SilStateSpecifiedReference(reference, state)
      }
      case _ => {
        SilUnrecognizedReference(syntaxTree)
      }
    }
  }

  override def expectAdpositionalState(
    tree : SprSyntaxTree, extracted : Boolean)
    : SilState =
  {
    val seq = tree.children
    val adpTree = seq.head.unwrapPhrase
    if (seq.size == 2) {
      extractAdposition(adpTree) match {
        // "in the car"
        case Some(adposition) => {
          val valid = {
            if (extracted) {
              // FIXME ugh
              adposition match {
                case SilAdposition.OF | SilAdposition.GENITIVE_OF
                    | SilAdposition.TO => false
                case _ => true
              }
            } else {
              true
            }
          }
          if (valid) {
            SilAdpositionalState(adposition, expectReference(seq.last))
          } else {
            SilUnrecognizedState(tree)
          }
        }
        case _ => {
          if (seq.last.isAdpositionalPhrase) {
            // "south of the border"
            expectAdpositionalState(seq.last, false) match {
              case SilAdpositionalState(SilAdposition(word), ref) => {
                val unwrapped = adpTree.unwrapPhrase
                if ((unwrapped.children.size == 1) &&
                  unwrapped.children.head.isLeaf &&
                  !unwrapped.isInstanceOf[SptNNQ])
                {
                  SilAdpositionalState(
                    SilAdposition(
                      getWord(
                        requireLeaf(unwrapped.children)) +: word.decomposed),
                    ref)
                } else {
                  SilUnrecognizedState(tree)
                }
              }
              case _ => {
                SilUnrecognizedState(tree)
              }
            }
          } else {
            SilUnrecognizedState(tree)
          }
        }
      }
    } else if ((seq.size == 1) && seq.head.isNounPhrase) {
      SilAdpositionalState(
        SilAdposition.ADVERBIAL_TMP, expectReference(seq.head))
    } else {
      SilUnrecognizedState(tree)
    }
  }

  private def extractAdposition(preTerminal : SprSyntaxTree)
      : Option[SilAdposition] =
  {
    preTerminal match {
      case adp : SprSyntaxAdposition => {
        val leaf = adp.child
        if (isAdposition(getWord(leaf).inflected)) {
          Some(SilAdposition(getWord(adp.child)))
        } else {
          None
        }
      }
      case _ => preTerminal.firstChild.lemma match {
        case LEMMA_IN => Some(SilAdposition.IN)
        case LEMMA_INSIDE => Some(SilAdposition.INSIDE)
        case LEMMA_WITHIN => Some(SilAdposition.WITHIN)
        case LEMMA_OUTSIDE => Some(SilAdposition.OUTSIDE)
        case LEMMA_AT => Some(SilAdposition.AT)
        case LEMMA_WITH => Some(SilAdposition.WITH)
        case LEMMA_AMONG => Some(SilAdposition.AMONG)
        case LEMMA_AS => Some(SilAdposition.AS)
        case LEMMA_NEAR => Some(SilAdposition.NEAR)
        case LEMMA_NEARBY => Some(SilAdposition.NEARBY)
        case LEMMA_ON => Some(SilAdposition.ON)
        case LEMMA_ABOVE => Some(SilAdposition.ABOVE)
        case LEMMA_OVER => Some(SilAdposition.OVER)
        case LEMMA_BELOW => Some(SilAdposition.BELOW)
        case LEMMA_UNDER => Some(SilAdposition.UNDER)
        case LEMMA_BENEATH => Some(SilAdposition.BENEATH)
        case LEMMA_UNDERNEATH => Some(SilAdposition.UNDERNEATH)
        case LEMMA_BEHIND => Some(SilAdposition.BEHIND)
        case LEMMA_OF => Some(SilAdposition.OF)
        case LEMMA_TO => Some(SilAdposition.TO)
        case _ => None
      }
    }
  }

  private def analyzeActionPredicate(
    syntaxTree : SprSyntaxTree,
    np : SprSyntaxTree,
    vp : SprSyntaxTree,
    specifiedDirectObject : Option[SilReference],
    verbModifiers : Seq[SilExpectedVerbModifier],
    imperative : Boolean = false)
      : (Boolean, SilPredicate) =
  {
    val (negative, seq) = extractNegative(vp.children)
    // FIXME should support "there goes the mailman"?
    if (np.isExistential) {
      return tupleN((negative, SilUnrecognizedPredicate(syntaxTree)))
    }
    val subject = expectReference(np)
    if (seq.isEmpty) {
      return tupleN((negative, SilUnrecognizedPredicate(syntaxTree)))
    }
    val verbHead = seq.head
    val verb = verbHead match {
      case vb : SprSyntaxVerb if (
        !imperative || (!verbHead.isModal && !verbHead.isPossessionVerb)
      ) => {
        getTreeWord(vb)
      }
      case _ => {
        return tupleN((negative, SilUnrecognizedPredicate(syntaxTree)))
      }
    }
    val (directObject, extraModifiers) =
      expectVerbObjectsAndModifiers(seq.drop(1), specifiedDirectObject)
    val directObjects = Seq(directObject, specifiedDirectObject).flatten
    val adpositionObject = {
      if (directObjects.size == 2) {
        specifiedDirectObject
      } else {
        None
      }
    }
    val predicate = expectActionPredicate(
      syntaxTree,
      subject, verb,
      directObjects.headOption,
      adpositionObject,
      extraModifiers ++ verbModifiers)
    tupleN((negative, predicate))
  }

  private def expectVerbObjectsAndModifiers(
    seq : Seq[SprSyntaxTree],
    specifiedDirectObjectOrig : Option[SilReference]) =
  {
    val specifiedDirectObject = seq.lastOption match {
      case Some(SptPP(pt)) => {
        None
      }
      case _ => {
        specifiedDirectObjectOrig
      }
    }
    val objCandidates = seq.filter(_.isNounNode)
    val directCandidates =
      objCandidates.filter(_.containsIncomingDependency("dobj"))
    val directObjTree = {
      if (directCandidates.size == 1) {
        directCandidates.headOption
      } else {
        if (specifiedDirectObject.isEmpty) {
          objCandidates.lastOption
        } else {
          None
        }
      }
    }
    val minusDirect = objCandidates.filterNot(Some(_) == directObjTree)
    val indirectObjTree = minusDirect.find(
      _.containsIncomingDependency("iobj")) match
    {
      case Some(iobj) => {
        Some(iobj)
      }
      case _ => {
        minusDirect.headOption
      }
    }
    val objTrees = directObjTree.toSeq ++ indirectObjTree
    val indirectAdposition = indirectObjTree.map(indirectObj => {
      val modifier = SilAdpositionalVerbModifier(
        SilAdposition.TO,
        expectReference(indirectObj)
      )
      modifier.rememberSyntaxTree(indirectObj)
      modifier
    })
    val modifiers = expectVerbModifiers(seq).filterNot(
      vm => objTrees.contains(vm.syntaxTree))
    tupleN((directObjTree.map(expectReference),
      indirectAdposition.toSeq ++ modifiers))
  }

  override def expectVerbModifierPhrase(
    tree : SprSyntaxPhrase)
      : SilVerbModifier =
  {
    if (tree.children.size == 1) {
      expectVerbModifier(tree.firstChild)
    } else {
      val qualified = tree.firstChild match {
        case s : SprSyntaxSimpleAdverb => {
          // FIXME full list of qualifying adverbs
          s.child.lemma match {
            case LEMMA_NO | "very" => true
            case _ => false
          }
        }
        case _ => false
      }
      if (!qualified) {
        SilUnrecognizedVerbModifier(tree)
      } else {
        val words = tree.children.flatMap(_ match {
          case adverb : SprSyntaxSimpleAdverb => {
            Seq(getWord(adverb.child))
          }
          case particle : SptRP => {
            Seq(getWord(particle.child))
          }
          case rbc : SptRBC => {
            getCompoundWord(rbc).components
          }
          case _ => return expectAdpositionalVerbModifier(tree)
        })
        SilBasicVerbModifier(SilCompoundWord(words))
      }
    }
  }

  override def expectAdpositionalVerbModifier(
    tree : SprSyntaxTree) =
  {
    tree match {
      case SptPP(pt : SprSyntaxPreTerminal) => {
        if (isAdposition(pt.child.lemma)) {
          SilDanglingVerbModifier(
            SilAdposition(getWord(pt.child)))
        } else {
          SilUnrecognizedVerbModifier(tree)
        }
      }
      case _ => {
        expectAdpositionalState(tree, false) match {
          case SilAdpositionalState(adposition, objRef) => {
            SilAdpositionalVerbModifier(adposition, objRef)
          }
          case _ => {
            SilUnrecognizedVerbModifier(tree)
          }
        }
      }
    }
  }

  private def expectPredicate(
    syntaxTree : SprSyntaxTree,
    np : SprSyntaxTree,
    complement : SprSyntaxTree,
    specifiedState : SilState,
    verb : SilWord,
    verbModifiers : Seq[SilExpectedVerbModifier])
      : (Boolean, SilPredicate) =
  {
    val (negative, seq) = {
      if (complement.isPreTerminal) {
        tupleN((false, Seq(complement)))
      } else {
        extractNegative(complement.children)
      }
    }
    if (np.isExistential) {
      if (!isBeingLemma(verb)) {
        return tupleN((false, SilUnrecognizedPredicate(syntaxTree)))
      }
      val subject = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          specifyReference(expectReference(seq), specifiedState)
        }
        case (determiner, separator, split) => {
          val conjunctiveRef = SilConjunctiveReference(
            determiner,
            split.map(x => specifyReference(
              expectReference(x), specifiedState)),
            separator)
          rememberSyntheticNP(conjunctiveRef, seq)
          conjunctiveRef
        }
      }
      tupleN((negative, expectStatePredicate(
        syntaxTree, subject, verb,
        expectExistenceState(np), SilNullState(),
        verbModifiers)))
    } else if (complement.isExistential) {
      if (!isBeingLemma(verb)) {
        return tupleN((false, SilUnrecognizedPredicate(syntaxTree)))
      }
      tupleN((negative, expectStatePredicate(
        syntaxTree,
        specifyReference(
          expectReference(np), specifiedState),
        verb,
        expectExistenceState(complement),
        SilNullState(),
        verbModifiers)))
    } else if (complement.isNounNode) {
      // FIXME this is quite arbitrary
      val (subjectRef, complementRef) = {
        if (isBeingLemma(verb)) {
          tupleN((specifyReference(expectReference(np), specifiedState),
            expectReference(seq)))
        } else {
          tupleN((expectReference(np),
            specifyReference(expectReference(seq), specifiedState)))
        }
      }
      val relationshipPredicate = expectRelationshipPredicate(
        syntaxTree,
        subjectRef,
        complementRef,
        verb,
        verbModifiers)
      tupleN((negative, relationshipPredicate))
    } else {
      if (!isBeingLemma(verb)) {
        if (enforceTransitive) {
          return tupleN((false, SilUnrecognizedPredicate(syntaxTree)))
        } else {
          return tupleN((negative, expectStatePredicate(
            syntaxTree, expectReference(np),
            STATE_PREDEF_BE.toVerb,
            SilExistenceState(),
            specifiedState,
            verbModifiers)))
        }
      }
      val (state, extraModifiers, refinedState) = {
        complement match {
          // FIXME there are all kinds of other verb modifiers that need to
          // be handled, and that goes for the other predicate types above
          // too!
          case advp @ (_ : SptTMOD | _ : SptADVP) if (
            specifiedState != SilNullState()
          ) => {
            tupleN((specifiedState, Seq(advp), SilNullState()))
          }
          case _ => {
            val seqState = splitCoordinatingConjunction(seq) match {
              case (DETERMINER_UNSPECIFIED, _, _) => {
                val recomposed = {
                  if (complement.isAdjective) {
                    SptADJP(complement)
                  } else {
                    SprSyntaxRewriter.recompose(complement, seq)
                  }
                }
                expectComplementState(recomposed)
              }
              case (determiner, separator, split) => {
                val conjunctiveState = SilConjunctiveState(
                  determiner,
                  split.map(
                    subseq => expectComplementState(
                      SprSyntaxRewriter.recompose(complement, subseq))),
                  separator)
                rememberSyntheticADJP(conjunctiveState, seq)
                conjunctiveState
              }
            }
            tupleN((seqState, Seq.empty, specifiedState))
          }
        }
      }
      tupleN((negative, expectStatePredicate(
        syntaxTree, expectReference(np), verb, state, refinedState,
        verbModifiers ++ expectVerbModifiers(extraModifiers))))
    }
  }

  override def expectPropertyComplementState(
    tree : SprSyntaxTree) : SilState =
  {
    val seq = tree.children
    if (isStrict) {
      tree match {
        case _ : SptVP => SilUnrecognizedState(tree)
        case _ : SptADVP => SilUnrecognizedState(tree)
        case SptADJP(_ : SptRB) => SilUnrecognizedState(tree)
        case SptADJP(nn) if (nn.isNounNode) => SilUnrecognizedState(tree)
        case ap : SptADJP if (
          ap.children.exists(
            child => child.isInstanceOf[SptPP] && (child.numChildren < 2))
        ) =>
          SilUnrecognizedState(tree)
        case _ => {
          splitCoordinatingConjunction(seq) match {
            case (DETERMINER_UNSPECIFIED, _, _) => {
              analyzePropertyComplementState(tree, seq)
            }
            case (determiner, separator, split) => {
              val state = SilConjunctiveState(
                determiner,
                split.map(x => {
                  val sub = {
                    if (isSinglePhrase(x)) {
                      x.head.children
                    } else {
                      x
                    }
                  }
                  analyzePropertyComplementState(tree, sub)
                }),
                separator)
              state
            }
          }
        }
      }
    } else {
      analyzePropertyComplementState(tree, seq)
    }
  }

  private def analyzePropertyComplementState(
    tree : SprSyntaxTree,
    seq : Seq[SprSyntaxTree]) : SilState =
  {
    def state = expectPropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      // FIXME generalize this, but needs semantic analysis, e.g.
      // "happy in the bathtub" is different from "west of the bathtub"
      val compoundAdposition = seq.last match {
        case SptPP(
          SptIN(adp),
          _
        ) if (adp.lemma == LEMMA_OF) => {
          Some(adp)
        }
        case _ => None
      }
      if ((seq.size == 2) && !compoundAdposition.isEmpty) {
        expectAdpositionalState(tree, false)
      } else {
        SilConjunctiveState(
          DETERMINER_UNSPECIFIED,
          Seq(state) ++ seq.tail.map(
            component => expectComplementState(component)))
      }
    }
  }

  private def isImperative(children : Seq[SprSyntaxTree]) =
  {
    (children.size == 1) && children.head.isVerbPhrase
  }

  private def extractAntecedent(children : Seq[SprSyntaxTree])
      : Option[(SilWord, SptS)] =
  {
    children.headOption match {
      case Some(SptSBAR(SptIN(leaf), antecedent : SptS)) => {
        leaf.lemma match {
          case LEMMA_IF | LEMMA_WHEN | LEMMA_WHENEVER |
              LEMMA_BEFORE | LEMMA_AFTER =>
            Some(tupleN((getWord(leaf), antecedent)))
          case _ =>
            None
        }
      }
      case Some(SptSBAR(SptWHADVP(SptWRB(leaf)), antecedent : SptS)) => {
        leaf.lemma match {
          case LEMMA_WHEN | LEMMA_WHENEVER =>
            Some(tupleN((getWord(leaf), antecedent)))
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def isCoordinatingDeterminer(
    syntaxTree : SprSyntaxTree, determiner : SilDeterminer) : Boolean =
  {
    syntaxTree.unwrapPhrase match {
      case preTerminal : SprSyntaxPreTerminal => {
        preTerminal match {
          case (_ : SptDT | _ : SptCC | _ : SprSyntaxAdverb) => {
            preTerminal.child.lemma match {
              case LEMMA_BOTH => (determiner == DETERMINER_ALL)
              case LEMMA_EITHER => (determiner == DETERMINER_ANY)
              case LEMMA_NEITHER => (determiner == DETERMINER_NONE)
              case _ => false
            }
          }
          case _ => false
        }
      }
      case _ => false
    }
  }

  private def tamForAux(leaf : SprSyntaxLeaf) : SilTam =
  {
    val tam = SilTam.indicative
    leaf.lemma match {
      case LEMMA_MUST => tam.withModality(MODAL_MUST)
      case LEMMA_MAY => tam.withModality(MODAL_MAY)
      case LEMMA_COULD | LEMMA_CAN => tam.withModality(MODAL_CAPABLE)
      case LEMMA_MIGHT => tam.withModality(MODAL_POSSIBLE)
      case LEMMA_SHOULD => tam.withModality(MODAL_SHOULD)
      case LEMMA_DO => tam.withModality(MODAL_EMPHATIC)
      case LEMMA_BE => tam.progressive
      case _ => tam
    }
  }

  private def relationshipVerb(
    verbHead : SprSyntaxTree) : SilWord =
  {
    getWord(verbHead.asInstanceOf[SprSyntaxPreTerminal].child)
  }

  private def maybeQuestionFor(
    seq : Seq[SprSyntaxTree])
      : Option[(SilQuestion, Option[SprSyntaxTree], Seq[SprSyntaxTree])] =
  {
    val tree = seq.head
    tree match {
      case SptWHADJP(how, many) => {
        if (how.hasTerminalLemma(LEMMA_HOW) &&
          many.hasTerminalLemma(LEMMA_MANY))
        {
          // FIXME for HOW_MANY, force the corresponding noun reference
          // to plural
          Some((QUESTION_HOW_MANY, None, seq.tail))
        } else {
          None
        }
      }
      case SptWRB(where) => {
        where.lemma match {
          case LEMMA_WHERE =>
            Some((QUESTION_WHERE, None, tree.children))
          case _ => None
        }
      }
      case SptWDT(wdt) => {
        wdt.lemma match {
          case LEMMA_WHICH | LEMMA_WHAT =>
            Some((QUESTION_WHICH, None, seq.tail))
          case _ => None
        }
      }
      case SptWP_POS(wpp) => {
        Some((QUESTION_WHO, None,
          Seq(SptNP(
            (SptNP(SptNN(makeLeaf(LEMMA_WHO)), SptPOS(makeLeaf("'s"))) +:
              seq.tail):_*))))
      }
      case SptWP(wp) => {
        wp.lemma match {
          case LEMMA_WHO | LEMMA_WHOM =>
            Some((QUESTION_WHO, None, tree.children))
          case LEMMA_WHAT =>
            Some((QUESTION_WHAT, None, tree.children))
          case _ => None
        }
      }
      case adp : SprSyntaxAdposition => {
        if (seq.size == 2) {
          val whnp = seq.last
          whnp match {
            case _ : SptWHNP => {
              maybeQuestionFor(whnp.children) match {
                case Some((question, None, questionChildren)) => {
                  Some((question, Some(adp), questionChildren))
                }
                case _ => None
              }
            }
            case _ => None
          }
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private def determinerFor(leaf : SprSyntaxLeaf) : SilDeterminer =
  {
    maybeDeterminerFor(leaf.lemma).getOrElse(DETERMINER_ANY)
  }

  private def extractAdpositionalState(seq : Seq[SprSyntaxTree])
      : (SilState, Seq[SprSyntaxTree])=
  {
    // skip first child since that should always be treated
    // as verb modifier, not state; FIXME:  need better
    // logic for this
    val i = seq.indexWhere(_.isCompoundAdpositionalPhrase, 1)
    if (i == -1) {
      val last2 = seq.takeRight(2)
      last2 match {
        case Seq(
          advp : SptADVP, np : SptNP
        ) if (extractAdposition(advp.firstChild).nonEmpty) => {
          val rewrite = SprSyntaxRewriter.recompose(
            advp,
            Seq(advp.firstChild, np))
          tupleN((SilNullState(), seq.dropRight(2) :+ rewrite))
        }
        case _ => {
          tupleN((SilNullState(), seq))
        }
      }
    } else {
      tupleN((SilExpectedAdpositionalState(seq(i), true),
        seq.take(i) ++ seq.drop(i + 1)))
    }
  }

  private def extractAux(
    seq : Seq[SprSyntaxTree])
      : (SilTam, Seq[SprSyntaxTree], SilCount) =
  {
    // FIXME for "does", we need to be careful to make sure it's
    // acting as an auxiliary, e.g. "Luke does know" but not
    // "Luke does the dishes"
    val iModal = seq.indexWhere(_.unwrapPhrase.isModal)

    val (nonAux, iAux) = {
      if (iModal < 0) {
        val (progressive, iVerb) = detectProgressive(seq)
        if (!progressive) {
          return tupleN((SilTam.indicative, seq, COUNT_SINGULAR))
        } else {
          val iBeing = seq.indexWhere(_.isVerbNode)
          val being = seq(iBeing).unwrapPhrase
          assert(being.isRelationshipVerb)
          val nonBeing = seq.patch(iBeing, Seq.empty, 1)
          tupleN((nonBeing, iBeing))
        }
      } else {
        val nonModal = seq.patch(iModal, Seq.empty, 1)
        tupleN((nonModal, iModal))
      }
    }

    val remainder = {
      if (isSinglePhrase(nonAux) && nonAux.head.isVerbPhrase) {
        nonAux.head.children
      } else {
        nonAux
      }
    }
    val aux = seq(iAux).unwrapPhrase
    val leaf = requireLeaf(aux.children)
    val tam = tamForAux(leaf)
    val tamTensed = extractTense(aux, tam)
    tupleN((tamTensed, remainder, getVerbCount(aux)))
  }

  private def extractNegative(
    seq : Seq[SprSyntaxTree])
      : (Boolean, Seq[SprSyntaxTree]) =
  {
    // FIXME:  don't reduce to empty seq
    val pos = seq.indexWhere(isNegative)
    if (pos == -1) {
      tupleN((false, seq))
    } else {
      tupleN((true, seq.patch(pos, Seq.empty, 1)))
    }
  }

  private def isNegative(tree : SprSyntaxTree) : Boolean =
  {
    // FIXME I just can't even
    if (tree.unwrapPhrase.hasTerminalLemma(LEMMA_NOT)) {
      true
    } else if (tree.isAdverbPhrase && (tree.children.size > 1)) {
      tree.children.exists(c =>
        c.hasTerminalLemma(LEMMA_NOT) || c.hasTerminalLemma(LEMMA_NO))
    } else {
      false
    }
  }

  private def splitCoordinatingConjunction(
    components : Seq[SprSyntaxTree])
      : (SilDeterminer, SilSeparator, Seq[Seq[SprSyntaxTree]]) =
  {
    if (isSinglePhrase(components)) {
      return splitCoordinatingConjunction(components.head.children)
    }
    val pos = components.indexWhere(t => t.isCoordinatingConjunction, 1)
    if (pos == -1) {
      val (commaSplit, commaSeparator) = splitCommas(components)
      commaSeparator match {
        case SEPARATOR_COMMA | SEPARATOR_OXFORD_COMMA => {
          splitCoordinatingConjunction(commaSplit.last) match {
            case (DETERMINER_UNSPECIFIED, _, _) => {
              tupleN((DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, Seq.empty))
            }
            case (determiner, separator, subSplit) => {
              // FIXME:  deal with coordinating determiner in
              // first sub-phrase
              val seq = commaSplit.dropRight(1) ++ subSplit
              tupleN((determiner, commaSeparator, seq))
            }
          }
        }
        case _ => {
          tupleN((DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, Seq.empty))
        }
      }
    } else {
      var determiner = components(pos) match {
        case SptCC(leaf) => {
          determinerFor(leaf)
        }
        case _ => throw new AssertionError("CC required")
      }
      val prefix = {
        if (isCoordinatingDeterminer(components.head, determiner)) {
          if (determiner == DETERMINER_ANY) {
            determiner = DETERMINER_UNIQUE
          }
          components.take(pos).drop(1)
        } else {
          components.take(pos)
        }
      }
      val (commaSplit, commaSeparator) = splitCommas(prefix)
      val suffix = components.drop(pos + 1)
      splitCoordinatingConjunction(suffix) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          tupleN((determiner, commaSeparator, commaSplit ++ Seq(suffix)))
        }
        case (subDeterminer, _, subSplit) => {
          val seq = {
            if (determiner == subDeterminer) {
              commaSplit ++ subSplit
            } else {
              commaSplit ++ Seq(suffix)
            }
          }
          tupleN((determiner, commaSeparator, seq.filterNot(_.isEmpty)))
        }
      }
    }
  }

  override protected def getVerbCount(verb : SprSyntaxTree) : SilCount =
  {
    verb match {
      case _ : SptVBP => {
        if (verb.firstChild.token == "am") {
          COUNT_SINGULAR
        } else {
          COUNT_PLURAL
        }
      }
      case _ => COUNT_SINGULAR
    }
  }

  override def analyzePronounReference(
    leaf : SprSyntaxLeaf)
      : SilPronounReference =
  {
    val lemma = leaf.lemma
    assert(isPronounWord(lemma), lemma)
    val person = lemma match {
      case LEMMA_I | LEMMA_ME | LEMMA_WE | LEMMA_MY |
          LEMMA_OUR | LEMMA_MINE | LEMMA_OURS => PERSON_FIRST
      case LEMMA_YOU | LEMMA_YOUR | LEMMA_YOURS => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case LEMMA_WE | LEMMA_US | LEMMA_THEY | LEMMA_THESE | LEMMA_THOSE |
          LEMMA_OUR | LEMMA_THEM | LEMMA_THEIR => COUNT_PLURAL
      case _ => COUNT_SINGULAR
    }
    val gender = lemma match {
      case LEMMA_HE | LEMMA_HIM | LEMMA_HIS => GENDER_M
      case LEMMA_SHE | LEMMA_HER | LEMMA_HERS => GENDER_F
      case _ => GENDER_N
    }
    val distance = lemma match {
      case LEMMA_THIS | LEMMA_THESE => DISTANCE_HERE
      case LEMMA_THAT | LEMMA_THOSE => DISTANCE_THERE
      case _ => DISTANCE_UNSPECIFIED
    }
    SilPronounReference(person, gender, count, distance)
  }

  override def isProhibitedPropertyState(
    preTerminal : SprSyntaxPreTerminal) : Boolean =
  {
    val lemma = getWord(preTerminal.child).lemma
    (
      lemma == LEMMA_BE
    ) || (
      isStrict && preTerminal.isAdposition &&
        (isAdposition(lemma) ||
          isSubordinatingConjunction(lemma))
    )
  }

  override def expectBasicVerbModifier(
    preTerminal : SprSyntaxPreTerminal)
      : SilVerbModifier =
  {
    SilBasicVerbModifier(getWord(preTerminal.child))
  }
}
