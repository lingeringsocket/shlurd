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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import SprUtils._

abstract class SnlCorenlpSyntaxAnalyzer(
  context : SprContext,
  guessedQuestion : Boolean,
  strictness : SprStrictness,
  enforceTransitive : Boolean
) extends SnlSyntaxAnalyzer(
  context, guessedQuestion, strictness, enforceTransitive)
{
  protected def isImperative(children : Seq[SprSyntaxTree]) : Boolean

  override protected def analyzeSentenceChildren(
    tree : SprSyntaxTree, children : Seq[SprSyntaxTree],
    mood : SilMood, force : SilForce) =
  {
    val isQuestion = (mood == MOOD_INTERROGATIVE)
    if (isImperative(children)) {
      expectCommand(tree, children.head, SilFormality(force))
    } else if (children.size >= 2) {
      val tail = children.takeRight(2)
      val np = tail.head
      val vp = tail.last
      val verbModifiers = children.dropRight(2)
      expectFullPredicate(tree, np, vp, verbModifiers, isQuestion, force)
    } else if ((children.size == 1) && tongue.allowElidedSubject) {
      val np = SptNP(SptNNE())
      val vp = children.head
      expectFullPredicate(tree, np, vp, Seq.empty, isQuestion, force)
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private def expectFullPredicate(
    tree : SprSyntaxTree,
    np : SprSyntaxTree,
    vp : SprSyntaxTree,
    verbModifiers : Seq[SprSyntaxTree],
    isQuestion : Boolean,
    force : SilForce) =
  {
    if (np.isNounOrPronoun && vp.isVerbPhrase &&
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
        force, tam,
        SilVerbInflection(PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR), false)
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  override def analyzeSQ(
    tree : SprSyntaxTree, forceSQ : Boolean)
      : SilSentence =
  {
    val punctless = stripPauses(tree)
    val (specifiedState, children) = {
      val unwrapped = {
        if (forceSQ && isSinglePhrase(punctless) &&
          !punctless.head.isVerbPhrase)
        {
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
    val seq = {
      if (tongue.allowElidedSubject && (children.size == 1)) {
        SptNP(SptNNE()) +: children
      } else {
        children
      }
    }
    if (seq.size > 1) {
      analyzeSubQueryChildren(tree, seq, specifiedState) match {
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
    if (seq(iFirstVerb).unwrapPhrase.isProgressiveAux &&
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
    extraModifiers : Seq[SipExpectedVerbModifier] = Seq.empty,
    question : Option[SilQuestion] = None)
      : Option[(SilPredicate, SilTam)] =
  {
    val (tam, auxless, auxInflection) = extractAux(children)
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
          sub.last, combineNegatives(negativeSub, negativeSuper),
          expectVerbModifiers(seq).patch(iNoun, Seq.empty, expectedSize),
          (sub.size > 2)))
      }
    }
    if (np.children.isEmpty) {
      return None
    }
    val tamTensed = extractTense(verbHead, tam)
    val tamMoody = applyInterrogative(tamTensed)
    if ((verbHead.isRelationshipVerb || verbHead.isBeingVerb)
      && specifiedDirectObject.isEmpty && !verbHead.isImpersonalVerb)
    {
      if (rhsLoss) {
        return None
      }
      val (negativeSub, predicate) =
        expectPredicate(tree, np, rhs, specifiedState,
          relationshipVerb(verbHead), verbModifiers ++ extraModifiers, question)
      val polarity = !combineNegatives(negative, negativeSub)
      rememberPredicateInflection(predicate, verbHead, tam, auxInflection)
      Some((predicate,
        tamMoody.withPolarity(polarity)))
    } else {
      val (negativeSub, predicate) = analyzeActionPredicate(
        tree, np, vp, specifiedDirectObject, extraModifiers)
      val polarity = !combineNegatives(negative, negativeSub)
      rememberPredicateInflection(predicate, verbHead, tam, auxInflection)
      Some((predicate,
        tamMoody.withPolarity(polarity)))
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
    val seq = unwrapQuery(whpc)
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
    val np = nounPhraseForQuestion(question, questionChildren)
    val (progressive, iVerb) = detectProgressive(secondSub)
    assert(iVerb >= 0, secondSub)
    if ((verbHead.isBeingVerb || verbHead.isRelationshipVerb) && !progressive) {
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
              if (!tongue.isAdposition(pt.child.lemma) ||
                (complement.size < 3))
              {
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
        modifiers,
        Some(question))
      rememberPredicateInflection(predicate, verbHead)
      val tam = SilTam.interrogative.
        withPolarity(!combineNegatives(negativeSuper, negativeSub))
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
          if (adpositionOpt.nonEmpty) {
            return SilUnrecognizedSentence(tree)
          }
          tupleN((None, INFLECT_NOMINATIVE,
            Seq(np) ++ secondUnwrapped, Seq.empty))
        }
      }
      analyzeSubQueryChildren(
        tree, sqChildren, specifiedState,
        specifiedDirectObject, extraModifiers, Some(question)) match
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

  private def expectPredicateSentence(
    tree : SprSyntaxTree,
    np : SprSyntaxTree, vp : SprSyntaxTree,
    verbModifiers : Seq[SipExpectedVerbModifier],
    force : SilForce, tam : SilTam, auxInflection : SilVerbInflection,
    negativeSuper : Boolean) : SilSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    val negative = combineNegatives(negativeSuper, negativeSub)
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
        val candidateModifiers = expectVerbModifiers(vpChildren.tail).
          filterNot(_.syntaxTree == vSub)
        val (adpositions, extraModifiers) =
          candidateModifiers.partition(_.syntaxTree.isAdposition)
        val verbHeadLeaf = requireLeaf(verbHead.children)
        if (validateAuxAdposition(
          verbHeadLeaf.lemma, adpositions.map(_.syntaxTree)))
        {
          return expectPredicateSentence(
            tree, np, vpSub, verbModifiers ++ extraModifiers,
            force,
            tamForAux(verbHeadLeaf, Seq(vpSub)).withMood(tam.mood).
              withTense(tamTensed.tense),
            getVerbInflection(verbHead),
            negative)
        }
      }
    }
    if ((verbHead.isBeingVerb || verbHead.isPossessionVerb) &&
      !verbHead.isImpersonalVerb)
    {
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
        relationshipVerb(verbHead), verbModifiers ++ extraModifiers,
        Some(QUESTION_HOW_MANY))
      val polarity = !combineNegatives(negative, negativeComplement)
      rememberPredicateInflection(predicate, verbHead, tam, auxInflection)
      SilPredicateSentence(
        predicate, tamTensed.withPolarity(polarity), SilFormality(force))
    } else {
      val (negativeVerb, predicate) = analyzeActionPredicate(
        tree, np, vp, None, verbModifiers)
      assert(negativeVerb == negativeSub)
      val polarity = !negative
      rememberPredicateInflection(
        predicate, verbHead, tam, auxInflection)
      SilPredicateSentence(
        predicate,
        tamTensed.withPolarity(polarity),
        SilFormality(force))
    }
  }

  private def validateAuxAdposition(
    auxLemma : String,
    adpositions : Seq[SprSyntaxTree]) : Boolean =
  {
    if (adpositions.size > 1) {
      false
    } else {
      val adpositionLemma = adpositions.
        headOption.map(_.firstChild.lemma).getOrElse("")
      tongue.adpositionForAux(auxLemma) == adpositionLemma
    }
  }

  private def expectPredicate(
    syntaxTree : SprSyntaxTree,
    np : SprSyntaxTree,
    complement : SprSyntaxTree,
    specifiedState : SilState,
    verb : SilWord,
    verbModifiers : Seq[SipExpectedVerbModifier],
    question : Option[SilQuestion])
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
      if (!tongue.isBeingLemma(verb)) {
        return tupleN((false, SilUnrecognizedPredicate(syntaxTree)))
      }
      val subject = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_ABSENT, _, _) => {
          specifyReference(expectReference(seq), specifiedState)
        }
        case (determiner, separator, split) => {
          val conjunctiveRef = annotator.conjunctiveRef(
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
    } else if (complement.isExistsVerb ||
      // FIXME this is somewhat arbitrary
      ((question == Some(QUESTION_HOW_MANY)) && complement.isExistential)
    ) {
      if (!tongue.isBeingLemma(verb)) {
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
    } else if (complement.isNounOrPronoun) {
      // FIXME this is quite arbitrary
      val (subjectRef, complementRef) = {
        if (tongue.isBeingLemma(verb)) {
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
      if (!tongue.isBeingLemma(verb)) {
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
          case vb : SprSyntaxSimpleVerb if (
            tongue.allowElidedSubject && (getWord(vb.child) == verb)
          ) => {
            val elided = SptNP(SptNNE())
            return expectPredicate(
              syntaxTree, np, elided,
              specifiedState, verb, verbModifiers, question)
          }
          case _ => {
            val seqState = splitCoordinatingConjunction(seq) match {
              case (DETERMINER_ABSENT, _, _) => {
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
      tupleN((SipExpectedAdpositionalState(seq(i), true),
        seq.take(i) ++ seq.drop(i + 1)))
    }
  }

  private def extractAux(
    seq : Seq[SprSyntaxTree])
      : (SilTam, Seq[SprSyntaxTree], SilVerbInflection) =
  {
    // FIXME for "does", we need to be careful to make sure it's
    // acting as an auxiliary, e.g. "Luke does know" but not
    // "Luke does the dishes"
    val iModal = seq.indexWhere(_.unwrapPhrase.isModal)

    val (nonAux, iAux) = {
      if (iModal < 0) {
        val (progressive, iVerb) = detectProgressive(seq)
        if (!progressive) {
          return tupleN((SilTam.indicative, seq,
            SilVerbInflection(PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR)))
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
    val tam = tamForAux(leaf, nonAux)
    val tamTensed = extractTense(aux, tam)
    tupleN((tamTensed, remainder, getVerbInflection(aux)))
  }

  private def analyzeActionPredicate(
    syntaxTree : SprSyntaxTree,
    np : SprSyntaxTree,
    vp : SprSyntaxTree,
    specifiedDirectObject : Option[SilReference],
    verbModifiers : Seq[SipExpectedVerbModifier],
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
    val objCandidates = seq.filter(_.isNounOrPronoun)
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
        SprPredefAdposition(PD_DATIVE_TO),
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
}
