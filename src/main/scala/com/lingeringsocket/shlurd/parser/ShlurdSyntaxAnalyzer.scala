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

import ShlurdPennTreebankLabels._
import ShlurdEnglishLemmas._
import ShlurdParseUtils._

class ShlurdSyntaxAnalyzer(guessedQuestion : Boolean)
{
  private[parser] def analyzeSentence(tree : SptS)
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
    val children =
      truncatePunctuation(
        tree, Seq(LABEL_DOT, LABEL_EXCLAMATION_MARK, LABEL_QUESTION_MARK))
    val antecedent = extractAntecedent(children)
    if (!antecedent.isEmpty) {
      expectConditionalSentence(
        tree,
        antecedent.get,
        SptS(children.tail.filterNot(c => c.isComma || c.isThen):_*),
        SilFormality(force))
    } else  if (isImperative(children)) {
      expectCommand(tree, children.head, SilFormality(force))
    } else if (children.size >= 2) {
      val tail = children.takeRight(2)
      val np = tail.head
      val vp = tail.last
      val verbModifiers = children.dropRight(2)
      if (np.isNounPhrase && vp.isVerbPhrase &&
        verbModifiers.forall(_.isAdverbialPhrase))
      {
        expectPredicateSentence(
          tree, np, vp, verbModifiers,
          isQuestion, force, MODAL_NEUTRAL, COUNT_SINGULAR, false)
      } else {
        SilUnrecognizedSentence(tree)
      }
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private[parser] def analyzeSQ(tree : ShlurdSyntaxTree, forceSQ : Boolean)
      : SilSentence =
  {
    val punctless = truncatePunctuation(tree, Seq(LABEL_QUESTION_MARK))
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
        (SilNullState(), unwrapped)
      } else {
        (s, c)
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
    seq : Seq[ShlurdSyntaxTree]) : (Boolean, Int) =
  {
    val iFirstVerb = seq.indexWhere(_.isVerbNode)
    if (iFirstVerb < 0) {
      return (false, -1)
    }
    val iNextVerb = seq.indexWhere(_.isVerbNode, iFirstVerb + 1)
    if (iNextVerb < 0) {
      return (false, iFirstVerb)
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
      nextVerb.unwrapPhrase.isGerund)
    {
      (true, iNextVerb)
    } else {
      (false, iFirstVerb)
    }
  }

  private def analyzeSubQueryChildren(
    tree : ShlurdSyntaxTree,
    children : Seq[ShlurdSyntaxTree],
    specifiedState : SilState,
    specifiedDirectObject : Option[SilReference] = None)
      : Option[(SilPredicate, SilTam)] =
  {
    val (modality, modeless, modalCount) = extractModality(children)
    val (negativeSuper, seq) = extractNegative(modeless)
    val iVerb = seq.indexWhere(_.isVerbNode)
    if (iVerb < 0) {
      return None
    }
    val expectedSize = modality match {
      case MODAL_NEUTRAL => 3
      case _ => 2
    }
    if (seq.size < expectedSize) {
      return None
    }

    val (verbHead, np, vp, rhs, negative, verbModifiers) = {
      if (seq.head.unwrapPhrase.isRelationshipVerb) {
        // "is Larry smart?"
        if (seq.size < (iVerb + expectedSize)) {
          return None
        }
        val fromVerbSlice = seq.slice(iVerb, iVerb + expectedSize)
        val vp = SptVP(fromVerbSlice(0), fromVerbSlice(2))
        (fromVerbSlice(0), fromVerbSlice(1), vp,
          fromVerbSlice(2), negativeSuper,
          seq.patch(iVerb, Seq.empty, expectedSize))
      } else {
        // "(can) Larry [be [smart]]?"
        assert(iVerb > 0)
        val iNoun = iVerb - 1
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
        (sub.head, fromNounSlice(0), vp,
          sub.last, (negativeSub ^ negativeSuper),
          seq.patch(iNoun, Seq.empty, expectedSize))
      }
    }

    if (verbHead.isRelationshipVerb) {
      assert(specifiedDirectObject.isEmpty)
      val (negativeSub, predicate) =
        expectPredicate(tree, np, rhs, specifiedState,
          relationshipFor(verbHead), verbModifiers)
      val positive = !(negative ^ negativeSub)
      rememberPredicateCount(predicate, verbHead, modality, modalCount)
      Some((predicate, SilTam.interrogative.withPositivity(positive).
        withModality(modality)))
    } else {
      val (negativeSub, predicate) = analyzeActionPredicate(
        tree, np, vp, specifiedDirectObject, verbModifiers)
      val positive = !(negative ^ negativeSub)
      rememberPredicateCount(predicate, verbHead, modality, modalCount)
      Some((predicate, SilTam.interrogative.withPositivity(positive).
        withModality(modality)))
    }
  }

  private[parser] def analyzeSBARQ(tree : SptSBARQ)
      : SilSentence =
  {
    val children = truncatePunctuation(tree, Seq(LABEL_QUESTION_MARK))
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
    val question = maybeQuestionFor(seq.head) match {
      case Some(q) => q
      case _ => return SilUnrecognizedSentence(tree)
    }
    val verbHead = secondSub.head
    if ((question == QUESTION_WHERE) && !verbHead.isBeingVerb) {
      return SilUnrecognizedSentence(tree)
    }
    // FIXME for QUESTION_WHERE, it shouldn't be a noun phrase at all;
    // it should be either a state or verb modifier
    val np = question match {
      // FIXME for QUESTION_WHAT, there are two flavors (plain "what
      // do you want?" and also "what beer is most delicious?")
      case QUESTION_WHO | QUESTION_WHERE  | QUESTION_WHAT => {
        SptNP(SptNN(requireLeaf(seq.head.children)))
      }
      case _ => {
        // FIXME likewise, QUESTION_WHICH has two flavors "which do you want?"
        // and "which flavor do you want?"
        SptNP(seq.tail:_*)
      }
    }
    val (progressive, iVerb) = detectProgressive(secondSub)
    assert(iVerb >= 0)
    if (verbHead.isRelationshipVerb && !progressive) {
      val complement = secondSub.tail
      val (combinedState, complementRemainder) = {
        if (specifiedState == SilNullState()) {
          val (s, r) = extractAdpositionalState(complement)
          if (r.isEmpty) {
            (specifiedState, complement)
          } else {
            (s, r)
          }
        } else {
          (specifiedState, complement)
        }
      }
      val recomposedComplement = {
        if (complement.isEmpty) {
          verbHead
        } else {
          ShlurdSyntaxRewrite.recompose(
            complement.head, complementRemainder)
        }
      }
      val (negativeSub, predicate) = expectPredicate(
        tree,
        np,
        recomposedComplement,
        combinedState,
        relationshipFor(verbHead))
      rememberPredicateCount(predicate, verbHead)
      SilPredicateQuery(
        predicate, question, INFLECT_NOMINATIVE,
        SilTam.interrogative.withPositivity(!(negativeSuper ^ negativeSub)))
    } else {
      // FIXME support dative and adpositional objects too
      val (specifiedDirectObject, answerInflection, sqChildren) = {
        if (verbHead.isModal || (progressive && (iVerb > 1))) {
          (Some(expectReference(np)), INFLECT_ACCUSATIVE, secondUnwrapped)
        } else {
          (None, INFLECT_NOMINATIVE, Seq(np) ++ secondUnwrapped)
        }
      }
      analyzeSubQueryChildren(
        tree, sqChildren, specifiedState, specifiedDirectObject) match
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

  private[parser] def analyzeNounPhrase(
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
    if (seq.head.lastChild.isPossessive) {
      return SilGenitiveReference(
        expectReference(seq.head.children.dropRight(1)),
        expectReference(seq.tail))
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_UNSPECIFIED, _, _) => {
      }
      case (determiner, separator, split) => {
        return SilConjunctiveReference(
          determiner, split.map(expectReference(_)), separator)
      }
    }
    val (determiner, components) = {
      val first = seq.head.unwrapPhrase
      first match {
        case pt @ (_ : SptDT | _ : SptCD) => {
          (determinerFor(requireLeaf(pt.children)), seq.drop(1))
        }
        case _ => {
          (DETERMINER_UNSPECIFIED, seq)
        }
      }
    }
    if ((components.size == 2) && components.head.isPronoun) {
      val pronounReference = expectReference(components.head)
      val entityReference = expectNounReference(
        tree, components.last, determiner)
      SilGenitiveReference(pronounReference, entityReference)
    } else if (components.last.isCompoundAdpositionalPhrase) {
      SilStateSpecifiedReference(
        expectReference(seqIn.dropRight(1)),
        SilExpectedAdpositionalState(components.last))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = expectReference(components.head)
      expectRelativeReference(tree, entityReference, components.last)
    } else if (components.forall(c => c.isNoun || c.isAdjectival)) {
      val entityReference = expectNounReference(
        tree, components.last, determiner)
      if (components.size > 1) {
        val adjComponents = components.dropRight(1)
        val qualifiedReference = SilReference.qualifiedByProperties(
          entityReference,
          adjComponents.map(expectPropertyState))
        qualifiedReference match {
          case SilStateSpecifiedReference(
            _, state : SilConjunctiveState) =>
            {
              rememberSyntheticADJP(state, adjComponents)
            }
          case _ =>
        }
        qualifiedReference
      } else {
        entityReference
      }
    } else {
      SilUnrecognizedReference(tree)
    }
  }

  private def expectPredicateSentence(
    tree : SptS,
    np : ShlurdSyntaxTree, vp : ShlurdSyntaxTree,
    verbModifiers : Seq[ShlurdSyntaxTree], isQuestion : Boolean,
    force : SilForce, modality : SilModality, modalCount : SilCount,
    negativeSuper : Boolean) : SilSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    // FIXME:  representation for double negatives?
    val negative = negativeSuper ^ negativeSub
    val verbHead = vpChildren.head
    val (progressive, iVerb) = detectProgressive(vpChildren)
    if (verbHead.isModal || progressive) {
      val vpSub = vpChildren.last
      if ((vpChildren.size == 2) && vpSub.isVerbPhrase) {
        expectPredicateSentence(
          tree, np, vpSub, verbModifiers,
          isQuestion, force,
          modalityFor(requireLeaf(verbHead.children)),
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
          (vpRemainder.last, maybeSpecifiedState)
        } else {
          (vpChildren.last, SilNullState())
        }
      }
      val extraModifiers = vpRemainder.filterNot(
        child => Seq(verbHead, complement).contains(child))
      val (negativeComplement, predicate) = expectPredicate(
        tree, np, complement, specifiedState,
        relationshipFor(verbHead), verbModifiers ++ extraModifiers)
      val positive = !(negative ^ negativeComplement)
      rememberPredicateCount(predicate, verbHead, modality, modalCount)
      val tam = {
        if (isQuestion) {
          SilTam.interrogative.withPositivity(positive).withModality(modality)
        } else {
          SilTam.indicative.withPositivity(positive).withModality(modality)
        }
      }
      SilPredicateSentence(
        predicate, tam, SilFormality(force))
    } else {
      val (negativeVerb, predicate) = analyzeActionPredicate(
        tree, np, vp, None, verbModifiers)
      val positive = !(negative ^ negativeVerb)
      rememberPredicateCount(predicate, verbHead, modality, modalCount)
      val tam = {
        if (isQuestion) {
          SilTam.interrogative.withPositivity(positive).withModality(modality)
        } else {
          SilTam.indicative.withPositivity(positive).withModality(modality)
        }
      }
      SilPredicateSentence(
        predicate,
        tam,
        SilFormality(force))
    }
  }

  private def expectConditionalSentence(
    tree : ShlurdSyntaxTree,
    antecedent : SptS,
    consequent : SptS,
    formality : SilFormality) : SilSentence =
  {
    val antecedentSentence = analyzeSentence(antecedent)
    val consequentSentence = analyzeSentence(consequent)

    if (!antecedentSentence.tam.isPositive) {
      return SilUnrecognizedSentence(tree)
    }
    antecedentSentence.tam.modality match {
      case MODAL_NEUTRAL | MODAL_PROGRESSIVE =>
      case _ => {
        // Oooooo....modal logic.  Maybe one day.
        return SilUnrecognizedSentence(tree)
      }
    }
    val antecedentPredicate = antecedentSentence match {
      case SilPredicateSentence(predicate, _, _) => predicate
      case _ => {
        return SilUnrecognizedSentence(tree)
      }
    }
    val consequentPredicate = consequentSentence match {
      case SilPredicateSentence(predicate, _, _) => predicate
      case _ => {
        return SilUnrecognizedSentence(tree)
      }
    }
    SilConditionalSentence(
      antecedentPredicate,
      consequentPredicate,
      antecedentSentence.tam,
      consequentSentence.tam,
      formality
    )
  }

  private def expectCommand(
    tree : ShlurdSyntaxTree,
    vp : ShlurdSyntaxTree, formality : SilFormality) : SilSentence =
  {
    val alternative1 = {
      val (particle, unparticled) =
        extractParticle(vp.children)
      val (specifiedState, seq) =
        extractAdpositionalState(unparticled)
      expectCommand(tree, particle, specifiedState, seq, formality)
    }

    val alternative2 = {
      val (specifiedState, unspecified) =
        extractAdpositionalState(vp.children)
      val (particle, seq) =
        extractParticle(unspecified)
      expectCommand(tree, particle, specifiedState, seq, formality)
    }

    SilAmbiguousSentence(Seq(alternative1, alternative2))
  }

  private def expectCommand(
    tree : ShlurdSyntaxTree,
    particle : Option[ShlurdSyntaxTree],
    specifiedState : SilState,
    seq : Seq[ShlurdSyntaxTree],
    formality : SilFormality) : SilSentence =
  {
    if (seq.size == 2) {
      val (state, changeVerb) = particle match {
        // FIXME:  restrict verb pairing when particle is present
        case Some(preTerminal) => {
          (expectPropertyState(preTerminal),
            Some(getWord(requireLeaf(seq.head.children))))
        }
        case _ => {
          (expectPropertyState(seq.head), None)
        }
      }
      val subject = specifyReference(
        expectReference(seq.last), specifiedState)
      SilStateChangeCommand(
        expectStatePredicate(tree, subject, state),
        changeVerb,
        formality)
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private def expectStatePredicate(
    syntaxTree : ShlurdSyntaxTree,
    subject : SilReference, state : SilState,
    specifiedState : SilState = SilNullState(),
    verbModifiers : Seq[SilVerbModifier] = Seq.empty) =
  {
    SilUnresolvedStatePredicate(
      syntaxTree, subject, state, specifiedState, verbModifiers)
  }

  private def expectActionPredicate(
    syntaxTree : ShlurdSyntaxTree,
    subject : SilReference, action : SilWord,
    directObject : Option[SilReference],
    indirectObject : Option[SilReference],
    verbModifiers : Seq[SilVerbModifier]) =
  {
    SilUnresolvedActionPredicate(
      syntaxTree, subject, action, directObject, indirectObject, verbModifiers)
  }

  private def expectRelationshipPredicate(
    syntaxTree : ShlurdSyntaxTree,
    subject : SilReference,
    complement : SilReference,
    relationship : SilRelationship,
    verbModifiers : Seq[SilVerbModifier]) =
  {
    SilUnresolvedRelationshipPredicate(
      syntaxTree, subject, complement, relationship, verbModifiers)
  }

  private def expectReference(
    seq : Seq[ShlurdSyntaxTree]) : SilReference =
  {
    SilExpectedReference(SptNP(seq:_*))
  }

  private def expectReference(np : ShlurdSyntaxTree)
      : SilExpectedReference =
  {
    SilExpectedReference(np)
  }

  private def expectRelativeReference(
    syntaxTree : ShlurdSyntaxTree,
    reference : SilReference,
    relativeTree : ShlurdSyntaxTree) : SilReference =
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

  private def expectNounReference(
    syntaxTree : ShlurdSyntaxTree,
    preTerminal : ShlurdSyntaxTree,
    determiner : SilDeterminer) =
  {
    SilExpectedNounlikeReference(syntaxTree, preTerminal, determiner)
  }

  private[parser] def expectPropertyState(syntaxTree : ShlurdSyntaxTree) =
  {
    SilExpectedPropertyState(syntaxTree)
  }

  private[parser] def expectAdpositionalState(tree : ShlurdSyntaxTree)
    : SilState =
  {
    val seq = tree.children
    val adpTree = seq.head.unwrapPhrase
    if ((seq.size == 2) && (adpTree.isAdposition || adpTree.isAdverb)) {
      extractAdposition(adpTree) match {
        // "in the car"
        case Some(adposition) => {
          SilAdpositionalState(adposition, expectReference(seq.last))
        }
        case _ => {
          if (adpTree.isAdverb && seq.last.isAdpositionalPhrase) {
            // "south of the border"
            expectAdpositionalState(seq.last) match {
              case SilAdpositionalState(SilAdposition(words), ref) => {
                SilAdpositionalState(
                  SilAdposition(getWord(adpTree.firstChild) +: words),
                  ref)
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
    } else {
      SilUnrecognizedState(tree)
    }
  }

  private def extractAdposition(preTerminal : ShlurdSyntaxTree)
      : Option[SilAdposition] =
  {
    preTerminal match {
      case adp : ShlurdSyntaxAdposition => {
        Some(SilAdposition(Seq(getWord(adp.child))))
      }
      case _ => preTerminal.firstChild.lemma match {
        case LEMMA_IN => Some(SilAdposition.IN)
        case LEMMA_INSIDE => Some(SilAdposition.INSIDE)
        case LEMMA_WITHIN => Some(SilAdposition.WITHIN)
        case LEMMA_OUTSIDE => Some(SilAdposition.OUTSIDE)
        case LEMMA_AT => Some(SilAdposition.AT)
        case LEMMA_WITH => Some(SilAdposition.WITH)
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
    syntaxTree : ShlurdSyntaxTree,
    np : ShlurdSyntaxTree,
    vp : ShlurdSyntaxTree,
    specifiedDirectObject : Option[SilReference],
    verbModifiers : Seq[ShlurdSyntaxTree])
      : (Boolean, SilPredicate) =
  {
    val (negative, seq) = extractNegative(vp.children)
    // FIXME should support "there goes the mailman"?
    if (np.isExistential) {
      return (negative, SilUnrecognizedPredicate(syntaxTree))
    }
    val subject = expectReference(np)
    if (seq.isEmpty) {
      return (negative, SilUnrecognizedPredicate(syntaxTree))
    }
    val verbHead = seq.head
    val action = verbHead match {
      case verb : ShlurdSyntaxVerb => {
        getWord(verb.child)
      }
      case _ => {
        return (negative, SilUnrecognizedPredicate(syntaxTree))
      }
    }
    val (directObject, indirectObject, extraModifiers) =
      expectVerbObjectsAndModifiers(seq.drop(1), specifiedDirectObject)
    val directObjects = Seq(directObject, specifiedDirectObject).flatten
    assert(directObject.size < 2)
    val predicate = expectActionPredicate(
      syntaxTree,
      subject, action,
      directObjects.headOption,
      indirectObject,
      verbModifiers.map(expectVerbModifier) ++ extraModifiers)
    (negative, predicate)
  }

  private def expectVerbObjectsAndModifiers(
    seq : Seq[ShlurdSyntaxTree],
    specifiedDirectObject : Option[SilReference]) =
  {
    val objCandidates = seq.filter(_.isNounNode)
    val directObjTree = objCandidates.find(
      _.containsIncomingDependency("dobj")) match
    {
      case Some(dobj) => {
        Some(dobj)
      }
      case _ => {
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
    val modifiers = seq.filterNot(objTrees.contains(_))
    (directObjTree.map(expectReference),
      indirectObjTree.map(expectReference),
      modifiers.map(expectVerbModifier))
  }

  private def expectVerbModifier(tree : ShlurdSyntaxTree) =
  {
    SilExpectedVerbModifier(tree)
  }

  private[parser] def expectVerbModifierPhrase(tree : ShlurdSyntaxPhrase)
      : SilVerbModifier =
  {
    val words = tree.children.map(_ match {
      case adverb : ShlurdSyntaxAdverb => {
        getWord(adverb.child)
      }
      case particle : SptRP => {
        getWord(particle.child)
      }
      case _ => return expectAdpositionalVerbModifier(tree)
    })
    SilBasicVerbModifier(words)
  }

  private[parser] def expectBasicVerbModifier(
    preTerminal : ShlurdSyntaxPreTerminal)
      : SilVerbModifier =
  {
    SilBasicVerbModifier(Seq(getWord(preTerminal.child)))
  }

  private[parser] def expectAdpositionalVerbModifier(tree : ShlurdSyntaxTree) =
  {
    expectAdpositionalState(tree) match {
      case SilAdpositionalState(adposition, objRef) => {
        SilAdpositionalVerbModifier(adposition, objRef)
      }
      case _ => {
        SilUnrecognizedVerbModifier(tree)
      }
    }
  }

  private def expectPredicate(
    syntaxTree : ShlurdSyntaxTree,
    np : ShlurdSyntaxTree,
    complement : ShlurdSyntaxTree,
    specifiedState : SilState,
    relationship : SilRelationship,
    verbModifiers : Seq[ShlurdSyntaxTree] = Seq.empty)
      : (Boolean, SilPredicate) =
  {
    val (negative, seq) = extractNegative(complement.children)
    if (np.isExistential) {
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
      (negative, expectStatePredicate(
        syntaxTree, subject, expectExistenceState(np), SilNullState(),
        verbModifiers.map(expectVerbModifier)))
    } else if (complement.isExistential) {
      (negative, expectStatePredicate(
        syntaxTree,
        specifyReference(
          expectReference(np), specifiedState),
        expectExistenceState(complement),
        SilNullState(),
        verbModifiers.map(expectVerbModifier)))
    } else if (complement.isNounPhrase) {
      // FIXME this is quite arbitrary
      val (subjectRef, complementRef) = relationship match {
        case REL_IDENTITY => {
          (specifyReference(expectReference(np), specifiedState),
            expectReference(seq))
        }
        case REL_ASSOCIATION => {
          (expectReference(np),
            specifyReference(expectReference(seq), specifiedState))
        }
      }
      val relationshipPredicate = expectRelationshipPredicate(
        syntaxTree,
        subjectRef,
        complementRef,
        relationship,
        verbModifiers.map(expectVerbModifier))
      (negative, relationshipPredicate)
    } else {
      val state = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          expectComplementState(
            ShlurdSyntaxRewrite.recompose(complement, seq))
        }
        case (determiner, separator, split) => {
          val conjunctiveState = SilConjunctiveState(
            determiner,
            split.map(
              subseq => expectComplementState(
                ShlurdSyntaxRewrite.recompose(complement, subseq))),
            separator)
          rememberSyntheticADJP(conjunctiveState, seq)
          conjunctiveState
        }
      }
      (negative, expectStatePredicate(
        syntaxTree, expectReference(np), state, specifiedState,
        verbModifiers.map(expectVerbModifier))
      )
    }
  }

  private def expectExistenceState(syntaxTree : ShlurdSyntaxTree) =
  {
    SilExpectedExistenceState(syntaxTree)
  }

  private[parser] def expectPropertyComplementState(
    seq : Seq[ShlurdSyntaxTree]) : SilState =
  {
    val state = expectPropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      SilConjunctiveState(
        DETERMINER_UNSPECIFIED,
        Seq(state) ++ seq.tail.map(
          component => expectComplementState(component)))
    }
  }

  private def expectComplementState(
    tree : ShlurdSyntaxTree) : SilState =
  {
    if (isSinglePhrase(tree.children)) {
      expectComplementState(tree.firstChild)
    } else {
      SilExpectedComplementState(tree)
    }
  }

  private def maybeRecognizeParticle(
    pt : ShlurdSyntaxTree) : Option[ShlurdSyntaxTree] =
  {
    pt match {
      case phrase @ (_ : SptPRT | _ : SptPP) => {
        Some(phrase.firstChild)
      }
      case rp : SptRP => Some(rp)
      case _ => None
    }
  }

  private def isSinglePhrase(seq : Seq[ShlurdSyntaxTree]) =
  {
    (seq.size == 1) && !seq.head.isPreTerminal
  }

  private def isImperative(children : Seq[ShlurdSyntaxTree]) =
  {
    (children.size == 1) && children.head.isVerbPhrase
  }

  private def extractAntecedent(children : Seq[ShlurdSyntaxTree])
      : Option[SptS] =
  {
    children.headOption match {
      case Some(SptSBAR(SptIN(leaf), antecedent : SptS)) => {
        leaf.lemma match {
          case LEMMA_IF => Some(antecedent)
          case _ => None
        }
      }
      case Some(SptSBAR(SptWHADVP(SptWRB(leaf)), antecedent : SptS)) => {
        leaf.lemma match {
          case LEMMA_WHEN => Some(antecedent)
          case _ => None
        }
      }
      case _ => None
    }
  }

  private[parser] def getCount(tree : ShlurdSyntaxTree) : SilCount =
  {
    if (tree.label.endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  private[parser] def getWord(leaf : ShlurdSyntaxTree) =
  {
    SilWord(leaf.foldedToken, leaf.lemma)
  }

  private def isCoordinatingDeterminer(
    syntaxTree : ShlurdSyntaxTree, determiner : SilDeterminer) : Boolean =
  {
    syntaxTree.unwrapPhrase match {
      case preTerminal : ShlurdSyntaxPreTerminal => {
        preTerminal match {
          case (_ : SptDT | _ : SptCC | _ : ShlurdSyntaxAdverb) => {
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

  private def modalityFor(leaf : ShlurdSyntaxLeaf) : SilModality =
  {
    leaf.lemma match {
      case LEMMA_MUST => MODAL_MUST
      case LEMMA_MAY => MODAL_MAY
      case LEMMA_COULD | LEMMA_CAN => MODAL_CAPABLE
      case LEMMA_MIGHT => MODAL_POSSIBLE
      case LEMMA_SHOULD => MODAL_SHOULD
      case LEMMA_DO => MODAL_EMPHATIC
      case LEMMA_BE => MODAL_PROGRESSIVE
      case _ => MODAL_NEUTRAL
    }
  }

  private def relationshipFor(
    verbHead : ShlurdSyntaxTree) : SilRelationship =
  {
    if (verbHead.isPossessionVerb) {
      REL_ASSOCIATION
    } else {
      assert(verbHead.isBeingVerb)
      REL_IDENTITY
    }
  }

  private def maybeQuestionFor(
    tree : ShlurdSyntaxTree) : Option[SilQuestion] =
  {
    tree match {
      case SptWHADJP(how, many) => {
        if (how.hasTerminalLemma(LEMMA_HOW) &&
          many.hasTerminalLemma(LEMMA_MANY))
        {
          Some(QUESTION_HOW_MANY)
        } else {
          None
        }
      }
      case SptWRB(where) => {
        where.lemma match {
          case LEMMA_WHERE => Some(QUESTION_WHERE)
          case _ => None
        }
      }
      case SptWDT(wdt) => {
        wdt.lemma match {
          case LEMMA_WHICH | LEMMA_WHAT => Some(QUESTION_WHICH)
          case _ => None
        }
      }
      case SptWP(wp) => {
        wp.lemma match {
          case LEMMA_WHO => Some(QUESTION_WHO)
          case LEMMA_WHAT => Some(QUESTION_WHAT)
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def determinerFor(leaf : ShlurdSyntaxLeaf) : SilDeterminer =
  {
    leaf.lemma match {
      case LEMMA_NO | LEMMA_NEITHER | LEMMA_NOR => DETERMINER_NONE
      case LEMMA_BOTH | LEMMA_AND | LEMMA_ALL | LEMMA_EVERY => DETERMINER_ALL
      // FIXME LEMMA_ONE should really map to SilIntegerDeterminer
      case LEMMA_ONE | LEMMA_A => DETERMINER_NONSPECIFIC
      case LEMMA_THE | LEMMA_EITHER => DETERMINER_UNIQUE
      case LEMMA_SOME => DETERMINER_SOME
      case _ => DETERMINER_ANY
    }
  }

  private def truncatePunctuation(
    tree : ShlurdSyntaxTree, punctuationMarks : Iterable[String])
      : Seq[ShlurdSyntaxTree] =
  {
    val children = tree.children
    if (punctuationMarks.exists(punctuation =>
      children.last.hasTerminalLabel(LABEL_DOT, punctuation)))
    {
      children.dropRight(1)
    } else {
      children
    }
  }

  private def extractParticle(seq : Seq[ShlurdSyntaxTree])
      : (Option[ShlurdSyntaxTree], Seq[ShlurdSyntaxTree]) =
  {
    seq.indexWhere(_.isParticleNode) match {
      case -1 => {
        val pp = seq.last
        if (pp.isAdpositionalPhrase) {
          (maybeRecognizeParticle(pp), seq.dropRight(1) ++ pp.children.drop(1))
        } else {
          (None, seq)
        }
      }
      case i => {
        (maybeRecognizeParticle(seq(i)), seq.patch(i, Seq.empty, 1))
      }
    }
  }

  private def extractAdpositionalState(seq : Seq[ShlurdSyntaxTree])
      : (SilState, Seq[ShlurdSyntaxTree])=
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
        ) if (!extractAdposition(advp.firstChild).isEmpty) => {
          val rewrite = ShlurdSyntaxRewrite.recompose(
            advp,
            Seq(advp.firstChild, np))
          (SilNullState(), seq.dropRight(2) :+ rewrite)
        }
        case _ => {
          (SilNullState(), seq)
        }
      }
    } else {
      (SilExpectedAdpositionalState(seq(i)),
        seq.take(i) ++ seq.drop(i + 1))
    }
  }

  private def extractModality(
    seq : Seq[ShlurdSyntaxTree])
      : (SilModality, Seq[ShlurdSyntaxTree], SilCount) =
  {
    // FIXME for "does", we need to be careful to make sure it's
    // acting as an auxiliary, e.g. "Luke does know" but not
    // "Luke does the dishes"
    val iModal = seq.indexWhere(_.unwrapPhrase.isModal)

    val (nonAux, iAux) = {
      if (iModal < 0) {
        val (progressive, iVerb) = detectProgressive(seq)
        if (!progressive) {
          return (MODAL_NEUTRAL, seq, COUNT_SINGULAR)
        } else {
          val iBeing = seq.indexWhere(_.isVerbNode)
          val being = seq(iBeing).unwrapPhrase
          assert(being.isRelationshipVerb)
          val nonBeing = seq.patch(iBeing, Seq.empty, 1)
          (nonBeing, iBeing)
        }
      } else {
        val nonModal = seq.patch(iModal, Seq.empty, 1)
        (nonModal, iModal)
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
    (modalityFor(leaf),
      remainder,
      getVerbCount(aux))
  }

  private def extractNegative(
    seq : Seq[ShlurdSyntaxTree])
      : (Boolean, Seq[ShlurdSyntaxTree]) =
  {
    // FIXME:  don't reduce to empty seq
    val pos = seq.map(_.unwrapPhrase).indexWhere(
      sub => sub.isAdverb && sub.hasTerminalLemma(LEMMA_NOT))
    if (pos == -1) {
      (false, seq)
    } else {
      (true, seq.patch(pos, Seq.empty, 1))
    }
  }

  private def splitCommas(components : Seq[ShlurdSyntaxTree])
      : (Seq[Seq[ShlurdSyntaxTree]], SilSeparator) =
  {
    val pos = components.indexWhere(_.isComma)
    if (pos == -1) {
      (Seq(components), SEPARATOR_CONJOINED)
    } else if ((pos + 1 == components.size)) {
      (Seq(components.dropRight(1)), SEPARATOR_OXFORD_COMMA)
    } else {
      val prefix = components.take(pos)
      val suffix = components.drop(pos + 1)
      val (subSplit, subSeparator) = splitCommas(suffix)
      val separator = subSeparator match {
        case SEPARATOR_OXFORD_COMMA => subSeparator
        case _ => SEPARATOR_COMMA
      }
      (Seq(prefix) ++ subSplit, separator)
    }
  }

  private def splitCoordinatingConjunction(
    components : Seq[ShlurdSyntaxTree])
      : (SilDeterminer, SilSeparator, Seq[Seq[ShlurdSyntaxTree]]) =
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
              (DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, Seq.empty)
            }
            case (determiner, separator, subSplit) => {
              // FIXME:  deal with coordinating determiner in
              // first sub-phrase
              val seq = commaSplit.dropRight(1) ++ subSplit
              (determiner, commaSeparator, seq)
            }
          }
        }
        case _ => {
          (DETERMINER_UNSPECIFIED, SEPARATOR_CONJOINED, Seq.empty)
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
          (determiner, commaSeparator, commaSplit ++ Seq(suffix))
        }
        case (subDeterminer, _, subSplit) => {
          val seq = {
            if (determiner == subDeterminer) {
              commaSplit ++ subSplit
            } else {
              commaSplit ++ Seq(suffix)
            }
          }
          (determiner, commaSeparator, seq.filterNot(_.isEmpty))
        }
      }
    }
  }

  private[parser] def specifyReference(
    ref : SilReference, specifiedState : SilState) : SilReference =
  {
    if (specifiedState == SilNullState()) {
      ref
    } else {
      val specifiedReference = SilStateSpecifiedReference(
        ref, specifiedState)
      ref.maybeSyntaxTree.foreach(
        refSyntaxTree => specifiedState.maybeSyntaxTree.foreach(
          stateSyntaxTree => {
            rememberSyntheticNP(
              specifiedReference,
              Seq(refSyntaxTree, stateSyntaxTree))
          }
        )
      )
      specifiedReference
    }
  }

  private def rememberSyntheticNP(
    reference : SilTransformedPhrase,
    seq : Seq[ShlurdSyntaxTree])
  {
    reference.rememberSyntaxTree(SptNP(seq:_*))
  }

  private def rememberSyntheticADJP(
    reference : SilTransformedPhrase,
    seq : Seq[ShlurdSyntaxTree])
  {
    reference.rememberSyntaxTree(SptADJP(seq:_*))
  }

  private def rememberPredicateCount(
    predicate : SilPredicate,
    count : SilCount)
  {
    predicate.setInflectedCount(count)
  }

  private def getVerbCount(verb : ShlurdSyntaxTree) : SilCount =
  {
    verb match {
      case _ : SptVBP => {
        if (verb.firstChild.label == "am") {
          COUNT_SINGULAR
        } else {
          COUNT_PLURAL
        }
      }
      case _ => COUNT_SINGULAR
    }
  }

  private def rememberPredicateCount(
    predicate : SilPredicate,
    verbHead : ShlurdSyntaxTree)
  {
    rememberPredicateCount(predicate, getVerbCount(verbHead))
  }

  private def rememberPredicateCount(
    predicate : SilPredicate,
    verbHead : ShlurdSyntaxTree,
    modality : SilModality,
    modalCount : SilCount)
  {
    modality match {
      case MODAL_NEUTRAL => {
        rememberPredicateCount(predicate, verbHead)
      }
      case _ => {
        rememberPredicateCount(predicate, modalCount)
      }
    }
  }
}
