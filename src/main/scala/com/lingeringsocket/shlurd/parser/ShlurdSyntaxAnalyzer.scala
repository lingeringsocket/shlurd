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
    if (isImperative(children)) {
      expectCommand(tree, children.head, SilFormality(force))
    } else if (children.size == 2) {
      val np = children.head
      val vp = children.last
      if (np.isNounPhrase && vp.isVerbPhrase) {
        expectPredicateSentence(
          tree, np, vp, isQuestion, force, MODAL_NEUTRAL, false)
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
      val (s, c) = extractPrepositionalState(unwrapped)
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
      val (modality, modeless) = extractModality(children)
      val (negativeSuper, seq) = extractNegative(modeless)
      val expectedSize = modality match {
        case MODAL_NEUTRAL => 3
        case _ => 2
      }
      if (seq.size == expectedSize) {
        val (verbHead, np, ap, negative) = modality match {
          // "is Larry smart?"
          case MODAL_NEUTRAL => {
            (seq(0), seq(1), seq(2), negativeSuper)
          }
          // "(can) Larry [be [smart]]?"
          case _ => {
            val vp = seq(1)
            val (negativeSub, sub) = extractNegative(vp.children)
            (sub.head, seq(0), sub.last, (negativeSub ^ negativeSuper))
          }
        }
        if (verbHead.isRelationshipVerb) {
          if (!np.isExistential && verbHead.isExistsVerb) {
            SilUnrecognizedSentence(tree)
          } else {
            val (negativeSub, predicate) =
              expectPredicate(tree, np, ap, specifiedState,
                relationshipFor(verbHead))
            val positive = !(negative ^ negativeSub)
            rememberPredicateCount(predicate, verbHead)
            SilPredicateSentence(
              predicate,
              SilInterrogativeMood(positive, modality))
          }
        } else {
          SilUnrecognizedSentence(tree)
        }
      } else {
        SilUnrecognizedSentence(tree)
      }
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private[parser] def analyzeSBARQ(tree : SptSBARQ)
      : SilSentence =
  {
    val children = truncatePunctuation(tree, Seq(LABEL_QUESTION_MARK))
    val first = children.head
    val second = children.last
    val secondUnwrapped = {
      if ((second.numChildren == 1) && second.firstChild.isVerbPhrase) {
        second.firstChild.children
      } else {
        second.children
      }
    }
    val (negativeSuper, secondSub) = extractNegative(secondUnwrapped)
    val verbHead = secondSub.head
    if ((children.size != 2) ||
      !first.isQueryNoun ||
      !second.isSubQuestion ||
      !verbHead.isRelationshipVerb)
    {
      SilUnrecognizedSentence(tree)
    } else {
      // FIXME support modality
      val (specifiedState, whnpc) = extractPrepositionalState(first.children)
      val seq = {
        if ((whnpc.size == 1) && whnpc.head.isQueryNoun) {
          whnpc.head.children
        } else {
          whnpc
        }
      }
      val question = maybeQuestionFor(seq.head) match {
        case Some(q) => q
        case _ => return SilUnrecognizedSentence(tree)
      }
      val np = question match {
        case QUESTION_WHO => {
          SptNP(SptNN(requireLeaf(seq.head.children)))
        }
        case _ => {
          SptNP(seq.tail:_*)
        }
      }
      val complement = secondSub.tail
      val (combinedState, complementRemainder) = {
        if (specifiedState == SilNullState()) {
          val (s, r) = extractPrepositionalState(complement)
          if (r.isEmpty) {
            (specifiedState, complement)
          } else {
            (s, r)
          }
        } else {
          (specifiedState, complement)
        }
      }
      val (negativeSub, predicate) = expectPredicate(
        tree,
        np,
        ShlurdSyntaxRewrite.recompose(
          complement.head, complementRemainder),
        combinedState,
        relationshipFor(verbHead))
      rememberPredicateCount(predicate, verbHead)
      SilPredicateQuery(
        predicate, question,
        SilInterrogativeMood(!(negativeSuper ^ negativeSub)))
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
    } else if (components.last.isCompoundPrepositionalPhrase) {
      SilStateSpecifiedReference(
        expectReference(seqIn.dropRight(1)),
        SilExpectedPrepositionalState(components.last))
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
    np : ShlurdSyntaxTree, vp : ShlurdSyntaxTree, isQuestion : Boolean,
    force : SilForce, modality : SilModality,
    negativeSuper : Boolean) : SilSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    // FIXME:  representation for double negatives?
    val negative = negativeSuper ^ negativeSub
    val verbHead = vpChildren.head
    if (verbHead.isModal) {
      val vpSub = vpChildren.last
      if ((vpChildren.size == 2) && vpSub.isVerbPhrase) {
        val modality = modalityFor(requireLeaf(verbHead.children))
        expectPredicateSentence(
          tree, np, vpSub, isQuestion, force, modality, negative)
      } else {
        SilUnrecognizedSentence(tree)
      }
    } else if (verbHead.isRelationshipVerb) {
      if ((vpChildren.size > 2) || (!np.isExistential && verbHead.isExistsVerb))
      {
        SilUnrecognizedSentence(tree)
      } else {
        val (specifiedState, vpRemainder) =
          extractPrepositionalState(vpChildren)
        val complement = vpRemainder.last
        val (negativeComplement, predicate) = expectPredicate(
          tree, np, complement, specifiedState,
          relationshipFor(verbHead))
        val positive = !(negative ^ negativeComplement)
        rememberPredicateCount(predicate, verbHead)
        if (isQuestion) {
          SilPredicateSentence(
            predicate, SilInterrogativeMood(positive, modality),
            SilFormality(force))
        } else {
          SilPredicateSentence(
            predicate, SilIndicativeMood(positive, modality),
            SilFormality(force))
        }
      }
    } else {
      SilUnrecognizedSentence(tree)
    }
  }

  private def expectCommand(
    tree : ShlurdSyntaxTree,
    vp : ShlurdSyntaxTree, formality : SilFormality) : SilSentence =
  {
    val alternative1 = {
      val (particle, unparticled) =
        extractParticle(vp.children)
      val (specifiedState, seq) =
        extractPrepositionalState(unparticled)
      expectCommand(tree, particle, specifiedState, seq, formality)
    }

    val alternative2 = {
      val (specifiedState, unspecified) =
        extractPrepositionalState(vp.children)
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
    specifiedState : SilState = SilNullState()) =
  {
    SilUnresolvedStatePredicate(syntaxTree, subject, state, specifiedState)
  }

  private def expectRelationshipPredicate(
    syntaxTree : ShlurdSyntaxTree,
    subject : SilReference,
    complement : SilReference,
    relationship : SilRelationship) =
  {
    SilUnresolvedRelationshipPredicate(
      syntaxTree, subject, complement, relationship)
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

  private[parser] def expectPrepositionalState(tree : ShlurdSyntaxTree)
    : SilState =
  {
    val seq = tree.children
    val prep = seq.head.unwrapPhrase
    if ((seq.size == 2) && (prep.isPreposition || prep.isAdverb)) {
      val prepLemma = prep.firstChild.lemma
      val adposition = prepLemma match {
        case LEMMA_IN | LEMMA_INSIDE | LEMMA_WITHIN => ADP_INSIDE
        case LEMMA_OUTSIDE => ADP_OUTSIDE
        case LEMMA_AT => ADP_AT
        case LEMMA_AS => ADP_AS
        case LEMMA_NEAR | LEMMA_NEARBY => ADP_NEAR
        case LEMMA_ON => ADP_ON
        case LEMMA_ABOVE | LEMMA_OVER => ADP_ABOVE
        case LEMMA_BELOW | LEMMA_UNDER | LEMMA_BENEATH |
            LEMMA_UNDERNEATH => ADP_BELOW
        case LEMMA_BEHIND => ADP_BEHIND
        case _ => return SilUnrecognizedState(tree)
      }
      SilAdpositionalState(adposition, expectReference(seq.last))
    } else {
      SilUnrecognizedState(tree)
    }
  }

  private def expectPredicate(
    syntaxTree : ShlurdSyntaxTree,
    np : ShlurdSyntaxTree,
    complement : ShlurdSyntaxTree,
    specifiedState : SilState,
    relationship : SilRelationship)
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
        syntaxTree, subject, expectExistenceState(np)))
    } else if (complement.isExistential) {
      (negative, expectStatePredicate(
        syntaxTree,
        specifyReference(
          expectReference(np), specifiedState),
        expectExistenceState(complement)))
    } else if (complement.isNounPhrase) {
      val relationshipPredicate = expectRelationshipPredicate(
        syntaxTree,
        specifyReference(expectReference(np), specifiedState),
        expectReference(seq),
        relationship)
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
        syntaxTree, expectReference(np), state, specifiedState)
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
      case SptWDT(wdt) => {
        wdt.lemma match {
          case LEMMA_WHICH | LEMMA_WHAT => Some(QUESTION_WHICH)
          case _ => None
        }
      }
      case SptWP(wp) => {
        wp.lemma match {
          case LEMMA_WHO => Some(QUESTION_WHO)
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
        if (pp.isPrepositionalPhrase) {
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

  private def extractPrepositionalState(seq : Seq[ShlurdSyntaxTree])
      : (SilState, Seq[ShlurdSyntaxTree])=
  {
    val i = seq.indexWhere(_.isCompoundPrepositionalPhrase)
    if (i == -1) {
      val last2 = seq.takeRight(2)
      last2 match {
        case Seq(advp : SptADVP, np : SptNP) => {
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
      (SilExpectedPrepositionalState(seq(i)),
        seq.take(i) ++ seq.drop(i + 1))
    }
  }

  private def extractModality(
    seq : Seq[ShlurdSyntaxTree])
      : (SilModality, Seq[ShlurdSyntaxTree]) =
  {
    val intro = seq.head
    if (intro.isModal) {
      val suffix = seq.drop(1)
      val remainder = {
        if (isSinglePhrase(suffix) && suffix.head.isVerbPhrase) {
          suffix.head.children
        } else {
          suffix
        }
      }
      (modalityFor(requireLeaf(intro.children)), remainder)
    } else {
      (MODAL_NEUTRAL, seq)
    }
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
    verbHead : ShlurdSyntaxTree)
  {
    verbHead match {
      case _ : SptVBP => predicate.setInflectedCount(COUNT_PLURAL)
      case _ => predicate.setInflectedCount(COUNT_SINGULAR)
    }
  }
}
