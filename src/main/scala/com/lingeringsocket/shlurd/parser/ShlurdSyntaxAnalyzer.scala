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
      : ShlurdSentence =
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
      requireCommand(tree, children.head, ShlurdFormality(force))
    } else if (children.size == 2) {
      val np = children.head
      val vp = children.last
      if (np.isNounPhrase && vp.isVerbPhrase) {
        requirePredicateSentence(
          tree, np, vp, isQuestion, force, MODAL_NEUTRAL, false)
      } else {
        ShlurdUnrecognizedSentence(tree)
      }
    } else {
      ShlurdUnrecognizedSentence(tree)
    }
  }

  private[parser] def analyzeSQ(tree : ShlurdSyntaxTree, forceSQ : Boolean)
      : ShlurdSentence =
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
        (ShlurdNullState(), unwrapped)
      } else {
        (s, c)
      }
    }
    if (!forceSQ && isImperative(punctless)) {
      assert(specifiedState == ShlurdNullState())
      return requireCommand(tree, children.head, ShlurdFormality.DEFAULT)
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
            ShlurdUnrecognizedSentence(tree)
          } else {
            val (negativeSub, predicate) =
              requirePredicate(tree, np, ap, specifiedState,
                relationshipFor(verbHead))
            val positive = !(negative ^ negativeSub)
            ShlurdPredicateSentence(
              predicate,
              ShlurdInterrogativeMood(positive, modality))
          }
        } else {
          ShlurdUnrecognizedSentence(tree)
        }
      } else {
        ShlurdUnrecognizedSentence(tree)
      }
    } else {
      ShlurdUnrecognizedSentence(tree)
    }
  }

  private[parser] def analyzeSBARQ(tree : SptSBARQ)
      : ShlurdSentence =
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
    if ((children.size != 2) ||
      !first.isQueryNoun ||
      !second.isSubQuestion ||
      !secondSub.head.isBeingVerb)
    {
      ShlurdUnrecognizedSentence(tree)
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
        case _ => return ShlurdUnrecognizedSentence(tree)
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
        if (specifiedState == ShlurdNullState()) {
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
      val (negativeSub, predicate) = requirePredicate(
        tree,
        np,
        ShlurdSyntaxRewrite.recompose(
          complement.head, complementRemainder),
        combinedState,
        REL_IDENTITY)
      ShlurdPredicateQuery(
        predicate, question,
        ShlurdInterrogativeMood(!(negativeSuper ^ negativeSub)))
    }
  }

  private[parser] def analyzeNounPhrase(
    tree : SptNP) : ShlurdReference =
  {
    val seqIn = tree.children
    if (seqIn.isEmpty) {
      return ShlurdUnrecognizedReference(tree)
    }
    val seq = seqIn.map(_.unwrapPhrase)
    if (seq.size == 1) {
      return requireReference(seq.head)
    }
    if (seq.head.lastChild.isPossessive) {
      return ShlurdGenitiveReference(
        requireReference(seq.head.children.dropRight(1)),
        requireReference(seq.tail))
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_UNSPECIFIED, _, _) => {
      }
      case (determiner, separator, split) => {
        return ShlurdConjunctiveReference(
          determiner, split.map(requireReference(_)), separator)
      }
    }
    val (determiner, components) = {
      val first = seq.head.unwrapPhrase
      first match {
        case SptDT(leaf) => {
          (determinerFor(leaf), seq.drop(1))
        }
        case _ => {
        (DETERMINER_UNSPECIFIED, seq)
        }
      }
    }
    if ((components.size == 2) && components.head.isPronoun) {
      val pronounReference = recognizePronounReference(
        requireLeaf(components.head.children))
      val entityReference = requireNounReference(components.last, determiner)
      ShlurdGenitiveReference(pronounReference, entityReference)
    } else if (components.last.isCompoundPrepositionalPhrase) {
      ShlurdStateSpecifiedReference(
        requireReference(seqIn.dropRight(1)),
        ShlurdExpectedPrepositionalState(components.last))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = requireReference(components.head)
      requireRelativeReference(tree, entityReference, components.last)
    } else if (components.forall(c => c.isNoun || c.isAdjectival)) {
      val entityReference = requireNounReference(components.last, determiner)
      if (components.size > 1) {
        ShlurdReference.qualified(
          entityReference,
          components.dropRight(1).map(c => getWord(c.firstChild)))
      } else {
        entityReference
      }
    } else {
      ShlurdUnrecognizedReference(tree)
    }
  }

  private def requirePredicateSentence(
    tree : SptS,
    np : ShlurdSyntaxTree, vp : ShlurdSyntaxTree, isQuestion : Boolean,
    force : ShlurdForce, modality : ShlurdModality,
    negativeSuper : Boolean) : ShlurdSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    // FIXME:  representation for double negatives?
    val negative = negativeSuper ^ negativeSub
    val verbHead = vpChildren.head
    if (verbHead.isModal) {
      val vpSub = vpChildren.last
      if ((vpChildren.size == 2) && vpSub.isVerbPhrase) {
        val modality = modalityFor(requireLeaf(verbHead.children))
        requirePredicateSentence(
          tree, np, vpSub, isQuestion, force, modality, negative)
      } else {
        ShlurdUnrecognizedSentence(tree)
      }
    } else if (verbHead.isRelationshipVerb) {
      if ((vpChildren.size > 2) || (!np.isExistential && verbHead.isExistsVerb))
      {
        ShlurdUnrecognizedSentence(tree)
      } else {
        val (specifiedState, vpRemainder) =
          extractPrepositionalState(vpChildren)
        val complement = vpRemainder.last
        val (negativeComplement, predicate) = requirePredicate(
          tree,
          np, complement, specifiedState,
          relationshipFor(verbHead))
        val positive = !(negative ^ negativeComplement)
        if (isQuestion) {
          ShlurdPredicateSentence(
            predicate, ShlurdInterrogativeMood(positive, modality),
            ShlurdFormality(force))
        } else {
          ShlurdPredicateSentence(
            predicate, ShlurdIndicativeMood(positive, modality),
            ShlurdFormality(force))
        }
      }
    } else {
      ShlurdUnrecognizedSentence(tree)
    }
  }

  private def requireCommand(
    tree : ShlurdSyntaxTree,
    vp : ShlurdSyntaxTree, formality : ShlurdFormality) : ShlurdSentence =
  {
    val alternative1 = {
      val (particle, unparticled) =
        extractParticle(vp.children)
      val (specifiedState, seq) =
        extractPrepositionalState(unparticled)
      requireCommand(tree, particle, specifiedState, seq, formality)
    }

    val alternative2 = {
      val (specifiedState, unspecified) =
        extractPrepositionalState(vp.children)
      val (particle, seq) =
        extractParticle(unspecified)
      requireCommand(tree, particle, specifiedState, seq, formality)
    }

    ShlurdAmbiguousSentence(Seq(alternative1, alternative2))
  }

  private def requireCommand(
    tree : ShlurdSyntaxTree,
    particle : Option[ShlurdWord],
    specifiedState : ShlurdState,
    seq : Seq[ShlurdSyntaxTree],
    formality : ShlurdFormality) : ShlurdSentence =
  {
    if (seq.size == 2) {
      val state = particle match {
        // FIXME:  restrict verb pairing when particle is present
        case Some(word) => ShlurdPropertyState(word)
        case _ => requirePropertyState(seq.head)
      }
      val subject = specifyReference(
        requireReference(seq.last), specifiedState)
      ShlurdStateChangeCommand(
        ShlurdStatePredicate(subject, state),
        formality)
    } else {
      ShlurdUnrecognizedSentence(tree)
    }
  }

  private def requireReference(
    seq : Seq[ShlurdSyntaxTree]) : ShlurdReference =
  {
    ShlurdExpectedReference(SptNP(seq:_*))
  }

  private def requireReference(np : ShlurdSyntaxTree)
      : ShlurdExpectedReference =
  {
    ShlurdExpectedReference(np)
  }

  private def requireRelativeReference(
    syntaxTree : ShlurdSyntaxTree,
    reference : ShlurdReference,
    relativeTree : ShlurdSyntaxTree) : ShlurdReference =
  {
    relativeTree match {
      case SptSBAR(
        SptWHNP(SptWDT(_)),
        SptS(SptVP(verb, complement))
      ) if (verb.isBeingVerb) => {
        val state = requireComplementState(SptVP(complement))
        ShlurdUnresolvedRelativeReference(syntaxTree, reference, state)
      }
      case _ => {
        ShlurdUnrecognizedReference(syntaxTree)
      }
    }
  }

  private def requireNounReference(
    pt : ShlurdSyntaxTree, determiner : ShlurdDeterminer) =
  {
    // we allow mislabeled adjectives to handle
    // cases like "roll up the blind"
    if (pt.isNoun || pt.isAdjectival) {
      val noun = pt.firstChild
      ShlurdEntityReference(
        getWord(noun),
        determiner,
        getCount(pt))
    } else {
      ShlurdUnrecognizedReference(pt)
    }
  }

  private[parser] def requirePropertyState(ap : ShlurdSyntaxTree) =
  {
    if (ap.isPreTerminal) {
      ShlurdPropertyState(getWord(ap.firstChild))
    } else {
      ShlurdUnrecognizedState(ap)
    }
  }

  private[parser] def requirePrepositionalState(tree : ShlurdSyntaxTree)
    : ShlurdState =
  {
    val seq = tree.children
    val prep = seq.head.unwrapPhrase
    if ((seq.size == 2) && (prep.isPreposition || prep.isAdverb)) {
      val prepLemma = prep.firstChild.lemma
      val locative = prepLemma match {
        case LEMMA_IN | LEMMA_INSIDE | LEMMA_WITHIN => LOC_INSIDE
        case LEMMA_OUTSIDE => LOC_OUTSIDE
        case LEMMA_AT => LOC_AT
        case LEMMA_NEAR | LEMMA_NEARBY => LOC_NEAR
        case LEMMA_ON => LOC_ON
        case LEMMA_ABOVE | LEMMA_OVER => LOC_ABOVE
        case LEMMA_BELOW | LEMMA_UNDER | LEMMA_BENEATH |
            LEMMA_UNDERNEATH => LOC_BELOW
        case LEMMA_BEHIND => LOC_BEHIND
        case _ => return ShlurdUnrecognizedState(tree)
      }
      ShlurdLocationState(locative, requireReference(seq.last))
    } else {
      ShlurdUnrecognizedState(tree)
    }
  }

  private def requirePredicate(
    syntaxTree : ShlurdSyntaxTree,
    np : ShlurdSyntaxTree,
    complement : ShlurdSyntaxTree,
    specifiedState : ShlurdState,
    relationship : ShlurdRelationship)
      : (Boolean, ShlurdPredicate) =
  {
    val (negative, seq) = extractNegative(complement.children)
    if (np.isExistential) {
      val subject = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          specifyReference(requireReference(seq), specifiedState)
        }
        case (determiner, separator, split) => {
          ShlurdConjunctiveReference(
            determiner,
            split.map(x => specifyReference(
              requireReference(x), specifiedState)),
            separator)
        }
      }
      (negative, ShlurdStatePredicate(subject, ShlurdExistenceState()))
    } else if (complement.isExistential) {
      (negative, ShlurdStatePredicate(
        specifyReference(requireReference(np), specifiedState),
          ShlurdExistenceState()))
    } else if (complement.isNounPhrase) {
      val relationshipPredicate = ShlurdRelationshipPredicate(
        specifyReference(requireReference(np), specifiedState),
        requireReference(seq),
        relationship)
      (negative, relationshipPredicate)
    } else {
      val state = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          requireComplementState(
            ShlurdSyntaxRewrite.recompose(complement, seq))
        }
        case (determiner, separator, split) => {
          ShlurdConjunctiveState(
            determiner,
            split.map(
              subseq => requireComplementState(
                ShlurdSyntaxRewrite.recompose(complement, subseq))),
            separator)
        }
      }
      (negative, ShlurdUnresolvedPredicate(
        syntaxTree, requireReference(np), state, specifiedState)
      )
    }
  }

  private[parser] def requirePropertyComplementState(
    seq : Seq[ShlurdSyntaxTree]) : ShlurdState =
  {
    val state = requirePropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      ShlurdConjunctiveState(
        DETERMINER_UNSPECIFIED,
        Seq(state) ++ seq.tail.map(
          component => requireComplementState(component)))
    }
  }

  private def requireComplementState(
    tree : ShlurdSyntaxTree) : ShlurdState =
  {
    if (isSinglePhrase(tree.children)) {
      requireComplementState(tree.firstChild)
    } else {
      ShlurdExpectedComplementState(tree)
    }
  }

  private[parser] def recognizePronounReference(leaf : ShlurdSyntaxLeaf)
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

  private def maybeRecognizeParticle(
    pt : ShlurdSyntaxTree) : Option[ShlurdWord] =
  {
    pt match {
      case phrase @ (_ : SptPRT | _ : SptPP) => {
        Some(getWord(phrase.firstChild.firstChild))
      }
      case SptRP(leaf) => Some(getWord(leaf))
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

  private[parser] def getCount(tree : ShlurdSyntaxTree) : ShlurdCount =
  {
    if (tree.label.endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  private[parser] def getWord(leaf : ShlurdSyntaxTree) =
  {
    ShlurdWord(leaf.token, leaf.lemma)
  }

  private def isCoordinatingDeterminer(
    syntaxTree : ShlurdSyntaxTree, determiner : ShlurdDeterminer) : Boolean =
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

  private def modalityFor(leaf : ShlurdSyntaxLeaf) : ShlurdModality =
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
    verbHead : ShlurdSyntaxTree) : ShlurdRelationship =
  {
    if (verbHead.isPossessionVerb) {
      REL_ASSOCIATION
    } else {
      assert(verbHead.isBeingVerb)
      REL_IDENTITY
    }
  }

  private def maybeQuestionFor(
    tree : ShlurdSyntaxTree) : Option[ShlurdQuestion] =
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

  private def determinerFor(leaf : ShlurdSyntaxLeaf) : ShlurdDeterminer =
  {
    leaf.lemma match {
      case LEMMA_NO | LEMMA_NEITHER | LEMMA_NOR => DETERMINER_NONE
      case LEMMA_BOTH | LEMMA_AND | LEMMA_ALL | LEMMA_EVERY => DETERMINER_ALL
      case LEMMA_A => DETERMINER_NONSPECIFIC
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
      : (Option[ShlurdWord], Seq[ShlurdSyntaxTree]) =
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
      : (ShlurdState, Seq[ShlurdSyntaxTree])=
  {
    val i = seq.indexWhere(_.isCompoundPrepositionalPhrase)
    if (i == -1) {
      val last2 = seq.takeRight(2)
      last2 match {
        case Seq(advp : SptADVP, np : SptNP) => {
          val rewrite = ShlurdSyntaxRewrite.recompose(
            advp,
            Seq(advp.firstChild, np))
          (ShlurdNullState(), seq.dropRight(2) :+ rewrite)
        }
        case _ => {
          (ShlurdNullState(), seq)
        }
      }
    } else {
      (ShlurdExpectedPrepositionalState(seq(i)),
        seq.take(i) ++ seq.drop(i + 1))
    }
  }

  private def extractModality(
    seq : Seq[ShlurdSyntaxTree])
      : (ShlurdModality, Seq[ShlurdSyntaxTree]) =
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
    val pos = seq.map(_.unwrapPhrase).indexWhere(
      sub => sub.isAdverb && sub.hasTerminalLemma(LEMMA_NOT))
    if (pos == -1) {
      (false, seq)
    } else {
      (true, seq.patch(pos, Seq.empty, 1))
    }
  }

  private def splitCommas(components : Seq[ShlurdSyntaxTree])
      : (Seq[Seq[ShlurdSyntaxTree]], ShlurdSeparator) =
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
      : (ShlurdDeterminer, ShlurdSeparator, Seq[Seq[ShlurdSyntaxTree]]) =
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
    ref : ShlurdReference, specifiedState : ShlurdState) : ShlurdReference =
  {
    if (specifiedState == ShlurdNullState()) {
      ref
    } else {
      ShlurdStateSpecifiedReference(ref, specifiedState)
    }
  }
}
