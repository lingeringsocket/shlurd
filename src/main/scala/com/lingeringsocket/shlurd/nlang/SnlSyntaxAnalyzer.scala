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

import SprPennTreebankLabels._
import SprUtils._

abstract class SnlSyntaxAnalyzer(
  context : SprContext,
  guessedQuestion : Boolean,
  strictness : SprStrictness,
  enforceTransitive : Boolean)
    extends SprAbstractSyntaxAnalyzer(context, strictness)
{
  private implicit val tongue = context.getTongue

  override def analyzeSentence(tree : SptS)
      : SilSentence =
  {
    val hasQuestionMark =
      tree.children.last.hasTerminalLabel(LABEL_DOT, LABEL_QUESTION_MARK)
    val isQuestion = hasQuestionMark && !guessedQuestion
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
        DETERMINER_ABSENT,
        semiSplits.map(split => SipExpectedSentence(split.head)),
        semiSeparator)
    }
    splitCoordinatingConjunction(tree.children) match {
      case (DETERMINER_ABSENT, _, _) => ;
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
            subs.flatten.map(s => SipExpectedSentence(s)),
            separator)
        }
      }
    }
    val children = stripPauses(tree)
    extractAntecedent(children) match {
      case Some((conjunction, antecedent)) => {
        SipExpectedConditionalSentence(
          tree,
          conjunction,
          SipExpectedSentence(antecedent),
          SipExpectedSentence(
            SptS(children.tail.filterNot(
              c => (c.isThen || c.isEquivalently)):_*)),
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
        expectFullPredicate(tree, np, vp, verbModifiers, isQuestion, force)
      }
      case _ if ((children.size == 1) && allowElidedSubject) => {
        val np = SptNP(SptNNE())
        val vp = children.head
        expectFullPredicate(tree, np, vp, Seq.empty, isQuestion, force)
      }
      case _ => {
        SilUnrecognizedSentence(tree)
      }
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
      if (allowElidedSubject && (children.size == 1)) {
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
    if (verbHead.isRelationshipVerb && specifiedDirectObject.isEmpty) {
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
      case QUESTION_WHICH => {
        // FIXME likewise, these have two flavors "which do you want?"
        // and "which flavor do you want?"

        questionChildren match {
          case Seq(SptNP(first : SptNP, second)) => {
            SptNP(
              SptNP((SptDT(makeLeaf(PD_WHICH.toLemma)) +:
                unwrapSinglePhrase(first.children)):_*),
              second
            )
          }
          case _ => {
            SptNP((SptDT(makeLeaf(PD_WHICH.toLemma)) +:
              unwrapSinglePhrase(questionChildren)):_*)
          }
        }
      }
      case QUESTION_HOW_MANY => {
        // FIXME likewise, these have two flavors "how many trees are there"
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
          assert(adpositionOpt.isEmpty)
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
    if (seq.head.hasLabel(LABEL_LPAREN) && seq.last.hasLabel(LABEL_RPAREN)) {
      return annotator.parenthesizedRef(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_PAREN
      )
    }
    if (seq.head.hasLabel(LABEL_LCURLY) && seq.last.hasLabel(LABEL_RCURLY)) {
      return annotator.parenthesizedRef(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_CURLY
      )
    }
    if (seq.head.lastChild.isPossessiveClitic) {
      return annotator.genitiveRef(
        expectReference(seq.head.children.dropRight(1)),
        expectReference(seq.tail))
    }
    if ((seq.size == 2) && seq.last.isNounPhrase &&
      seq.last.firstChild.hasLabel(LABEL_LPAREN)
    ) {
      return annotator.appositionalRef(
        expectReference(seq.head),
        expectReference(seq.last)
      )
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_ABSENT, _, _) => {
      }
      case (determiner, separator, split) => {
        return annotator.conjunctiveRef(
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
            !tongue.isCoordinatingDeterminer(pt.firstChild.lemma)
        ) => {
          tupleN((determinerFor(requireLeaf(pt.children)), seq.drop(1)))
        }
        case _ => {
          tupleN((DETERMINER_ABSENT, seq))
        }
      }
    }
    if ((components.size == 2) &&
      (components.head.isPossessivePronoun ||
        components.head.isDemonstrative))
    {
      val pronounReference = SipExpectedPossessiveReference(components.head)
      val entityReference = expectNounReference(
        tree, components.last, determiner)
      annotator.genitiveRef(pronounReference, entityReference)
    } else if (components.last.isCompoundAdpositionalPhrase) {
      annotator.stateSpecifiedRef(
        expectReference(seqIn.dropRight(1)),
        SipExpectedAdpositionalState(components.last, false))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = expectReference(components.head)
      expectRelativeReference(tree, entityReference, components.last)
    } else {
      analyzeQualifiedNounPhrase(tree, determiner, components)
    }
  }

  private def findNounPhraseHead(components : Seq[SprSyntaxTree]) : Int =
  {
    tongue.getAdjectivePosition match {
      case MOD_BEFORE_ALWAYS => {
        components.size - 1
      }
      case MOD_BEFORE_DEFAULT => {
        components.lastIndexWhere(isNounPhraseHead)
      }
      case MOD_AFTER_ALWAYS => {
        0
      }
      case MOD_AFTER_DEFAULT => {
        components.indexWhere(isNounPhraseHead)
      }
    }
  }

  protected def analyzeQualifiedNounPhrase(
    tree : SptNP,
    determiner : SilDeterminer,
    components : Seq[SprSyntaxTree]) : SilReference =
  {
    val iHead = findNounPhraseHead(components)
    if (iHead == -1) {
      SilUnrecognizedReference(tree)
    } else {
      val head = components(iHead)
      val adjComponents = components.patch(iHead, Seq.empty, 1)
      if (isNounPhraseHead(head) && adjComponents.forall(
        c => isNounPhraseModifier(c, head)))
      {
        val qr = {
          val entityReference = expectNounReference(
            tree, head, DETERMINER_ABSENT)
          if (components.size > 1) {
            val qualifiedReference = annotator.stateQualifiedRef(
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
        annotator.determinedRef(qr, determiner)
      } else {
        SilUnrecognizedReference(tree)
      }
    }
  }

  protected def combineNegatives(n1 : Boolean, n2 : Boolean) : Boolean

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
          expectPredicateSentence(
            tree, np, vpSub, verbModifiers ++ extraModifiers,
            force,
            tamForAux(verbHeadLeaf, Seq(vpSub)).withMood(tam.mood).
              withTense(tamTensed.tense),
            getVerbInflection(verbHead),
            negative)
        } else {
          SilUnrecognizedSentence(tree)
        }
      } else {
        SilUnrecognizedSentence(tree)
      }
    } else if (verbHead.isBeingVerb || verbHead.isPossessionVerb) {
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
                case SprPredefAdposition(
                  PD_OF | PD_GENITIVE_OF | PD_TO) => false
                case _ => true
              }
            } else {
              true
            }
          }
          if (valid) {
            val obj = seq.last
            if (isAdpositionable(obj.unwrapPhrase)) {
              SilAdpositionalState(adposition, expectReference(obj))
            } else {
              SilUnrecognizedState(tree)
            }
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
        SprPredefAdposition(PD_ADVERBIAL_TMP), expectReference(seq.head))
    } else {
      SilUnrecognizedState(tree)
    }
  }

  override protected def analyzeActionPredicate(
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

  protected def allowObjectPronounsAfterVerb() : Boolean = true

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
    val objCandidates = {
      if (allowObjectPronounsAfterVerb) {
        seq.filter(_.isNounOrPronoun)
      } else {
        seq.filter(t => t.isNoun ||
          (t.isNounPhrase && !t.unwrapPhrase.isPronoun))
      }
    }
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
        SprPredefAdposition(PD_TO),
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
            case SnlEnglishLemmas.LEMMA_NO | "very" => true
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
        if (tongue.isAdposition(pt.child.lemma)) {
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

  override def expectPropertyComplementState(
    tree : SprSyntaxTree) : SilState =
  {
    val seq = tree.children
    if (isStrict) {
      tree match {
        case _ : SptVP => SilUnrecognizedState(tree)
        case _ : SptADVP => SilUnrecognizedState(tree)
        case SptADJP(_ : SptRB) => SilUnrecognizedState(tree)
        case SptADJP(nn) if (nn.isNounOrPronoun) => SilUnrecognizedState(tree)
        case ap : SptADJP if (
          ap.children.exists(
            child => child.isInstanceOf[SptPP] && (child.numChildren < 2))
        ) =>
          SilUnrecognizedState(tree)
        case _ => {
          splitCoordinatingConjunction(seq) match {
            case (DETERMINER_ABSENT, _, _) => {
              analyzePropertyComplementState(tree, seq)
            }
            case (determiner, separator, split) => {
              val state = SilConjunctiveState(
                determiner,
                split.map(x => {
                  analyzePropertyComplementState(tree, unwrapSinglePhrase(x))
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
        ) if (SilWord(adp.lemma) == SprPredefWord(PD_OF)) => {
          Some(adp)
        }
        case _ => None
      }
      if ((seq.size == 2) && !compoundAdposition.isEmpty) {
        expectAdpositionalState(tree, false)
      } else {
        SilConjunctiveState(
          DETERMINER_ABSENT,
          Seq(state) ++ seq.tail.map(
            component => expectComplementState(component)))
      }
    }
  }

  private def extractAntecedent(children : Seq[SprSyntaxTree])
      : Option[(SilWord, SptS)] =
  {
    children.headOption match {
      case Some(SptSBAR(SptIN(leaf), antecedent : SptS)) => {
        SilWord(leaf.lemma) match {
          case SprPredefWord(PD_IF | PD_WHEN | PD_WHENEVER |
              PD_BEFORE | PD_AFTER) =>
            Some(tupleN((getWord(leaf), antecedent)))
          case _ =>
            None
        }
      }
      case Some(SptSBAR(SptWHADVP(SptWRB(leaf)), antecedent : SptS)) => {
        SilWord(leaf.lemma) match {
          case SprPredefWord(PD_WHEN | PD_WHENEVER) =>
            Some(tupleN((getWord(leaf), antecedent)))
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def maybeQuestionFor(
    seq : Seq[SprSyntaxTree])
      : Option[(SilQuestion, Option[SprSyntaxTree], Seq[SprSyntaxTree])] =
  {
    val tree = seq.head
    tree match {
      case SptWHADJP(children @ _*) => {
        if (children.forall(_.isPreTerminal)) {
          val lemma = children.map(_.firstChild.lemma).mkString(" ")
          SilWord(lemma) match {
            case SprPredefWord(PD_HOW_MANY) => {
              // FIXME for HOW_MANY, force the corresponding noun reference
              // to plural
              Some((QUESTION_HOW_MANY, None, seq.tail))
            }
            case _ => None
          }
        } else {
          None
        }
      }
      case SptWRB(where) => {
        SilWord(where.lemma) match {
          case SprPredefWord(PD_WHERE) =>
            Some((QUESTION_WHERE, None, tree.children))
          case _ => None
        }
      }
      case SptWDT(wdt) => {
        SilWord(wdt.lemma) match {
          case SprPredefWord(PD_WHICH | PD_WHAT) =>
            Some((QUESTION_WHICH, None, seq.tail))
          case _ => None
        }
      }
      case SptWP_POS(wpp) => {
        Some((QUESTION_WHO, None,
          Seq(SptNP(
            (SptNP(SptNN(makeLeaf(PD_WHO.toLemma)), SptPOS(makeLeaf("'s"))) +:
              seq.tail):_*))))
      }
      case SptWP(wp) => {
        SilWord(wp.lemma) match {
          case SprPredefWord(PD_WHO | PD_WHOM) =>
            Some((QUESTION_WHO, None, tree.children))
          case SprPredefWord(PD_WHAT) =>
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
            case (DETERMINER_ABSENT, _, _) => {
              tupleN((DETERMINER_ABSENT, SEPARATOR_CONJOINED, Seq.empty))
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
          tupleN((DETERMINER_ABSENT, SEPARATOR_CONJOINED, Seq.empty))
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
          if (determiner.isInstanceOf[SilUnlimitedDeterminer]) {
            determiner = DETERMINER_DEFINITE
          }
          components.take(pos).drop(1)
        } else {
          components.take(pos)
        }
      }
      val (commaSplit, commaSeparator) = splitCommas(prefix)
      val suffix = components.drop(pos + 1)
      splitCoordinatingConjunction(suffix) match {
        case (DETERMINER_ABSENT, _, _) => {
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

  override def isProhibitedPropertyState(
    preTerminal : SprSyntaxPreTerminal) : Boolean =
  {
    val lemma = getWord(preTerminal.child).lemma
    (
      tongue.isBeingLemma(lemma)
    ) || (
      isStrict && preTerminal.isAdposition &&
        (tongue.isAdposition(lemma) ||
          tongue.isSubordinatingConjunction(lemma))
    )
  }
}
