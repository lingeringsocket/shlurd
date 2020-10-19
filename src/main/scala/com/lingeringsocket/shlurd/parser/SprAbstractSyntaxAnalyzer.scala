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

import scala.collection._

sealed trait SprStrictness
case object SPR_STRICTNESS_TIGHT extends SprStrictness
case object SPR_STRICTNESS_LOOSE extends SprStrictness

abstract class SprAbstractSyntaxAnalyzer(
  context : SprContext,
  strictness : SprStrictness = SPR_STRICTNESS_LOOSE)
    extends SprSyntaxAnalyzer
{
  protected def annotator = context.annotator

  private implicit val tongue = context.getTongue

  def isStrict = (strictness == SPR_STRICTNESS_TIGHT)

  protected def stripPauses(tree : SprSyntaxTree)
      : Seq[SprSyntaxTree] =
  {
    tree.children.filterNot(_.isPause)
  }

  protected def applyInterrogative(tam : SilTam) : SilTam =
  {
    tam.withMood(MOOD_INTERROGATIVE)
  }

  protected def isAdpositionable(tree : SprSyntaxTree) : Boolean =
  {
    tree match {
      case SptPRP(leaf) => {
        tongue.isAdpositionablePronoun(getWord(leaf).lemma)
      }
      case _ => true
    }
  }

  protected def extractNegative(
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

  protected def isNegative(tree : SprSyntaxTree) : Boolean

  protected def tamForAux(
    leaf : SprSyntaxLeaf,
    seq : Seq[SprSyntaxTree]) : SilTam =
  {
    seq.find(_.isVerbNode) match {
      case Some(mainVerb) => {
        mainVerb match {
          case vp : SptVP => tamForAux(leaf, vp.children)
          case _ => tongue.tamForAuxLemma(
            leaf.lemma, mainVerb.firstChild.lemma)
        }
      }
      case _ => SilTam.indicative
    }
  }

  protected def determinerFor(leaf : SprSyntaxLeaf) : SilDeterminer =
  {
    tongue.maybeDeterminerFor(leaf.lemma).getOrElse(DETERMINER_ANY)
  }

  protected def relationshipVerb(
    verbHead : SprSyntaxTree) : SilWord =
  {
    getWord(verbHead.asInstanceOf[SprSyntaxPreTerminal].child)
  }


  protected def isCoordinatingDeterminer(
    syntaxTree : SprSyntaxTree, determiner : SilDeterminer) : Boolean =
  {
    syntaxTree.unwrapPhrase match {
      case preTerminal : SprSyntaxPreTerminal => {
        preTerminal match {
          case (_ : SptDT | _ : SptCC | _ : SprSyntaxAdverb) => {
            SilWord(preTerminal.child.lemma) match {
              case SprPredefDeterminerWord(PD_BOTH) =>
                (determiner == DETERMINER_ALL)
              case SprPredefDeterminerWord(PD_EITHER) =>
                determiner.isInstanceOf[SilUnlimitedDeterminer]
              case SprPredefDeterminerWord(PD_NEITHER_DETERMINER) =>
                (determiner == DETERMINER_NONE)
              case _ => false
            }
          }
          case _ => false
        }
      }
      case _ => false
    }
  }

  override def analyzePronounReference(
    leaf : SprSyntaxLeaf)
      : SilPronounReference =
  {
    val lemma = leaf.lemma
    val (person, count, gender, _, proximityOpt, _, politeness) =
      tongue.analyzePronoun(lemma)
    val proximity = proximityOpt.getOrElse {
      val seq = context.wordLabeler.labelWords(
        Seq(tupleN((lemma, lemma, 0))),
        foldEphemeralLabels = false)
      assert(seq.size == 1)
      if (seq.head.head.label == LABEL_PRP_REFLEXIVE) {
        PROXIMITY_REFLEXIVE
      } else {
        PROXIMITY_ENTITY
      }
    }
    annotator.pronounRef(
      person, gender, count,
      context.genderAnalyzer, proximity, politeness, Some(getWord(leaf)))
  }

  override def analyzeConditionalSentence(
    tree : SprSyntaxTree,
    conjunction : SilWord,
    antecedentSentence : SilSentence,
    consequentSentence : SilSentence,
    biconditional : Boolean,
    formality : SilFormality) : SilSentence =
  {
    if (!antecedentSentence.tam.isPositive) {
      return SilUnrecognizedSentence(tree)
    }
    antecedentSentence.tam.modality match {
      case MODAL_NEUTRAL =>
      case _ => {
        // Oooooo....modal logic.  Maybe one day.
        return SilUnrecognizedSentence(tree)
      }
    }
    val consequentPredicate = consequentSentence match {
      case SilPredicateSentence(predicate, _, _) => predicate
      case _ => {
        return SilUnrecognizedSentence(tree)
      }
    }
    val antecedentPredicate = antecedentSentence match {
      case SilPredicateSentence(predicate, _, _) => predicate
      case SilConjunctiveSentence(
        DETERMINER_ALL,
        Seq(
          SilPredicateSentence(p1, t1, _),
          SilPredicateSentence(p2, t2, _)),
        separator
      ) if (!biconditional && !t2.isProgressive) => {
        // FIXME this rewrite, if it happens at all, should be done
        // downstream, not here!
        val conditional = SilConditionalSentence(
          conjunction,
          p1,
          p2,
          t1,
          t2.withModality(MODAL_POSSIBLE).withPolarity(t2.isNegative),
          biconditional,
          formality)
        val otherwiseModifier = SilBasicVerbModifier(
          SprPredefWord(PD_OTHERWISE))
        val cp = consequentPredicate.withNewModifiers(
            consequentPredicate.getModifiers :+ otherwiseModifier)
        val modifiedConsequent = SilPredicateSentence(
          cp,
          consequentSentence.tam,
          consequentSentence.formality)
        // this grossness can only be eliminated by moving the
        // entire transformation downstream
        conditional.rememberSyntaxTree(tree)
        modifiedConsequent.rememberSyntaxTree(tree)
        cp.asInstanceOf[SilTransformedPhrase].rememberSyntaxTree(tree)
        otherwiseModifier.rememberSyntaxTree(tree)
        return SilConjunctiveSentence(
          DETERMINER_ABSENT,
          Seq(
            conditional,
            modifiedConsequent
          ),
          SEPARATOR_SEMICOLON
        )
      }
      case _ => {
        return SilUnrecognizedSentence(tree)
      }
    }
    SilConditionalSentence(
      conjunction,
      antecedentPredicate,
      consequentPredicate,
      antecedentSentence.tam,
      consequentSentence.tam,
      biconditional,
      formality
    )
  }

  protected def extractAdposition(preTerminal : SprSyntaxTree)
      : Option[SilAdposition] =
  {
    preTerminal match {
      case adp : SprSyntaxAdposition => {
        val leaf = adp.child
        if (tongue.isAdposition(getWord(leaf).inflected)) {
          Some(SilAdposition(getWord(adp.child)))
        } else {
          None
        }
      }
      case pt : SprSyntaxPreTerminal => {
        SprPredefWord.unapply(getWord(pt.child)) match {
          case Some(amw : SprAdpositionPredef) =>
            Some(SprPredefAdposition(amw))
          case _ => None
        }
      }
      case _ => None
    }
  }

  override def expectBasicVerbModifier(
    preTerminal : SprSyntaxPreTerminal)
      : SilVerbModifier =
  {
    SilBasicVerbModifier(getWord(preTerminal.child))
  }

  protected def expectRelativeReference(
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
        annotator.stateSpecifiedRef(reference, state)
      }
      case _ => {
        SilUnrecognizedReference(syntaxTree)
      }
    }
  }

  protected def expectVerbModifiers(seq : Seq[SprSyntaxTree]) =
  {
    seq.map(expectVerbModifier)
  }

  protected def expectStatePredicate(
    syntaxTree : SprSyntaxTree,
    subject : SilReference, verb : SilWord, state : SilState,
    specifiedState : SilState = SilNullState(),
    verbModifiers : Seq[SilVerbModifier] = Seq.empty) =
  {
    SipUnresolvedStatePredicate(
      syntaxTree, subject, verb, state, specifiedState, verbModifiers)
  }

  protected def expectActionPredicate(
    syntaxTree : SprSyntaxTree,
    subject : SilReference, verb : SilWord,
    directObject : Option[SilReference],
    adpositionObject : Option[SilReference],
    verbModifiers : Seq[SilVerbModifier]) =
  {
    SipUnresolvedActionPredicate(
      syntaxTree, subject, verb, directObject,
      adpositionObject, verbModifiers)
  }

  protected def expectRelationshipPredicate(
    syntaxTree : SprSyntaxTree,
    subject : SilReference,
    complement : SilReference,
    verb : SilWord,
    verbModifiers : Seq[SilVerbModifier]) =
  {
    SipUnresolvedRelationshipPredicate(
      syntaxTree, subject, complement, verb, verbModifiers)
  }

  protected def expectReference(
    seq : Seq[SprSyntaxTree]) : SilReference =
  {
    SipExpectedReference(SptNP(seq.toSeq:_*))
  }

  protected def expectReference(np : SprSyntaxTree)
      : SipExpectedReference =
  {
    SipExpectedReference(np)
  }

  protected def expectNounReference(
    syntaxTree : SprSyntaxTree,
    preTerminal : SprSyntaxTree,
    determiner : SilDeterminer) =
  {
    SipExpectedNounlikeReference(syntaxTree, preTerminal, determiner)
  }

  override def expectPropertyState(
    syntaxTree : SprSyntaxTree) =
  {
    SipExpectedPropertyState(syntaxTree)
  }

  protected def expectVerbModifier(
    tree : SprSyntaxTree) =
  {
    SipExpectedVerbModifier(tree)
  }

  override def expectTemporalVerbModifier(tmod : SptTMOD)
      : SilVerbModifier =
  {
    SilAdpositionalVerbModifier(
      SprPredefAdposition(PD_ADVERBIAL_TMP),
      expectReference(tmod.child))
  }

  override def expectBasicVerbModifier(
    compound : SptRBC)
      : SilVerbModifier =
  {
    SilBasicVerbModifier(getCompoundWord(compound))
  }

  override def getCompoundWord(tree : SprSyntaxTree) : SilCompoundWord =
  {
    val preTerminals = tree.children.map(_.asInstanceOf[SprSyntaxPreTerminal])
    val simpleWords = preTerminals.map(
      pt => getWord(pt.child))
    SilCompoundWord(simpleWords)
  }

  protected def expectExistenceState(syntaxTree : SprSyntaxTree) =
  {
    SipExpectedExistenceState(syntaxTree)
  }

  override def expectComplementState(
    tree : SprSyntaxTree) : SipExpectedComplementState =
  {
    if (isSinglePhrase(tree.children)) {
      expectComplementState(tree.firstChild)
    } else {
      SipExpectedComplementState(tree)
    }
  }

  protected def maybeRecognizeParticle(
    pt : SprSyntaxTree) : Option[SprSyntaxTree] =
  {
    pt match {
      case phrase @ (_ : SptPRT | _ : SptPP) => {
        Some(phrase.firstChild)
      }
      case rp : SptRP => Some(rp)
      case _ => None
    }
  }

  protected def isSinglePhrase(seq : Seq[SprSyntaxTree]) =
  {
    (seq.size == 1) && !seq.head.isPreTerminal && !seq.head.isLeaf
  }

  protected def unwrapSinglePhrase(seq : Seq[SprSyntaxTree]) =
  {
    if (isSinglePhrase(seq)) {
      seq.head.children
    } else {
      seq
    }
  }

  override def getCount(tree : SprSyntaxTree) : SilCount =
  {
    if (tree.label.endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  override def getWord(leaf : SprSyntaxLeaf) =
  {
    SilSimpleWord(leaf.foldedToken, leaf.lemma)
  }

  protected def extractTense(verbHead : SprSyntaxTree, tam : SilTam) : SilTam =
  {
    if (verbHead.isVerbPastTense) {
      tam.past
    } else if (verbHead.isVerbFutureTense) {
      tam.future
    } else {
      tam
    }
  }

  protected def splitCommas(components : Seq[SprSyntaxTree])
      : (Seq[Seq[SprSyntaxTree]], SilSeparator) =
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

  protected def splitSemicolons(components : Seq[SprSyntaxTree])
      : (Seq[Seq[SprSyntaxTree]], SilSeparator) =
  {
    val pos = components.indexWhere(_.isSemicolon)
    if (pos == -1) {
      (Seq(components), SEPARATOR_CONJOINED)
    } else {
      val prefix = components.take(pos)
      val suffix = components.drop(pos + 1)
      val (subSplit, subSeparator) = splitSemicolons(suffix)
      (Seq(prefix) ++ subSplit, SEPARATOR_SEMICOLON)
    }
  }

  override def specifyReference(
    ref : SilReference, specifiedState : SilState) : SilReference =
  {
    if (specifiedState == SilNullState()) {
      ref
    } else {
      val specifiedReference = annotator.stateSpecifiedRef(
        ref, specifiedState)
      SprUtils.maybeSyntaxTree(ref).foreach(
        refSyntaxTree => SprUtils.maybeSyntaxTree(specifiedState).foreach(
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

  protected def rememberSyntheticNP(
    reference : SilTransformedPhrase,
    seq : Seq[SprSyntaxTree]) : Unit =
  {
    reference.rememberSyntaxTree(SptNP(seq.toSeq:_*))
  }

  protected def rememberSyntheticADJP(
    reference : SilTransformedPhrase,
    seq : Seq[SprSyntaxTree]) : Unit =
  {
    reference.rememberSyntaxTree(SptADJP(seq.toSeq:_*))
  }

  protected def rememberPredicateInflection(
    predicate : SilPredicate,
    verbInflection : SilVerbInflection) : Unit =
  {
    predicate.setInflectedAttributes(verbInflection)
  }

  protected def rememberPredicateInflection(
    predicate : SilPredicate,
    verbHead : SprSyntaxTree) : Unit =
  {
    rememberPredicateInflection(predicate, getVerbInflection(verbHead))
  }

  protected def rememberPredicateInflection(
    predicate : SilPredicate,
    verbHead : SprSyntaxTree,
    tam : SilTam,
    auxInflection : SilVerbInflection) : Unit =
  {
    if (tam.requiresAux) {
      rememberPredicateInflection(predicate, auxInflection)
    } else {
      rememberPredicateInflection(predicate, verbHead)
    }
  }

  protected def getVerbInflection(verb : SprSyntaxTree) : SilVerbInflection

  override def isNounPhraseModifier(
    tree : SprSyntaxTree, head : SprSyntaxTree) : Boolean =
  {
    if (head.isInstanceOf[SptNNQ]) {
      false
    } else if (tree.isAdjectival) {
      true
    } else if (tree.isNoun) {
      tupleN((tree, head)) match {
        case (n1 : SprSyntaxNoun, n2 : SprSyntaxNoun) => {
          n1.isProper == n2.isProper
        }
        case _ => true
      }
    } else {
      false
    }
  }

  override def isNounPhraseHead(
    tree : SprSyntaxTree) : Boolean =
  {
    if (tree.isInstanceOf[SptNNQ]) {
      false
    } else {
      strictness match {
        case SPR_STRICTNESS_TIGHT => {
          tree.isNoun || tree.isGerund
        }
        case SPR_STRICTNESS_LOOSE => {
          tree.isNoun || tree.isAdjectival
        }
      }
    }
  }
}
