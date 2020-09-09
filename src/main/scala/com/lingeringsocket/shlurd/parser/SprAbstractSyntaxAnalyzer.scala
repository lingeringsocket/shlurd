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
        val otherwiseModifier = SilBasicVerbModifier(SilMagicWord(MW_OTHERWISE))
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

  protected def expectStatePredicate(
    syntaxTree : SprSyntaxTree,
    subject : SilReference, verb : SilWord, state : SilState,
    specifiedState : SilState = SilNullState(),
    verbModifiers : Seq[SilVerbModifier] = Seq.empty) =
  {
    SilUnresolvedStatePredicate(
      syntaxTree, subject, verb, state, specifiedState, verbModifiers)
  }

  protected def expectActionPredicate(
    syntaxTree : SprSyntaxTree,
    subject : SilReference, verb : SilWord,
    directObject : Option[SilReference],
    adpositionObject : Option[SilReference],
    verbModifiers : Seq[SilVerbModifier]) =
  {
    SilUnresolvedActionPredicate(
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
    SilUnresolvedRelationshipPredicate(
      syntaxTree, subject, complement, verb, verbModifiers)
  }

  protected def expectReference(
    seq : Seq[SprSyntaxTree]) : SilReference =
  {
    SilExpectedReference(SptNP(seq:_*))
  }

  protected def expectReference(np : SprSyntaxTree)
      : SilExpectedReference =
  {
    SilExpectedReference(np)
  }

  protected def expectNounReference(
    syntaxTree : SprSyntaxTree,
    preTerminal : SprSyntaxTree,
    determiner : SilDeterminer) =
  {
    SilExpectedNounlikeReference(syntaxTree, preTerminal, determiner)
  }

  override def expectPropertyState(
    syntaxTree : SprSyntaxTree) =
  {
    SilExpectedPropertyState(syntaxTree)
  }

  protected def expectVerbModifier(
    tree : SprSyntaxTree) =
  {
    SilExpectedVerbModifier(tree)
  }

  override def expectTemporalVerbModifier(tmod : SptTMOD)
      : SilVerbModifier =
  {
    SilAdpositionalVerbModifier(
      SilAdposition(MW_ADVERBIAL_TMP),
      expectReference(tmod.child))
  }

  override def expectBasicVerbModifier(
    preTerminal : SprSyntaxPreTerminal)
      : SilVerbModifier =
  {
    SilBasicVerbModifier(getWord(preTerminal.child))
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
    SilExpectedExistenceState(syntaxTree)
  }

  override def expectComplementState(
    tree : SprSyntaxTree) : SilExpectedComplementState =
  {
    if (isSinglePhrase(tree.children)) {
      expectComplementState(tree.firstChild)
    } else {
      SilExpectedComplementState(tree)
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

  protected def rememberSyntheticNP(
    reference : SilTransformedPhrase,
    seq : Seq[SprSyntaxTree])
  {
    reference.rememberSyntaxTree(SptNP(seq:_*))
  }

  protected def rememberSyntheticADJP(
    reference : SilTransformedPhrase,
    seq : Seq[SprSyntaxTree])
  {
    reference.rememberSyntaxTree(SptADJP(seq:_*))
  }

  protected def rememberPredicateCount(
    predicate : SilPredicate,
    count : SilCount)
  {
    predicate.setInflectedCount(count)
  }

  protected def rememberPredicateCount(
    predicate : SilPredicate,
    verbHead : SprSyntaxTree)
  {
    rememberPredicateCount(predicate, getVerbCount(verbHead))
  }

  protected def rememberPredicateCount(
    predicate : SilPredicate,
    verbHead : SprSyntaxTree,
    tam : SilTam,
    auxCount : SilCount)
  {
    if (tam.requiresAux) {
      rememberPredicateCount(predicate, auxCount)
    } else {
      rememberPredicateCount(predicate, verbHead)
    }
  }

  protected def getVerbCount(verb : SprSyntaxTree) : SilCount

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
