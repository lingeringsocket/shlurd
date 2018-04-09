// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.io._
import scala.collection.JavaConverters._

import java.io._
import java.util.Properties

trait ShlurdParser
{
  def parseOne() : ShlurdSentence

  def parseFirst() : ShlurdSentence

  def parseAll() : Seq[ShlurdSentence]
}

class ShlurdFallbackParser(
  parsers : Seq[() => ShlurdParser])
    extends ShlurdParser
{
  override def parseOne() : ShlurdSentence =
  {
    var sentence : ShlurdSentence = ShlurdUnknownSentence
    parsers.foreach(parserSupplier => {
      val parser = parserSupplier()
      sentence = parser.parseOne
      if (!sentence.hasUnknown) {
        return sentence
      }
    })
    sentence
  }

  override def parseFirst() = parseOne

  override def parseAll() = Seq(parseOne)
}

class ShlurdSingleParser(
  tree : Tree, tokens : Seq[String], lemmas : Seq[String],
  guessedQuestion : Boolean)
    extends ShlurdParser with ShlurdParseUtils
{
  override def getLemma(leaf : Tree) : String =
  {
    lemmas(leaf.label.asInstanceOf[HasIndex].index).toLowerCase
  }

  override def getToken(leaf : Tree) : String =
  {
    tokens(leaf.label.asInstanceOf[HasIndex].index)
  }

  private def expectParticle(pt : Tree) : Option[ShlurdWord] =
  {
    getLabel(pt) match {
      case "PP" | "PRT" => Some(getWord(pt.firstChild.firstChild))
      case "RP" => Some(getWord(pt.firstChild))
      case _ => None
    }
  }

  private def extractParticle(seq : Seq[Tree])
      : (Option[ShlurdWord], Seq[Tree]) =
  {
    seq.indexWhere(isParticle(_)) match {
      case -1 => {
        val pp = seq.last
        if (isPrepositionalPhrase(pp)) {
          (expectParticle(pp), seq.dropRight(1) ++ pp.children.drop(1))
        } else {
          (None, seq)
        }
      }
      case i => {
        (expectParticle(seq(i)), seq.patch(i, Seq.empty, 1))
      }
    }
  }
  private def isCoordinatingDeterminer(
    pt : Tree, determiner : ShlurdDeterminer) : Boolean =
  {
    val leaf = unwrapPhrase(pt)
    if (isDeterminer(leaf) || isCoordinatingConjunction(leaf) ||
      isAdverb(leaf))
    {
      getLemma(leaf.firstChild) match {
        case "both" => (determiner == DETERMINER_ALL)
        case "either" => (determiner == DETERMINER_ANY)
        case "neither" => (determiner == DETERMINER_NONE)
        case _ => false
      }
    } else {
      false
    }
  }

  private def expectRoot(tree : Tree, guessedQuestion : Boolean) =
  {
    if (hasLabel(tree, "ROOT")) {
      assert(tree.numChildren == 1)
      expectSentence(tree.firstChild, guessedQuestion)
    } else {
      ShlurdUnknownSentence
    }
  }

  private def truncatePunctuation(
    tree : Tree, punctuationMarks : Iterable[String]) : Seq[Tree] =
  {
    val children = tree.children
    if (punctuationMarks.exists(punctuation =>
      hasTerminalLabel(children.last, ".", punctuation)))
    {
      children.dropRight(1)
    } else {
      children
    }
  }

  private def extractPrepositionalState(seq : Seq[Tree])
      : (ShlurdState, Seq[Tree])=
  {
    val i = seq.indexWhere(isCompoundPrepositionalPhrase(_))
    if (i == -1) {
      val last2 = seq.takeRight(2)
      if (last2.map(getLabel) == Seq("ADVP", "NP")) {
        val rewrite = new LabeledScoredTreeNode(last2.head.label)
        rewrite.setChildren(Array(last2.head.firstChild, last2.last))
        (ShlurdNullState(), seq.dropRight(2) :+ rewrite)
      } else {
        (ShlurdNullState(), seq)
      }
    } else {
      (expectPrepositionalState(seq(i).children),
        seq.take(i) ++ seq.drop(i + 1))
    }
  }

  private def expectSentence(tree : Tree, guessedQuestion : Boolean)
      : ShlurdSentence =
  {
    val forceSQ = isCopula(tree.firstChild.firstChild)
    if (hasLabel(tree, "S") && !forceSQ) {
      val hasQuestionMark = hasTerminalLabel(tree.children.last, ".", "?")
      val isQuestion =
        hasQuestionMark && !guessedQuestion
      val force = {
        if (hasTerminalLabel(tree.children.last, ".", "!")) {
          FORCE_EXCLAMATION
        } else {
          FORCE_NEUTRAL
        }
      }
      val children = truncatePunctuation(tree, Seq(".", "!", "?"))
      if (isImperative(children)) {
        expectCommand(children.head, ShlurdFormality(force))
      } else if (children.size == 2) {
        val np = children.head
        val vp = children.last
        if (isNounPhrase(np) && isVerbPhrase(vp)) {
          expectPredicateSentence(
            np, vp, isQuestion, force, MODAL_NEUTRAL, false)
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else if (forceSQ || isSubQuestion(tree)) {
      val punctless = truncatePunctuation(tree, Seq("?"))
      val (specifiedState, children) = {
        val unwrapped : Seq[Tree] = {
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
        return expectCommand(children.head, ShlurdFormality.DEFAULT)
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
          if (isCopula(verbHead)) {
            if (!isExistential(np) && isExists(verbHead)) {
              ShlurdUnknownSentence
            } else {
              val (negativeSub, predicate) =
                expectPredicate(np, ap.children, getLabel(ap), specifiedState)
              val positive = !(negative ^ negativeSub)
              ShlurdPredicateSentence(
                predicate,
                ShlurdInterrogativeMood(positive, modality))
            }
          } else {
            ShlurdUnknownSentence
          }
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else if (hasLabel(tree, "SBARQ")) {
      val children = truncatePunctuation(tree, Seq("?"))
      val first = children.head
      val second = children.last
      val secondUnwrapped = {
        if ((second.numChildren == 1) && isVerbPhrase(second.firstChild)) {
          second.firstChild.children
        } else {
          second.children
        }
      }
      val (negativeSuper, secondSub) = extractNegative(secondUnwrapped)
      if ((children.size != 2) ||
        !isQueryNoun(first) ||
        !isSubQuestion(second) ||
        !isCopula(secondSub.head))
      {
        ShlurdUnknownSentence
      } else {
        // FIXME support modality
        val (specifiedState, whnpc) = extractPrepositionalState(first.children)
        val seq : Seq[Tree] = {
          if ((whnpc.size == 1) && isQueryNoun(whnpc.head)) {
            whnpc.head.children
          } else {
            whnpc
          }
        }
        val question = expectQuestion(seq.head) match {
          case Some(q) => q
          case _ => return ShlurdUnknownSentence
        }
        val np = new LabeledScoredTreeNode(new StringLabel("NP"))
        np.setChildren(seq.tail.toArray)
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
        val (negativeSub, predicate) = expectPredicate(
            np, complementRemainder, "VP", combinedState)
        ShlurdPredicateQuery(
          predicate, question,
          ShlurdInterrogativeMood(!(negativeSuper ^ negativeSub)))
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectQuestion(tree : Tree) : Option[ShlurdQuestion] =
  {
    if (hasLabel(tree, "WHADJP")) {
      if (tree.numChildren != 2) {
        None
      } else {
        if (hasTerminalLemma(tree.firstChild, "how") &&
          hasTerminalLemma(tree.lastChild, "many"))
        {
          Some(QUESTION_HOW_MANY)
        } else {
          None
        }
      }
    } else if (hasLabel(tree, "WDT")) {
      getLemma(tree.firstChild) match {
        case "which" | "what" => Some(QUESTION_WHICH)
        case _ => None
      }
    } else {
      None
    }
  }

  private def extractModality(seq : Seq[Tree]) : (ShlurdModality, Seq[Tree]) =
  {
    val intro = seq.head
    if (isModal(intro)) {
      val suffix = seq.drop(1)
      val remainder : Seq[Tree] = {
        if (isSinglePhrase(suffix) && isVerbPhrase(suffix.head)) {
          suffix.head.children
        } else {
          suffix
        }
      }
      (modalityFor(getLemma(intro.firstChild)), remainder)
    } else {
      (MODAL_NEUTRAL, seq)
    }
  }

  private def extractNegative(seq : Seq[Tree]) : (Boolean, Seq[Tree]) =
  {
    val pos = seq.map(unwrapPhrase).indexWhere(
      sub => isAdverb(sub) && hasTerminalLemma(sub, "not"))
    if (pos == -1) {
      (false, seq)
    } else {
      (true, seq.patch(pos, Seq.empty, 1))
    }
  }

  private def expectPredicateSentence(
    np : Tree, vp : Tree, isQuestion : Boolean,
    force : ShlurdForce, modality : ShlurdModality,
    negativeSuper : Boolean) : ShlurdSentence =
  {
    val (negativeSub, vpChildren) = extractNegative(vp.children)
    // FIXME:  representation for double negatives?
    val negative = negativeSuper ^ negativeSub
    val verbHead = vpChildren.head
    if (isModal(verbHead)) {
      val vpSub = vpChildren.last
      if ((vpChildren.size == 2) && isVerbPhrase(vpSub)) {
        val modality = modalityFor(getLemma(verbHead.firstChild))
        expectPredicateSentence(
          np, vpSub, isQuestion, force, modality, negative)
      } else {
        ShlurdUnknownSentence
      }
    } else if (isCopula(verbHead)) {
      if ((vpChildren.size > 2) || (!isExistential(np) && isExists(verbHead)))
      {
        ShlurdUnknownSentence
      } else {
        val (specifiedState, vpRemainder) =
          extractPrepositionalState(vpChildren)
        val complement = vpRemainder.last
        val (negativeComplement, predicate) = expectPredicate(
          np, complement.children, getLabel(complement), specifiedState)
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
      ShlurdUnknownSentence
    }
  }

  private def composeAmbiguity(alternatives : Seq[ShlurdSentence])
      : ShlurdSentence =
  {
    if (alternatives.isEmpty) {
      return ShlurdUnknownSentence
    }
    val dedup = alternatives.distinct
    if (dedup.size == 1) {
      return dedup.head
    }
    val clean = dedup.filterNot(_.hasUnknown)
    if (clean.isEmpty) {
      return ShlurdAmbiguousSentence(dedup)
    }
    if (clean.size == 1) {
      return clean.head
    }
    return ShlurdAmbiguousSentence(clean)
  }

  private def expectCommand(
    vp : Tree, formality : ShlurdFormality) : ShlurdSentence =
  {
    val alternative1 = {
      val (particle, unparticled) =
        extractParticle(vp.children)
      val (specifiedState, seq) =
        extractPrepositionalState(unparticled)
      expectCommand(particle, specifiedState, seq, formality)
    }

    val alternative2 = {
      val (specifiedState, unspecified) =
        extractPrepositionalState(vp.children)
      val (particle, seq) =
        extractParticle(unspecified)
      expectCommand(particle, specifiedState, seq, formality)
    }

    composeAmbiguity(Seq(alternative1, alternative2))
  }

  private def expectCommand(
    particle : Option[ShlurdWord],
    specifiedState : ShlurdState,
    seq : Seq[Tree],
    formality : ShlurdFormality) : ShlurdSentence =
  {
    if (seq.size == 2) {
      val state = particle match {
        // FIXME:  restrict verb pairing when particle is present
        case Some(word) => ShlurdPropertyState(word)
        case _ => expectPropertyState(seq.head)
      }
      val subject = specifyReference(
        expectReference(seq.last), specifiedState)
      ShlurdStateChangeCommand(
        ShlurdStatePredicate(subject, state),
        formality)
    } else {
      ShlurdUnknownSentence
    }
  }

  private def splitCommas(components : Seq[Tree])
      : (Seq[Seq[Tree]], ShlurdSeparator) =
  {
    val pos = components.indexWhere(t => isComma(t))
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

  private def splitCoordinatingConjunction(components : Seq[Tree])
      : (ShlurdDeterminer, ShlurdSeparator, Seq[Seq[Tree]]) =
  {
    if (isSinglePhrase(components)) {
      return splitCoordinatingConjunction(components.head.children)
    }
    val pos = components.indexWhere(t => isCoordinatingConjunction(t), 1)
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
      val cc = components(pos)
      var determiner = expectDeterminer(cc.firstChild)
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

  private def specifyReference(
    ref : ShlurdReference, specifiedState : ShlurdState) : ShlurdReference =
  {
    if (specifiedState == ShlurdNullState()) {
      ref
    } else {
      ShlurdStateSpecifiedReference(ref, specifiedState)
    }
  }

  private def expectReference(seqIn : Seq[Tree]) : ShlurdReference =
  {
    val seq = seqIn.map(unwrapPhrase(_))
    if (seq.size == 1) {
      return expectReference(seq.head)
    }
    if (isPossessive(seq.head.lastChild)) {
      return ShlurdGenitiveReference(
        expectReference(seq.head.children.dropRight(1)),
        expectReference(seq.tail))
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_UNSPECIFIED, _, _) => {
      }
      case (determiner, separator, split) => {
        return ShlurdConjunctiveReference(
          determiner, split.map(expectReference(_)), separator)
      }
    }
    val (determiner, components) = {
      val first = unwrapPhrase(seq.head)
      if (isDeterminer(first)) {
        (expectDeterminer(first.firstChild), seq.drop(1))
      } else {
        (DETERMINER_UNSPECIFIED, seq)
      }
    }
    if ((components.size == 2) && isPronoun(components.head)) {
      val pronounReference = pronounFor(
        getLemma(components.head.firstChild))
      val entityReference = expectNounReference(components.last, determiner)
      ShlurdGenitiveReference(pronounReference, entityReference)
    } else if (isCompoundPrepositionalPhrase(components.last)) {
      ShlurdStateSpecifiedReference(
        expectReference(seqIn.dropRight(1)),
        expectPrepositionalState(components.last.children))
    } else if ((components.size == 2) && isNounPhrase(components.head)) {
      val entityReference = expectReference(components.head)
      expectRelativeQualifier(components.last) match {
        case Some(qualifiers) => {
          ShlurdReference.qualified(entityReference, qualifiers)
        }
        case _ => {
          ShlurdUnknownReference
        }
      }
    } else if (components.forall(c => isNoun(c) || isAdjectival(c))) {
      val entityReference = expectNounReference(components.last, determiner)
      if (components.size > 1) {
        ShlurdReference.qualified(
          entityReference,
          components.dropRight(1).map(c => getWord(c.firstChild)))
      } else {
        entityReference
      }
    } else {
      ShlurdUnknownReference
    }
  }

  private def expectReference(np : Tree) : ShlurdReference =
  {
    if (isNounPhrase(np)) {
      if (np.numChildren == 1) {
        expectReference(np.firstChild)
      } else {
        expectReference(np.children)
      }
    } else if (isNoun(np)) {
      ShlurdEntityReference(
        getWord(np.firstChild),
        DETERMINER_UNSPECIFIED,
        getCount(np))
    } else if (isPronoun(np)) {
      pronounFor(getLemma(np.firstChild))
    } else {
      ShlurdUnknownReference
    }
  }

  private def expectRelativeQualifier(tree : Tree) : Option[Seq[ShlurdWord]] =
  {
    if (!hasLabel(tree, "SBAR") || (tree.numChildren != 2)) {
      return None
    }
    val whnp = tree.firstChild
    if (!isQueryNoun(whnp) || (whnp.numChildren != 1)) {
      return None
    }
    val wdt = whnp.firstChild
    if (!hasLabel(wdt, "WDT") || !wdt.isPreTerminal) {
      return None
    }
    val sub = tree.lastChild
    if (!hasLabel(sub, "S") || (sub.numChildren != 1)) {
      return None
    }
    val vp = sub.firstChild
    if (!isVerbPhrase(vp) || (vp.numChildren != 2)) {
      return None
    }
    if (!isCopula(vp.firstChild)) {
      return None
    }
    val state = expectStateComplement(Seq(vp.lastChild), getLabel(vp))
    state match {
      case ShlurdPropertyState(qualifier) => {
        Some(Seq(qualifier))
      }
      case _ => None
    }
  }

  private def modalityFor(lemma : String) =
  {
    lemma match {
      case "must" => MODAL_MUST
      case "may" => MODAL_MAY
      case "could" | "can" => MODAL_CAPABLE
      case "might" => MODAL_POSSIBLE
      case "should" => MODAL_SHOULD
      case _ => MODAL_NEUTRAL
    }
  }

  private def pronounFor(lemma : String) =
  {
    val person = lemma match {
      case "i" | "me" | "we" | "my" | "our" | "mine" | "ours" => PERSON_FIRST
      case "you" | "your" | "yours" => PERSON_SECOND
      case _ => PERSON_THIRD
    }
    val count = lemma match {
      case "we" | "us" | "they" | "our" | "their" => COUNT_PLURAL
      case _ => COUNT_SINGULAR
    }
    val gender = lemma match {
      case "he" | "him" | "his" => GENDER_M
      case "she" | "her" | "hers" => GENDER_F
      case _ => GENDER_N
    }
    ShlurdPronounReference(person, gender, count)
  }

  private def expectDeterminer(leaf : Tree) : ShlurdDeterminer =
  {
    getLemma(leaf) match {
      case "no" | "neither" | "nor" => DETERMINER_NONE
      case "both" | "and" | "all" | "every" => DETERMINER_ALL
      case "a" => DETERMINER_NONSPECIFIC
      case "the" | "either" => DETERMINER_UNIQUE
      case "some" => DETERMINER_SOME
      case _ => DETERMINER_ANY
    }
  }

  private def expectNounReference(
    pt : Tree, determiner : ShlurdDeterminer) =
  {
    // we allow mislabeled adjectives to handle
    // cases like "roll up the blind"
    if (isNoun(pt) || isAdjectival(pt)) {
      val noun = pt.firstChild
      ShlurdEntityReference(
        getWord(noun),
        determiner,
        getCount(pt))
    } else {
      ShlurdUnknownReference
    }
  }

  private def getCount(tree : Tree) : ShlurdCount =
  {
    if (getLabel(tree).endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  private def getWord(leaf : Tree) =
  {
    ShlurdWord(getToken(leaf), getLemma(leaf))
  }

  private def expectPropertyState(ap : Tree) =
  {
    if (ap.isPreTerminal) {
      ShlurdPropertyState(getWord(ap.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPrepositionalState(seq : Seq[Tree]) : ShlurdState =
  {
    val prep = unwrapPhrase(seq.head)
    if ((seq.size == 2) && (isPreposition(prep) || isAdverb(prep))) {
      val prepLemma = getLemma(prep.firstChild)
      val locative = prepLemma match {
        case "in" | "inside" | "within" => LOC_INSIDE
        case "outside" => LOC_OUTSIDE
        case "at" => LOC_AT
        case "near" | "nearby" => LOC_NEAR
        case "on" => LOC_ON
        case "above" | "over" => LOC_ABOVE
        case "below" | "under" | "beneath" | "underneath" => LOC_BELOW
        case "behind" => LOC_BEHIND
        case _ => return ShlurdUnknownState
      }
      ShlurdLocationState(locative, expectReference(seq.last))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPredicate(
    np : Tree, complement : Seq[Tree], label : String,
    specifiedState : ShlurdState = ShlurdNullState())
      : (Boolean, ShlurdPredicate) =
  {
    val (negative, seq) = extractNegative(complement)
    if (isExistential(np)) {
      val subject = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          specifyReference(expectReference(seq), specifiedState)
        }
        case (determiner, separator, split) => {
          ShlurdConjunctiveReference(
            determiner,
            split.map(x => specifyReference(
              expectReference(x), specifiedState)),
            separator)
        }
      }
      (negative, ShlurdStatePredicate(subject, ShlurdExistenceState()))
    } else if (label == "NP") {
      val identityPredicate = ShlurdIdentityPredicate(
        specifyReference(expectReference(np), specifiedState),
        expectReference(seq))
      (negative, identityPredicate)
    } else {
      val state = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          expectStateComplement(seq, label)
        }
        case (determiner, separator, split) => {
          ShlurdConjunctiveState(
            determiner, split.map(expectStateComplement(_, label)), separator)
        }
      }
      state match {
        case ShlurdConjunctiveState(DETERMINER_UNSPECIFIED, states, _) => {
          val propertyState = states.head
          val fullySpecifiedState = {
            if (specifiedState == ShlurdNullState()) {
              if (states.size == 2) {
                states.last
              } else {
                ShlurdConjunctiveState(DETERMINER_ALL, states.tail)
              }
            } else {
              ShlurdConjunctiveState(
                DETERMINER_ALL, Seq(specifiedState) ++ states.tail)
            }
          }
          val subject = specifyReference(
            expectReference(np), fullySpecifiedState)
          (negative, ShlurdStatePredicate(subject, propertyState))
        }
        case _ => {
          val subject = specifyReference(expectReference(np), specifiedState)
          (negative, ShlurdStatePredicate(subject, state))
        }
      }
    }
  }

  private def expectStateComplement(seq : Seq[Tree], label : String)
      : ShlurdState =
  {
    if (isSinglePhrase(seq)) {
      val sub = seq.head
      return expectStateComplement(sub.children, getLabel(sub))
    }
    label match {
      case "ADJP" => {
        expectPropertyStateComplement(seq)
      }
      case "ADVP" | "PP" => {
        if ((isPreposition(seq.head) || isAdverb(seq.head)) && (seq.size > 1) &&
          (!seq.exists(isPrepositionalPhrase)))
        {
          expectPrepositionalState(seq)
        } else {
          expectPropertyStateComplement(seq)
        }
      }
      case "VP" => {
        // TODO:  ambiguity for action (passive construction) vs
        // state (participial adjective)
        expectPropertyStateComplement(seq)
      }
      case "PRT" => {
        if (seq.size == 1) {
          expectPropertyState(seq.head)
        } else {
          ShlurdUnknownState
        }
      }
      case _ => {
        ShlurdUnknownState
      }
    }
  }

  private def expectPropertyStateComplement(seq : Seq[Tree]) : ShlurdState =
  {
    val state = expectPropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      ShlurdConjunctiveState(
        DETERMINER_UNSPECIFIED,
        Seq(state) ++ seq.tail.map(
          component => expectStateComplement(
            component.children, getLabel(component))))
    }
  }

  override def parseOne() = expectRoot(tree, guessedQuestion)

  override def parseFirst() = parseOne

  override def parseAll() = Seq(parseOne)
}

class ShlurdMultipleParser(singles : Seq[ShlurdParser])
    extends ShlurdParser
{
  override def parseOne() : ShlurdSentence =
  {
    assert(singles.size == 1)
    parseFirst
  }

  override def parseFirst() = singles.head.parseOne

  override def parseAll() = singles.map(_.parseOne)
}

object ShlurdParser
{
  def getEmptyDocument() = new Document("")

  def debug(s : String)
  {
    tokenize(s).foreach(sentence => {
      val parser = prepareOne(sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }

  private def tokenize(input : String) : Seq[Sentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala
  }

  private def newParser(
    sentence : Sentence, tokens : Seq[String],
    tree : Tree, guessedQuestion : Boolean)
      : ShlurdSingleParser =
  {
    tree.indexLeaves(0, true)
    val lemmas = sentence.lemmas.asScala
    new ShlurdSingleParser(tree, tokens, lemmas, guessedQuestion)
  }

  private def prepareOne(
    sentence : Sentence, dump : Boolean = false) : ShlurdParser =
  {
    val tokens = sentence.originalTexts.asScala
    val sentenceString = sentence.text
    if (Set(".", "?", "!").contains(tokens.last)) {
      prepareFallbacks(sentenceString, tokens, false, dump, "PUNCTUATED")
    } else {
      val questionString = sentenceString + "?"
      prepareFallbacks(questionString, tokens, true, dump, "GUESSED QUESTION")
    }
  }

  private def prepareFallbacks(
    sentenceString : String, tokens : Seq[String],
    guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    val props = new Properties
    props.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    val props2 = new Properties
    props2.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
    val props3 = new Properties
    props3.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
    val capitalizedString = ShlurdParseUtils.capitalize(sentenceString)
    def main() = prepareParser(
      capitalizedString, tokens, props, true, guessedQuestion,
      dump, dumpPrefix + " RNN")
    def fallback2() = prepareParser(
      capitalizedString, tokens, props2, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR")
    def fallback3() = prepareParser(
      capitalizedString, tokens, props3, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK PCFG")
    def fallback4() = prepareParser(
      sentenceString, tokens, props2, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR CAPITALIZED")
    new ShlurdFallbackParser(Seq(
      main, fallback2, fallback3, fallback4))
  }

  private def prepareParser(
    sentenceString : String, tokens : Seq[String], props : Properties,
    needDependencies : Boolean, guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    val sentence = tokenize(sentenceString).head
    if (needDependencies) {
      // It's important to analyze dependencies BEFORE parsing in
      // order to get the best parse
      analyzeDependencies(sentence)
    }
    val tree = sentence.parse(props)
    if (dump) {
        println(dumpPrefix + " PARSE = " + tree)
    }
    newParser(sentence, tokens, tree, guessedQuestion)
  }

  private def analyzeDependencies(sentence : Sentence) =
  {
    val props = new Properties
    props.setProperty(
      "depparse.model",
      "edu/stanford/nlp/models/parser/nndep/english_SD.gz")
    sentence.dependencyGraph(props)
  }

  def getResourcePath(resource : String) =
    getClass.getResource(resource).getPath

  def getResourceFile(resource : String) =
    new File(getResourcePath(resource))

  def readResource(resource : String) : String =
    Source.fromFile(getResourcePath(resource)).
      getLines.mkString("\n")

  def apply(input : String) : ShlurdParser =
  {
    val sentences = tokenize(input)
    if (sentences.size == 1) {
      prepareOne(sentences.head)
    } else {
      new ShlurdMultipleParser(sentences.map(prepareOne(_)))
    }
  }
}
