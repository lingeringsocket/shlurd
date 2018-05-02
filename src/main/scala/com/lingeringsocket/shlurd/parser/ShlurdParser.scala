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

import ShlurdParseUtils._

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
  tree : ShlurdSyntaxTree, tokens : Seq[String], lemmas : Seq[String],
  guessedQuestion : Boolean)
    extends ShlurdParser
{
  private def expectParticle(
    pt : ShlurdSyntaxTree) : Option[ShlurdWord] =
  {
    pt.label match {
      case "PP" | "PRT" => Some(getWord(pt.firstChild.firstChild))
      case "RP" => Some(getWord(pt.firstChild))
      case _ => None
    }
  }

  private def extractParticle(seq : Seq[ShlurdSyntaxTree])
      : (Option[ShlurdWord], Seq[ShlurdSyntaxTree]) =
  {
    seq.indexWhere(_.isParticle) match {
      case -1 => {
        val pp = seq.last
        if (pp.isPrepositionalPhrase) {
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
    pt : ShlurdSyntaxTree, determiner : ShlurdDeterminer) : Boolean =
  {
    val leaf = pt.unwrapPhrase
    if (leaf.isDeterminer || leaf.isCoordinatingConjunction ||
      leaf.isAdverb)
    {
      leaf.firstChild.lemma match {
        case LEMMA_BOTH => (determiner == DETERMINER_ALL)
        case LEMMA_EITHER => (determiner == DETERMINER_ANY)
        case LEMMA_NEITHER => (determiner == DETERMINER_NONE)
        case _ => false
      }
    } else {
      false
    }
  }

  private def expectRoot(tree : ShlurdSyntaxTree, guessedQuestion : Boolean) =
  {
    tree match {
      case SptROOT(sentence) => {
        expectSentence(sentence, guessedQuestion)
      }
      case _ => ShlurdUnknownSentence
    }
  }

  private def truncatePunctuation(
    tree : ShlurdSyntaxTree, punctuationMarks : Iterable[String])
      : Seq[ShlurdSyntaxTree] =
  {
    val children = tree.children
    if (punctuationMarks.exists(punctuation =>
      children.last.hasTerminalLabel(".", punctuation)))
    {
      children.dropRight(1)
    } else {
      children
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
      (expectPrepositionalState(seq(i).children),
        seq.take(i) ++ seq.drop(i + 1))
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

  private def expectSentence(
    tree : ShlurdSyntaxTree, guessedQuestion : Boolean)
      : ShlurdSentence =
  {
    val forceSQ = tree.firstChild.firstChild.isBeingVerb
    if (tree.isSentence && !forceSQ) {
      val hasQuestionMark = tree.children.last.hasTerminalLabel(".", "?")
      val isQuestion =
        hasQuestionMark && !guessedQuestion
      val force = {
        if (tree.children.last.hasTerminalLabel(".", "!")) {
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
        if (np.isNounPhrase && vp.isVerbPhrase) {
          expectPredicateSentence(
            np, vp, isQuestion, force, MODAL_NEUTRAL, false)
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else if (forceSQ || tree.isSubQuestion) {
      val punctless = truncatePunctuation(tree, Seq("?"))
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
          if (verbHead.isRelationshipVerb) {
            if (!np.isExistential && verbHead.isExistsVerb) {
              ShlurdUnknownSentence
            } else {
              val (negativeSub, predicate) =
                expectPredicate(np, ap.children, ap, specifiedState,
                  extractRelationship(verbHead))
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
    } else if (tree.isSBARQ) {
      val children = truncatePunctuation(tree, Seq("?"))
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
        ShlurdUnknownSentence
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
        val question = expectQuestion(seq.head) match {
          case Some(q) => q
          case _ => return ShlurdUnknownSentence
        }
        val np = question match {
          case QUESTION_WHO => {
            SptNP(ShlurdSyntaxNode("NN", seq.head.children))
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
        val (negativeSub, predicate) = expectPredicate(
          np, complementRemainder, complement.head, combinedState,
          REL_IDENTITY)
        ShlurdPredicateQuery(
          predicate, question,
          ShlurdInterrogativeMood(!(negativeSuper ^ negativeSub)))
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def extractRelationship(
    verbHead : ShlurdSyntaxTree) : ShlurdRelationship =
  {
    if (verbHead.isPossessionVerb) {
      REL_ASSOCIATION
    } else {
      assert(verbHead.isBeingVerb)
      REL_IDENTITY
    }
  }

  private def expectQuestion(
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
      (modalityFor(intro.firstChild.lemma), remainder)
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

  private def expectPredicateSentence(
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
        val modality = modalityFor(verbHead.firstChild.lemma)
        expectPredicateSentence(
          np, vpSub, isQuestion, force, modality, negative)
      } else {
        ShlurdUnknownSentence
      }
    } else if (verbHead.isRelationshipVerb) {
      if ((vpChildren.size > 2) || (!np.isExistential && verbHead.isExistsVerb))
      {
        ShlurdUnknownSentence
      } else {
        val (specifiedState, vpRemainder) =
          extractPrepositionalState(vpChildren)
        val complement = vpRemainder.last
        val (negativeComplement, predicate) = expectPredicate(
          np, complement.children, complement, specifiedState,
          extractRelationship(verbHead))
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
    vp : ShlurdSyntaxTree, formality : ShlurdFormality) : ShlurdSentence =
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
    seq : Seq[ShlurdSyntaxTree],
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

  private def expectReference(
    seqIn : Seq[ShlurdSyntaxTree]) : ShlurdReference =
  {
    if (seqIn.isEmpty) {
      return ShlurdUnknownReference
    }
    val seq = seqIn.map(_.unwrapPhrase)
    if (seq.size == 1) {
      return expectReference(seq.head)
    }
    if (seq.head.lastChild.isPossessive) {
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
      val first = seq.head.unwrapPhrase
      if (first.isDeterminer) {
        (expectDeterminer(first.firstChild), seq.drop(1))
      } else {
        (DETERMINER_UNSPECIFIED, seq)
      }
    }
    if ((components.size == 2) && components.head.isPronoun) {
      val pronounReference = pronounFor(
        components.head.firstChild.lemma)
      val entityReference = expectNounReference(components.last, determiner)
      ShlurdGenitiveReference(pronounReference, entityReference)
    } else if (components.last.isCompoundPrepositionalPhrase) {
      ShlurdStateSpecifiedReference(
        expectReference(seqIn.dropRight(1)),
        expectPrepositionalState(components.last.children))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = expectReference(components.head)
      expectRelativeQualifier(components.last) match {
        case Some(qualifiers) => {
          ShlurdReference.qualified(entityReference, qualifiers)
        }
        case _ => {
          ShlurdUnknownReference
        }
      }
    } else if (components.forall(c => c.isNoun || c.isAdjectival)) {
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

  private def expectReference(np : ShlurdSyntaxTree) : ShlurdReference =
  {
    if (np.isNounPhrase) {
      if (np.numChildren == 1) {
        expectReference(np.firstChild)
      } else {
        expectReference(np.children)
      }
    } else if (np.isNoun) {
      ShlurdEntityReference(
        getWord(np.firstChild),
        DETERMINER_UNSPECIFIED,
        getCount(np))
    } else if (np.isPronoun) {
      pronounFor(np.firstChild.lemma)
    } else {
      ShlurdUnknownReference
    }
  }

  private def expectRelativeQualifier(
    tree : ShlurdSyntaxTree) : Option[Seq[ShlurdWord]] =
  {
    tree match {
      case SptSBAR(
        SptWHNP(SptWDT(_)),
        SptS(SptVP(verb, complement))
      ) if (verb.isBeingVerb) => {
        val state = expectStateComplement(SptVP(complement))
        state match {
          case ShlurdPropertyState(qualifier) => {
            Some(Seq(qualifier))
          }
          case _ => None
        }
      }
      case _ => None
    }
  }

  private def modalityFor(lemma : String) =
  {
    lemma match {
      case LEMMA_MUST => MODAL_MUST
      case LEMMA_MAY => MODAL_MAY
      case LEMMA_COULD | LEMMA_CAN => MODAL_CAPABLE
      case LEMMA_MIGHT => MODAL_POSSIBLE
      case LEMMA_SHOULD => MODAL_SHOULD
      case LEMMA_DO => MODAL_EMPHATIC
      case _ => MODAL_NEUTRAL
    }
  }

  private def pronounFor(lemma : String) =
  {
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

  private def expectDeterminer(leaf : ShlurdSyntaxTree) : ShlurdDeterminer =
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

  private def expectNounReference(
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
      ShlurdUnknownReference
    }
  }

  private def getCount(tree : ShlurdSyntaxTree) : ShlurdCount =
  {
    if (tree.label.endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  private def getWord(leaf : ShlurdSyntaxTree) =
  {
    ShlurdWord(leaf.token, leaf.lemma)
  }

  private def expectPropertyState(ap : ShlurdSyntaxTree) =
  {
    if (ap.isPreTerminal) {
      ShlurdPropertyState(getWord(ap.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPrepositionalState(
    seq : Seq[ShlurdSyntaxTree]) : ShlurdState =
  {
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
        case _ => return ShlurdUnknownState
      }
      ShlurdLocationState(locative, expectReference(seq.last))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPredicate(
    np : ShlurdSyntaxTree,
    complement : Seq[ShlurdSyntaxTree],
    complementPhrase : ShlurdSyntaxTree,
    specifiedState : ShlurdState,
    relationship : ShlurdRelationship)
      : (Boolean, ShlurdPredicate) =
  {
    val (negative, seq) = extractNegative(complement)
    if (np.isExistential) {
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
    } else if (complementPhrase.isNounPhrase) {
      val relationshipPredicate = ShlurdRelationshipPredicate(
        specifyReference(expectReference(np), specifiedState),
        expectReference(seq),
        relationship)
      (negative, relationshipPredicate)
    } else {
      val state = splitCoordinatingConjunction(seq) match {
        case (DETERMINER_UNSPECIFIED, _, _) => {
          expectStateComplement(
            ShlurdSyntaxRewrite.recompose(complementPhrase, seq))
        }
        case (determiner, separator, split) => {
          ShlurdConjunctiveState(
            determiner,
            split.map(
              subseq => expectStateComplement(
                ShlurdSyntaxRewrite.recompose(complementPhrase, subseq))),
            separator)
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

  private def expectStateComplement(complement : ShlurdSyntaxTree)
      : ShlurdState =
  {
    if (isSinglePhrase(complement.children)) {
      return expectStateComplement(complement.firstChild)
    }
    complement match {
      case SptADJP(children @ _*) => {
        expectPropertyStateComplement(children)
      }
      case phrase @ (_: SptADVP | _: SptPP) => {
        val seq = phrase.children
        if ((seq.head.isPreposition || seq.head.isAdverb) && (seq.size > 1) &&
          (!seq.exists(_.isPrepositionalPhrase)))
        {
          expectPrepositionalState(seq)
        } else {
          expectPropertyStateComplement(seq)
        }
      }
      case SptVP(children @ _*) => {
        // TODO:  ambiguity for action (passive construction) vs
        // state (participial adjective)
        expectPropertyStateComplement(children)
      }
      case SptPRT(particle) => {
        expectPropertyState(particle)
      }
      case _ => {
        ShlurdUnknownState
      }
    }
  }

  private def expectPropertyStateComplement(
    seq : Seq[ShlurdSyntaxTree]) : ShlurdState =
  {
    val state = expectPropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      ShlurdConjunctiveState(
        DETERMINER_UNSPECIFIED,
        Seq(state) ++ seq.tail.map(
          component => expectStateComplement(component)))
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

class CorenlpTreeWrapper(
  corenlp : Tree, tokens : Seq[String], lemmas : Seq[String])
    extends ShlurdAbstractSyntaxTree
{
  private val wrappedChildren =
    corenlp.children.map(new CorenlpTreeWrapper(_, tokens, lemmas))

  override def label =
    corenlp.label.value.split("-").head

  override def lemma =
    lemmas(corenlp.label.asInstanceOf[HasIndex].index).toLowerCase

  override def token = tokens(corenlp.label.asInstanceOf[HasIndex].index)

  override def children = wrappedChildren
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

  private def prepareOne(
    sentence : Sentence, dump : Boolean = false) : ShlurdParser =
  {
    val tokens = sentence.originalTexts.asScala
    val sentenceString = sentence.text
    if (Set(".", "?", "!").contains(tokens.last)) {
      prepareFallbacks(
        sentenceString, tokens, false, dump, "PUNCTUATED")
    } else {
      val questionString = sentenceString + "?"
      prepareFallbacks(
        questionString, tokens :+ "?", true, dump, "GUESSED QUESTION")
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
    val capitalizedString = capitalize(sentenceString)
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
    val corenlp = sentence.parse(props)
    if (dump) {
        println(dumpPrefix + " PARSE = " + corenlp)
    }
    corenlp.indexLeaves(0, true)
    val lemmas = sentence.lemmas.asScala
    val syntaxTree = ShlurdSyntaxRewrite.rewriteAbstract(
      new CorenlpTreeWrapper(corenlp, tokens, lemmas))
    val rewrittenTree = ShlurdSyntaxRewrite.rewriteEither(syntaxTree)
    if (dump) {
        println(dumpPrefix + " REWRITE = " + rewrittenTree)
    }
    new ShlurdSingleParser(rewrittenTree, tokens, lemmas, guessedQuestion)
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
