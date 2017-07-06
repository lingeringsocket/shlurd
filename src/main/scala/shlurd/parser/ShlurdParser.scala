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
package shlurd.parser

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.collection.JavaConverters._

trait ShlurdParser
{
  def parseOne() : ShlurdSentence

  def parseFirst() : ShlurdSentence

  def parseAll() : Seq[ShlurdSentence]
}

class ShlurdSingleParser(
  tree : Tree, lemmas : Seq[String], implicitQuestion : Boolean)
    extends ShlurdParser
{
  private def getLabel(tree : Tree) : String =
  {
    tree.label.value
  }

  private def hasLabel(tree : Tree, label : String) : Boolean =
  {
    getLabel(tree) == label
  }

  private def isVerb(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("VB")
  }

  private def isParticipleOrGerund(verbal : Tree) : Boolean =
  {
    getLabel(verbal) match {
      case "VBG" | "VBN" => {
        true
      }
      case _ => {
        false
      }
    }
  }

  private def isNounPhrase(np : Tree) : Boolean =
  {
    hasLabel(np, "NP")
  }

  private def isVerbPhrase(vp : Tree) : Boolean =
  {
    hasLabel(vp, "VP")
  }

  private def isNoun(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("NN")
  }

  private def isPronoun(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("PRP")
  }

  private def isAdjective(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("JJ")
  }

  private def isAdverb(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("RB")
  }

  private def isPreposition(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("IN")
  }

  private def isDeterminer(pt : Tree) : Boolean =
  {
    getLabel(pt) == "DT"
  }

  private def isCoordinatingDeterminer(
    pt : Tree, determiner : ShlurdDeterminer) : Boolean =
  {
    if (isDeterminer(pt)) {
      getLemma(pt.firstChild) match {
        case "both" => (determiner == DETERMINER_ALL)
        case "either" => (determiner == DETERMINER_ANY)
        case "neither" => (determiner == DETERMINER_NONE)
        case _ => false
      }
    } else {
      false
    }
  }

  private def isComma(pt : Tree) : Boolean =
  {
    getLabel(pt) == ","
  }

  private def isCoordinatingConjunction(pt : Tree) : Boolean =
  {
    getLabel(pt) == "CC"
  }

  private def hasTerminalLabel(
    tree : Tree, label : String, terminalLabel : String) : Boolean =
  {
    tree.isPreTerminal && hasLabel(tree, label) &&
      hasLabel(tree.firstChild, terminalLabel)
  }

  private def expectRoot(tree : Tree, implicitQuestion : Boolean) =
  {
    if (hasLabel(tree, "ROOT")) {
      assert(tree.numChildren == 1)
      expectSentence(tree.firstChild, implicitQuestion)
    } else {
      ShlurdUnknownSentence
    }
  }

  private def truncatePunctuation(
    tree : Tree, punctuationMarks : Iterable[String]) : Array[Tree] =
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

  private def isImperative(children : Array[Tree]) =
  {
    (children.size == 1) && hasLabel(children.head, "VP")
  }

  private def expectSentence(tree : Tree, implicitQuestion : Boolean) =
  {
    if (hasLabel(tree, "S")) {
      val isQuestion =
        hasTerminalLabel(tree.children.last, ".", "?") && !implicitQuestion
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
          expectPredicateSentence(np, vp, isQuestion, force)
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else if (hasLabel(tree, "SQ")) {
      val children = truncatePunctuation(tree, Seq("?"))
      if (isImperative(children)) {
        expectCommand(children.head, ShlurdFormality.DEFAULT)
      } else  if (children.size == 3) {
        val verbHead = children(0)
        val np = children(1)
        val ap = children(2)
        if (isVerb(verbHead) && hasTerminalLemma(verbHead, "be")) {
          ShlurdPredicateSentence(
            expectPredicate(np, ap),
            MOOD_INTERROGATIVE)
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectPredicateSentence(
    np : Tree, vp : Tree, isQuestion : Boolean, force : ShlurdForce) =
  {
    val verbHead = vp.firstChild
    if (isVerb(verbHead) && hasTerminalLemma(verbHead, "be")) {
      if (vp.numChildren > 3) {
        ShlurdUnknownSentence
      } else {
        val predicate = expectPredicate(np, vp.lastChild)
        if (isQuestion) {
          ShlurdPredicateSentence(
            predicate, MOOD_INTERROGATIVE, ShlurdFormality(force))
        } else {
          val mood = {
            if (vp.numChildren == 3) {
              val adverb = vp.children()(1)
              if (isAdverb(adverb) && hasTerminalLemma(adverb, "not")) {
                MOOD_INDICATIVE_NEGATIVE
              } else {
                MOOD_INDICATIVE_POSITIVE
              }
            } else {
              MOOD_INDICATIVE_POSITIVE
            }
          }
          ShlurdPredicateSentence(predicate, mood, ShlurdFormality(force))
        }
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectCommand(vp : Tree, formality : ShlurdFormality) =
  {
    if (vp.numChildren == 2) {
      val state = expectVerbState(vp.firstChild)
      val subject = expectReference(vp.lastChild)
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

  private def isSinglePhrase(seq : Seq[Tree]) : Boolean =
  {
    if (seq.size == 1) {
      getLabel(seq.head).endsWith("P")
    } else {
      false
    }
  }

  private def splitCoordinatingConjunction(components : Seq[Tree])
      : (ShlurdDeterminer, ShlurdSeparator, Seq[Seq[Tree]]) =
  {
    if (isSinglePhrase(components)) {
      return splitCoordinatingConjunction(components.head.children)
    }
    val pos = components.indexWhere(t => isCoordinatingConjunction(t))
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

  private def expectReference(seq : Seq[Tree]) : ShlurdReference =
  {
    if (seq.size == 1) {
      return expectReference(seq.head)
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
      val first = seq.head
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
    } else if (components.forall(c => isNoun(c) || isAdjective(c))) {
      val entityReference = expectNounReference(components.last, determiner)
      if (components.size > 1) {
        ShlurdQualifiedReference(
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
      case "both" | "and" | "all" => DETERMINER_ALL
      case "a" => DETERMINER_NONSPECIFIC
      case "the" | "either" => DETERMINER_UNIQUE
      case _ => DETERMINER_ANY
    }
  }

  private def expectNounReference(
    pt : Tree, determiner : ShlurdDeterminer) =
  {
    // we allow mislabeled adjectives to handle
    // cases like "roll up the blind"
    if (isNoun(pt) || isAdjective(pt)) {
      val noun = pt.firstChild
      ShlurdEntityReference(
        getWord(noun),
        determiner,
        getCount(pt))
    } else {
      ShlurdUnknownReference
    }
  }

  private def hasTerminalLemma(tree : Tree, lemma : String) =
  {
    tree.isPreTerminal && (getLemma(tree.firstChild) == lemma)
  }

  private def getLemma(leaf : Tree) : String =
  {
    lemmas(leaf.label.asInstanceOf[HasIndex].index).toLowerCase
  }

  private def getCount(tree : Tree) : ShlurdCount =
  {
    if (getLabel(tree).endsWith("S")) {
      COUNT_PLURAL
    } else {
      COUNT_SINGULAR
    }
  }

  private def getWord(tree : Tree) =
  {
    ShlurdWord(getLabel(tree), getLemma(tree))
  }

  private def expectVerbState(pt : Tree) =
  {
    if (pt.isPreTerminal && isVerb(pt)) {
      ShlurdPropertyState(getWord(pt.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectAdjectiveState(ap : Tree) =
  {
    // sometimes adverbs sneak into adjectival phrases,
    // e.g. is the lion asleep?
    if ((isAdjective(ap) || isParticipleOrGerund(ap) || isAdverb(ap))
      && ap.isPreTerminal)
    {
      ShlurdPropertyState(getWord(ap.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectAdverbState(ap : Tree) =
  {
    if (isAdverb(ap) && ap.isPreTerminal) {
      ShlurdPropertyState(getWord(ap.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPrepositionalState(seq : Seq[Tree]) : ShlurdState =
  {
    val prep = seq.head
    if ((seq.size == 2) && isPreposition(prep)) {
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

  private def expectPredicate(np : Tree, complement : Tree) =
  {
    val subject = expectReference(np)
    val label = getLabel(complement)
    val state = splitCoordinatingConjunction(complement.children) match {
      case (DETERMINER_UNSPECIFIED, _, _) => {
        expectStateComplement(complement.children, label)
      }
      case (determiner, separator, split) => {
        ShlurdConjunctiveState(
          determiner, split.map(expectStateComplement(_, label)), separator)
      }
    }
    ShlurdStatePredicate(subject, state)
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
        expectAdjectiveState(seq.head)
      }
      case "ADVP" => {
        if (seq.size == 1) {
          expectAdverbState(seq.head)
        } else {
          expectPrepositionalState(seq)
        }
      }
      case "VP" => {
        // TODO:  ambiguity for action (passive construction) vs
        // state (participial adjective)
        if ((seq.size == 1) && seq.head.isPreTerminal) {
          expectVerbState(seq.head)
        } else {
          ShlurdUnknownState
        }
      }
      case _ => {
        ShlurdUnknownState
      }
    }
  }

  override def parseOne() = expectRoot(tree, implicitQuestion)

  override def parseFirst() = parseOne

  override def parseAll() = Seq(parseOne)
}

class ShlurdMultipleParser(singles : Seq[ShlurdSingleParser])
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
  private def dump(sentence : Sentence)
  {
    val tree = sentence.parse
    println("TREE = " + tree)
    Range(0, sentence.length).foreach(i => {
      println("DEP = " + sentence.incomingDependencyLabel(i))
    })
  }

  private def tokenize(input : String) : Seq[Sentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala
  }

  private def newParser(
    sentence : Sentence, tree : Tree, implicitQuestion : Boolean)
      : ShlurdSingleParser =
  {
    tree.indexLeaves(0, true)
    val lemmas = sentence.lemmas.asScala
    new ShlurdSingleParser(tree, lemmas, implicitQuestion)
  }

  private def prepareOne(sentence : Sentence) : ShlurdSingleParser =
  {
    val tree = sentence.parse
    if (tree.preTerminalYield.asScala.last.value == ".") {
      newParser(sentence, tree, false)
    } else {
      val question = tokenize(sentence.text + "?").head
      newParser(question, question.parse, true)
    }
  }

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
