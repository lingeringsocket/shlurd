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
package com.lingeringsocket.shlurd.ilang

import com.lingeringsocket.shlurd.parser._

import scala.collection._

sealed trait SilPhrase
{
  def children : Seq[SilPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)

  def hasUnresolved : Boolean = hasUnresolvedChildren

  def hasUnresolvedChildren : Boolean = children.exists(_.hasUnresolved)

  def isUninterpretable : Boolean =
    hasUnknown || children.exists(_.isUninterpretable)

  def isConjunctive : Boolean = false

  override def toString = SprPrettyPrinter.prettyPrint(this)

  def maybeSyntaxTree : Option[SprSyntaxTree] = None

  def toWordString : String =
  {
    maybeSyntaxTree match {
      case Some(syntaxTree) => syntaxTree.toWordString
      case _ => toString
    }
  }

  def countUnknownSyntaxLeaves : Int =
  {
    children.map(_.countUnknownSyntaxLeaves).sum
  }
}

sealed trait SilSentence extends SilPhrase
{
  def tam : SilTam

  def formality : SilFormality

  def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence
}

sealed trait SilPredicate extends SilPhrase
{
  def getSubject : SilReference

  def getVerb : SilWord

  private var inflectedCount : SilCount = COUNT_SINGULAR

  def getInflectedCount = inflectedCount

  def setInflectedCount(count : SilCount)
  {
    inflectedCount = count
  }

  def getModifiers : Seq[SilVerbModifier] = Seq.empty

  def withNewModifiers(newModifiers : Seq[SilVerbModifier]) : SilPredicate
}

sealed trait SilReference extends SilPhrase
{
  def acceptsSpecifiers : Boolean = true

  def descendantReferences : Set[SilReference] = {
    Set(this) ++ children.filter(_.isInstanceOf[SilReference]).
      map(_.asInstanceOf[SilReference]).flatMap(_.descendantReferences)
  }
}

sealed trait SilState extends SilPhrase
{
}

sealed trait SilVerbModifier extends SilPhrase
{
}

sealed trait SilUnknownPhrase extends SilPhrase
{
  override def hasUnknown = true

  def syntaxTree : SprSyntaxTree

  override def maybeSyntaxTree = Some(syntaxTree)

  override def countUnknownSyntaxLeaves : Int =
  {
    syntaxTree.countLeaves
  }
}

sealed trait SilUnknownSentence
    extends SilSentence with SilUnknownPhrase
{
  override def tam = SilTam.indicative

  override def formality = SilFormality.DEFAULT

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

sealed trait SilUnknownPredicate
    extends SilPredicate with SilUnknownPhrase
{
  override def getSubject : SilReference = SilUnrecognizedReference(syntaxTree)

  override def getVerb = SilWord("<UNKNOWN>")

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) = this
}

sealed trait SilUnknownReference
    extends SilReference with SilUnknownPhrase
{
}

sealed trait SilUnknownState
    extends SilState with SilUnknownPhrase
{
}

sealed trait SilUnknownVerbModifier
    extends SilVerbModifier with SilUnknownPhrase
{
}

sealed trait SilUnrecognizedPhrase extends SilPhrase
{
}

sealed trait SilUnresolvedPhrase extends SilPhrase
{
  override def hasUnresolved = true
}

abstract class SilTransformedPhrase extends SilPhrase
{
  protected var syntaxTreeOpt : Option[SprSyntaxTree] = None

  def rememberSyntaxTree(syntaxTree : SprSyntaxTree)
  {
    syntaxTreeOpt = Some(syntaxTree)
  }

  def hasSyntaxTree = !syntaxTreeOpt.isEmpty

  override def maybeSyntaxTree = syntaxTreeOpt
}

sealed trait SilAdpositionalPhrase extends SilTransformedPhrase
{
  def adposition : SilAdposition

  def objRef : SilReference

  override def children = Seq(objRef)
}

case class SilUnparsedSentence(
  text : String
) extends SilSentence with SilUnrecognizedPhrase
{
  override def hasUnknown = true

  override def toString = text

  override def toWordString = text

  override def countUnknownSyntaxLeaves = Int.MaxValue

  override def tam = SilTam.indicative

  override def formality = SilFormality.DEFAULT

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

case class SilUnrecognizedSentence(
  syntaxTree : SprSyntaxTree
) extends SilUnknownSentence with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedPredicate(
  syntaxTree : SprSyntaxTree
) extends SilUnknownPredicate with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedReference(
  syntaxTree : SprSyntaxTree
) extends SilUnknownReference with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedVerbModifier(
  syntaxTree : SprSyntaxTree
) extends SilUnknownVerbModifier with SilUnrecognizedPhrase
{
}

case class SilExpectedSentence(
  syntaxTree : SprSyntaxTree,
  forceSQ : Boolean = false
) extends SilUnknownSentence with SilUnresolvedPhrase
{
}

case class SilExpectedPredicate(
  syntaxTree : SprSyntaxTree
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
}

case class SilExpectedReference(
  syntaxTree : SprSyntaxTree
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedNounlikeReference(
  syntaxTree : SprSyntaxTree,
  preTerminal : SprSyntaxTree,
  determiner : SilDeterminer
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedComplementState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedAdpositionalState(
  syntaxTree : SprSyntaxTree,
  extracted : Boolean
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedPropertyState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedExistenceState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedVerbModifier(
  syntaxTree : SprSyntaxTree
) extends SilUnknownVerbModifier with SilUnresolvedPhrase
{
}

case class SilUnresolvedStatePredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  state : SilState,
  specifiedState : SilState,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, state, specifiedState) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilUnresolvedActionPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  directObject : Option[SilReference],
  adpositionObject : Option[SilReference],
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ adpositionObject ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilUnresolvedRelationshipPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  complement : SilReference,
  verb : SilWord,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilPredicateSentence(
  predicate : SilPredicate,
  tam : SilTam  = SilTam.indicative,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tam = newTam, formality = newFormality)
  }
}

case class SilConditionalSentence(
  conjunction : SilWord,
  antecedent : SilPredicate,
  consequent : SilPredicate,
  tamAntecedent : SilTam,
  tamConsequent : SilTam,
  biconditional : Boolean,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(antecedent, consequent)

  override def tam = tamConsequent

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tamConsequent = newTam, formality = newFormality)
  }
}

case class SilPredicateQuery(
  predicate : SilPredicate,
  question : SilQuestion,
  answerInflection : SilInflection,
  tam : SilTam,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tam = newTam, formality = newFormality)
  }
}

case class SilConjunctiveSentence(
  determiner : SilDeterminer,
  sentences : Seq[SilSentence],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilSentence
{
  override def children = sentences

  override def isConjunctive : Boolean = true

  // not really sure there's any more meaningful implementation
  override def tam = sentences.head.tam

  override def formality = sentences.head.formality

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(sentences = sentences.headOption.map(
      _.withNewTamFormality(newTam, newFormality)).toSeq ++ sentences.tail)
  }
}

case class SilAmbiguousSentence(
  alternatives : Seq[SilSentence],
  done : Boolean = false
) extends SilTransformedPhrase with SilSentence
{
  override def children = alternatives

  override def tam = alternatives.head.tam

  override def formality = alternatives.head.formality

  def isRipe = !hasUnresolvedChildren && !done

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

case class SilStatePredicate(
  subject : SilReference,
  verb : SilWord,
  state : SilState,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children = Seq(subject, state) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilRelationshipPredicate(
  subject : SilReference,
  verb : SilWord,
  complement : SilReference,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilActionPredicate(
  subject : SilReference,
  verb : SilWord,
  directObject : Option[SilReference] = None,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilParenthesizedReference(
  reference : SilReference
) extends SilTransformedPhrase with SilReference
{
  override def children = Seq(reference)

  override def acceptsSpecifiers = reference.acceptsSpecifiers
}

case class SilStateSpecifiedReference(
  reference : SilReference,
  state : SilState
) extends SilTransformedPhrase with SilReference
{
  override def children = Seq(reference, state)

  override def acceptsSpecifiers : Boolean = {
    // FIXME:  weird special case for "3 of them"
    state match {
      case SilAdpositionalState(SilAdposition.OF, pn : SilPronounReference) => {
        reference match {
          case SilNounReference(SilWordLemma(lemma), _, _) => {
            if (lemma.forall(Character.isDigit)) {
              return false
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
    reference.acceptsSpecifiers
  }
}

case class SilGenitiveReference(
  possessor : SilReference,
  possessee : SilReference
) extends SilTransformedPhrase with SilReference
{
  override def children = Seq(possessor, possessee)
}

case class SilPronounReference(
  person : SilPerson,
  gender : SilGender,
  count : SilCount,
  distance : SilDistance = DISTANCE_UNSPECIFIED
) extends SilTransformedPhrase with SilReference
{
  override def acceptsSpecifiers = false
}

case class SilConjunctiveReference(
  determiner : SilDeterminer,
  references : Seq[SilReference],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilReference
{
  override def children = references

  override def isConjunctive : Boolean = true

  override def acceptsSpecifiers = children.exists(_.acceptsSpecifiers)
}

case class SilNounReference(
  noun : SilWord,
  determiner : SilDeterminer = DETERMINER_UNSPECIFIED,
  count : SilCount = COUNT_SINGULAR
) extends SilTransformedPhrase with SilReference
{
}

case class SilMappedReference(
  key : String,
  determiner : SilDeterminer
) extends SilUnknownReference
{
  override def syntaxTree = SprSyntaxLeaf(key, key, key)
}

case class SilQuotationReference(
  quotation : String
) extends SilTransformedPhrase with SilReference
{
}

case class SilExistenceState(
  existentialPronoun : Option[SilWord] = None
) extends SilTransformedPhrase with SilState
{
}

case class SilNullState(
) extends SilTransformedPhrase with SilState
{
  override def hasUnknown = true
}

case class SilPropertyQueryState(
  propertyName : String
) extends SilTransformedPhrase with SilState
{
}

case class SilPropertyState(
  state : SilWord
) extends SilTransformedPhrase with SilState
{
}

case class SilAdpositionalState(
  adposition : SilAdposition,
  objRef : SilReference
) extends SilAdpositionalPhrase with SilState
{
}

case class SilConjunctiveState(
  determiner : SilDeterminer,
  states : Seq[SilState],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilState
{
  override def children = states

  override def isConjunctive : Boolean = true
}

case class SilBasicVerbModifier(
  word : SilWord
) extends SilTransformedPhrase with SilVerbModifier
{
}

case class SilDanglingVerbModifier(
  adposition : SilAdposition
) extends SilTransformedPhrase with SilVerbModifier
{
}

case class SilAdpositionalVerbModifier(
  adposition : SilAdposition,
  objRef : SilReference
) extends SilAdpositionalPhrase with SilVerbModifier
{
}

trait SilWord
{
  def senseId : String = ""

  def isProper : Boolean

  def withSense(senseId : String) : SilWord

  def decomposed : Seq[SilSimpleWord]

  def recompose(seq : Seq[String]) : String

  def toLemma : String

  def toNounLemma : String = toLemma

  def toUnfoldedLemma : String

  def toUninflected : SilWord

  def toNounUninflected = toUninflected
}

case class SilSimpleWord(
  inflected : String,
  lemmaUnfolded : String,
  override val senseId : String = ""
) extends SilWord
{
  def lemma = lemmaUnfolded.toLowerCase

  override def toUnfoldedLemma = lemmaUnfolded

  override def isProper = lemmaUnfolded.head.isUpper

  def uninflected = SilSimpleWord("", lemmaUnfolded, senseId)

  override def withSense(senseId : String) =
    SilSimpleWord(inflected, lemmaUnfolded, senseId)

  override def decomposed = Seq(this)

  override def toLemma = lemma

  override def toUninflected =
  {
    SilWord.uninflected(lemma).withSense(senseId)
  }

  override def recompose(seq : Seq[String]) =
  {
    seq.mkString(" ")
  }
}

case class SilCompoundWord(
  components : Seq[SilSimpleWord],
  style : SilCompoundStyle = COMPOUND_OPEN,
  override val senseId : String = ""
) extends SilWord
{
  override def isProper = components.exists(_.isProper)

  override def withSense(senseId : String) =
    SilCompoundWord(components, style, senseId)

  override def decomposed = components

  override def toNounLemma =
    recompose(components.dropRight(1).map(_.inflected) :+
      components.last.lemma)

  override def toLemma =
    recompose(components.map(_.lemma))

  override def toNounUninflected =
  {
    SilCompoundWord(
      components.dropRight(1) :+
        SilWord.uninflected(components.last.lemma),
      style,
      senseId)
  }

  override def toUninflected =
  {
    SilCompoundWord(
      components.map(c => SilWord.uninflected(c.lemma)),
      style,
      senseId)
  }

  override def toUnfoldedLemma =
    recompose(components.map(_.lemmaUnfolded))

  override def recompose(seq : Seq[String]) =
  {
    seq.mkString(separator)
  }

  def separator : String =
  {
    style match {
      case COMPOUND_OPEN => " "
      case COMPOUND_CLOSED => ""
      case COMPOUND_HYPHENATED => "-"
    }
  }
}

object SilWord
{
  def apply(s : String) : SilSimpleWord = SilSimpleWord(s, s)

  def apply(
    inflected : String,
    lemmaUnfolded : String,
    senseId : String = "") : SilSimpleWord =
  {
    SilSimpleWord(inflected, lemmaUnfolded, senseId)
  }

  def uninflected(s : String) = SilSimpleWord("", s)

  def withSense(s : String, senseId : String) =
    SilSimpleWord(s, s, senseId)
}

object SilWordLemma
{
  def unapply(w : SilSimpleWord) =
  {
    Some(w.lemma)
  }
}

object SilWordInflected
{
  def unapply(w : SilSimpleWord) =
  {
    Some(w.inflected)
  }
}

object SilReference
{
  def qualifiedByProperties(
    reference : SilReference,
    qualifiers : Seq[SilState])
      : SilReference =
  {
    if (qualifiers.isEmpty) {
      reference
    } else if (qualifiers.size == 1) {
      SilStateSpecifiedReference(
        reference, qualifiers.head)
    } else {
      SilStateSpecifiedReference(
        reference,
        SilConjunctiveState(
          DETERMINER_ALL,
          qualifiers,
          SEPARATOR_CONJOINED))
    }
  }

  def qualified(reference : SilReference, qualifiers : Seq[SilWord])
      : SilReference =
  {
    qualifiedByProperties(reference, qualifiers.map(SilPropertyState))
  }
}
