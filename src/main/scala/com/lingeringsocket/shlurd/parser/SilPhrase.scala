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

import scala.collection._

trait ShlurdEntity
{
  def isTentative : Boolean = false
}

trait ShlurdProperty
{
}

sealed trait SilPhrase
{
  def children : Seq[SilPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)

  def hasUnresolved : Boolean = children.exists(_.hasUnresolved)

  def isUninterpretable : Boolean =
    hasUnknown || children.exists(_.isUninterpretable)

  override def toString = ShlurdPrettyPrinter.prettyPrint(this)

  def maybeSyntaxTree : Option[ShlurdSyntaxTree] = None

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
  def mood : SilMood

  def formality : SilFormality
}

sealed trait SilPredicate extends SilPhrase
{
  def getSubject : SilReference

  private var inflectedCount : SilCount = COUNT_SINGULAR

  def getInflectedCount = inflectedCount

  def setInflectedCount(count : SilCount)
  {
    inflectedCount = count
  }

  def getModifiers : Seq[SilVerbModifier] = Seq.empty
}

sealed trait SilReference extends SilPhrase
{
  def acceptsSpecifiers : Boolean = true
}

sealed trait SilState extends SilPhrase
{
}

sealed trait SilVerbModifier extends SilPhrase
{
  override def isUninterpretable = true
}

sealed trait SilUnknownPhrase extends SilPhrase
{
  override def hasUnknown = true

  def syntaxTree : ShlurdSyntaxTree

  override def maybeSyntaxTree = Some(syntaxTree)

  override def countUnknownSyntaxLeaves : Int =
  {
    syntaxTree.countLeaves
  }
}

sealed trait SilUnknownSentence
    extends SilSentence with SilUnknownPhrase
{
  override def mood = MOOD_INDICATIVE_POSITIVE

  override def formality = SilFormality.DEFAULT
}

sealed trait SilUnknownPredicate
    extends SilPredicate with SilUnknownPhrase
{
  override def getSubject : SilReference = SilUnrecognizedReference(syntaxTree)
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
  private var syntaxTreeOpt : Option[ShlurdSyntaxTree] = None

  private[parser] def rememberSyntaxTree(syntaxTree : ShlurdSyntaxTree)
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

case class SilUnrecognizedSentence(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownSentence with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedPredicate(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownPredicate with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedReference(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownReference with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedState(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownState with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedVerbModifier(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownVerbModifier with SilUnrecognizedPhrase
{
}

case class SilExpectedSentence(
  syntaxTree : ShlurdSyntaxTree,
  forceSQ : Boolean = false
) extends SilUnknownSentence with SilUnresolvedPhrase
{
}

case class SilExpectedPredicate(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
}

case class SilExpectedReference(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedNounlikeReference(
  syntaxTree : ShlurdSyntaxTree,
  preTerminal : ShlurdSyntaxTree,
  determiner : SilDeterminer
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedComplementState(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedAdpositionalState(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedPropertyState(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedExistenceState(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedVerbModifier(
  syntaxTree : ShlurdSyntaxTree
) extends SilUnknownVerbModifier with SilUnresolvedPhrase
{
}

case class SilUnresolvedStatePredicate(
  syntaxTree : ShlurdSyntaxTree,
  subject : SilReference,
  state : SilState,
  specifiedState : SilState,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, state) ++ modifiers
}

case class SilUnresolvedActionPredicate(
  syntaxTree : ShlurdSyntaxTree,
  subject : SilReference,
  action : SilWord,
  directObject : Option[SilReference],
  indirectObject : Option[SilReference],
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ indirectObject ++ modifiers
}

case class SilUnresolvedRelationshipPredicate(
  syntaxTree : ShlurdSyntaxTree,
  subject : SilReference,
  complement : SilReference,
  relationship : SilRelationship,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers
}

case class SilPredicateSentence(
  predicate : SilPredicate,
  mood : SilMood = MOOD_INDICATIVE_POSITIVE,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)
}

case class SilConditionalSentence(
  antecedent : SilPredicate,
  consequent : SilPredicate,
  mood : SilMood = MOOD_INDICATIVE_POSITIVE,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(antecedent, consequent)
}

case class SilStateChangeCommand(
  predicate : SilPredicate,
  changeVerb : Option[SilWord] = None,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)

  override def mood = MOOD_IMPERATIVE
}

case class SilPredicateQuery(
  predicate : SilPredicate,
  question : SilQuestion,
  mood : SilMood,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)
}

case class SilConjunctiveSentence(
  determiner : SilDeterminer,
  sentences : Seq[SilSentence],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilSentence
{
  override def children = sentences

  // not really sure there's any more meaningful implementation
  override def mood = sentences.head.mood

  override def formality = sentences.head.formality
}

case class SilAmbiguousSentence(
  alternatives : Seq[SilSentence],
  done : Boolean = false
) extends SilTransformedPhrase with SilSentence
{
  override def children = alternatives

  override def mood = alternatives.head.mood

  override def formality = alternatives.head.formality

  def isRipe = !hasUnresolved && !done
}

case class SilStatePredicate(
  subject : SilReference,
  state : SilState,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, state) ++ modifiers
}

case class SilRelationshipPredicate(
  subject : SilReference,
  complement : SilReference,
  relationship : SilRelationship,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers
}

case class SilActionPredicate(
  subject : SilReference,
  action : SilWord,
  directObject : Option[SilReference] = None,
  indirectObject : Option[SilReference] = None,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ indirectObject ++ modifiers
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
          case SilNounReference(word, _, _) => {
            if (word.lemma.forall(Character.isDigit)) {
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

  override def acceptsSpecifiers = children.exists(_.acceptsSpecifiers)
}

case class SilNounReference(
  noun : SilWord,
  determiner : SilDeterminer = DETERMINER_UNSPECIFIED,
  count : SilCount = COUNT_SINGULAR
) extends SilTransformedPhrase with SilReference
{
}

case class SilResolvedReference[E<:ShlurdEntity](
  entities : Set[E],
  noun : SilWord,
  determiner : SilDeterminer
) extends SilTransformedPhrase with SilReference
{
  override def acceptsSpecifiers = false
}

case class SilExistenceState(
) extends SilTransformedPhrase with SilState
{
}

case class SilNullState(
) extends SilTransformedPhrase with SilState
{
  override def hasUnknown = true
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
}

case class SilBasicVerbModifier(
  words : Seq[SilWord]
) extends SilTransformedPhrase with SilVerbModifier
{
}

case class SilAdpositionalVerbModifier(
  adposition : SilAdposition,
  objRef : SilReference
) extends SilAdpositionalPhrase with SilVerbModifier
{
}

case class SilWord(
  inflected : String,
  lemmaUnfolded : String)
{
  def lemma = lemmaUnfolded.toLowerCase

  def isProper = lemmaUnfolded.head.isUpper
}

object SilWord
{
  def apply(s : String) : SilWord = SilWord(s, s)
}

object SilReference
{
  def isCountCoercible(reference : SilReference) : Boolean =
  {
    reference match {
      case _ : SilPronounReference =>
        false
      case SilNounReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_NONE => false
          case DETERMINER_UNSPECIFIED => false
          case DETERMINER_UNIQUE => false
          case _ => true
        }
      }
      case _ : SilConjunctiveReference =>
        false
      case SilStateSpecifiedReference(reference, _) =>
        isCountCoercible(reference)
      case _ : SilGenitiveReference => true
      case _ : SilResolvedReference[_] => false
      case _ : SilUnknownReference => false
    }
  }

  def getCount(reference : SilReference) : SilCount =
  {
    reference match {
      case SilPronounReference(_, _, count, _) =>
        count
      case SilNounReference(_, _, count) =>
        count
      case SilConjunctiveReference(determiner, _, _) => {
        determiner match {
          case DETERMINER_ALL => COUNT_PLURAL
          // DETERMINER_NONE is debatable
          case _ => COUNT_SINGULAR
        }
      }
      case SilStateSpecifiedReference(reference, _) =>
        getCount(reference)
      case SilGenitiveReference(_, possessee) =>
        getCount(possessee)
      case SilResolvedReference(entities, _, _) => {
        if (entities.size < 2) {
          COUNT_SINGULAR
        } else {
          COUNT_PLURAL
        }
      }
      case _ : SilUnknownReference => COUNT_SINGULAR
    }
  }

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
    qualifiedByProperties(reference, qualifiers.map(SilPropertyState(_)))
  }

  def extractAdpositionSpecifiers(state : SilState)
      : Seq[SilAdpositionalState] =
  {
    state match {
      case SilConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractAdpositionSpecifiers(_))
      case adp : SilAdpositionalState => Seq(adp)
      case SilNullState() | SilPropertyState(_) |
          SilExistenceState() => Seq.empty
      case _ => {
        assert(false)
        Seq.empty
      }
    }
  }

  def extractQualifiers(state : SilState) : Seq[SilWord] =
  {
    state match {
      case SilConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractQualifiers(_))
      case SilPropertyState(state) => Seq(state)
      case SilNullState() | SilAdpositionalState(_, _) |
          SilExistenceState() => Seq.empty
      case _ => {
        assert(false)
        Seq.empty
      }
    }
  }
}
