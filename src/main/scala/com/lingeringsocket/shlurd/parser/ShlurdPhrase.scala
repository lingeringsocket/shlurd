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

sealed trait ShlurdPhrase
{
  def children : Seq[ShlurdPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)

  def hasUnrecognized : Boolean = children.exists(_.hasUnrecognized)

  def hasUnresolved : Boolean = children.exists(_.hasUnresolved)

  override def toString = ShlurdPrettyPrinter.prettyPrint(this)
}

sealed trait ShlurdSentence extends ShlurdPhrase
{
  def mood : ShlurdMood

  def formality : ShlurdFormality
}

sealed trait ShlurdPredicate extends ShlurdPhrase
{
}

sealed trait ShlurdReference extends ShlurdPhrase
{
}

sealed trait ShlurdState extends ShlurdPhrase
{
}

sealed trait ShlurdUnknownPhrase extends ShlurdPhrase
{
  override def hasUnknown = true

  def syntaxTree : ShlurdSyntaxTree
}

sealed trait ShlurdUnknownSentence
    extends ShlurdSentence with ShlurdUnknownPhrase
{
  override def mood = MOOD_INDICATIVE_POSITIVE

  override def formality = ShlurdFormality.DEFAULT
}

sealed trait ShlurdUnknownPredicate
    extends ShlurdPredicate with ShlurdUnknownPhrase
{
}

sealed trait ShlurdUnknownReference
    extends ShlurdReference with ShlurdUnknownPhrase
{
}

sealed trait ShlurdUnknownState
    extends ShlurdState with ShlurdUnknownPhrase
{
}

sealed trait ShlurdUnrecognizedPhrase extends ShlurdPhrase
{
  override def hasUnrecognized = true
}

sealed trait ShlurdUnresolvedPhrase extends ShlurdPhrase
{
  override def hasUnresolved = true
}

case class ShlurdUnrecognizedSentence(
  syntaxTree : ShlurdSyntaxTree ,
  forceSQ : Boolean = false)
    extends ShlurdUnknownSentence with ShlurdUnrecognizedPhrase
{
}

case class ShlurdUnrecognizedPredicate(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownPredicate with ShlurdUnrecognizedPhrase
{
}

case class ShlurdUnrecognizedReference(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownReference with ShlurdUnrecognizedPhrase
{
}

case class ShlurdUnrecognizedState(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownState with ShlurdUnrecognizedPhrase
{
}

case class ShlurdExpectedSentence(
  syntaxTree : ShlurdSyntaxTree,
  forceSQ : Boolean = false)
    extends ShlurdUnknownSentence with ShlurdUnresolvedPhrase
{
}

case class ShlurdExpectedPredicate(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownPredicate with ShlurdUnresolvedPhrase
{
}

case class ShlurdExpectedReference(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownReference with ShlurdUnresolvedPhrase
{
}

case class ShlurdExpectedComplementState(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownState with ShlurdUnresolvedPhrase
{
}

case class ShlurdExpectedPrepositionalState(
  syntaxTree : ShlurdSyntaxTree)
    extends ShlurdUnknownState with ShlurdUnresolvedPhrase
{
}

case class ShlurdUnresolvedPredicate(
  syntaxTree : ShlurdSyntaxTree,
  subject : ShlurdReference,
  state : ShlurdState,
  specifiedState : ShlurdState
)
    extends ShlurdUnknownPredicate with ShlurdUnresolvedPhrase
{
}

case class ShlurdUnresolvedRelativeReference(
  syntaxTree : ShlurdSyntaxTree,
  reference : ShlurdReference,
  state : ShlurdState
)
    extends ShlurdUnknownReference with ShlurdUnresolvedPhrase
{
}

case class ShlurdPredicateSentence(
  predicate : ShlurdPredicate,
  mood : ShlurdMood = MOOD_INDICATIVE_POSITIVE,
  formality : ShlurdFormality = ShlurdFormality.DEFAULT
) extends ShlurdSentence
{
  override def children = Seq(predicate)
}

case class ShlurdStateChangeCommand(
  predicate : ShlurdStatePredicate,
  formality : ShlurdFormality = ShlurdFormality.DEFAULT
) extends ShlurdSentence
{
  override def children = Seq(predicate)

  override def mood = MOOD_IMPERATIVE
}

case class ShlurdPredicateQuery(
  predicate : ShlurdPredicate,
  question : ShlurdQuestion,
  mood : ShlurdMood,
  formality : ShlurdFormality = ShlurdFormality.DEFAULT
) extends ShlurdSentence
{
  override def children = Seq(predicate)
}


case class ShlurdAmbiguousSentence(
  alternatives : Seq[ShlurdSentence]
) extends ShlurdSentence
{
  override def children = alternatives

  override def mood = alternatives.head.mood

  override def formality = alternatives.head.formality
}

case class ShlurdStatePredicate(
  subject : ShlurdReference,
  state : ShlurdState
) extends ShlurdPredicate
{
  override def children = Seq(subject, state)
}

case class ShlurdRelationshipPredicate(
  subject : ShlurdReference,
  complement : ShlurdReference,
  relationship : ShlurdRelationship
) extends ShlurdPredicate
{
  override def children = Seq(subject, complement)
}

case class ShlurdStateSpecifiedReference(
  reference : ShlurdReference,
  state : ShlurdState
) extends ShlurdReference
{
  override def children = Seq(reference, state)
}

case class ShlurdGenitiveReference(
  genitive : ShlurdReference,
  reference : ShlurdReference
) extends ShlurdReference
{
  override def children = Seq(genitive, reference)
}

case class ShlurdPronounReference(
  person : ShlurdPerson,
  gender : ShlurdGender,
  count : ShlurdCount
) extends ShlurdReference
{
}

case class ShlurdConjunctiveReference(
  determiner : ShlurdDeterminer,
  references : Seq[ShlurdReference],
  separator : ShlurdSeparator = SEPARATOR_CONJOINED
) extends ShlurdReference
{
  override def children = references
}

case class ShlurdEntityReference(
  entity : ShlurdWord,
  determiner : ShlurdDeterminer = DETERMINER_UNSPECIFIED,
  count : ShlurdCount = COUNT_SINGULAR
) extends ShlurdReference
{
}

case class ShlurdExistenceState(
) extends ShlurdState
{
}

case class ShlurdNullState(
) extends ShlurdState
{
}

case class ShlurdPropertyState(
  state : ShlurdWord
) extends ShlurdState
{
}

case class ShlurdLocationState(
  locative : ShlurdLocative,
  location : ShlurdReference
) extends ShlurdState
{
  override def children = Seq(location)
}

case class ShlurdConjunctiveState(
  determiner : ShlurdDeterminer,
  states : Seq[ShlurdState],
  separator : ShlurdSeparator = SEPARATOR_CONJOINED
) extends ShlurdState
{
  override def children = states
}

case class ShlurdWord(
  inflected : String,
  lemma : String)
{
}

object ShlurdReference
{
  def isCountCoercible(reference : ShlurdReference) : Boolean =
  {
    reference match {
      case pr : ShlurdPronounReference =>
        false
      case ShlurdEntityReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_NONE => false
          case DETERMINER_UNSPECIFIED => false
          case DETERMINER_UNIQUE => false
          case _ => true
        }
      }
      case cr : ShlurdConjunctiveReference =>
        false
      case ShlurdStateSpecifiedReference(reference, _) =>
        isCountCoercible(reference)
      case gr : ShlurdGenitiveReference => true
      case _ : ShlurdUnknownReference => false
    }
  }

  def getCount(reference : ShlurdReference) : ShlurdCount =
  {
    reference match {
      case ShlurdPronounReference(_, _, count) =>
        count
      case ShlurdEntityReference(_, _, count) =>
        count
      case ShlurdConjunctiveReference(determiner, _, _) => {
        determiner match {
          case DETERMINER_ALL => COUNT_PLURAL
          // DETERMINER_NONE is debatable
          case _ => COUNT_SINGULAR
        }
      }
      case ShlurdStateSpecifiedReference(reference, _) =>
        getCount(reference)
      case ShlurdGenitiveReference(_, reference) =>
        getCount(reference)
      case _ : ShlurdUnknownReference => COUNT_SINGULAR
    }
  }

  def qualified(reference : ShlurdReference, qualifiers : Seq[ShlurdWord]) =
  {
    if (qualifiers.isEmpty) {
      reference
    } else if (qualifiers.size == 1) {
      ShlurdStateSpecifiedReference(
        reference, ShlurdPropertyState(qualifiers.head))
    } else {
      ShlurdStateSpecifiedReference(
        reference,
        ShlurdConjunctiveState(
          DETERMINER_ALL,
          qualifiers.map(ShlurdPropertyState(_)),
          SEPARATOR_CONJOINED))
    }
  }

  def extractLocationSpecifiers(state : ShlurdState)
      : Seq[ShlurdLocationState] =
  {
    state match {
      case ShlurdConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractLocationSpecifiers(_))
      case ls : ShlurdLocationState => Seq(ls)
      case ShlurdNullState() | ShlurdPropertyState(_) |
          ShlurdExistenceState() => Seq.empty
      case _ => {
        assert(false)
        Seq.empty
      }
    }
  }

  def extractQualifiers(state : ShlurdState) : Seq[ShlurdWord] =
  {
    state match {
      case ShlurdConjunctiveState(DETERMINER_ALL, states, _) =>
        states.flatMap(extractQualifiers(_))
      case ShlurdPropertyState(state) => Seq(state)
      case ShlurdNullState() | ShlurdLocationState(_, _) |
          ShlurdExistenceState() => Seq.empty
      case _ => {
        assert(false)
        Seq.empty
      }
    }
  }
}
