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

trait ShlurdPhrase
{
  def children : Seq[ShlurdPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)
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

case object ShlurdUnknownSentence extends ShlurdSentence
{
  override def mood = MOOD_INDICATIVE_POSITIVE

  override def formality = ShlurdFormality.DEFAULT

  override def hasUnknown = true
}

case object ShlurdUnknownReference extends ShlurdReference
{
  override def hasUnknown = true
}

case object ShlurdUnknownPredicate extends ShlurdPredicate
{
  override def hasUnknown = true
}

case object ShlurdUnknownState extends ShlurdState
{
  override def hasUnknown = true
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
