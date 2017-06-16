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

trait ShlurdPhrase
{
  def children : Seq[ShlurdPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)
}

sealed trait ShlurdSentence extends ShlurdPhrase
{
  def mood : ShlurdMood
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
  mood : ShlurdMood = MOOD_INDICATIVE_POSITIVE
) extends ShlurdSentence
{
  override def children = Seq(predicate)
}

case class ShlurdStateChangeCommand(
  predicate : ShlurdStatePredicate
) extends ShlurdSentence
{
  override def children = Seq(predicate)

  override def mood = MOOD_IMPERATIVE
}

case object ShlurdUnknownSentence extends ShlurdSentence
{
  override def mood = MOOD_INDICATIVE_POSITIVE

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

case class ShlurdQualifiedReference(
  reference : ShlurdReference,
  qualifiers : Seq[ShlurdWord]
) extends ShlurdReference
{
  override def children = Seq(reference)
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
  count : ShlurdCount,
  reference : ShlurdReference = ShlurdUnknownReference
) extends ShlurdReference
{
}

case class ShlurdEntityReference(
  entity : ShlurdWord,
  determiner : ShlurdDeterminer = DETERMINER_UNSPECIFIED,
  count : ShlurdCount = COUNT_SINGULAR
) extends ShlurdReference
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

case class ShlurdWord(
  inflected : String,
  lemma : String)
{
}
