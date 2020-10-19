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

import com.lingeringsocket.shlurd.ilang._

import scala.collection._

case class SipExpectedSentence(
  syntaxTree : SprSyntaxTree,
  forceSQ : Boolean = false
) extends SilUnresolvedSentence
{
}

case class SipExpectedConditionalSentence(
  syntaxTree : SprSyntaxTree,
  conjunction : SilWord,
  antecedent : SilSentence,
  consequent : SilSentence,
  biconditional : Boolean,
  expectedFormality : SilFormality
) extends SilUnresolvedSentence
{
  override def formality = expectedFormality
}

case class SipExpectedPredicate(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedPredicate
{
}

case class SipExpectedReference(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedReference
{
}

case class SipExpectedPossessiveReference(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedReference
{
}

case class SipExpectedNounlikeReference(
  syntaxTree : SprSyntaxTree,
  preTerminal : SprSyntaxTree,
  determiner : SilDeterminer
) extends SilUnresolvedReference
{
}

case class SipExpectedComplementState(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedState
{
}

case class SipExpectedAdpositionalState(
  syntaxTree : SprSyntaxTree,
  extracted : Boolean
) extends SilUnresolvedState
{
}

case class SipExpectedPropertyState(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedState
{
}

case class SipExpectedExistenceState(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedState
{
}

case class SipExpectedVerbModifier(
  syntaxTree : SprSyntaxTree
) extends SilUnresolvedVerbModifier
{
}

case class SipUnresolvedStatePredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  state : SilState,
  specifiedState : SilState,
  modifiers : Seq[SilVerbModifier]
) extends SilUnresolvedPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, state, specifiedState) ++ modifiers

  override def withNewSubject(reference : SilReference) =
    copy(subject = reference)

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SipUnresolvedActionPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  directObject : Option[SilReference],
  adpositionObject : Option[SilReference],
  modifiers : Seq[SilVerbModifier]
) extends SilUnresolvedPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ adpositionObject ++ modifiers

  override def withNewSubject(reference : SilReference) =
    copy(subject = reference)

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SipUnresolvedRelationshipPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  complement : SilReference,
  verb : SilWord,
  modifiers : Seq[SilVerbModifier]
) extends SilUnresolvedPredicate
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}
