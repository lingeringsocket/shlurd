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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd.parser._

import scala.collection._

case class SpcCardinalityConstraint(lower : Int, upper : Int)
{
}

sealed trait SpcBelief
{
  def sentence : SilSentence
}

case class UnimplementedBelief(
  sentence : SilSentence
) extends SpcBelief
{
}

case class StateEquivalenceBelief(
  sentence : SilSentence,
  formName : SilWord,
  state1 : SilState,
  state2 : SilState
) extends SpcBelief
{
}

case class IdealTaxonomyBelief(
  sentence : SilSentence,
  hyponymIdealName : SilWord,
  hypernymIdealName : SilWord,
  hyponymIsRole : Boolean
) extends SpcBelief
{
}

case class IdealAliasBelief(
  sentence : SilSentence,
  synonym : SilWord,
  idealName : SilWord
) extends SpcBelief
{
}

case class FormAssocBelief(
  sentence : SilSentence,
  possessorIdealName : SilWord,
  possesseeRoleNames : Seq[SilWord],
  constraint : SpcCardinalityConstraint,
  isProperty : Boolean
) extends SpcBelief
{
}

case class InverseAssocBelief(
  sentence : SilSentence,
  possessorFormName : SilWord,
  possessorRoleName : SilWord,
  possesseeRoleNames : Seq[SilWord]
) extends SpcBelief
{
}

case class EntityExistenceBelief(
  sentence : SilSentence,
  entityRef : SilReference,
  formName : SilWord,
  qualifiers : Seq[SilWord],
  properName : String
) extends SpcBelief
{
}

case class EntityAssocBelief(
  sentence : SilSentence,
  possessorRef : SilReference,
  possesseeRef : SilReference,
  roleName : SilWord
) extends SpcBelief
{
}

case class EntityNoAssocBelief(
  sentence : SilSentence,
  possessor : SilReference,
  roleName : SilWord
) extends SpcBelief
{
}

case class FormPropertyBelief(
  sentence : SilSentence,
  formName : SilWord,
  states : Seq[SilWord],
  isClosed : Boolean,
  propertyName : Option[SilWord] = None
) extends SpcBelief
{
}

case class EntityPropertyBelief(
  sentence : SilSentence,
  reference : SilReference,
  propertyName : Option[SilWord],
  stateName : SilWord
) extends SpcBelief
{
}

case class ConsequenceBelief(
  sentence : SilConditionalSentence
) extends SpcBelief
{
}

case class EpsilonBelief(
  sentence : SilSentence
) extends SpcBelief
{
}
