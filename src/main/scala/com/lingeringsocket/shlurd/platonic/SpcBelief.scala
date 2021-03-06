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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

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

case class NonvalidBelief(
  sentence : SilSentence,
  exceptionCode : ShlurdExceptionCode
) extends SpcBelief
{
}

case class FormTaxonomyBelief(
  sentence : SilSentence,
  hyponymFormName : SilWord,
  hypernymFormName : SilWord
) extends SpcBelief
{
}

case class RoleTaxonomyBelief(
  sentence : SilSentence,
  possessorFormName : SilWord,
  hyponymRoleName : SilWord,
  hypernymIdealName : SilWord,
  isRefinement : Boolean
) extends SpcBelief
{
}

case class IdealAliasBelief(
  sentence : SilSentence,
  synonym : SilWord,
  idealName : SilWord,
  possessorOpt : Option[SilWord] = None
) extends SpcBelief
{
}

case class FormAssocBelief(
  sentence : SilSentence,
  possessorFormName : SilWord,
  possesseeRoleNames : Seq[SilWord],
  constraint : SpcCardinalityConstraint
) extends SpcBelief
{
}

case class InverseAssocBelief(
  sentence : SilSentence,
  possessorFormName : SilWord,
  possessorRoleName : SilWord,
  possesseeFormName : SilWord,
  possesseeRoleName : SilWord
) extends SpcBelief
{
}

case class EntityExistenceBelief(
  sentence : SilSentence,
  entityRef : SilReference,
  formName : SilWord,
  qualifiers : Seq[SilWord],
  properName : String,
  positive : Boolean = true
) extends SpcBelief
{
}

sealed trait EntityAssocInstantiation
case object ENTITY_ASSOC_EXISTING extends EntityAssocInstantiation
case object ENTITY_ASSOC_DEFINITE extends EntityAssocInstantiation
case object ENTITY_ASSOC_INDEFINITE extends EntityAssocInstantiation

case class EntityAssocBelief(
  sentence : SilSentence,
  possessorRef : SilReference,
  possesseeRef : SilReference,
  instantiation : EntityAssocInstantiation,
  roleName : SilWord,
  positive : Boolean = true
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

case class FormEnumPropertyBelief(
  sentence : SilSentence,
  formName : SilWord,
  states : Seq[SilWord],
  isClosed : Boolean,
  propertyName : Option[SilWord] = None
) extends SpcBelief
{
}

case class FormTypedPropertyBelief(
  sentence : SilSentence,
  formName : SilWord,
  propertyName : SilWord,
  domain : SpcPropertyDomain
) extends SpcBelief
{
}

case class EntityPropertyBelief(
  sentence : SilSentence,
  reference : SilReference,
  propertyName : Option[SilWord],
  propertyValue : Either[SilWord, String]
) extends SpcBelief
{
}

case class AssertionBelief(
  sentence : SilSentence,
  additionalConsequents : Seq[SilPredicateSentence] = Seq.empty,
  alternative : Option[SilPredicateSentence] = None
) extends SpcBelief
{
}

case class UniquenessBelief(
  sentence : SilSentence,
  ref : SilReference
) extends SpcBelief
{
}

case class IndirectBelief(
  sentence : SilSentence,
  resourceName : String
) extends SpcBelief
{
}

case class EpsilonBelief(
  sentence : SilSentence
) extends SpcBelief
{
}
