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
package com.lingeringsocket.shlurd

sealed trait ShlurdExceptionCode
{
  def getUrl = s"https://undefined.com/exceptionCodes#${getFragmentIdentifier}"

  def getFragmentIdentifier =
    getClass.getName.stripSuffix("$").split("\\$").last
}

object ShlurdExceptionCode
{
  case object FailedParse extends ShlurdExceptionCode
  case object UnknownForm extends ShlurdExceptionCode
  case object UnknownState extends ShlurdExceptionCode
  case object NonExistent extends ShlurdExceptionCode
  case object NotUnique extends ShlurdExceptionCode
  case object UnresolvedPronoun extends ShlurdExceptionCode
  case object TriggerLimit extends ShlurdExceptionCode
  case object CausalityViolation extends ShlurdExceptionCode
  case object NotYetImplemented extends ShlurdExceptionCode

  case object IncomprehensibleBelief extends ShlurdExceptionCode
  case object InvalidBelief extends ShlurdExceptionCode
  case object BeliefNotYetImplemented extends ShlurdExceptionCode
  case object ReferenceNotYetImplemented extends ShlurdExceptionCode
  case object NewBeliefsProhibited extends ShlurdExceptionCode
  case object ImplicitIdealsProhibited extends ShlurdExceptionCode
  case object ImplicitPropertiesProhibited extends ShlurdExceptionCode
  case object TentativeIdealsProhibited extends ShlurdExceptionCode
  case object TentativeEntitiesProhibited extends ShlurdExceptionCode
  case object AmbiguousInterpretation extends ShlurdExceptionCode
  case object CardinalityConstraint extends ShlurdExceptionCode
  case object TaxonomyCycle extends ShlurdExceptionCode

  // FIXME need test
  case object OverlappingProperties extends ShlurdExceptionCode

  case object PropertyAlreadyClosed extends ShlurdExceptionCode

  // FIXME need test
  case object RoleHypernymNonExistent extends ShlurdExceptionCode
  // FIXME need test
  case object RoleHyponymConflictsWithForm extends ShlurdExceptionCode
  // FIXME need test
  case object RoleHyponymAlreadyExists extends ShlurdExceptionCode
  // FIXME need test
  case object RoleTaxonomyIncompatible extends ShlurdExceptionCode
  // FIXME need test
  case object FormTaxonomyIncompatible extends ShlurdExceptionCode

  case object FormRoleIncompatible extends ShlurdExceptionCode
  case object PropertyDomainIncompatible extends ShlurdExceptionCode
  case object AbsenceConstraint extends ShlurdExceptionCode
}

case class ShlurdException(
  code : ShlurdExceptionCode,
  msg : String
) extends RuntimeException(msg)
{
}

