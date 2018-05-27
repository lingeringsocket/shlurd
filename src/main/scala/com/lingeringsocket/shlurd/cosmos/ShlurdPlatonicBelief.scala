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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import scala.collection._

import ShlurdPlatonicCosmos._

sealed trait ShlurdPlatonicBelief
{
  def sentence : SilSentence
}

case class UnimplementedBelief(
  sentence : SilSentence
) extends ShlurdPlatonicBelief
{
}

case class StateEquivalenceBelief(
  sentence : SilSentence,
  formName : SilWord,
  state1 : SilState,
  state2 : SilState
) extends ShlurdPlatonicBelief
{
}

case class FormAliasBelief(
  sentence : SilSentence,
  synonym : SilWord,
  formName : SilWord
) extends ShlurdPlatonicBelief
{
}

case class FormAssocBelief(
  sentence : SilSentence,
  possessorFormName : SilWord,
  possesseeFormName : SilWord,
  constraint : CardinalityConstraint,
  isProperty : Boolean
) extends ShlurdPlatonicBelief
{
}

case class EntityExistenceBelief(
  sentence : SilSentence,
  formName : SilWord,
  qualifiers : Seq[SilWord],
  properName : String
) extends ShlurdPlatonicBelief
{
}

case class EntityAssocBelief(
  sentence : SilSentence,
  possessorEntityName : SilWord,
  possesseeEntityName : SilWord,
  labelName : SilWord
) extends ShlurdPlatonicBelief
{
}

case class FormPropertyBelief(
  sentence : SilSentence,
  formName : SilWord,
  states : Seq[SilWord],
  isClosed : Boolean
) extends ShlurdPlatonicBelief
{
}
