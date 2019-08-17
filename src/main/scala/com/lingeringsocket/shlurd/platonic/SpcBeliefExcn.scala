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

sealed abstract class SpcBeliefExcn(
  code : ShlurdExceptionCode,
  cause : String) extends RuntimeException(cause)
{
  def getCode : ShlurdExceptionCode = code
}

sealed abstract class RejectedBeliefExcn(
  code : ShlurdExceptionCode,
  cause : String) extends SpcBeliefExcn(code, cause)
{
  def belief : SilSentence
}

case class IncomprehensibleBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString)
{
}

case class UnimplementedBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString)
{
}

case class InvalidBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString)
{
}

case class ProhibitedBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString)
{
}

case class ContradictoryBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence,
  originalBelief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class AmbiguousBeliefExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence,
  originalBelief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class IncrementalCardinalityExcn(
  code : ShlurdExceptionCode,
  belief : SilSentence,
  originalBelief : SilSentence)
    extends RejectedBeliefExcn(code, "New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class UnacceptableBeliefExcn(
  code : ShlurdExceptionCode,
  cause : String,
  belief : SilSentence
) extends RejectedBeliefExcn(code, cause)
{
}
