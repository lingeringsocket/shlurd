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

sealed abstract class SpcBeliefExcn(
  cause : String) extends RuntimeException(cause)

sealed abstract class RejectedBeliefExcn(
  cause : String) extends SpcBeliefExcn(cause)
{
  def belief : SilSentence
}

case class IncomprehensibleBeliefExcn(belief : SilSentence)
    extends RejectedBeliefExcn("New belief:  " +
      belief.toWordString)
{
}

case class UnimplementedBeliefExcn(belief : SilSentence)
    extends RejectedBeliefExcn("New belief:  " +
      belief.toWordString)
{
}

case class ContradictoryBeliefExcn(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBeliefExcn("New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class AmbiguousBeliefExcn(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBeliefExcn("New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class IncrementalCardinalityExcn(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBeliefExcn("New belief:  " +
      belief.toWordString + " vs original:  " + originalBelief.toWordString)
{
}

case class CardinalityExcn(
  originalBelief : SilSentence)
    extends SpcBeliefExcn("Belief:  " +
      originalBelief.toString)
{
}
