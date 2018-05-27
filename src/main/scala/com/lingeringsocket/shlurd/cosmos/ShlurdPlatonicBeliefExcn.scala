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

sealed abstract class ShlurdPlatonicBeliefExcn(
  cause : String) extends RuntimeException(cause)

sealed abstract class RejectedBelief(
  cause : String) extends ShlurdPlatonicBeliefExcn(cause)
{
  def belief : SilSentence
}

case class IncomprehensibleBelief(belief : SilSentence)
    extends RejectedBelief(
      "New belief cannot be understood.")
{
}

case class ContradictoryBelief(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBelief(
      "New belief contradicts previously accepted beliefs.")
{
}

case class AmbiguousBelief(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBelief(
      "New belief introduces ambiguity with previously accepted beliefs.")
{
}

case class IncrementalCardinalityViolation(
  belief : SilSentence, originalBelief : SilSentence)
    extends RejectedBelief(
      "New belief violates a previously accepted cardinality constraint.")
{
}

case class CardinalityViolation(
  originalBelief : SilSentence)
    extends ShlurdPlatonicBeliefExcn(
  "Cardinality constraint belief violated.")
{
}
