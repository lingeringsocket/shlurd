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

import org.specs2.mutable._

class SmcTimelineSpec extends Specification
{
  private val PRED_A = makePredicate("a")

  private val REFERENCE_D = makeReference("d")

  private val ENTITY_1 = new SmcEntity {}

  private val ENTITY_2 = new SmcEntity {}

  val REF_MAP_1 = Map[SilReference, Set[SmcEntity]](
    REFERENCE_D -> Set(ENTITY_1)
  )

  private def makeLeaf(s : String) =
    SprSyntaxLeaf(s, s, s)

  private def makePredicate(s : String) =
    SilUnrecognizedPredicate(makeLeaf(s))

  private def makeReference(s : String) =
    SilUnrecognizedReference(makeLeaf(s))

  "SmcTimeline" should
  {
    "record events" in
    {
      val timeline = new SmcTimeline[
        SmcEntity, SmcProperty,
        SmcCosmos[SmcEntity, SmcProperty]
      ]
      val cosmos : SmcCosmos[SmcEntity, SmcProperty] = null
      val entry = SmcTimelineEntry[
        SmcEntity, SmcProperty,
        SmcCosmos[SmcEntity, SmcProperty]
      ](cosmos, PRED_A, REF_MAP_1)
      timeline.addEntry(entry)
      val entries = timeline.getEntries
      entries must be equalTo Seq(entry)
    }
  }
}
