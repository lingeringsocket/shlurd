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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._

import spire.math._

class SmcTimelineSpec extends Specification
{
  type CosmosType = ZooCosmos

  type TimelineType = SmcTimeline[SmcEntity, SmcProperty, CosmosType]

  type TimelineEntry = SmcTimelineEntry[
    SmcEntity, SmcProperty,
    CosmosType
  ]

  private val PRED_A = makePredicate("a")

  private val REFERENCE_D = makeReference("d")

  private val ENTITY_1 = new SmcEntity {
    override def getUniqueIdentifier = "1"
  }

  private val REF_MAP_1 = SmcRefMap[SmcEntity](
    REFERENCE_D -> Set(ENTITY_1)
  )

  private val TIME_1 = Interval.point[SmcTimePoint](
    SmcTimePointOrder.ONCE_UPON_A_TIME_POINT)

  private val TIME_AUTO = SmcTimeInterval.NEXT_INSTANT

  private val COSMOS_1 = new ZooCosmos

  private val COSMOS_2 = new ZooCosmos

  private val COSMOS_3 = new ZooCosmos

  private def makeLeaf(s : String) =
    SprSyntaxLeaf(s, s, s)

  private def makePredicate(s : String) =
    SilUnrecognizedPredicate(makeLeaf(s))

  private def makeReference(s : String) =
    SilUnrecognizedReference(makeLeaf(s))

  private def nullCosmosMutator(
    eventPredicate : SilPredicate,
    eventCosmos : CosmosType) : CosmosType =
  {
    eventCosmos
  }

  private def constantCosmosMutator(
    eventPredicate : SilPredicate,
    eventCosmos : CosmosType) : CosmosType =
  {
    COSMOS_1
  }

  private def failCosmosMutator(
    eventPredicate : SilPredicate,
    eventCosmos : CosmosType) : CosmosType =
  {
    if (eventCosmos == COSMOS_2) {
      throw ShlurdException(
        ShlurdExceptionCode.CausalityViolation,
        "You've kill your own grandfather")
    } else {
      COSMOS_2
    }
  }

  private def populateTimeline(timeline : TimelineType) =
  {
    val entry1 = timeline.addEntry(
      new TimelineEntry(TIME_AUTO, COSMOS_1, PRED_A, REF_MAP_1),
      nullCosmosMutator)
    val entry2 = timeline.addEntry(
      new TimelineEntry(TIME_AUTO, COSMOS_2, PRED_A, REF_MAP_1),
      nullCosmosMutator)
    val entry3 = timeline.addEntry(
      new TimelineEntry(TIME_AUTO, COSMOS_3, PRED_A, REF_MAP_1),
      nullCosmosMutator)
    tupleN(entry1, entry2, entry3)
  }

  "SmcTimeline" should
  {
    "record events" in
    {
      val timeline = new TimelineType
      val cosmos = COSMOS_1
      val entry = new TimelineEntry(TIME_1, cosmos, PRED_A, REF_MAP_1)
      timeline.addEntry(entry, nullCosmosMutator)
      val entries = timeline.getEntries
      entries must be equalTo Seq(entry)
    }

    "find glb" in
    {
      val timeline = new TimelineType
      val (entry1, entry2, entry3) = populateTimeline(timeline)
      timeline.getEntries must be equalTo Seq(entry1, entry2, entry3)
      timeline.findGlb(entry1.interval) must be equalTo None
      timeline.findGlb(entry2.interval) must be equalTo Some(entry1)
      timeline.findGlb(entry3.interval) must be equalTo Some(entry2)
    }

    "apply causality" in
    {
      val timeline = new TimelineType

      // just to reserve an early time point
      timeline.generateSequence

      val (entry1, entry2, entry3) = populateTimeline(timeline)
      val originalEntries = timeline.getEntries
      originalEntries.size must be equalTo 3

      timeline.addEntry(
        new TimelineEntry(
          Interval.point(SmcTimePointOrder.ONCE_UPON_A_TIME_POINT),
          COSMOS_1, PRED_A, REF_MAP_1),
        constantCosmosMutator)

      val entries = timeline.getEntries
      entries.size must be equalTo 4
      entries.map(_.updatedCosmos) must contain(COSMOS_1).forall
      val modifiedEntries = entries.tail
      modifiedEntries.map(_.interval) must be equalTo
        originalEntries.map(_.interval)
      modifiedEntries.map(_.predicate) must be equalTo
        originalEntries.map(_.predicate)
    }

    "handle causality violations" in
    {
      val timeline = new TimelineType

      // just to reserve an early time point
      timeline.generateSequence
      val (entry1, entry2, entry3) = populateTimeline(timeline)

      timeline.addEntry(
        new TimelineEntry(
          Interval.point(SmcTimePointOrder.ONCE_UPON_A_TIME_POINT),
          COSMOS_1, PRED_A, REF_MAP_1),
        failCosmosMutator
      ) must throwA[ShlurdException].like {
        case ShlurdException(code, _) => {
          code must be equalTo ShlurdExceptionCode.CausalityViolation
        }
      }

      // nothing should be modified
      timeline.getEntries must be equalTo Seq(entry1, entry2, entry3)
    }
  }
}
