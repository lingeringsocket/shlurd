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

import scala.collection._

import spire.math._
import spire.math.interval._
import spire.math.extras.interval._
import spire.algebra._
import spire.std._

trait SmcTimePoint
{
}

case object SmcNextTimePoint extends SmcTimePoint

object SmcTimeInterval
{
  val NEXT_INSTANT = Interval.point[SmcTimePoint](SmcNextTimePoint)

  val NEXT_IN_SEQUENCE = Int.MaxValue
}

case class SmcRelativeTimePoint(
  reference : SilReference,
  sequence : Int = SmcTimeInterval.NEXT_IN_SEQUENCE
) extends SmcTimePoint
{
}

object SmcTimePointOrder extends Order[SmcTimePoint]
{
  val ONCE_UPON_A_TIME = "once upon a time"

  val ONCE_UPON_A_TIME_POINT = SmcRelativeTimePoint(
    SilNounReference(SilWord(SmcTimePointOrder.ONCE_UPON_A_TIME)), 0)

  override def compare(x : SmcTimePoint, y : SmcTimePoint) : Int =
  {
    tupleN((x, y)) match {
      case (SmcRelativeTimePoint(r1, s1), SmcRelativeTimePoint(r2, s2)) => {
        val t1 = interpretTemporal(r1)
        val t2 = interpretTemporal(r2)
        val c = int.IntAlgebra.compare(t1, t2)
        if (c == 0) {
          int.IntAlgebra.compare(s1, s2)
        } else {
          c
        }
      }
      case _ => {
        throw new IllegalArgumentException(s"$x $y")
      }
    }
  }

  // FIXME Mickey Mouse
  private def interpretTemporal(ref : SilReference) : Int =
  {
    ref match {
      case SilNounReference(word, DETERMINER_UNSPECIFIED, COUNT_SINGULAR) => {
        word.lemma match {
          case ONCE_UPON_A_TIME => Int.MinValue
          case "yesterday" => -1
          case _ => throw new IllegalArgumentException
        }
      }
      case SilGenitiveReference(
        SilPronounReference(
          PERSON_THIRD, GENDER_N, COUNT_SINGULAR, DISTANCE_HERE),
        SilNounReference(word, DETERMINER_UNSPECIFIED, COUNT_SINGULAR)
      ) => {
        word.lemma match {
          case "morning" => 1
          case "afternoon" => 2
          case "evening" => 3
          case _ => throw new IllegalArgumentException
        }
      }
      case _ => {
        throw new IllegalArgumentException
      }
    }
  }
}

case class SmcTimelineEntry[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType,PropertyType]
](
  interval : SmcTimeInterval,
  updatedCosmos : CosmosType,
  predicate : SilPredicate,
  referenceMap : Map[SilReference, Set[EntityType]]
)
{
}

class SmcTimeline[
  EntityType <: SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType]
] {
  type EntryType = SmcTimelineEntry[EntityType, PropertyType, CosmosType]

  type CosmosMutator = (SilPredicate, CosmosType) => CosmosType

  private var sequence = 1

  // FIXME allow overlap using IntervalSet
  private var intervals = IntervalSeq.empty[SmcTimePoint]

  private var previousTime = SmcTimePointOrder.ONCE_UPON_A_TIME_POINT

  private val intervalToEntry = new mutable.HashMap[SmcTimeInterval, EntryType]

  private[mind] def generateSequence() =
  {
    val timestamp = sequence
    sequence += 1
    timestamp
  }

  def findGlb(interval : SmcTimeInterval) : Option[EntryType] =
  {
    // FIXME there's got to be a better way
    val combined = intervals | IntervalSeq(interval)
    combined.intervalIterator.sliding(2).foreach(pair => {
      if (pair.head == interval) {
        return None
      }
      if (pair.last == interval) {
        val prev = pair.head
        return Some(intervalToEntry(prev))
      }
    })
    throw new AssertionError("Huh?")
  }

  def addEntry(entry : EntryType, cosmosMutator : CosmosMutator) : EntryType =
  {
    val timestamp = generateSequence
    val actualInterval = {
      entry.interval match {
        case SmcTimeInterval.NEXT_INSTANT => {
          Interval.point[SmcTimePoint](
            SmcRelativeTimePoint(previousTime.reference, timestamp))
        }
        case interval => {
          interval.mapBounds(_ match {
            case SmcRelativeTimePoint(
              ref, SmcTimeInterval.NEXT_IN_SEQUENCE
            ) => {
              SmcRelativeTimePoint(ref, timestamp)
            }
            case tp : SmcTimePoint => {
              tp
            }
          })
        }
      }
    }
    val actualEntry = new EntryType(
      actualInterval,
      entry.updatedCosmos,
      entry.predicate,
      entry.referenceMap
    )
    val newIntervalSeq = IntervalSeq(actualInterval)
    assert(!newIntervalSeq.intersects(intervals))
    val newIntervals = intervals | newIntervalSeq
    previousTime = actualInterval.upperBound match {
      case Closed(t : SmcRelativeTimePoint) => t
      case _ => SmcTimePointOrder.ONCE_UPON_A_TIME_POINT
    }
    applyCausality(
      newIntervals, actualInterval, entry.updatedCosmos, cosmosMutator)
    // defer update until after loop to provide atomicity in
    // case of causality violations
    intervalToEntry.put(actualInterval, actualEntry)
    actualEntry
  }

  def getEntries() : Seq[EntryType] =
    intervals.intervals.map(intervalToEntry).toSeq

  private def applyCausality(
    newIntervals : IntervalSeq[SmcTimePoint],
    eventInterval : SmcTimeInterval,
    eventCosmos : CosmosType,
    cosmosMutator : CosmosMutator)
  {
    val iter = newIntervals.intervalIterator
    var found = false
    while (iter.next != eventInterval) {}
    var prevCosmos = eventCosmos
    val updated = new mutable.HashMap[SmcTimeInterval, EntryType]
    while (iter.hasNext) {
      val interval = iter.next
      val entry = intervalToEntry(interval)
      val updatedCosmos = cosmosMutator(entry.predicate, prevCosmos)
      updated.put(
        interval,
        new EntryType(
          interval, updatedCosmos, entry.predicate, entry.referenceMap))
      prevCosmos = updatedCosmos
    }
    // defer updates until after loop to provide atomicity in
    // case of causality violations
    intervalToEntry ++= updated
    intervals = newIntervals
  }
}
