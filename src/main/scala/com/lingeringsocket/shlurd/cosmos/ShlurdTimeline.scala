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

case class ShlurdTimelineEntry[
  EntityType<:ShlurdEntity,
  PropertyType<:ShlurdProperty,
  CosmosType<:ShlurdCosmos[EntityType,PropertyType]
](
  updatedCosmos : CosmosType,
  predicate : SilPredicate,
  referenceMap : Map[SilReference, Set[EntityType]]
)
{
}

class ShlurdTimeline[
  EntityType <: ShlurdEntity,
  PropertyType<:ShlurdProperty,
  CosmosType<:ShlurdCosmos[EntityType, PropertyType]]
{
  type EntryType = ShlurdTimelineEntry[EntityType, PropertyType, CosmosType]

  // FIXME use spire intervals instead
  val entries = new mutable.ArrayBuffer[EntryType]

  def addEntry(entry : EntryType)
  {
    entries += entry
  }

  def getEntries() : Seq[EntryType] = entries
}
