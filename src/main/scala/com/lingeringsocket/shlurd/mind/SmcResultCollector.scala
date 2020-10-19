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

import com.lingeringsocket.shlurd.ilang._

import spire.math._

import scala.collection._

class SmcResultCollector[EntityType<:SmcEntity](
  val annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]],
  val refMap : SmcMutableRefMap[EntityType])
{
  private val entityMap = new mutable.LinkedHashMap[EntityType, Trilean]
  val states = new mutable.LinkedHashSet[SilWord]
  val neutralizedEntities = new mutable.LinkedHashSet[EntityType]
  var isCategorization = false
  var suppressWildcardExpansion = 0
  var swapSpeakerListener = false
  var resolvingReferences = false
  var analyzingAssertion = false

  protected def preSpawn =
  {
    new SmcResultCollector[EntityType](annotator, refMap)
  }

  def spawn = {
    val newCollector = preSpawn
    newCollector.suppressWildcardExpansion = suppressWildcardExpansion
    newCollector.swapSpeakerListener = swapSpeakerListener
    newCollector.resolvingReferences = resolvingReferences
    newCollector.analyzingAssertion = analyzingAssertion
    newCollector
  }

  def lookup(ref : SilReference) : Option[Set[EntityType]] =
  {
    refMap.get(ref)
  }

  def fullEntityMap = entityMap

  def neutralizedEntityMap =
  {
    entityMap.view.filterKeys(
      entity => !neutralizedEntities.contains(entity)).toMap
  }

  def saveEntityResult(entity : EntityType, result : Trilean) : Unit =
  {
    entityMap.put(entity, result)
  }
}

object SmcResultCollector
{
  def apply[EntityType<:SmcEntity](
    annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]]
  ) =
    new SmcResultCollector(
      annotator, newAnnotationRefMap[EntityType](annotator))

  def newAnnotationRefMap[EntityType<:SmcEntity] =
  {
    val newAnnotator = SmcAnnotator[EntityType]()
    SmcMutableRefMap.fromAnnotation(newAnnotator)
  }

  def newAnnotationRefMap[EntityType<:SmcEntity](
    annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]]) =
  {
    SmcMutableRefMap.fromAnnotation(annotator)
  }

  def modifiableRefMap[EntityType<:SmcEntity](
    map : SmcRefMap[EntityType]) : SmcMutableRefMap[EntityType] =
  {
    val newMap = newAnnotationRefMap[EntityType]
    newMap ++= map
    newMap
  }

  def snapshotRefMap[EntityType<:SmcEntity](
    map : SmcRefMap[EntityType]) : SmcRefMap[EntityType] =
  {
    modifiableRefMap(map)
  }
}
