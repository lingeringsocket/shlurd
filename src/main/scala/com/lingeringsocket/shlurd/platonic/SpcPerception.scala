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

import org.jgrapht._

import scala.collection._
import scala.jdk.CollectionConverters._

object SpcTimestamp
{
  val ZERO = SpcTimestamp(0)
}

case class SpcTimestamp(when : Long)
{
  def successor = SpcTimestamp(when + 1)

  def isBefore(another : SpcTimestamp) : Boolean =
  {
    when < another.when
  }

  def isAfter(another : SpcTimestamp) : Boolean =
  {
    when > another.when
  }
}

case class SpcEntityPerception(
  earliest : SpcTimestamp,
  latest : SpcTimestamp
)

class SpcPerception(
  val noumenalCosmos : SpcCosmos,
  val phenomenalCosmos : SpcCosmos,
  val timestampMap : mutable.Map[SpcEntity, SpcEntityPerception] =
    new mutable.LinkedHashMap[SpcEntity, SpcEntityPerception])
{
  private def noumenalGraph = noumenalCosmos.getGraph

  private def phenomenalGraph = phenomenalCosmos.getModifiableGraph

  private def touchTimestamp(
    entity : SpcEntity, timestamp : SpcTimestamp) : Unit =
  {
    val entityPerception =
      timestampMap.get(entity).map(_.copy(latest = timestamp)).getOrElse(
        SpcEntityPerception(timestamp, timestamp))
    timestampMap.put(entity, entityPerception)
  }

  def perceiveEntity(entity : SpcEntity, timestamp : SpcTimestamp) : Unit =
  {
    assert(noumenalGraph.entitySynonyms.containsVertex(entity))

    if (!phenomenalGraph.entitySynonyms.containsVertex(entity)) {
      phenomenalCosmos.createOrReplaceEntity(entity)
    }

    touchTimestamp(entity, timestamp)

    phenomenalCosmos.getIdGenerator.set(
      math.max(
        phenomenalCosmos.getIdGenerator.get,
        noumenalCosmos.getIdGenerator.get))
  }

  def perceiveEntityAssociations(
    entity : SpcEntity, timestamp : SpcTimestamp) : Unit =
  {
    perceiveEntity(entity, timestamp)

    val noumenalEdges = noumenalGraph.entityAssocs.edgesOf(entity).asScala
    val phenomenalEdges = phenomenalGraph.entityAssocs.edgesOf(entity).asScala

    phenomenalEdges.foreach(edge => {
      phenomenalCosmos.removeEntityAssocEdge(edge)
    })

    noumenalEdges.foreach(edge => {
      val opposite = Graphs.getOppositeVertex(
        noumenalGraph.entityAssocs,
        edge,
        entity)
      if (timestampMap.contains(opposite) || SpcMeta.isMetaEntity(opposite)) {
        val (possessor, possessee) = {
          if (entity == noumenalGraph.getPossessorEntity(edge)) {
            tupleN(entity, opposite)
          } else {
            tupleN(opposite, entity)
          }
        }
        val role = noumenalCosmos.getPossesseeRole(edge)
        phenomenalCosmos.addEntityAssocEdge(
          possessor,
          possessee,
          role)
      }
    })
  }

  def perceiveEntityProperties(
    entity : SpcEntity, timestamp : SpcTimestamp) : Unit =
  {
    perceiveEntity(entity, timestamp)

    val map = noumenalCosmos.getEntityPropertyMap(entity)
    map.values.foreach(ps => {
      phenomenalCosmos.resolvePropertyName(entity, ps.propertyName).foreach(
        property => {
          phenomenalCosmos.updateEntityProperty(entity, property, ps.lemma)
        })
    })
  }

  def getEntityEarliestTimestamp(entity : SpcEntity) : Option[SpcTimestamp] =
  {
    timestampMap.get(entity).map(_.earliest)
  }

  def getEntityTimestamp(entity : SpcEntity) : Option[SpcTimestamp] =
  {
    timestampMap.get(entity).map(_.latest)
  }
}
