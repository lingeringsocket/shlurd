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

import scala.collection.JavaConverters._

class SpcPerception(
  noumenalCosmos : SpcCosmos, phenomenalCosmos : SpcCosmos)
{
  private val noumenalGraph = noumenalCosmos.getGraph

  private val phenomenalGraph = phenomenalCosmos.getModifiableGraph

  def perceiveEntity(entity : SpcEntity)
  {
    assert(noumenalGraph.entitySynonyms.containsVertex(entity))

    if (!phenomenalGraph.entitySynonyms.containsVertex(entity)) {
      phenomenalCosmos.createOrReplaceEntity(entity)
    }

    phenomenalCosmos.getIdGenerator.set(
      math.max(
        phenomenalCosmos.getIdGenerator.get,
        noumenalCosmos.getIdGenerator.get))
  }

  def perceiveEntityAssociations(entity : SpcEntity)
  {
    perceiveEntity(entity)

    noumenalGraph.entityAssocs.edgesOf(entity).asScala.foreach(edge => {
      val opposite = Graphs.getOppositeVertex(
        noumenalGraph.entityAssocs,
        edge,
        entity)
      perceiveEntity(opposite)
      val (possessor, possessee) = {
        if (entity == noumenalGraph.getPossessorEntity(edge)) {
          tupleN((entity, opposite))
        } else {
          tupleN((opposite, entity))
        }
      }
      phenomenalCosmos.addEntityAssocEdge(
        possessor,
        possessee,
        edge.formEdge)
    })
  }

  def perceiveEntityProperties(entity : SpcEntity)
  {
    perceiveEntity(entity)

    val map = noumenalCosmos.getEntityPropertyMap(entity)
    map.values.foreach(ps => {
      val property =
        phenomenalCosmos.resolvePropertyName(entity, ps.propertyName).get
      phenomenalCosmos.updateEntityProperty(entity, property, ps.lemma)
    })
  }
}
