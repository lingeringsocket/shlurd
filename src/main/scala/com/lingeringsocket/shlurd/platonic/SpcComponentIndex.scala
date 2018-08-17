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

import org.jgrapht._
import org.jgrapht.event._

import scala.collection._
import scala.collection.JavaConverters._

class SpcComponentIndex[KeyType, ComponentType](
  graph : ListenableGraph[SpcContainmentVertex, SpcComponentEdge],
  keyExtractor : (SpcContainmentVertex) => Option[KeyType])
    extends GraphListener[SpcContainmentVertex, SpcComponentEdge]
{
  private val map =
    new mutable.HashMap[SpcContainmentVertex, Map[KeyType, ComponentType]]

  init()

  private def init()
  {
    graph.addGraphListener(this)
  }

  def accessComponentMap(container : SpcContainmentVertex)
      : Map[KeyType, ComponentType] =
  {
    map.get(container).getOrElse(buildComponentMap(container))
  }

  private def buildComponentMap(container : SpcContainmentVertex)
      : Map[KeyType, ComponentType] =
  {
    val seq = graph.outgoingEdgesOf(container).asScala.toSeq.flatMap(
      edge => {
        val component = graph.getEdgeTarget(edge)
        keyExtractor(component).map(key => {
          (key, component.asInstanceOf[ComponentType])
        })
      }
    )
    Map(seq:_*)
  }

  private def invalidateComponentMap(container : SpcContainmentVertex)
  {
    map.remove(container)
  }

  override def vertexAdded(
    event : GraphVertexChangeEvent[SpcContainmentVertex])
  {
  }

  override def vertexRemoved(
    event : GraphVertexChangeEvent[SpcContainmentVertex])
  {
    invalidateComponentMap(event.getVertex)
  }

  override def edgeAdded(
    event : GraphEdgeChangeEvent[SpcContainmentVertex, SpcComponentEdge])
  {
    invalidateComponentMap(event.getEdgeSource)
  }

  override def edgeRemoved(
    event : GraphEdgeChangeEvent[SpcContainmentVertex, SpcComponentEdge])
  {
    invalidateComponentMap(event.getEdgeSource)
  }
}
