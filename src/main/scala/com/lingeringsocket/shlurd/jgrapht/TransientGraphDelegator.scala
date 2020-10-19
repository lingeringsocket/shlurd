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
package com.lingeringsocket.shlurd.jgrapht

import org.jgrapht._
import org.jgrapht.graph._

import java.io._

object TransientGraphDelegator
{
  def apply[V, E](graph : Graph[V, E]) =
  {
    val delegator = new TransientGraphDelegator[V, E]
    delegator.setDelegate(graph)
    delegator
  }
}

class TransientGraphDelegator[V, E] extends AbstractGraph[V, E]
    with Serializable
{
  @transient private var delegate : Graph[V, E] = null

  def getDelegate =
  {
    delegate
  }

  def setDelegate(graph : Graph[V, E]) : Unit =
  {
    delegate = graph
  }

  override def getVertexSupplier =
  {
    delegate.getVertexSupplier
  }

  override def getEdgeSupplier =
  {
    delegate.getEdgeSupplier
  }

  override def getAllEdges(sourceVertex : V, targetVertex : V) =
  {
    delegate.getAllEdges(sourceVertex, targetVertex)
  }

  override def getEdge(sourceVertex : V, targetVertex : V) =
  {
    delegate.getEdge(sourceVertex, targetVertex)
  }

  override def addEdge(sourceVertex : V, targetVertex : V) =
  {
    delegate.addEdge(sourceVertex, targetVertex)
  }

  override def addEdge(sourceVertex : V, targetVertex : V, e : E) =
  {
    delegate.addEdge(sourceVertex, targetVertex, e)
  }

  override def addVertex =
  {
    delegate.addVertex
  }

  override def addVertex(v : V) =
  {
    delegate.addVertex(v)
  }

  override def containsEdge(e : E) =
  {
    delegate.containsEdge(e)
  }

  override def containsVertex(v : V) =
  {
    delegate.containsVertex(v)
  }

  override def degreeOf(v : V) =
  {
    delegate.degreeOf(v)
  }

  override def edgeSet =
  {
    delegate.edgeSet
  }

  override def edgesOf(v : V) =
  {
    delegate.edgesOf(v)
  }

  override def inDegreeOf(v : V) =
  {
    delegate.inDegreeOf(v)
  }

  override def incomingEdgesOf(v : V) =
  {
    delegate.incomingEdgesOf(v)
  }

  override def outDegreeOf(v : V) =
  {
    delegate.outDegreeOf(v)
  }

  override def outgoingEdgesOf(v : V) =
  {
    delegate.outgoingEdgesOf(v)
  }

  override def removeEdge(e : E) =
  {
    delegate.removeEdge(e)
  }

  override def removeEdge(sourceVertex : V, targetVertex : V) =
  {
    delegate.removeEdge(sourceVertex, targetVertex)
  }

  override def removeVertex(v : V) =
  {
    delegate.removeVertex(v)
  }

  override def toString =
  {
    delegate.toString
  }

  override def vertexSet =
  {
    delegate.vertexSet
  }

  override def getEdgeSource(e : E) =
  {
    delegate.getEdgeSource(e)
  }

  override def getEdgeTarget(e : E) =
  {
    delegate.getEdgeTarget(e)
  }

  override def getEdgeWeight(e : E) =
  {
    delegate.getEdgeWeight(e)
  }

  override def setEdgeWeight(e : E, weight : Double) : Unit =
  {
    delegate.setEdgeWeight(e, weight)
  }

  override def getType =
  {
    delegate.getType
  }
}
