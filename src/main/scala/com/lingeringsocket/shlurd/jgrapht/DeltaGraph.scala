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
import org.jgrapht.graph.builder._
import org.jgrapht.util._

import scala.collection._
import scala.collection.Set
import scala.collection.JavaConverters._

trait DeltaModification
{
  def applyModifications()
}

object DeltaGraph
{
  private def setToPredicate[A](set : Set[A])
      : java.util.function.Predicate[A] with Serializable =
  {
    new java.util.function.Predicate[A] with Serializable {
      override def test(a: A): Boolean = set.contains(a)
    }
  }

  def apply[V, E](baseGraph : Graph[V, E]) : DeltaGraph[V, E] =
  {
    val plusGraph =
      GraphTypeBuilder.forGraphType[V, E](
        baseGraph.getType.asModifiable).
        edgeSupplier(baseGraph.getEdgeSupplier).
        vertexSupplier(baseGraph.getVertexSupplier).
        buildGraph
    apply(baseGraph, plusGraph,
      new mutable.LinkedHashSet[V], new mutable.LinkedHashSet[E])
  }

  def apply[V, E](
    baseGraph : Graph[V, E],
    plusGraph : Graph[V, E],
    minusVertices : mutable.Set[V],
    minusEdges : mutable.Set[E])
      : DeltaGraph[V, E] =
  {
    val baseType = baseGraph.getType
    val plusType = plusGraph.getType
    assert(plusType.isModifiable)

    // FIXME allow more variations, including mismatches between base and plus
    assert(baseType.isAllowingCycles)
    assert(!baseType.isMixed)
    assert(!baseType.isWeighted)

    assert(baseType.isMixed == plusType.isMixed)
    assert(baseType.isWeighted == plusType.isWeighted)
    assert(baseType.isDirected == plusType.isDirected)
    assert(baseType.isUndirected == plusType.isUndirected)
    assert(baseType.isAllowingCycles == plusType.isAllowingCycles)
    assert(baseType.isAllowingMultipleEdges == plusType.isAllowingMultipleEdges)
    assert(baseType.isAllowingSelfLoops == plusType.isAllowingSelfLoops)

    val baseTransient = TransientGraphDelegator(baseGraph)

    val minusGraph = TransientGraphDelegator(
      new MaskSubgraph(
        baseTransient,
        setToPredicate(minusVertices),
        setToPredicate(minusEdges)))

    val unionGraph = new AsGraphUnion(minusGraph, plusGraph, SerializableWeightCombiner) {
      override def getVertexSupplier = plusGraph.getVertexSupplier
      override def getEdgeSupplier = plusGraph.getEdgeSupplier
    }
    new DeltaGraph(
      baseTransient, unionGraph, plusGraph, minusVertices, minusEdges)
  }

  object SerializableWeightCombiner extends WeightCombiner with Serializable
  {
    override def combine(a : Double, b : Double) =
      WeightCombiner.SUM.combine(a, b)
  }
}

class DeltaGraph[V, E](
  val baseGraph : Graph[V, E],
  val unionGraph : Graph[V, E],
  val plusGraph : Graph[V, E],
  val minusVertices : mutable.Set[V],
  val minusEdges : mutable.Set[E]
) extends GraphDelegator[V, E](unionGraph) with DeltaModification
{
  private def preAddEdge(sourceVertex : V, targetVertex : V) : Boolean =
  {
    assertVertexExist(sourceVertex)
    assertVertexExist(targetVertex)

    if (!baseGraph.getType.isAllowingMultipleEdges &&
      containsEdge(sourceVertex, targetVertex))
    {
      false
    } else {
      plusGraph.addVertex(sourceVertex)
      plusGraph.addVertex(targetVertex)
      true
    }
  }

  override def addEdge(sourceVertex : V, targetVertex : V) : E =
  {
    if (preAddEdge(sourceVertex, targetVertex)) {
      plusGraph.addEdge(sourceVertex, targetVertex)
    } else {
      null.asInstanceOf[E]
    }
  }

  override def addEdge(sourceVertex : V, targetVertex : V, e : E) : Boolean =
  {
    if (preAddEdge(sourceVertex, targetVertex)) {
      if (containsEdge(e)) {
        false
      } else {
        if (minusEdges.contains(e) &&
          (sourceVertex == baseGraph.getEdgeSource(e)) &&
          (targetVertex == baseGraph.getEdgeTarget(e)))
        {
          minusEdges.remove(e)
        } else {
          plusGraph.addEdge(sourceVertex, targetVertex, e)
        }
      }
    } else {
      false
    }
  }

  override def addVertex() : V =
  {
    plusGraph.addVertex
  }

  override def addVertex(v : V) : Boolean =
  {
    if (plusGraph.containsVertex(v)) {
      false
    } else if (baseGraph.containsVertex(v)) {
      minusVertices.remove(v)
    } else {
      plusGraph.addVertex(v)
    }
  }

  override def removeEdge(sourceVertex : V, targetVertex : V) : E =
  {
    val edge = getEdge(sourceVertex, targetVertex)

    if (edge != null) {
      val rc = removeEdge(edge)
      assert(rc)
    }

    edge
  }

  override def removeEdge(e : E) : Boolean =
  {
    if (plusGraph.removeEdge(e)) {
      true
    } else if (baseGraph.containsEdge(e)) {
      minusEdges.add(e)
    } else {
      false
    }
  }

  override def removeVertex(v : V) : Boolean =
  {
    val inBase = baseGraph.containsVertex(v)
    val inPlus = plusGraph.containsVertex(v)
    if (inPlus && inBase) {
      minusVertices.add(v)
      plusGraph.removeVertex(v)
    } else if (inPlus) {
      plusGraph.removeVertex(v)
    } else if (inBase) {
      minusVertices.add(v)
    } else {
      false
    }
  }

  override def getType() : GraphType =
  {
    plusGraph.getType
  }

  override def applyModifications()
  {
    assert(baseGraph.getType.isModifiable)
    val plusVertices = plusGraph.vertexSet.asScala.toSeq
    baseGraph.removeAllEdges(minusEdges.asJava)
    Graphs.addGraph(baseGraph, plusGraph)
    baseGraph.removeAllVertices(minusVertices.asJava)
    plusGraph.removeAllVertices(plusVertices.asJava)
  }

  def swapBase(newBase : Graph[V, E]) : DeltaGraph[V, E] =
  {
    DeltaGraph(newBase, plusGraph, minusVertices, minusEdges)
  }
}
