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
import org.jgrapht.graph._
import org.jgrapht.io._

import scala.collection._
import scala.collection.JavaConverters._

import scala.sys.process._

import java.io._

case class SpcGraphVisualizationOptions(
  includeIdeals : Boolean = false,
  includeEntities : Boolean = false,
  includeTaxonomy : Boolean = false,
  includeRealizations : Boolean = false,
  includeFormAssocs : Boolean = false,
  includeEntityAssocs : Boolean = false,
  // FIXME implement this
  includeSynonyms : Boolean = false,
  // FIXME implement this
  includeComponents : Boolean = false,
  includeMeta : Boolean = false
)

object SpcGraphVisualizer
{
  val fullOptions = SpcGraphVisualizationOptions(
    includeIdeals = true, includeEntities = true,
    includeTaxonomy = true, includeRealizations = true,
    includeFormAssocs = true, includeEntityAssocs = true
  )

  val entityFullOptions = SpcGraphVisualizationOptions(
    includeEntities = true, includeRealizations = true,
    includeEntityAssocs = true
  )

  val entityAssocOptions = SpcGraphVisualizationOptions(
    includeEntities = true,
    includeEntityAssocs = true
  )

  val arrowEmpty =
    new DefaultAttribute[String]("empty", AttributeType.STRING)

  val lineDashed =
    new DefaultAttribute[String]("dashed", AttributeType.STRING)

  val taxonomyEdgeAttributes = Map(
    "arrowhead" -> arrowEmpty
  )

  val realizationEdgeAttributes = Map(
    "arrowhead" -> arrowEmpty,
    "style" -> lineDashed
  )

  case class CombinedVertex(label : String)
  {
  }

  class CombinedEdge(
    val label : String,
    val attrMap : Map[String, Attribute] = Map.empty)
  {
  }

  type CombinedGraph = Graph[CombinedVertex, CombinedEdge]

  def displayFull(graph : SpcGraph)
  {
    val visualizer = new SpcGraphVisualizer(graph, fullOptions)
    visualizer.display
  }

  def displayEntities(graph : SpcGraph)
  {
    val visualizer = new SpcGraphVisualizer(graph, entityFullOptions)
    visualizer.display
  }

  def displayEntityAssociations(graph : SpcGraph)
  {
    val visualizer = new SpcGraphVisualizer(graph, entityAssocOptions)
    visualizer.display
  }
}

class SpcGraphVisualizer(
  graph : SpcGraph,
  options : SpcGraphVisualizationOptions
)
{
  import SpcGraphVisualizer._

  private val idGenerator = new IntegerComponentNameProvider[CombinedVertex]

  private val combinedGraph = new DefaultDirectedGraph[CombinedVertex, CombinedEdge](
    classOf[CombinedEdge])
  combineGraphs

  def display()
  {
    val exporter = createDotExporter
    val dot = renderToString(exporter)
    val dotStream = new ByteArrayInputStream(dot.getBytes)
    shellCommand("xdot -" #< dotStream).!!
  }

  def renderToString(
    exporter : DOTExporter[CombinedVertex, CombinedEdge] =
      createDotExporter) : String =
  {
    val sw = new StringWriter
    renderToWriter(sw, exporter)
    sw.toString
  }

  def renderToFile(file : File)
  {
    val fw = new FileWriter(file)
    renderToWriter(fw)
    fw.close
  }

  def renderToWriter(
    writer : Writer,
    exporter : DOTExporter[CombinedVertex, CombinedEdge] = createDotExporter)
  {
    if (options.includeTaxonomy || options.includeRealizations) {
      exporter.putGraphAttribute("rankdir", "BT")
    } else {
      exporter.putGraphAttribute("rankdir", "LR")
    }
    exporter.exportGraph(combinedGraph, writer)
  }

  private def createDotExporter() =
  {
    new DOTExporter[CombinedVertex, CombinedEdge](
      idGenerator,
      new ComponentNameProvider[CombinedVertex]{
        override def getName(vertex : CombinedVertex) = {
          vertex.label
        }
      },
      new ComponentNameProvider[CombinedEdge]{
        override def getName(edge : CombinedEdge) = {
          edge.label
        }
      },
      null,
      new ComponentAttributeProvider[CombinedEdge]{
        override def getComponentAttributes(edge : CombinedEdge) =
        {
          edge.attrMap.asJava
        }
      })
  }

  private def addCombinedVertex(v : CombinedVertex) : CombinedVertex =
  {
    combinedGraph.addVertex(v)
    v
  }

  private def combineVertex(nym : SpcNym) : CombinedVertex =
  {
    addCombinedVertex(CombinedVertex(nym.toString))
  }

  private def combineVertex(entity : SpcEntity) : CombinedVertex =
  {
    addCombinedVertex(CombinedVertex(s"SpcEntity(${entity.name})"))
  }

  private def combineEdge(edge : SpcTaxonomyEdge) : CombinedEdge =
  {
    new CombinedEdge("isKindOf", taxonomyEdgeAttributes)
  }

  private def combineEdge(edge : SpcFormAssocEdge) : CombinedEdge =
  {
    // FIXME render isProperty/constraint
    new CombinedEdge(edge.getRoleName)
  }

  private def combineEdge(edge : SpcEntityAssocEdge) : CombinedEdge =
  {
    new CombinedEdge(edge.getRoleName)
  }

  private def includeIdeal(ideal : SpcIdeal) : Boolean =
  {
    if (options.includeMeta) {
      true
    } else {
      !SpcMeta.isMetaIdeal(ideal)
    }
  }

  private def includeEntity(entity : SpcEntity) : Boolean =
  {
    if (options.includeMeta) {
      true
    } else {
      !SpcMeta.isMetaEntity(entity)
    }
  }

  private def combineGraphs()
  {
    if (options.includeIdeals) {
      graph.idealSynonyms.vertexSet.asScala.toSeq.
        filter(_.isInstanceOf[SpcIdeal]).map(_.asInstanceOf[SpcIdeal]).foreach(
          v => if (includeIdeal(v)) {
            combinedGraph.addVertex(combineVertex(v))
          }
        )
    }
    if (options.includeTaxonomy) {
      graph.idealTaxonomy.edgeSet.asScala.toSeq.foreach(e => {
        val subclass = graph.getSubclassIdeal(e)
        val superclass = graph.getSuperclassIdeal(e)
        if (includeIdeal(subclass) && includeIdeal(superclass)) {
          combinedGraph.addEdge(
            combineVertex(subclass),
            combineVertex(superclass),
            combineEdge(e))
        }
      })
    }
    if (options.includeEntities || options.includeRealizations) {
      graph.entitySynonyms.vertexSet.asScala.toSeq.
        filter(_.isInstanceOf[SpcEntity]).map(_.asInstanceOf[SpcEntity]).
        foreach(v => if (includeEntity(v)) {
          combinedGraph.addVertex(combineVertex(v))
          if (options.includeRealizations) {
            combinedGraph.addEdge(
              combineVertex(v),
              combineVertex(v.form),
              new CombinedEdge(
                "isA",
                realizationEdgeAttributes
              )
            )
          }
        })
    }
    if (options.includeFormAssocs) {
      graph.formAssocs.edgeSet.asScala.toSeq.foreach(e => {
        val possessor = graph.getPossessorIdeal(e)
        val possessee = graph.getPossesseeRole(e)
        if (includeIdeal(possessor) && includeIdeal(possessee)) {
          combinedGraph.addEdge(
            combineVertex(possessor),
            combineVertex(possessee),
            combineEdge(e))
        }
      })
    }
    if (options.includeEntityAssocs) {
      graph.entityAssocs.edgeSet.asScala.toSeq.foreach(e => {
        val possessor = graph.getPossessorEntity(e)
        val possessee = graph.getPossesseeEntity(e)
        if (includeEntity(possessor) && includeEntity(possessee)) {
          combinedGraph.addEdge(
            combineVertex(possessor),
            combineVertex(possessee),
            combineEdge(e))
        }
      })
    }
  }
}
