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
import com.lingeringsocket.shlurd.mind._

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
  includeInverses : Boolean = false,
  includeEntityAssocs : Boolean = false,
  includeSynonyms : Boolean = false,
  includeProperties : Boolean = false,
  includeMeta : Boolean = false
)

object SpcGraphVisualizer
{
  val fullOptions = SpcGraphVisualizationOptions(
    includeIdeals = true, includeEntities = true,
    includeTaxonomy = true, includeRealizations = true,
    includeFormAssocs = true, includeEntityAssocs = true,
    includeInverses = true, includeSynonyms = true,
    includeProperties = true
  )

  val entityFullOptions = SpcGraphVisualizationOptions(
    includeEntities = true, includeRealizations = true,
    includeEntityAssocs = true, includeProperties = true
  )

  val entityAssocOptions = SpcGraphVisualizationOptions(
    includeEntities = true,
    includeEntityAssocs = true
  )

  val shapeBox = stringAttribute("box")
  val shapeRecord = stringAttribute("record")
  val shapeMRecord = stringAttribute("Mrecord")
  val shapeEllipse = stringAttribute("ellipse")
  val shapeHexagon = stringAttribute("hexagon")
  val shapePentagon = stringAttribute("pentagon")

  val arrowEmpty = stringAttribute("empty")
  val arrowOpen = stringAttribute("open")

  val styleDashed = stringAttribute("dashed")
  val styleBold = stringAttribute("bold")

  val dirBoth = stringAttribute("both")

  val formVertexAttributes = Map(
    "shape" -> shapeBox,
    "style" -> styleBold
  )

  val roleVertexAttributes = Map(
    "shape" -> shapeHexagon,
    "style" -> styleBold
  )

  val synonymVertexAttributes = Map(
    "shape" -> shapePentagon,
    "style" -> styleBold
  )

  val formPropertyVertexAttributes = Map(
    "shape" -> shapeRecord,
    "style" -> styleBold
  )

  val entityVertexAttributes = Map(
    "shape" -> shapeMRecord
  )

  val taxonomyEdgeAttributes = Map(
    "arrowhead" -> arrowEmpty,
    "style" -> styleBold
  )

  val roleTaxonomyEdgeAttributes = Map(
    "arrowhead" -> arrowEmpty,
    "style" -> styleBold
  )

  val realizationEdgeAttributes = Map(
    "arrowhead" -> arrowEmpty,
    "style" -> styleDashed
  )

  val formAssocEdgeAttributes = Map(
    "arrowhead" -> arrowOpen,
    "style" -> styleBold
  )

  val formPropertyEdgeAttributes = Map(
    "arrowhead" -> shapeBox,
    "style" -> styleBold
  )

  val entityAssocEdgeAttributes = Map(
    "arrowhead" -> arrowOpen
  )

  val inverseEdgeAttributes = Map(
    "dir" -> dirBoth,
    "style" -> styleBold
  )

  val synonymEdgeAttributes = Map(
    "arrowhead" -> arrowOpen,
    "style" -> styleBold
  )

  case class CombinedVertex(
    obj : SmcNamedObject)
  {
  }

  class CombinedEdge(
    val label : String,
    val attrMap : Map[String, Attribute] = Map.empty)
  {
  }

  type CombinedGraph = Graph[CombinedVertex, CombinedEdge]

  private def stringAttribute(value : String) : Attribute =
  {
    new DefaultAttribute[String](value, AttributeType.STRING)
  }

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

  private val combinedGraph =
    new DefaultDirectedGraph[CombinedVertex, CombinedEdge](
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

  def renderToImageFile(file : File)
  {
    val dot = renderToString()
    val dotStream = new ByteArrayInputStream(dot.getBytes)
    shellCommand(("dot -Tpng" #> file) #< dotStream).!!
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

  private def simpleName(obj : SmcNamedObject) : String =
  {
    obj.name.split(":").last
  }

  private def createDotExporter() =
  {
    new DOTExporter[CombinedVertex, CombinedEdge](
      idGenerator,
      new ComponentNameProvider[CombinedVertex]{
        override def getName(vertex : CombinedVertex) = {
          vertex.obj match {
            case property : SpcProperty => {
              val values = graph.propertyStateIndex.
                accessComponentMap(property).values.map(_.inflected).toSeq
              val valuesExtended = property.domain match {
                case PROPERTY_OPEN_ENUM => values :+ "..."
                case PROPERTY_CLOSED_ENUM => values
                case PROPERTY_TYPE_STRING => Seq("::string::")
              }
              "{" + simpleName(property) + "|{" +
                valuesExtended.mkString("|") + "}}"
            }
            case entity : SpcEntity => {
              if (options.includeProperties) {
                val propertyMap =
                  graph.entityPropertyIndex.accessComponentMap(entity)
                "{" + simpleName(entity) + "|" +
                    propertyMap.map(ps => ps._1 + "=" +
                      ps._2.lemma).mkString("|") + "}"
              } else {
                simpleName(entity)
              }
            }
            case obj => {
              simpleName(obj)
            }
          }
        }
      },
      new ComponentNameProvider[CombinedEdge]{
        override def getName(edge : CombinedEdge) = {
          edge.label
        }
      },
      new ComponentAttributeProvider[CombinedVertex]{
        override def getComponentAttributes(vertex : CombinedVertex) =
        {
          val attrMap = vertex.obj match {
            case _ : SpcEntity => entityVertexAttributes
            case _ : SpcRole => roleVertexAttributes
            case _ : SpcIdealSynonym => synonymVertexAttributes
            case _ : SpcProperty => formPropertyVertexAttributes
            case _ => formVertexAttributes
          }
          attrMap.asJava
        }
      },
      new ComponentAttributeProvider[CombinedEdge]{
        override def getComponentAttributes(edge : CombinedEdge) =
        {
          edge.attrMap.asJava
        }
      })
  }

  private def combineVertex(obj : SmcNamedObject) : CombinedVertex =
  {
    val v = CombinedVertex(obj)
    combinedGraph.addVertex(v)
    v
  }

  private def combineEdge(
    edge : SpcTaxonomyEdge, label : String) : CombinedEdge =
  {
    val attrMap = {
      if (label == "mustBeA") {
        roleTaxonomyEdgeAttributes
      } else {
        taxonomyEdgeAttributes
      }
    }
    new CombinedEdge(label, attrMap)
  }

  private def combineEdge(edge : SpcFormAssocEdge) : CombinedEdge =
  {
    val constraintLabel = edge.constraint match {
      case SpcCardinalityConstraint(0, Int.MaxValue) => "0..*"
      case SpcCardinalityConstraint(0, upper) => s"0..$upper"
      case SpcCardinalityConstraint(lower, Int.MaxValue) => s"$lower..*"
      case SpcCardinalityConstraint(lower, upper) => {
        if (lower == upper) {
          s"$lower"
        } else {
          s"$lower..$upper"
        }
      }
    }
    new CombinedEdge(
      s"has ($constraintLabel)",
      formAssocEdgeAttributes)
  }

  private def combineEdge(edge : SpcEntityAssocEdge) : CombinedEdge =
  {
    new CombinedEdge(edge.getRoleName, entityAssocEdgeAttributes)
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
    if (options.includeIdeals || options.includeSynonyms || options.includeProperties) {
      graph.idealSynonyms.vertexSet.asScala.toSeq.foreach(nym => {
        nym matchPartial {
          case ideal : SpcIdeal if (
            options.includeIdeals || options.includeProperties
          ) => {
            if (includeIdeal(ideal)) {
              lazy val idealVertex = combineVertex(ideal)
              if (options.includeIdeals) {
                idealVertex
              }
              if (options.includeProperties) {
                ideal matchPartial {
                  case form : SpcForm => {
                    val propertyMap =
                      graph.formPropertyIndex.accessComponentMap(form)
                    propertyMap.values.foreach(property => {
                      val propertyVertex = combineVertex(property)
                      combinedGraph.addEdge(
                        idealVertex,
                        propertyVertex,
                        new CombinedEdge(
                          "hasProperty", formPropertyEdgeAttributes))
                    })
                  }
                }
              }
            }
          }
          case synonym : SpcIdealSynonym if (options.includeSynonyms) => {
            val ideal = graph.getIdealBySynonym(synonym)
            if ((ideal.name != simpleName(synonym)) &&
              includeIdeal(ideal))
            {
              combinedGraph.addEdge(
                combineVertex(synonym),
                combineVertex(ideal),
                new CombinedEdge("isSynonymFor", synonymEdgeAttributes))
            }
          }
        }
      })
    }
    if (options.includeTaxonomy) {
      graph.idealTaxonomy.edgeSet.asScala.toSeq.foreach(e => {
        val subclass = graph.getSubclassIdeal(e)
        val superclass = graph.getSuperclassIdeal(e)
        if (includeIdeal(subclass) && includeIdeal(superclass)) {
          val label = tupleN((superclass, subclass)) match {
            case (_ : SpcForm, _ : SpcRole) => "mustBeA"
            case (_ : SpcRole, _ : SpcRole) => "refines"
            case _ => "isKindOf"
          }
          combinedGraph.addEdge(
            combineVertex(subclass),
            combineVertex(superclass),
            combineEdge(e, label))
        }
      })
    }
    if (options.includeEntities || options.includeRealizations) {
      graph.entitySynonyms.vertexSet.asScala.toSeq.
        filter(_.isInstanceOf[SpcEntity]).map(_.asInstanceOf[SpcEntity]).
        foreach(entity => if (includeEntity(entity)) {
          val entityVertex = combineVertex(entity)
          if (options.includeRealizations) {
            combinedGraph.addEdge(
              entityVertex,
              combineVertex(entity.form),
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
        val possessor = graph.getPossessorForm(e)
        val possessee = graph.getPossesseeRole(e)
        if (includeIdeal(possessor) && includeIdeal(possessee)) {
          if (options.includeInverses) {
            graph.getInverseAssocEdge(e).foreach(inverse => {
              if (e.hashCode <= inverse.hashCode) {
                combinedGraph.addEdge(
                  combineVertex(possessee),
                  combineVertex(graph.getPossesseeRole(inverse)),
                  new CombinedEdge("isInverseOf", inverseEdgeAttributes))
              }
            })
          }
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
