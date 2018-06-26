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
import org.jgrapht.graph._
import org.jgrapht.alg.shortestpath._
import org.jgrapht.traverse._
import org.jgrapht.io._

import java.io._

import scala.collection.JavaConverters._

import GmlExporter.Parameter._

object SpcGraph
{
  val LABEL_KIND = "aKindOf"

  def apply() =
  {
    val idealTaxonomy =
      new DirectedAcyclicGraph[SpcIdeal, SpcTaxonomyEdge](
        classOf[SpcTaxonomyEdge])
    val formAssocs =
      new DirectedPseudograph[SpcIdeal, SpcFormAssocEdge](
        classOf[SpcFormAssocEdge])
    val entityAssocs =
      new DirectedPseudograph[SpcEntity, SpcEntityAssocEdge](
        classOf[SpcEntityAssocEdge])
    new SpcGraph(idealTaxonomy, formAssocs, entityAssocs)
  }
}

class SpcLabeledEdge(
  val label : String) extends DefaultEdge
{
  override def hashCode() =
  {
    java.util.Objects.hash(getSource, getTarget, label)
  }

  override def equals(a : Any) =
  {
    a match {
      case that : SpcLabeledEdge => {
        (this.getSource == that.getSource) &&
        (this.getTarget == that.getTarget) &&
        (this.label == that.label)
      }
      case _ => false
    }
  }

  override def toString = super.toString + " : " + label
}

class SpcFormAssocEdge(
  label : String) extends SpcLabeledEdge(label)
{
}

class SpcTaxonomyEdge(
  label : String = SpcGraph.LABEL_KIND) extends SpcLabeledEdge(label)
{
}

class SpcEntityAssocEdge(
  val formEdge : SpcFormAssocEdge) extends SpcLabeledEdge(formEdge.label)
{
}

class SpcGraph(
  val idealTaxonomy : Graph[SpcIdeal, SpcTaxonomyEdge],
  val formAssocs : Graph[SpcIdeal, SpcFormAssocEdge],
  val entityAssocs : Graph[SpcEntity, SpcEntityAssocEdge]
)
{
  def asUnmodifiable() =
  {
    new SpcGraph(
      new AsUnmodifiableGraph(idealTaxonomy),
      new AsUnmodifiableGraph(formAssocs),
      new AsUnmodifiableGraph(entityAssocs)
    )
  }

  def getHyponymIdeal(edge : SpcTaxonomyEdge) =
    idealTaxonomy.getEdgeSource(edge)

  def getHypernymIdeal(edge : SpcTaxonomyEdge) =
    idealTaxonomy.getEdgeTarget(edge)

  def getPossessorForm(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeSource(edge).asInstanceOf[SpcForm]

  def getPossessorEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

  def getPossesseeRole(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeTarget(edge).asInstanceOf[SpcRole]

  // FIXME deal with nonexistent form
  def getPossesseeForm(edge : SpcFormAssocEdge) =
    getFormForRole(getPossesseeRole(edge)).get

  def getPossesseeEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  def getFormAssocEdge(
    possessor : SpcForm,
    possessee : SpcForm,
    label : String) : Option[SpcFormAssocEdge] =
  {
    val edges = formAssocs.edgeSet.asScala.filter(edge =>
      (edge.label == label) &&
        isHyponym(possessor, getPossessorForm(edge)) &&
        isHyponym(possessee, getPossesseeForm(edge)))
    def compareEdges(
      edge1 : SpcFormAssocEdge, edge2 : SpcFormAssocEdge) : Boolean =
    {
      def possessor1 = getPossessorForm(edge1)
      def possessor2 = getPossessorForm(edge2)
      if (isHyponym(possessor1, possessor2)) {
        if (possessor1 == possessor2) {
          val possessee1 = getPossesseeForm(edge1)
          val possessee2 = getPossesseeForm(edge2)
          assert (possessee1 != possessee2)
          isHyponym(possessee1, possessee2)
        } else {
          true
        }
      } else {
        false
      }
    }
    edges.toSeq.sortWith(compareEdges).headOption
  }

  def isHyponym(
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal) : Boolean =
  {
    if (hyponymIdeal == hypernymIdeal) {
      return true
    }
    val path = DijkstraShortestPath.findPathBetween(
      idealTaxonomy, hyponymIdeal, hypernymIdeal)
    return (path != null)
  }

  def getIdealHypernyms(
    ideal : SpcIdeal) : Iterator[SpcIdeal] =
  {
    new BreadthFirstIterator(idealTaxonomy, ideal).asScala
  }

  def getFormHypernyms(
    form : SpcForm) : Iterator[SpcForm] =
  {
    getIdealHypernyms(form).map(_.asInstanceOf[SpcForm])
  }

  def getFormForRole(
    role : SpcRole) : Option[SpcForm] =
  {
    // FIXME:  choose most specific form
    getIdealHypernyms(role).foreach(_ match {
      case form : SpcForm => return Some(form)
      case _ =>
    })
    None
  }

  def render[V, E](graph : Graph[V, E]) : String =
  {
    val exporter = new GmlExporter[V, E]
    exporter.setParameter(EXPORT_VERTEX_LABELS, true)
    exporter.setParameter(EXPORT_EDGE_LABELS, true)
    val sw = new StringWriter
    exporter.exportGraph(graph, sw)
    sw.toString
  }
}
