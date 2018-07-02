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
import org.jgrapht.alg._
import org.jgrapht.alg.cycle._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.alg.shortestpath._
import org.jgrapht.traverse._
import org.jgrapht.io._

import java.io._

import scala.collection.JavaConverters._

import GmlExporter.Parameter._

object SpcGraph
{
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
  override def toString = super.toString + " : " + label
}

class SpcFormAssocEdge(
  roleName : String) extends SpcLabeledEdge(roleName)
{
  def getRoleName = label
}

class SpcTaxonomyEdge extends DefaultEdge
{
}

class SpcEntityAssocEdge(
  val formEdge : SpcFormAssocEdge) extends DefaultEdge
{
  def getRoleName = formEdge.getRoleName
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

  def getPossesseeForm(edge : SpcFormAssocEdge) =
    getFormForRole(getPossesseeRole(edge))

  def getPossesseeEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  def getFormAssocEdge(
    possessor : SpcForm,
    possessee : SpcForm,
    role : SpcRole) : Option[SpcFormAssocEdge] =
  {
    val edges = formAssocs.edgeSet.asScala.filter(edge => {
      isHyponym(role, getPossesseeRole(edge)) &&
      isHyponym(possessor, getPossessorForm(edge)) &&
      isHyponym(possessee, getPossesseeForm(edge))
    })
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
          isHyponym(possessee1.get, possessee2)
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
    hypernymIdealOpt : Option[SpcIdeal]) : Boolean =
  {
    hypernymIdealOpt match {
      case Some(hypernymIdeal) => {
        isHyponym(hyponymIdeal, hypernymIdeal)
      }
      case _ => false
    }
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
    // choose most specific form in subgraph of role's hypernyms
    val subgraph = new AsSubgraph(
      idealTaxonomy, getIdealHypernyms(role).toSet.asJava)
    val iter = new TopologicalOrderIterator(subgraph)
    iter.asScala.foreach(_ match {
      case form : SpcForm => return Some(form)
      case _ =>
    })
    None
  }

  def specializeRoleForForm(
    role : SpcRole,
    form : SpcForm) : SpcRole =
  {
    // choose role which is lowest common ancestor for inputs;
    // forms cannot be hyponyms of roles, so any LCA must
    // be a role
    val alg = new NaiveLcaFinder(idealTaxonomy)
    Option(alg.findLca(role, form)).
      map(_.asInstanceOf[SpcRole]).getOrElse(role)
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

  def replaceVertex[V, E](graph : Graph[V, E], oldVertex : V, newVertex : V)
  {
    def replaceOld(v : V) = {
      if (v == oldVertex) {
        newVertex
      } else {
        v
      }
    }
    val edgeTriples = graph.edgesOf(oldVertex).asScala.toSeq.map(edge =>
      (edge,
        replaceOld(graph.getEdgeSource(edge)),
        replaceOld(graph.getEdgeTarget(edge))))
    graph.removeAllEdges(edgeTriples.map(_._1).asJava)
    edgeTriples.foreach(edgeTriple => {
      graph.addEdge(edgeTriple._2, edgeTriple._3, edgeTriple._1)
    })
  }

  def sanityCheck() : Boolean =
  {
    idealTaxonomy.edgeSet.asScala.foreach(taxonomyEdge => {
      val hyponym = getHyponymIdeal(taxonomyEdge)
      val hypernym = getHypernymIdeal(taxonomyEdge)
      assert(!(hyponym.isForm && hypernym.isRole), (hyponym, hypernym).toString)
    })
    assert(!new CycleDetector(idealTaxonomy).detectCycles)
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val role = getPossesseeRole(formEdge)
      assert(role.name == formEdge.getRoleName,
        (role, formEdge).toString)
      assert(!getFormForRole(role).isEmpty, role.toString)
    })
    entityAssocs.edgeSet.asScala.foreach(entityEdge => {
      val formEdge = entityEdge.formEdge
      assert(formAssocs.containsEdge(formEdge),
        entityEdge.toString)
      val possessorForm = getPossessorForm(formEdge)
      val possessorEntity = getPossessorEntity(entityEdge)
      val possesseeEntity = getPossesseeEntity(entityEdge)
      assert(isHyponym(possessorEntity.form, possessorForm))
      val role = getPossesseeRole(formEdge)
      getIdealHypernyms(role).filter(_.isForm).foreach(hypernym =>
        assert(isHyponym(possesseeEntity.form, hypernym)))
    })
    val taxonomyCountBeforeReduction = idealTaxonomy.edgeSet.size
    TransitiveReduction.INSTANCE.reduce(idealTaxonomy)
    assert(idealTaxonomy.edgeSet.size == taxonomyCountBeforeReduction)
    true
  }
}
