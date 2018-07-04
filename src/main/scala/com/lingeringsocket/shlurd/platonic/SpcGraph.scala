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

  override def toString = super.toString + " : " + getRoleName
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

  def getPossessorIdeal(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeSource(edge)

  def getPossessorEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

  def getPossesseeRole(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeTarget(edge).asInstanceOf[SpcRole]

  def getPossesseeEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  def getFormAssocEdge(
    possessor : SpcIdeal,
    role : SpcRole) : Option[SpcFormAssocEdge] =
  {
    getIdealHypernyms(possessor).foreach(hypernym => {
      formAssocs.outgoingEdgesOf(hypernym).asScala.foreach(edge => {
        if (isHyponym(role, getPossesseeRole(edge))) {
          return Some(edge)
        }
      })
    })
    None
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

  def getIdealHyponyms(
    ideal : SpcIdeal) : Iterator[SpcIdeal] =
  {
    new BreadthFirstIterator(
      new EdgeReversedGraph(idealTaxonomy), ideal).asScala
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

  def getFormsForRole(
    role : SpcRole) : Iterable[SpcForm] =
  {
    // choose most specific forms in subgraph of role's hypernyms
    val subgraph = new AsSubgraph(
      idealTaxonomy, getIdealHypernyms(role).filter(_.isForm).toSet.asJava)
    subgraph.vertexSet.asScala.toSeq.filter(
      vertex => subgraph.inDegreeOf(vertex) == 0).map(_.asInstanceOf[SpcForm])
  }

  def getRolesForForm(
    form : SpcForm) : Iterable[SpcRole] =
  {
    getIdealHyponyms(form).toSeq.filter(_.isRole).
      map(_.asInstanceOf[SpcRole]).filter(
        role => isCompatible(form, role))
  }

  def isCompatible(form : SpcForm, role : SpcRole) : Boolean =
  {
    getIdealHypernyms(role).filter(_.isForm).forall(hypernym =>
      isHyponym(form, hypernym))
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

  def render() : String =
  {
    render(idealTaxonomy) + "\n" + render(formAssocs) + "\n" +
      render(entityAssocs)
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
      assert(!getFormsForRole(role).isEmpty, role.toString)
    })
    entityAssocs.edgeSet.asScala.foreach(entityEdge => {
      val formEdge = entityEdge.formEdge
      assert(formAssocs.containsEdge(formEdge),
        entityEdge.toString)
      val possessorIdeal = getPossessorIdeal(formEdge)
      val possessorEntity = getPossessorEntity(entityEdge)
      val possesseeEntity = getPossesseeEntity(entityEdge)
      possessorIdeal match {
        case form : SpcForm => {
          assert(isHyponym(possessorEntity.form, form))
        }
        case role : SpcRole => {
          assert(isCompatible(possessorEntity.form, role))
        }
      }
      val role = getPossesseeRole(formEdge)
      assert(isCompatible(possesseeEntity.form, role),
        (possesseeEntity.form, role).toString)
    })
    val taxonomyCountBeforeReduction = idealTaxonomy.edgeSet.size
    TransitiveReduction.INSTANCE.reduce(idealTaxonomy)
    assert(idealTaxonomy.edgeSet.size == taxonomyCountBeforeReduction)
    true
  }
}
