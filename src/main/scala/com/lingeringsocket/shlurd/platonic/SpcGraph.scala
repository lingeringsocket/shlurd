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
import com.lingeringsocket.shlurd.ilang._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.alg._
import org.jgrapht.alg.cycle._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.alg.shortestpath._
import org.jgrapht.alg.lca._
import org.jgrapht.traverse._
import org.jgrapht.io._

import java.io._

import scala.collection.JavaConverters._

object SpcGraph
{
  def fork(base : SpcGraph) : SpcGraph =
  {
    val idealSynonyms = DeltaGraph(base.idealSynonyms)
    val idealTaxonomy = DeltaGraph(base.idealTaxonomy)
    val formAssocs = DeltaGraph(base.formAssocs)
    val inverseAssocs = DeltaGraph(base.inverseAssocs)
    val entitySynonyms = DeltaGraph(base.entitySynonyms)
    val entityAssocs = DeltaGraph(base.entityAssocs)
    val componentsDelta = DeltaGraph(base.components)
    val components = new DefaultListenableGraph(componentsDelta)
    val triggers = DeltaGraph(base.triggers)
    val (formPropertyIndex, entityPropertyIndex, propertyStateIndex,
      stateNormalizationIndex) = createIndexes(components)
    new SpcGraph(
      idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
      entitySynonyms, entityAssocs, components, triggers,
      formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex,
      Seq(idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
        entitySynonyms, entityAssocs, componentsDelta, triggers))
  }

  def apply() : SpcGraph =
  {
    val idealSynonyms =
      new SimpleDirectedGraph[SpcNym, SpcSynonymEdge](
        classOf[SpcSynonymEdge])
    val idealTaxonomy =
      new SimpleDirectedGraph[SpcIdeal, SpcTaxonomyEdge](
        classOf[SpcTaxonomyEdge])
    val formAssocs =
      new DirectedPseudograph[SpcIdeal, SpcFormAssocEdge](
        classOf[SpcFormAssocEdge])
    val inverseAssocs =
      new DefaultUndirectedGraph[SpcFormAssocEdge, SpcInverseAssocEdge](
        classOf[SpcInverseAssocEdge])
    val entitySynonyms =
      new SimpleDirectedGraph[SpcEntityVertex, SpcSynonymEdge](
        classOf[SpcSynonymEdge])
    val entityAssocs =
      new DirectedPseudograph[SpcEntity, SpcEntityAssocEdge](
        classOf[SpcEntityAssocEdge])
    val components = new DefaultListenableGraph(
      new SimpleDirectedGraph[SpcContainmentVertex, SpcComponentEdge](
        classOf[SpcComponentEdge]))
    val triggers =
      new SimpleDirectedGraph[SilConditionalSentence, SpcEdge](
        classOf[SpcEdge])
    val (formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex) = createIndexes(components)
    new SpcGraph(
      idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
      entitySynonyms, entityAssocs, components, triggers,
      formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex)
  }

  private def createIndexes(
    components : ListenableGraph[SpcContainmentVertex, SpcComponentEdge]) =
  {
    val formPropertyIndex =
      new SpcComponentIndex[String, SpcProperty](
        components, _ match {
          case property : SpcProperty => Some(property.name)
          case _ => None
        })
    val entityPropertyIndex =
      new SpcComponentIndex[String, SpcEntityPropertyState](
        components, _ match {
          case ps : SpcEntityPropertyState => Some(ps.propertyName)
          case _ => None
        })
    val propertyStateIndex =
      new SpcComponentIndex[String, SpcPropertyState](
        components, _ match {
          case ps : SpcPropertyState => Some(ps.lemma)
          case _ => None
        })
    val stateNormalizationIndex =
      new SpcComponentIndex[SilState, SpcStateNormalization](
        components, _ match {
          case sn : SpcStateNormalization => Some(sn.original)
          case _ => None
        })
    tupleN((formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex))
  }
}

class SpcEdge
{
  override def toString = super.toString.replaceAllLiterally(
    "com.lingeringsocket.shlurd.platonic.", "")
}

class SpcLabeledEdge(
  val label : String) extends SpcEdge
{
  override def toString = super.toString + " : " + label
}

class SpcFormAssocEdge(
  roleName : String,
  var isProperty : Boolean = false,
  var constraint : SpcCardinalityConstraint =
    SpcCardinalityConstraint(0, Int.MaxValue)
) extends SpcLabeledEdge(roleName)
{
  def getRoleName = label
}

class SpcInverseAssocEdge extends SpcEdge
{
}

class SpcTaxonomyEdge extends SpcEdge
{
}

class SpcSynonymEdge extends SpcEdge
{
}

class SpcEntityAssocEdge(
  val formEdge : SpcFormAssocEdge) extends SpcEdge
{
  def getRoleName = formEdge.getRoleName

  override def toString = super.toString + " : " + getRoleName
}

class SpcComponentEdge extends SpcEdge
{
}

class SpcGraph(
  val idealSynonyms : Graph[SpcNym, SpcSynonymEdge],
  val idealTaxonomy : Graph[SpcIdeal, SpcTaxonomyEdge],
  val formAssocs : Graph[SpcIdeal, SpcFormAssocEdge],
  val inverseAssocs : Graph[SpcFormAssocEdge, SpcInverseAssocEdge],
  val entitySynonyms : Graph[SpcEntityVertex, SpcSynonymEdge],
  val entityAssocs : Graph[SpcEntity, SpcEntityAssocEdge],
  val components : Graph[SpcContainmentVertex, SpcComponentEdge],
  val triggers : Graph[SilConditionalSentence, SpcEdge],
  val formPropertyIndex : SpcComponentIndex[String, SpcProperty],
  val entityPropertyIndex :
      SpcComponentIndex[String, SpcEntityPropertyState],
  val propertyStateIndex : SpcComponentIndex[String, SpcPropertyState],
  val stateNormalizationIndex :
      SpcComponentIndex[SilState, SpcStateNormalization],
  deltas : Iterable[DeltaModification] = Iterable.empty
) extends DeltaModification
{
  def asUnmodifiable() =
  {
    if (getGraphs().exists(_.getType.isModifiable)) {
      new SpcGraph(
        new AsUnmodifiableGraph(idealSynonyms),
        new AsUnmodifiableGraph(idealTaxonomy),
        new AsUnmodifiableGraph(formAssocs),
        new AsUnmodifiableGraph(inverseAssocs),
        new AsUnmodifiableGraph(entitySynonyms),
        new AsUnmodifiableGraph(entityAssocs),
        new AsUnmodifiableGraph(components),
        new AsUnmodifiableGraph(triggers),
        formPropertyIndex,
        entityPropertyIndex,
        propertyStateIndex,
        stateNormalizationIndex
      )
    } else {
      this
    }
  }

  def getGraphs() : Seq[Graph[_, _]] =
  {
    Seq(idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs, entitySynonyms,
      entityAssocs, components, triggers)
  }

  def getContainer(edge : SpcComponentEdge) =
    components.getEdgeSource(edge)

  def getComponent(edge : SpcComponentEdge) =
    components.getEdgeTarget(edge)

  def getSubclassIdeal(edge : SpcTaxonomyEdge) =
    idealTaxonomy.getEdgeSource(edge)

  def getSuperclassIdeal(edge : SpcTaxonomyEdge) =
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

  def getFormHyponyms(
    form : SpcForm) : Iterator[SpcForm] =
  {
    getIdealHyponyms(form).filter(_.isInstanceOf[SpcForm]).
      map(_.asInstanceOf[SpcForm])
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
        role => isFormCompatibleWithRole(form, role))
  }

  def isFormCompatibleWithRole(form : SpcForm, role : SpcRole) : Boolean =
  {
    getIdealHypernyms(role).filter(_.isForm).forall(hypernym =>
      isHyponym(form, hypernym))
  }

  def isFormCompatibleWithIdeal(
    form : SpcForm, possessorIdeal : SpcIdeal) : Boolean =
  {
    possessorIdeal match {
      case possessorForm : SpcForm => {
        isHyponym(form, possessorForm)
      }
      case role : SpcRole => {
        isFormCompatibleWithRole(form, role)
      }
    }
  }

  def specializeRoleForForm(
    role : SpcRole,
    form : SpcForm) : SpcRole =
  {
    // choose role which is lowest common ancestor for inputs;
    // forms cannot be hyponyms of roles, so any LCA must
    // be a role
    val alg = new NaiveLCAFinder(idealTaxonomy)
    Option(alg.getLCA(role, form)).
      map(_.asInstanceOf[SpcRole]).getOrElse(role)
  }

  def closestCommonHypernym(
    ideal1 : SpcIdeal, ideal2 : SpcIdeal) : Option[SpcIdeal] =
  {
    val alg = new NaiveLCAFinder(new EdgeReversedGraph(idealTaxonomy))
    Option(alg.getLCA(ideal1, ideal2))
  }

  def render() : String =
  {
    render(idealSynonyms, "Ideal synonyms") + "\n" +
    render(idealTaxonomy, "Ideal taxonomy") + "\n" +
    render(formAssocs, "Form associations") + "\n" +
    render(inverseAssocs, "Inverse associations") + "\n" +
    render(components, "Components") + "\n" +
    render(entitySynonyms, "Entity synonyms") + "\n" +
    render(entityAssocs, "Entity associations") + "\n" +
    render(triggers, "Triggers")
  }

  def render[V, E](graph : Graph[V, E], title : String) : String =
  {
    val exporter = new GmlExporter[V, E]
    exporter.setVertexIDProvider(new StringComponentNameProvider[V])
    exporter.setEdgeIDProvider(new StringComponentNameProvider[E])
    val sw = new StringWriter
    sw.write("# ")
    sw.write(title)
    sw.write("\n")
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
      tupleN((edge,
        replaceOld(graph.getEdgeSource(edge)),
        replaceOld(graph.getEdgeTarget(edge)))))
    graph.removeAllEdges(edgeTriples.map(_._1).asJava)
    edgeTriples.foreach(edgeTriple => {
      graph.addEdge(edgeTriple._2, edgeTriple._3, edgeTriple._1)
    })
  }

  def removeContainer(container : SpcContainmentVertex)
  {
    val reachable =
      new BreadthFirstIterator(components, container).asScala.toSet
    components.removeAllVertices(reachable.asJava)
  }

  def addComponent(
    container : SpcContainmentVertex, component : SpcContainmentVertex)
  {
    components.addVertex(component)
    components.addEdge(container, component)
  }

  def removeComponent(
    component : SpcContainmentVertex)
  {
    components.removeVertex(component)
  }

  def sanityCheck() : Boolean =
  {
    assert(!idealSynonyms.vertexSet.asScala.exists(v =>
      idealSynonyms.degreeOf(v) == 0))
    idealSynonyms.edgeSet.asScala.foreach(synonymEdge => {
      val synonym = idealSynonyms.getEdgeSource(synonymEdge)
      val ideal = idealSynonyms.getEdgeTarget(synonymEdge)
      assert(synonym.isInstanceOf[SpcIdealSynonym])
      assert(ideal.isInstanceOf[SpcIdeal])
      assert(idealSynonyms.inDegreeOf(synonym) == 0)
      assert(idealSynonyms.outDegreeOf(synonym) == 1)
      assert(idealSynonyms.outDegreeOf(ideal) == 0)
    })
    idealTaxonomy.edgeSet.asScala.foreach(taxonomyEdge => {
      val subclass = getSubclassIdeal(taxonomyEdge)
      val superclass = getSuperclassIdeal(taxonomyEdge)
      assert(!(subclass.isForm && superclass.isRole),
        tupleN((subclass, superclass)).toString)
    })
    assert(!new CycleDetector(idealTaxonomy).detectCycles)
    assert(!new CycleDetector(idealSynonyms).detectCycles)
    assert(!new CycleDetector(entitySynonyms).detectCycles)
    assert(!new CycleDetector(components).detectCycles)
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val role = getPossesseeRole(formEdge)
      assert(role.name == formEdge.getRoleName,
        tupleN((role, formEdge)).toString)
      assert(!getFormsForRole(role).isEmpty, role.toString)
    })
    val inverseAssocsVertexSet = inverseAssocs.vertexSet.asScala
    assert(inverseAssocsVertexSet.subsetOf(
      formAssocs.edgeSet.asScala))
    inverseAssocsVertexSet.foreach(
      vertex => assert(inverseAssocs.edgesOf(vertex).size == 1))
    assert(!entitySynonyms.vertexSet.asScala.exists(v =>
      entitySynonyms.degreeOf(v) == 0))
    entitySynonyms.edgeSet.asScala.foreach(synonymEdge => {
      val synonym = entitySynonyms.getEdgeSource(synonymEdge)
      val entity = entitySynonyms.getEdgeTarget(synonymEdge)
      assert(synonym.isInstanceOf[SpcEntitySynonym])
      assert(entity.isInstanceOf[SpcEntity])
      assert(entitySynonyms.inDegreeOf(synonym) == 0)
      assert(entitySynonyms.outDegreeOf(synonym) == 1)
      assert(entitySynonyms.inDegreeOf(entity) == 1)
      assert(entitySynonyms.outDegreeOf(entity) == 0)
    })
    entityAssocs.edgeSet.asScala.foreach(entityEdge => {
      val formEdge = entityEdge.formEdge
      assert(formAssocs.containsEdge(formEdge),
        entityEdge.toString)
      val possessorIdeal = getPossessorIdeal(formEdge)
      val possessorEntity = getPossessorEntity(entityEdge)
      val possesseeEntity = getPossesseeEntity(entityEdge)
      assert(isFormCompatibleWithIdeal(possessorEntity.form, possessorIdeal))
      val role = getPossesseeRole(formEdge)
      assert(isFormCompatibleWithRole(possesseeEntity.form, role),
        tupleN((possesseeEntity.form, role)).toString)
    })
    components.edgeSet.asScala.foreach(componentEdge => {
      val container = getContainer(componentEdge)
      val component = getComponent(componentEdge)
      assert(components.inDegreeOf(component) == 1)
      val pair = (container, component)
      pair match {
        case (form : SpcForm, property : SpcProperty) => {
        }
        case (form : SpcForm, normalization : SpcStateNormalization) => {
        }
        case (property : SpcProperty, state : SpcPropertyState) => {
        }
        case (entity : SpcEntity, state : SpcEntityPropertyState) => {
        }
        case unexpected => assert(false, unexpected)
      }
    })
    val taxonomyCountBeforeReduction = idealTaxonomy.edgeSet.size
    TransitiveReduction.INSTANCE.reduce(idealTaxonomy)
    assert(idealTaxonomy.edgeSet.size == taxonomyCountBeforeReduction)
    assert(triggers.edgeSet.size == 0)
    true
  }

  override def applyModifications()
  {
    deltas.foreach(_.applyModifications)
  }
}
