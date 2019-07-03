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
import com.lingeringsocket.shlurd.jgrapht._
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
  class ExposedListenableGraph[V, E](
    delegate : Graph[V, E]
  ) extends DefaultListenableGraph[V, E](delegate)
  {
    def getExposedDelegate() = delegate
  }

  class ExposedUnmodifiableGraph[V, E](
    delegate : Graph[V, E],
    val isFrozen : Boolean
  ) extends AsUnmodifiableGraph[V, E](delegate)
  {
    def getExposedDelegate() = delegate
  }

  def fork(base : SpcGraph) : SpcGraph =
  {
    val idealSynonyms = DeltaGraph(base.idealSynonyms)
    val idealTaxonomy = DeltaGraph(base.idealTaxonomy)
    val formAssocs = DeltaGraph(base.formAssocs)
    val inverseAssocs = DeltaGraph(base.inverseAssocs)
    val entitySynonyms = DeltaGraph(base.entitySynonyms)
    val entityAssocs = DeltaGraph(base.entityAssocs)
    val componentsDelta = DeltaGraph(base.components)
    val components = new ExposedListenableGraph(componentsDelta)
    val assertions = DeltaGraph(base.assertions)
    val (formPropertyIndex, entityPropertyIndex, propertyStateIndex,
      stateNormalizationIndex) = createIndexes(components)
    new SpcGraph(
      None,
      base.name,
      idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
      entitySynonyms, entityAssocs, components, assertions,
      formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex)
  }

  private def extractDelta(graph : Graph[_, _]) : Option[DeltaModification] =
  {
    graph match {
      case listenable : ExposedListenableGraph[_, _] => {
        extractDelta(listenable.getExposedDelegate)
      }
      case delta : DeltaModification => {
        Some(delta)
      }
      case _ => None
    }
  }

  private def cloneSub[V, E](
    graph : Graph[V, E],
    flattenDeltas : Boolean) : Graph[V, E] =
  {
    cloneSubImpl(graph, flattenDeltas)._1
  }

  private def cloneSubImpl[V, E](
    graph : Graph[V, E],
    flattenDeltas : Boolean) : (Graph[V, E], Boolean) =
  {
    graph match {
      case listenable : ExposedListenableGraph[V, E] => {
        val (subClone, subDelta) =
          cloneSubImpl(listenable.getExposedDelegate, flattenDeltas)
        subClone match {
          case _ : ExposedListenableGraph[V,E] => {
            tupleN((subClone, subDelta))
          }
          case _ => {
            tupleN((new ExposedListenableGraph(subClone), subDelta))
          }
        }
      }
      case unmodifiable : ExposedUnmodifiableGraph[V, E] => {
        if (unmodifiable.isFrozen) {
          tupleN((unmodifiable, false))
        } else {
          cloneSubImpl(unmodifiable.getExposedDelegate, flattenDeltas)
        }
      }
      case transient : TransientGraphDelegator[V, E] => {
        cloneSubImpl(transient.getDelegate, flattenDeltas)
      }
      case delta : DeltaGraph[V, E] => {
        val (baseClone, baseDelta) =
          cloneSubImpl(delta.baseGraph, flattenDeltas)
        val newDelta = DeltaGraph(
          baseClone,
          cloneSub(delta.plusGraph, flattenDeltas),
          delta.minusVertices.clone,
          delta.minusEdges.clone)
        if (flattenDeltas && baseDelta) {
          newDelta.applyModifications
          tupleN((newDelta.baseGraph, false))
        } else {
          tupleN((newDelta, true))
        }
      }
      case _ => {
        val cloned = graph.asInstanceOf[AbstractBaseGraph[V, E]].clone.
          asInstanceOf[Graph[V, E]]
        tupleN((cloned, false))
      }
    }
  }

  private def swapBase[V, E](base : Graph[V, E], forked : Graph[V, E])
      : Graph[V, E] =
  {
    val (modifiable, unmodifiable, isFrozen) = {
      forked match {
        case exposed : ExposedUnmodifiableGraph[V, E] => {
        tupleN((
          exposed.getExposedDelegate,
          true,
          exposed.isFrozen))
        }
        case _ => {
          tupleN((
            forked,
            false,
            false))
        }
      }
    }
    val (underlying, listenable) = {
      modifiable match {
        case exposed : ExposedListenableGraph[V, E] => {
        tupleN((
          exposed.getExposedDelegate,
          true))
        }
        case _ => {
          tupleN((
            modifiable,
            false))
        }
      }
    }
    if (underlying.isInstanceOf[DeltaGraph[V, E]]) {
      val delta = underlying.asInstanceOf[DeltaGraph[V, E]]
      val swapped = delta.swapBase(base)
      val swappedListenable = {
        if (listenable) {
          new ExposedListenableGraph(swapped)
        } else {
          swapped
        }
      }
      if (unmodifiable) {
        new ExposedUnmodifiableGraph(swappedListenable, isFrozen)
      } else {
        swappedListenable
      }
    } else {
      forked
    }
  }

  def apply(name : String) : SpcGraph =
  {
    apply(Some(name))
  }

  def apply(name : Option[String] = None) : SpcGraph =
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
    val components = new ExposedListenableGraph(
      new SimpleDirectedGraph[SpcContainmentVertex, SpcComponentEdge](
        classOf[SpcComponentEdge]))
    val assertions =
      new SimpleDirectedGraph[SpcAssertion, SpcEdge](
        classOf[SpcEdge])
    val (formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex) = createIndexes(components)
    new SpcGraph(
      name,
      None,
      idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
      entitySynonyms, entityAssocs, components, assertions,
      formPropertyIndex, entityPropertyIndex,
      propertyStateIndex, stateNormalizationIndex)
  }

  private def createIndexes(
    componentsIn : Graph[SpcContainmentVertex, SpcComponentEdge]) =
  {
    val components = componentsIn.asInstanceOf[
      ListenableGraph[SpcContainmentVertex, SpcComponentEdge]]
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

case class SpcFormAssocEdge(
  possessor : SpcForm,
  roleName : String
) extends SpcLabeledEdge(roleName)
{
  var isProperty : Boolean = false

  var constraint : SpcCardinalityConstraint =
    SpcCardinalityConstraint(0, Int.MaxValue)

  def getRoleName = label
}

abstract class SpcAnonymousEdge extends SpcEdge
{
  def id : Long
}

case class SpcInverseAssocEdge(id : Long) extends SpcAnonymousEdge
{
}

case class SpcTaxonomyEdge(id : Long) extends SpcAnonymousEdge
{
}

case class SpcSynonymEdge(id : Long) extends SpcAnonymousEdge
{
}

case class SpcEntityAssocEdge(
  id : Long,
  formEdge : SpcFormAssocEdge) extends SpcAnonymousEdge
{
  def getRoleName = formEdge.getRoleName

  override def toString = super.toString + " : " + getRoleName
}

case class SpcComponentEdge(id : Long) extends SpcAnonymousEdge
{
}

class SpcGraph(
  val name : Option[String],
  val baseName : Option[String],
  val idealSynonyms : Graph[SpcNym, SpcSynonymEdge],
  val idealTaxonomy : Graph[SpcIdeal, SpcTaxonomyEdge],
  val formAssocs : Graph[SpcIdeal, SpcFormAssocEdge],
  val inverseAssocs : Graph[SpcFormAssocEdge, SpcInverseAssocEdge],
  val entitySynonyms : Graph[SpcEntityVertex, SpcSynonymEdge],
  val entityAssocs : Graph[SpcEntity, SpcEntityAssocEdge],
  val components : Graph[SpcContainmentVertex, SpcComponentEdge],
  val assertions : Graph[SpcAssertion, SpcEdge],
  val formPropertyIndex : SpcComponentIndex[String, SpcProperty],
  val entityPropertyIndex :
      SpcComponentIndex[String, SpcEntityPropertyState],
  val propertyStateIndex : SpcComponentIndex[String, SpcPropertyState],
  val stateNormalizationIndex :
      SpcComponentIndex[SilState, SpcStateNormalization]
) extends DeltaModification
{
  import SpcGraph._

  private val deltas =
    Seq(idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs,
      entitySynonyms, entityAssocs, components, assertions).flatMap(
      extractDelta)

  def asUnmodifiable() =
  {
    if (getGraphs().exists(_.getType.isModifiable)) {
      val isFrozen = name.nonEmpty
      new SpcGraph(
        name,
        baseName,
        new ExposedUnmodifiableGraph(idealSynonyms, isFrozen),
        new ExposedUnmodifiableGraph(idealTaxonomy, isFrozen),
        new ExposedUnmodifiableGraph(formAssocs, isFrozen),
        new ExposedUnmodifiableGraph(inverseAssocs, isFrozen),
        new ExposedUnmodifiableGraph(entitySynonyms, isFrozen),
        new ExposedUnmodifiableGraph(entityAssocs, isFrozen),
        new ExposedUnmodifiableGraph(components, isFrozen),
        new ExposedUnmodifiableGraph(assertions, isFrozen),
        formPropertyIndex,
        entityPropertyIndex,
        propertyStateIndex,
        stateNormalizationIndex
      )
    } else {
      this
    }
  }

  def rebase(base : SpcGraph) : SpcGraph =
  {
    val componentsSwapped = swapBase(base.components, components)
    val (formPropertyIndexSwapped, entityPropertyIndexSwapped,
      propertyStateIndexSwapped, stateNormalizationIndexSwapped) =
      createIndexes(componentsSwapped)
    new SpcGraph(
      name,
      baseName,
      swapBase(base.idealSynonyms, idealSynonyms),
      swapBase(base.idealTaxonomy, idealTaxonomy),
      swapBase(base.formAssocs, formAssocs),
      swapBase(base.inverseAssocs, inverseAssocs),
      swapBase(base.entitySynonyms, entitySynonyms),
      swapBase(base.entityAssocs, entityAssocs),
      componentsSwapped,
      swapBase(base.assertions, assertions),
      formPropertyIndexSwapped,
      entityPropertyIndexSwapped,
      propertyStateIndexSwapped,
      stateNormalizationIndexSwapped
    )
  }

  def newClone(flattenDeltas : Boolean = false) : SpcGraph =
  {
    val componentsCloned = cloneSub(components, flattenDeltas)
    val (formPropertyIndexCloned, entityPropertyIndexCloned,
      propertyStateIndexCloned, stateNormalizationIndexCloned) =
      createIndexes(componentsCloned)
    new SpcGraph(
      name,
      baseName,
      cloneSub(idealSynonyms, flattenDeltas),
      cloneSub(idealTaxonomy, flattenDeltas),
      cloneSub(formAssocs, flattenDeltas),
      cloneSub(inverseAssocs, flattenDeltas),
      cloneSub(entitySynonyms, flattenDeltas),
      cloneSub(entityAssocs, flattenDeltas),
      componentsCloned,
      cloneSub(assertions, flattenDeltas),
      formPropertyIndexCloned,
      entityPropertyIndexCloned,
      propertyStateIndexCloned,
      stateNormalizationIndexCloned)
  }

  def getGraphs() : Seq[Graph[_, _]] =
  {
    Seq(idealSynonyms, idealTaxonomy, formAssocs, inverseAssocs, entitySynonyms,
      entityAssocs, components, assertions)
  }

  def getContainer(edge : SpcComponentEdge) =
    components.getEdgeSource(edge)

  def getComponent(edge : SpcComponentEdge) =
    components.getEdgeTarget(edge)

  def getSubclassIdeal(edge : SpcTaxonomyEdge) =
    idealTaxonomy.getEdgeSource(edge)

  def getSuperclassIdeal(edge : SpcTaxonomyEdge) =
    idealTaxonomy.getEdgeTarget(edge)

  def getPossessorForm(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeSource(edge).asInstanceOf[SpcForm]

  def getPossesseeRole(edge : SpcFormAssocEdge) =
    formAssocs.getEdgeTarget(edge).asInstanceOf[SpcRole]

  def getPossessorEntity(edge : SpcEntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

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

  def isFormCompatibleWithRole(form : SpcForm, role : SpcRole) : Boolean =
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
    render(assertions, "Assertions")
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
    container : SpcContainmentVertex, component : SpcContainmentVertex,
    edgeId : Long)
  {
    components.addVertex(component)
    components.addEdge(container, component, SpcComponentEdge(edgeId))
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
      assert(formAssocs.getEdgeSource(formEdge).isInstanceOf[SpcForm], formEdge)
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
      assert(entity.isInstanceOf[SpcPersistentEntity])
      assert(entitySynonyms.inDegreeOf(synonym) == 0)
      assert(entitySynonyms.outDegreeOf(synonym) == 1)
      assert(entitySynonyms.inDegreeOf(entity) == 1)
      assert(entitySynonyms.outDegreeOf(entity) == 0)
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
    assert(assertions.edgeSet.size == 0)
    true
  }

  override def applyModifications()
  {
    deltas.foreach(_.applyModifications)
  }
}
