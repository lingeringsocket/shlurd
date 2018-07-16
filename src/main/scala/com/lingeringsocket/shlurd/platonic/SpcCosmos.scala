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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.cosmos._

import spire.math._

import scala.io._
import scala.util._
import scala.collection._
import scala.collection.JavaConverters._

import java.util.concurrent.atomic._

import org.jgrapht._
import org.jgrapht.util._

import ShlurdEnglishLemmas._

trait SpcIdealVertex
{
}

class SpcPropertyState(val lemma : String, val inflected : String)
    extends SpcIdealVertex
{
  override def toString = s"SpcPropertyState($lemma -> $inflected)"
}

class SpcProperty(val name : String, val isClosed : Boolean)
    extends ShlurdProperty with ShlurdNamedObject with SpcIdealVertex
{
  def isSynthetic = name.contains('_')

  override def toString = s"SpcProperty($name)"
}

sealed trait SpcNym
    extends ShlurdNamedObject
{
  def isIdeal : Boolean = false

  def isRole : Boolean = false

  def isForm : Boolean = false

  def isSynonym : Boolean = false
}

case class SpcIdealSynonym(name : String)
    extends SpcNym
{
  override def isSynonym = true
}

sealed abstract class SpcIdeal(val name : String)
    extends SpcNym
{
  override def isIdeal : Boolean = true
}

class SpcStateNormalization(
  val original : SilState,
  val normalized : SilState,
  val isInflected : Boolean)
    extends SpcIdealVertex
{
}

object SpcForm
{
  private val TENTATIVE_SUFFIX = "_form"

  def tentativeName(word : SilWord) = SilWord(word.lemma + TENTATIVE_SUFFIX)
}

class SpcForm(name : String)
    extends SpcIdeal(name) with SpcIdealVertex
{
  import SpcForm._

  def isTentative = name.endsWith(TENTATIVE_SUFFIX)

  override def isForm = true

  override def toString = s"SpcForm($name)"
}

class SpcRole(name : String)
    extends SpcIdeal(name)
{
  override def isRole = true

  override def toString = s"SpcRole($name)"
}

trait SpcEntityVertex extends ShlurdNamedObject
{
}

case class SpcEntity(
  val name : String,
  val form : SpcForm,
  val qualifiers : Set[String],
  val properName : String = "")
    extends ShlurdEntity with SpcEntityVertex
{
  override def isTentative = properName.contains("_")
}

case class SpcEntitySynonym(val name : String)
    extends SpcEntityVertex
{
}

class SpcCosmos(
  graph : SpcGraph = SpcGraph(),
  idGenerator : AtomicLong = new AtomicLong
) extends ShlurdCosmos[SpcEntity, SpcProperty] with DeltaModification
{
  private val unmodifiableGraph = graph.asUnmodifiable

  def getForms = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isForm).map(_.asInstanceOf[SpcForm])

  def getRoles = graph.idealSynonyms.vertexSet.asScala.toSeq.
    filter(_.isRole).map(_.asInstanceOf[SpcRole])

  def getEntities =
    graph.entitySynonyms.vertexSet.asScala.toSeq.
      filter(_.isInstanceOf[SpcEntity]).map(_.asInstanceOf[SpcEntity])

  def getGraph = unmodifiableGraph

  private def getIdGenerator = idGenerator

  protected[platonic] def annotateFormAssoc(
    edge : SpcFormAssocEdge, constraint : SpcCardinalityConstraint,
    isProperty : Boolean)
  {
    edge.constraint = constraint
    edge.isProperty = isProperty
  }

  def fork() : SpcCosmos =
  {
    new SpcCosmos(SpcGraph.fork(graph), idGenerator)
  }

  def clear()
  {
    graph.entitySynonyms.removeAllVertices(
      new ArrayUnenforcedSet(graph.entitySynonyms.vertexSet))
    graph.entityAssocs.removeAllVertices(
      new ArrayUnenforcedSet(graph.entityAssocs.vertexSet))
  }

  protected[platonic] def copyFrom(src : SpcCosmos)
  {
    assert(idGenerator.get == 0)
    val dstGraphs = graph.getGraphs
    dstGraphs.foreach(graph => assert(graph.vertexSet.isEmpty))
    idGenerator.set(src.getIdGenerator.get)
    dstGraphs.zip(src.getGraph.getGraphs).foreach({
      case (dstGraph, srcGraph) => {
        // FIXME find a way to do this without ugly casts
        val dstGraphUp = dstGraph.asInstanceOf[Graph[Any, Any]]
        val srcGraphUp = srcGraph.asInstanceOf[Graph[Any, Any]]
        Graphs.addGraph(dstGraphUp, srcGraphUp)
      }
    })
  }

  private def registerIdeal(ideal : SpcIdeal) =
  {
    val synonym = SpcIdealSynonym(ideal.name)
    assert(!graph.idealSynonyms.containsVertex(synonym))
    graph.idealSynonyms.addVertex(synonym)
    graph.idealSynonyms.addVertex(ideal)
    graph.idealSynonyms.addEdge(synonym, ideal)
    graph.idealTaxonomy.addVertex(ideal)
    graph.formAssocs.addVertex(ideal)
    getIdealBySynonym(LEMMA_ENTITY) match {
      case Some(entityForm) => {
        addIdealTaxonomy(ideal, entityForm)
      }
      case _ =>
    }
    ideal
  }

  private def forgetIdeal(ideal : SpcIdeal)
  {
    assert(graph.idealSynonyms.degreeOf(ideal) == 0)
    assert(graph.idealTaxonomy.inDegreeOf(ideal) == 0)
    assert(graph.formAssocs.degreeOf(ideal) == 0)
    graph.idealSynonyms.removeVertex(ideal)
    graph.idealTaxonomy.removeVertex(ideal)
    graph.formAssocs.removeVertex(ideal)
  }

  private def registerForm(form : SpcForm) =
  {
    registerIdeal(form)
    graph.components.addVertex(form)
    form
  }

  private def registerRole(role : SpcRole) =
  {
    registerIdeal(role)
    role
  }

  private def getIdealBySynonym(name : String) : Option[SpcIdeal] =
  {
    val synonym = SpcIdealSynonym(name)
    if (graph.idealSynonyms.containsVertex(synonym)) {
      Some(Graphs.successorListOf(
        graph.idealSynonyms, synonym).iterator.next.asInstanceOf[SpcIdeal])
    } else {
      None
    }
  }

  protected[platonic] def getEntityBySynonym(name : String)
      : Option[SpcEntity] =
  {
    val synonym = SpcEntitySynonym(name)
    if (graph.entitySynonyms.containsVertex(synonym)) {
      Some(Graphs.successorListOf(
        graph.entitySynonyms, synonym).iterator.next.asInstanceOf[SpcEntity])
    } else {
      None
    }
  }

  def resolveIdealSynonym(name : String) : String =
  {
    getIdealBySynonym(name) match {
      case Some(ideal) => ideal.name
      case _ => name
    }
  }

  def instantiateIdeal(word : SilWord, assumeRole : Boolean = false) =
  {
    getIdealBySynonym(word.lemma).getOrElse({
      if (assumeRole) {
        instantiateRole(word)
      } else {
        instantiateForm(word)
      }
    })
  }

  def resolveForm(name : String) = resolveIdeal(name)._1

  def resolveRole(name : String) = resolveIdeal(name)._2

  def instantiateForm(word : SilWord) =
  {
    val name = word.lemma
    val ideal = getIdealBySynonym(name).getOrElse(
      registerForm(new SpcForm(name)))
    assert(ideal.isForm)
    ideal.asInstanceOf[SpcForm]
  }

  def instantiateRole(word : SilWord) =
  {
    val name = word.lemma
    val ideal = getIdealBySynonym(name).getOrElse(
      registerRole(new SpcRole(name)))
    assert(ideal.isRole)
    ideal.asInstanceOf[SpcRole]
  }

  private[platonic] def forgetForm(form : SpcForm)
  {
    assert(!getEntities.exists(_.form == form))
    val synonymEdges = graph.idealSynonyms.incomingEdgesOf(form).asScala.toSeq
    synonymEdges.foreach(edge => graph.idealSynonyms.removeVertex(
      graph.idealSynonyms.getEdgeSource(edge)))
    graph.removeContainer(form)
    forgetIdeal(form)
  }

  private[platonic] def forgetEntity(entity : SpcEntity)
  {
    assert(graph.entityAssocs.degreeOf(entity) == 0)
    if (getEntityBySynonym(entity.name) == Some(entity)) {
      val synonymEdges = graph.entitySynonyms.
        incomingEdgesOf(entity).asScala.toSeq
      synonymEdges.foreach(edge => graph.entitySynonyms.removeVertex(
        graph.entitySynonyms.getEdgeSource(edge)))
    }
    assert(graph.entitySynonyms.degreeOf(entity) == 0)
    graph.entitySynonyms.removeVertex(entity)
    graph.entityAssocs.removeVertex(entity)
  }

  def matchAssocs(oldForm : SpcForm, newForm : SpcForm)
      : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)]=
  {
    val formAssocs = graph.formAssocs
    formAssocs.outgoingEdgesOf(oldForm).asScala.toSeq.flatMap(
      oldEdge => {
        assert(!graph.inverseAssocs.containsVertex(oldEdge))
        graph.getFormAssocEdge(newForm, graph.getPossesseeRole(oldEdge)).map(
          newEdge => (oldEdge, newEdge))
      }
    )
  }

  private[platonic] def isValidMergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge) : Boolean =
  {
    val entityAssocs = graph.entityAssocs
    val constraint = newEdge.constraint
    if (constraint.upper < Int.MaxValue) {
      val oldEntityEdges =
        entityAssocs.edgeSet.asScala.filter(_.formEdge == oldEdge)
      oldEntityEdges.groupBy(graph.getPossessorEntity(_)).
        filter(_._2.size > constraint.upper).isEmpty
    } else {
      true
    }
  }

  private[platonic] def mergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge)
  {
    assert(!graph.inverseAssocs.containsVertex(oldEdge))
    // FIXME we should be able to support this
    assert(!graph.inverseAssocs.containsVertex(newEdge))
    val formAssocs = graph.formAssocs
    val entityAssocs = graph.entityAssocs
    val oldEntityEdges =
      entityAssocs.edgeSet.asScala.filter(_.formEdge == oldEdge)
    oldEntityEdges.foreach(
      entityEdge => {
        entityAssocs.addEdge(
          graph.getPossessorEntity(entityEdge),
          graph.getPossesseeEntity(entityEdge),
          new SpcEntityAssocEdge(newEdge))
        entityAssocs.removeEdge(entityEdge)
      }
    )
    formAssocs.removeEdge(oldEdge)
  }

  private[platonic] def replaceForm(oldForm : SpcForm, newForm : SpcForm)
  {
    assert(oldForm.isTentative)
    assert(graph.components.degreeOf(oldForm) == 0)
    graph.replaceVertex(graph.formAssocs, oldForm, newForm)
    forgetForm(oldForm)
  }

  private[platonic] def replaceEntity(
    oldEntity : SpcEntity, newEntity : SpcEntity)
  {
    // FIXME verify that entities are role-compatible across all
    // relevant form associations, constraints aren't violated, etc
    if (oldEntity != newEntity) {
      graph.replaceVertex(graph.entityAssocs, oldEntity, newEntity)
      forgetEntity(oldEntity)
    }
  }

  def addIdealSynonym(synonymName : String, fundamentalName : String)
  {
    val synonym = SpcIdealSynonym(synonymName)
    assert(!graph.idealSynonyms.containsVertex(synonym))
    graph.idealSynonyms.addVertex(synonym)
    val ideal = getIdealBySynonym(fundamentalName).get
    graph.idealSynonyms.addEdge(synonym, ideal)
  }

  protected[platonic] def getIdealSynonyms =
    graph.idealSynonyms.edgeSet.asScala.toSeq.map(edge =>
      (graph.idealSynonyms.getEdgeSource(edge).name,
        graph.idealSynonyms.getEdgeTarget(edge).name))

  def getIdealTaxonomyGraph =
    unmodifiableGraph.idealTaxonomy

  def getFormAssocGraph =
    unmodifiableGraph.formAssocs

  def getEntityAssocGraph =
    unmodifiableGraph.entityAssocs

  def getInverseAssocEdges : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)] =
    graph.inverseAssocs.vertexSet.asScala.toSeq.map(vertex =>
      (vertex, getInverseAssocEdge(vertex).get))

  private def hasQualifiers(
    existing : SpcEntity,
    form : SpcForm,
    qualifiers : Set[String],
    overlap : Boolean) : Boolean =
  {
    if (overlap) {
      graph.isHyponym(form, existing.form) &&
        (qualifiers.subsetOf(existing.qualifiers) ||
          existing.qualifiers.subsetOf(qualifiers))
    } else {
      graph.isHyponym(existing.form, form) &&
        qualifiers.subsetOf(existing.qualifiers)
    }
  }

  protected[platonic] def instantiateEntity(
    form : SpcForm,
    qualifierString : Seq[SilWord],
    properName : String = "") : (SpcEntity, Boolean) =
  {
    val qualifiers = qualifierSet(qualifierString)
    if (properName.isEmpty) {
      getEntities.find(hasQualifiers(_, form, qualifiers, true)) match {
        case Some(entity) => {
          return (entity, false)
        }
        case _ =>
      }
    } else {
      getEntities.filter(_.properName == properName).headOption match {
        case Some(entity) => {
          return (entity, false)
        }
        case _ =>
      }
    }
    val formId = idGenerator.getAndIncrement.toString
    val name =
      (qualifierString.map(_.lemma) ++
        Seq(form.name, formId)).mkString("_")
    val entity = new SpcEntity(name, form, qualifiers, properName)
    createOrReplaceEntity(entity)
    (entity, true)
  }

  protected[platonic] def createOrReplaceEntity(entity : SpcEntity)
  {
    graph.entityAssocs.addVertex(entity)
    graph.entitySynonyms.addVertex(entity)
    val synonym = SpcEntitySynonym(entity.name)
    getEntityBySynonym(entity.name) match {
      case Some(old) => {
        graph.entitySynonyms.removeEdge(synonym, old)
        graph.entitySynonyms.addEdge(synonym, entity)
        replaceEntity(old, entity)
      }
      case _ => {
        graph.entitySynonyms.addVertex(synonym)
        graph.entitySynonyms.addEdge(synonym, entity)
      }
    }
  }

  def getInverseAssocEdge(edge : SpcFormAssocEdge)
      : Option[SpcFormAssocEdge] =
  {
    val inverseAssocs = graph.inverseAssocs
    if (!inverseAssocs.containsVertex(edge)) {
      None
    } else {
      Some(Graphs.neighborListOf(inverseAssocs, edge).get(0))
    }
  }

  protected[platonic] def connectInverseAssocEdges(
    edge1 : SpcFormAssocEdge,
    edge2 : SpcFormAssocEdge)
  {
    val inverseAssocs = graph.inverseAssocs
    getInverseAssocEdge(edge1) match {
      case Some(existing) => {
        if (existing == edge2) {
          return
        }
      }
      case _ =>
    }
    inverseAssocs.removeVertex(edge1)
    inverseAssocs.removeVertex(edge2)
    inverseAssocs.addVertex(edge1)
    inverseAssocs.addVertex(edge2)
    inverseAssocs.addEdge(edge1, edge2)
  }

  protected[platonic] def addIdealTaxonomy(
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal)
  {
    if (!graph.isHyponym(hyponymIdeal, hypernymIdeal)) {
      val idealTaxonomy = graph.idealTaxonomy
      val newHypernyms = graph.getIdealHypernyms(hypernymIdeal)
      val redundantEdges = idealTaxonomy.outgoingEdgesOf(hyponymIdeal).
        asScala.filter(
          edge => newHypernyms.contains(graph.getHypernymIdeal(edge)))
      val newEdge = new SpcTaxonomyEdge()
      val added = idealTaxonomy.addEdge(hyponymIdeal, hypernymIdeal, newEdge)
      // since we already checked for an existing relationship and there
      // was none, the new edge should not have been redundant
      assert(added)
      // defer this until after addEdge since addEdge may throw a
      // cycle exception
      idealTaxonomy.removeAllEdges(redundantEdges.asJava)
    }
  }

  protected[platonic] def addFormAssoc(
    possessor : SpcIdeal,
    role : SpcRole) : SpcFormAssocEdge =
  {
    graph.getFormAssocEdge(possessor, role) match {
      case Some(edge) => edge
      case _ => {
        val edge = new SpcFormAssocEdge(role.name)
        graph.formAssocs.addEdge(possessor, role, edge)
        edge
      }
    }
  }

  protected[platonic] def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : SpcEntityAssocEdge =
  {
    assert(graph.isFormCompatibleWithRole(possessee.form, role))
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formAssocEdge) => {
        val edge = addEntityAssocEdge(
          possessor, possessee, formAssocEdge)
        getInverseAssocEdge(formAssocEdge) match {
          case Some(inverseAssocEdge) => {
            addEntityAssocEdge(
              possessee, possessor, inverseAssocEdge)
          }
          case _ =>
        }
        edge
      }
      case _ => {
        throw new IllegalArgumentException("addEntityAssoc")
      }
    }
  }

  protected[platonic] def addEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    formAssocEdge : SpcFormAssocEdge) : SpcEntityAssocEdge =
  {
    val role = graph.getPossesseeRole(formAssocEdge)
    getEntityAssocEdge(possessor, possessee, role) match {
      case Some(edge) => {
        assert(edge.formEdge == formAssocEdge)
        edge
      }
      case _ => {
        val edge = new SpcEntityAssocEdge(formAssocEdge)
        graph.entityAssocs.addEdge(
          possessor, possessee, edge)
        edge
      }
    }
  }

  def isEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole) : Boolean =
  {
    !getEntityAssocEdge(possessor, possessee, role).isEmpty
  }

  def getEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    role : SpcRole
  ) : Option[SpcEntityAssocEdge] =
  {
    graph.entityAssocs.getAllEdges(
      possessor, possessee).asScala.find(edge => {
        graph.isHyponym(role, graph.getPossesseeRole(edge.formEdge))
      }
    )
  }

  def loadBeliefs(source : Source)
  {
    val beliefs = source.getLines.mkString("\n")
    val sentences = ShlurdParser(beliefs).parseAll
    val interpreter = new SpcBeliefInterpreter(this)
    sentences.foreach(interpreter.interpretBelief(_))
    validateBeliefs
  }

  def validateBeliefs()
  {
    assert(sanityCheck)
  }

  def sanityCheck() : Boolean =
  {
    assert(graph.sanityCheck)
    val idealSet = graph.idealSynonyms.vertexSet.asScala.filter(_.isIdeal).
      map(_.asInstanceOf[SpcIdeal])
    assert(idealSet == graph.idealTaxonomy.vertexSet.asScala)
    assert(idealSet == graph.formAssocs.vertexSet.asScala)
    val formAssocs = graph.formAssocs
    idealSet.foreach(ideal => {
      assert((formAssocs.outDegreeOf(ideal) ==
        formAssocs.outgoingEdgesOf(ideal).
        asScala.map(_.getRoleName).toSet.size),
        ideal.toString)
      assert(getIdealBySynonym(ideal.name) == Some(ideal))
      ideal match {
        case form : SpcForm => {
          assert(graph.components.inDegreeOf(form) == 0)
          assert(
            getPropertyMap(form).size +
              getStateNormalizationMap(form).size ==
              graph.components.outDegreeOf(form))
        }
        case _ =>
      }
    })
    val entitySet = getEntities.toSet
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = formEdge.constraint
      if (constraint.upper < Int.MaxValue) {
        val ideal = graph.getPossessorIdeal(formEdge)
        entitySet.filter(
          e => graph.isHyponym(e.form, ideal)).foreach(entity =>
          {
            val entityEdges = getEntityAssocGraph.
              outgoingEdgesOf(entity).asScala
            val c = entityEdges.count(_.formEdge == formEdge)
            assert(c <= constraint.upper)
          }
        )
      }
    })
    val formSet = getForms.toSet
    entitySet.foreach(entity => {
      assert(getEntityBySynonym(entity.name) == Some(entity))
      assert(formSet.contains(entity.form))
    })
    assert(entitySet == graph.entityAssocs.vertexSet.asScala)
    val componentSet = graph.components.vertexSet.asScala
    val propertySet = formSet.flatMap(getPropertyMap(_).values).toSet
    val propertyStateSet =
      propertySet.flatMap(getPropertyStateObjMap(_).values).toSet
    val stateNormalizationSet =
      formSet.flatMap(getStateNormalizationMap(_).values).toSet
    assert(
      (formSet ++ propertySet ++ propertyStateSet ++ stateNormalizationSet)
      == componentSet)
    propertySet.foreach(property => {
      assert(getPropertyStateMap(property).keySet.size ==
        graph.components.outDegreeOf(property))
    })
    true
  }

  def resolveGenitive(
    possessor : SpcEntity,
    role : SpcRole)
      : Set[SpcEntity] =
  {
    ShlurdParseUtils.orderedSet(getEntityAssocGraph.outgoingEdgesOf(possessor).
      asScala.toSeq.filter(
        edge => {
          graph.isHyponym(role, graph.getPossesseeRole(edge.formEdge)) &&
            graph.isFormCompatibleWithRole(
              graph.getPossesseeEntity(edge).form, role)
        }
      ).map(
        graph.getPossesseeEntity))
  }

  private def resolveIdeal(
    lemma : String) : (Option[SpcForm], Option[SpcRole]) =
  {
    getIdealBySynonym(lemma) match {
      case Some(form : SpcForm) => {
        (Some(form), None)
      }
      case Some(role : SpcRole) => {
        (None, Some(role))
      }
      case _ => (None, None)
    }
  }

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    val (formOpt, roleOpt) = resolveIdeal(lemma)
    val entities = getEntities
    roleOpt match {
      case Some(role) => {
        Success(ShlurdParseUtils.orderedSet(
          entities.filter(entity =>
            graph.isFormCompatibleWithRole(entity.form, role) &&
              hasQualifiers(entity, entity.form, qualifiers, false))))
      }
      case _ => {
        formOpt match {
          case Some(form) => {
            Success(ShlurdParseUtils.orderedSet(
              entities.filter(
                hasQualifiers(_, form, qualifiers, false))))
          }
          case _ => {
            val results = entities.filter(
              entity => hasQualifiers(
                entity, entity.form, qualifiers + lemma, false))
            if (results.isEmpty) {
              fail(s"unknown ideal $lemma")
            } else {
              Success(ShlurdParseUtils.orderedSet(results))
            }
          }
        }
      }
    }
  }

  override def resolveProperty(
    entity : SpcEntity,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    resolveHypernymProperty(entity.form, lemma) match {
      case Some((property, stateName)) => Success((property, stateName))
      case _ => fail(s"unknown property $lemma")
    }
  }

  def resolveFormProperty(form : SpcForm, lemma : String)
      : Option[(SpcProperty, String)] =
  {
    val stateName = resolveStateSynonym(form, lemma)
    getPropertyMap(form).values.find(
      p => getPropertyStateMap(p).contains(stateName)).map((_, stateName))
  }

  def resolveHypernymProperty(
    form : SpcForm,
    lemma : String) : Option[(SpcProperty, String)] =
  {
    graph.getFormHypernyms(form).foreach(hyperForm => {
      resolveFormProperty(hyperForm, lemma) match {
        case Some((property, stateName)) => {
          return Some(
            (findProperty(form, property.name).getOrElse(property),
              stateName))
        }
        case _ =>
      }
    })
    None
  }

  def getPropertyMap(form : SpcForm) : Map[String, SpcProperty] =
  {
    graph.propertyIndex.accessComponentMap(form)
  }

  private def getPropertyStateObjMap(property : SpcProperty) =
  {
    graph.propertyStateIndex.accessComponentMap(property)
  }

  override def getPropertyStateMap(property : SpcProperty) =
  {
    getPropertyStateObjMap(property).mapValues(_.inflected)
  }

  def findProperty(
    form : SpcForm, name : String) : Option[SpcProperty] =
  {
    graph.getFormHypernyms(form).foreach(hyperForm => {
      getPropertyMap(hyperForm).get(name) match {
        case Some(matchingProperty) => {
          return Some(matchingProperty)
        }
        case _ =>
      }
    })
    None
  }

  def instantiateProperty(form : SpcForm, name : SilWord) : SpcProperty =
  {
    val propertyName = name.lemma
    getPropertyMap(form).get(propertyName) match {
      case Some(property) => property
      case _ => {
        val property = new SpcProperty(propertyName, false)
        assert(!getPropertyMap(form).contains(property.name))
        graph.addComponent(form, property)
        property
      }
    }
  }

  def closePropertyStates(property : SpcProperty)
  {
    if (!property.isClosed) {
      val closedProperty = new SpcProperty(property.name, true)
      graph.components.addVertex(closedProperty)
      graph.replaceVertex(graph.components, property, closedProperty)
      assert(graph.components.degreeOf(property) == 0)
      graph.components.removeVertex(property)
    }
  }

  def instantiatePropertyState(
    property : SpcProperty, word : SilWord)
  {
    val propertyState = new SpcPropertyState(word.lemma, word.inflected)
    assert(!getPropertyStateMap(property).contains(propertyState.lemma))
    graph.addComponent(property, propertyState)
  }

  def properReference(entity : SpcEntity) =
  {
    SilNounReference(
      SilWord(entity.properName), DETERMINER_UNSPECIFIED)
  }

  def qualifiedReference(
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    val formName = entity.form.name
    val nounRef = SilNounReference(
      SilWord(formName), determiner)
    if (entity.properName.isEmpty) {
      SilReference.qualified(
        nounRef, entity.qualifiers.map(q => SilWord(q)).toSeq)
    } else {
      nounRef
    }
  }

  override def specificReference(
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    if (!entity.properName.isEmpty) {
      properReference(entity)
    } else {
      qualifiedReference(entity, determiner)
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : SpcEntity,
    property : SpcProperty,
    lemma : String) : Try[Trilean] =
  {
    if (property.isClosed) {
      val propertyStates = getPropertyStateMap(property)
      if (propertyStates.size == 1) {
        return Success(Trilean(lemma == propertyStates.keySet.head))
      }
      if (!propertyStates.contains(lemma)) {
        return Success(Trilean.False)
      }
    }
    val hypernymSet = graph.getFormHypernyms(entity.form).toSet
    val outgoingPropertyEdges = hypernymSet.flatMap { form =>
      getFormAssocGraph.outgoingEdgesOf(form).asScala.
        filter(_.isProperty).toSet
    }
    getEntityAssocGraph.outgoingEdgesOf(entity).asScala.
      filter(edge => outgoingPropertyEdges.contains(edge.formEdge)).
      foreach(edge => {
        val propertyEntity = graph.getPossesseeEntity(edge)
        if (getPropertyMap(propertyEntity.form).
          values.toSeq.contains(property))
        {
          return evaluateEntityPropertyPredicate(
            propertyEntity,
            property,
            lemma)
        }
        resolveHypernymProperty(propertyEntity.form, lemma) match {
          case Some((underlyingProperty, stateName)) => {
            return evaluateEntityPropertyPredicate(
              propertyEntity,
              underlyingProperty,
              stateName)
          }
          case _ =>
        }
      })
    Success(Trilean.Unknown)
  }

  override def evaluateEntityAdpositionPredicate(
    entity : SpcEntity,
    objRef : SpcEntity,
    adposition : SilAdposition,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    if (adposition == SilAdposition.GENITIVE_OF) {
      if (qualifiers.size != 1) {
        Success(Trilean.Unknown)
      } else {
        val roleName = qualifiers.head
        resolveRole(roleName) match {
          case Some(role) => {
            Success(Trilean(isEntityAssoc(objRef, entity, role)))
          }
          case _ => {
            Success(Trilean.Unknown)
          }
        }
      }
    } else {
      Success(Trilean.Unknown)
    }
  }

  override def evaluateEntityCategoryPredicate(
    entity : SpcEntity,
    lemma : String,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    val (formOpt, roleOpt) = resolveIdeal(lemma)
    roleOpt match {
      case Some(role) => {
        Success(Trilean(
          graph.isFormCompatibleWithRole(entity.form, role) &&
            getEntityAssocGraph.incomingEdgesOf(entity).asScala.
            exists(edge =>
              graph.isHyponym(
                role,
                graph.getPossesseeRole(edge.formEdge)))))
      }
      case _ => {
        formOpt match {
          case Some(form) => {
            if (graph.isHyponym(entity.form, form)) {
              Success(Trilean.True)
            } else {
              if (entity.form.isTentative) {
                Success(Trilean.Unknown)
              } else {
                Success(Trilean.False)
              }
            }
          }
          case _ => {
            fail(s"unknown ideal $lemma")
          }
        }
      }
    }
  }

  def resolveStateSynonym(form : SpcForm, lemma : String) : String =
  {
    normalizeState(form, SilPropertyState(SilWord(lemma))) match {
      case SilPropertyState(word) => word.lemma
      case _ => lemma
    }
  }

  private def getStateNormalizationMap(form : SpcForm)
      : Map[SilState, SpcStateNormalization] =
  {
    graph.stateNormalizationIndex.accessComponentMap(form)
  }

  private[platonic] def addStateNormalization(
    form : SpcForm, state : SilState, transformed : SilState)
  {
    val normalized = normalizeState(form, transformed)
    val map = getStateNormalizationMap(form)
    val inflected =
      new SpcStateNormalization(state, normalized, true)
    val uninflected =
      new SpcStateNormalization(foldState(state), normalized, false)
    if (!map.contains(inflected.original)) {
      graph.addComponent(form, inflected)
    }
    if ((uninflected.original != inflected.original) &&
      !map.contains(uninflected.original))
    {
      graph.addComponent(form, uninflected)
    }
  }

  def normalizeState(form : SpcForm, state : SilState) : SilState =
  {
    val map = getStateNormalizationMap(form)
    map.get(state).map(_.normalized).getOrElse(
        map.get(foldState(state)).map(_.normalized).getOrElse(
          state))
  }

  private def foldState(state : SilState) : SilState =
  {
    // FIXME:  should fold compound states as well
    state match {
      case SilPropertyState(word) =>
        SilPropertyState(SilWord(word.lemma))
      case _ => state
    }
  }

  def getInflectedStateNormalizations(form : SpcForm) =
  {
    getStateNormalizationMap(form).values.filter(_.isInflected).map(
      sn => (sn.original, sn.normalized))
  }

  override def normalizeState(
    entity : SpcEntity, originalState : SilState) =
  {
    graph.getFormHypernyms(entity.form).foldLeft(originalState) {
      case (state, form) => {
        normalizeState(form, state)
      }
    }
  }

  override def reifyRole(
    possessor : SpcEntity,
    roleName : String,
    onlyIfProven : Boolean)
  {
    val role = resolveRole(roleName) match {
      case Some(r) => r
        // FIXME assert instead?
      case _ => return
    }
    graph.getFormAssocEdge(possessor.form, role) match {
      case Some(formEdge) => {
        if (onlyIfProven) {
          val constraint = formEdge.constraint
          if (constraint.lower == 0) {
            return
          }
        }
        val existing = resolveGenitive(possessor, role)
        if (existing.isEmpty) {
          // make up possessee out of thin air
          val name = possessor.name + "_" + roleName
          val form = instantiateForm(SpcForm.tentativeName(SilWord(name)))
          graph.getFormsForRole(graph.getPossesseeRole(formEdge)).foreach(
            hypernym => addIdealTaxonomy(form, hypernym))
          val (possessee, success) = instantiateEntity(
            form, Seq(SilWord(name)), name)
          assert(success)
          addEntityAssoc(possessor, possessee, role)
        }
      }
      case _ => {
        // FIXME make up role out of thin air?
      }
    }
  }

  override def applyModifications()
  {
    validateBeliefs
    graph.applyModifications
    validateBeliefs
  }
}
