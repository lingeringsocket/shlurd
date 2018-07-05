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

import org.jgrapht.util._

import ShlurdEnglishLemmas._

trait SpcVertex
{
}

class SpcProperty(val name : String)
    extends ShlurdProperty with ShlurdNamedObject with SpcVertex
{
  private[platonic] val states =
    new mutable.LinkedHashMap[String, String]

  private var closed : Boolean = false

  override def getStates : Map[String, String] = states

  def isClosed = closed

  private[platonic] def closeStates()
  {
    closed = true
  }

  def instantiateState(word : SilWord)
  {
    states.put(word.lemma, word.inflected)
  }

  def isSynthetic = name.contains('_')

  override def toString = s"SpcProperty($name)"
}

sealed abstract class SpcIdeal(val name : String)
    extends ShlurdNamedObject with SpcVertex
{
  def isRole : Boolean = false

  def isForm : Boolean = false
}

object SpcForm
{
  private val TENTATIVE_SUFFIX = "_form"

  def tentativeName(word : SilWord) = SilWord(word.lemma + TENTATIVE_SUFFIX)
}

class SpcForm(name : String)
    extends SpcIdeal(name)
{
  import SpcForm._

  private val inflectedStateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  private val stateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  def isTentative = name.endsWith(TENTATIVE_SUFFIX)

  def resolveStateSynonym(lemma : String) : String =
  {
    normalizeState(SilPropertyState(SilWord(lemma))) match {
      case SilPropertyState(word) => word.lemma
      case _ => lemma
    }
  }

  private[platonic] def addStateNormalization(
    state : SilState, transformed : SilState)
  {
    val normalized = normalizeState(transformed)
    inflectedStateNormalizations.put(state, normalized)
    stateNormalizations.put(foldState(state), normalized)
  }

  def normalizeState(state : SilState) : SilState =
  {
    inflectedStateNormalizations.get(state).getOrElse(
      stateNormalizations.get(foldState(state)).getOrElse(state))
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

  def getInflectedStateNormalizations = inflectedStateNormalizations.toIterable

  override def isForm = true

  override def toString = s"SpcForm($name)"
}

class SpcRole(name : String)
    extends SpcIdeal(name)
{
  override def isRole = true

  override def toString = s"SpcRole($name)"
}

case class SpcEntity(
  val name : String,
  val form : SpcForm,
  val qualifiers : Set[String],
  val properName : String = "")
    extends ShlurdEntity with ShlurdNamedObject with SpcVertex
{
  override def isTentative = properName.contains("_")
}

class SpcSynonymMap
{
  private val map = new mutable.LinkedHashMap[String, String]

  def addSynonym(synonym : String, fundamental : String)
  {
    // FIXME:  cycle detection
    map.put(synonym, fundamental)
  }

  def resolveSynonym(synonym : String) : String =
  {
    map.get(synonym).getOrElse(synonym)
  }

  def getAll : Map[String, String] = map
}

class SpcCosmos
    extends ShlurdCosmos[SpcEntity, SpcProperty]
{
  private val ideals = new mutable.LinkedHashMap[String, SpcIdeal]

  private val entities =
    new mutable.LinkedHashMap[String, SpcEntity]

  private val idealSynonyms = new SpcSynonymMap

  private val graph = SpcGraph()

  private val unmodifiableGraph = graph.asUnmodifiable

  private val assocConstraints =
    new mutable.LinkedHashMap[SpcFormAssocEdge, SpcCardinalityConstraint] {
      override def default(key : SpcFormAssocEdge) = {
        SpcCardinalityConstraint(0, Int.MaxValue)
      }
    }

  private val propertyEdges =
    new mutable.LinkedHashSet[SpcFormAssocEdge]

  private val inverseAssocEdges =
    new mutable.LinkedHashMap[SpcFormAssocEdge, SpcFormAssocEdge]

  private var nextId = 0

  def getForms = ideals.values.filter(_.isForm).map(_.asInstanceOf[SpcForm])

  def getRoles = ideals.values.filter(_.isRole).map(_.asInstanceOf[SpcRole])

  def getEntities : Map[String, SpcEntity] = entities

  def getGraph = unmodifiableGraph

  protected[platonic] def getPropertyEdges
      : Set[SpcFormAssocEdge] = propertyEdges

  protected[platonic] def getAssocConstraints
      : Map[SpcFormAssocEdge, SpcCardinalityConstraint] = assocConstraints

  protected[platonic] def annotateFormAssoc(
    edge : SpcFormAssocEdge, constraint : SpcCardinalityConstraint,
    isProperty : Boolean)
  {
    assocConstraints.put(edge, constraint)
    if (isProperty) {
      propertyEdges += edge
    }
  }

  def clear()
  {
    entities.clear()
    graph.entityAssocs.removeAllVertices(
      new ArrayUnenforcedSet(graph.entityAssocs.vertexSet))
  }

  private def registerIdeal(ideal : SpcIdeal) =
  {
    graph.idealTaxonomy.addVertex(ideal)
    graph.formAssocs.addVertex(ideal)
    ideals.get(LEMMA_ENTITY) match {
      case Some(entityForm) => {
        addIdealTaxonomy(ideal, entityForm)
      }
      case _ =>
    }
    ideal
  }

  private def forgetIdeal(ideal : SpcIdeal)
  {
    assert(graph.idealTaxonomy.inDegreeOf(ideal) == 0)
    assert(graph.formAssocs.degreeOf(ideal) == 0)
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

  def instantiateIdeal(word : SilWord, assumeRole : Boolean = false) =
  {
    val name = idealSynonyms.resolveSynonym(word.lemma)
    ideals.get(name).getOrElse({
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
    val name = idealSynonyms.resolveSynonym(word.lemma)
    val ideal = ideals.getOrElseUpdate(
      name, registerForm(new SpcForm(name)))
    assert(ideal.isForm)
    ideal.asInstanceOf[SpcForm]
  }

  def instantiateRole(word : SilWord) =
  {
    val name = idealSynonyms.resolveSynonym(word.lemma)
    val ideal = ideals.getOrElseUpdate(
      name, registerRole(new SpcRole(name)))
    assert(ideal.isRole)
    ideal.asInstanceOf[SpcRole]
  }

  private[platonic] def forgetForm(form : SpcForm)
  {
    // FIXME remove synonyms?
    assert(!entities.values.exists(_.form == form))
    ideals.remove(form.name)
    graph.removeContainer(form)
    forgetIdeal(form)
  }

  private[platonic] def forgetEntity(entity : SpcEntity)
  {
    assert(graph.entityAssocs.degreeOf(entity) == 0)
    if (entities.get(entity.name) == Some(entity)) {
      entities.remove(entity.name)
    }
    graph.entityAssocs.removeVertex(entity)
  }

  def matchAssocs(oldForm : SpcForm, newForm : SpcForm)
      : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)]=
  {
    val formAssocs = graph.formAssocs
    formAssocs.outgoingEdgesOf(oldForm).asScala.toSeq.flatMap(
      oldEdge => {
        assert(!inverseAssocEdges.contains(oldEdge))
        graph.getFormAssocEdge(newForm, graph.getPossesseeRole(oldEdge)).map(
          newEdge => (oldEdge, newEdge))
      }
    )
  }

  private[platonic] def isValidMergeAssoc(
    oldEdge : SpcFormAssocEdge, newEdge : SpcFormAssocEdge) : Boolean =
  {
    val entityAssocs = graph.entityAssocs
    val constraint = assocConstraints(newEdge)
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
    assert(!inverseAssocEdges.contains(oldEdge))
    // FIXME we should be able to support this
    assert(!inverseAssocEdges.contains(newEdge))
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

  def addIdealSynonym(synonym : String, fundamental : String)
  {
    idealSynonyms.addSynonym(synonym, fundamental)
  }

  protected[platonic] def getIdealSynonyms =
    idealSynonyms

  def getIdealTaxonomyGraph =
    unmodifiableGraph.idealTaxonomy

  def getFormAssocGraph =
    unmodifiableGraph.formAssocs

  def getEntityAssocGraph =
    unmodifiableGraph.entityAssocs

  def getInverseAssocEdges : Map[SpcFormAssocEdge, SpcFormAssocEdge] =
    inverseAssocEdges

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
      entities.values.find(hasQualifiers(_, form, qualifiers, true)) match {
        case Some(entity) => {
          return (entity, false)
        }
        case _ =>
      }
    } else {
      entities.values.filter(_.properName == properName).headOption match {
        case Some(entity) => {
          return (entity, false)
        }
        case _ =>
      }
    }
    val name =
      (qualifierString.map(_.lemma) ++
        Seq(form.name, nextId.toString)).mkString("_")
    nextId += 1
    val entity = new SpcEntity(name, form, qualifiers, properName)
    createOrReplaceEntity(entity)
    (entity, true)
  }

  protected[platonic] def createOrReplaceEntity(entity : SpcEntity)
  {
    graph.entityAssocs.addVertex(entity)
    entities.put(entity.name, entity) match {
      case Some(old) => {
        replaceEntity(old, entity)
      }
      case _ =>
    }
  }

  protected[platonic] def connectInverseAssocEdges(
    edge1 : SpcFormAssocEdge,
    edge2 : SpcFormAssocEdge)
  {
    inverseAssocEdges.get(edge1) match {
      case Some(existing) => {
        if (existing == edge2) {
          return
        } else {
          inverseAssocEdges.remove(existing)
          inverseAssocEdges.get(edge2).foreach(inverseAssocEdges.remove)
        }
      }
      case _ =>
    }
    inverseAssocEdges.put(edge1, edge2)
    inverseAssocEdges.put(edge2, edge1)
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
        inverseAssocEdges.get(formAssocEdge) match {
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
    val idealSet = ideals.values.toSet
    assert(idealSet == graph.idealTaxonomy.vertexSet.asScala)
    assert(idealSet == graph.formAssocs.vertexSet.asScala)
    assert(entities.values.toSet == graph.entityAssocs.vertexSet.asScala)
    ideals.foreach(pair => assert(pair._1 == pair._2.name))
    entities.foreach(pair => assert(pair._1 == pair._2.name))
    val formAssocs = graph.formAssocs
    idealSet.foreach(ideal => {
      assert((formAssocs.outDegreeOf(ideal) ==
        formAssocs.outgoingEdgesOf(ideal).
        asScala.map(_.getRoleName).toSet.size),
        ideal.toString)
    })
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if (constraint.upper < Int.MaxValue) {
        val ideal = graph.getPossessorIdeal(formEdge)
        entities.values.filter(
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
    val componentSet = graph.components.vertexSet.asScala
    val formSet = getForms.toSet
    val propertySet = formSet.flatMap(getProperties(_).values).toSet
    assert((formSet ++ propertySet) == componentSet)
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
    val name = idealSynonyms.resolveSynonym(lemma)
    ideals.get(name) match {
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
    roleOpt match {
      case Some(role) => {
        Success(ShlurdParseUtils.orderedSet(
          entities.values.filter(entity =>
            graph.isFormCompatibleWithRole(entity.form, role) &&
              hasQualifiers(entity, entity.form, qualifiers, false))))
      }
      case _ => {
        formOpt match {
          case Some(form) => {
            Success(ShlurdParseUtils.orderedSet(
              entities.values.filter(
                hasQualifiers(_, form, qualifiers, false))))
          }
          case _ => {
            val results = entities.values.filter(
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
    val stateName = form.resolveStateSynonym(lemma)
    getProperties(form).values.find(
      p => p.states.contains(stateName)).map((_, stateName))
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

  def getProperties(form : SpcForm) : Map[String, SpcProperty] =
  {
    val seq = graph.components.outgoingEdgesOf(form).asScala.toSeq.map(
      edge => {
        val property = graph.getComponent(edge).asInstanceOf[SpcProperty]
        (property.name, property)
      }
    )
    Map(seq:_*)
  }

  def findProperty(
    form : SpcForm, name : String) : Option[SpcProperty] =
  {
    graph.getFormHypernyms(form).foreach(hyperForm => {
      getProperties(hyperForm).get(name) match {
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
    getProperties(form).get(propertyName) match {
      case Some(property) => property
      case _ => {
        val property = new SpcProperty(propertyName)
        graph.components.addVertex(property)
        graph.components.addEdge(form, property)
        property
      }
    }
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
      if (property.getStates.size == 1) {
        return Success(Trilean(lemma == property.getStates.keySet.head))
      }
      if (!property.getStates.contains(lemma)) {
        return Success(Trilean.False)
      }
    }
    val hypernymSet = graph.getFormHypernyms(entity.form).toSet
    val outgoingPropertyEdges = hypernymSet.flatMap { form =>
      getFormAssocGraph.outgoingEdgesOf(form).asScala.
        filter(propertyEdges.contains(_)).toSet
    }
    getEntityAssocGraph.outgoingEdgesOf(entity).asScala.
      filter(edge => outgoingPropertyEdges.contains(edge.formEdge)).
      foreach(edge => {
        val propertyEntity = graph.getPossesseeEntity(edge)
        if (getProperties(propertyEntity.form).
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
    if (adposition == ADP_GENITIVE_OF) {
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

  override def normalizeState(
    entity : SpcEntity, originalState : SilState) =
  {
    graph.getFormHypernyms(entity.form).foldLeft(originalState) {
      case (state, form) => {
        form.normalizeState(state)
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
          val constraint = assocConstraints(formEdge)
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
}
