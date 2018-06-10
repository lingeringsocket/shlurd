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

class SpcProperty(val name : String)
    extends ShlurdProperty with ShlurdNamedObject
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

  override def toString = s"SpcProperty($name)"
}

class SpcForm(val name : String)
    extends ShlurdNamedObject
{
  private[platonic] val properties =
    new mutable.LinkedHashMap[String, SpcProperty]

  private val inflectedStateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  private val stateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  def getProperties : Map[String, SpcProperty] = properties

  def instantiateProperty(name : SilWord) =
  {
    val property = name.lemma
    properties.getOrElseUpdate(property, new SpcProperty(property))
  }

  def resolveProperty(lemma : String)
      : Option[(SpcProperty, String)] =
  {
    val stateName = resolveStateSynonym(lemma)
    properties.values.find(
      p => p.states.contains(stateName)).map((_, stateName))
  }

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

  override def toString = s"SpcForm($name)"
}

case class SpcEntity(
  val name : String,
  val form : SpcForm,
  val qualifiers : Set[String],
  val properName : String = "")
    extends ShlurdEntity with ShlurdNamedObject
{
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
  private val forms =
    new mutable.LinkedHashMap[String, SpcForm]

  private val roles =
    new mutable.LinkedHashSet[String]

  private val entities =
    new mutable.LinkedHashMap[String, SpcEntity]

  private val formSynonyms = new SpcSynonymMap

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

  init()

  private def init()
  {
    instantiateForm(SilWord(LEMMA_PERSON))
    formSynonyms.addSynonym(
      LEMMA_WHO, LEMMA_PERSON)
  }

  def getForms : Map[String, SpcForm] = forms

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

  def instantiateForm(word : SilWord) =
  {
    val name = formSynonyms.resolveSynonym(word.lemma)
    val form = forms.getOrElseUpdate(name, new SpcForm(name))
    graph.formTaxonomy.addVertex(form)
    graph.formAssocs.addVertex(form)
    form
  }

  def addFormSynonym(synonym : String, fundamental : String, isRole : Boolean)
  {
    formSynonyms.addSynonym(synonym, fundamental)
    if (isRole) {
      roles += synonym
    }
  }

  protected[platonic] def getFormSynonyms =
    formSynonyms

  def getFormTaxonomyGraph =
    unmodifiableGraph.formTaxonomy

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
          (overlap && existing.qualifiers.subsetOf(qualifiers)))
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
    entities.values.find(hasQualifiers(_, form, qualifiers, true)) match {
      case Some(entity) => {
        return (entity, false)
      }
      case _ =>
    }
    val name =
      (qualifierString.map(_.lemma) ++
        Seq(form.name, nextId.toString)).mkString("_")
    nextId += 1
    val entity = new SpcEntity(name, form, qualifiers, properName)
    addEntity(entity)
    (entity, true)
  }

  protected[platonic] def addEntity(entity : SpcEntity)
  {
    entities.put(entity.name, entity)
    graph.entityAssocs.addVertex(entity)
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

  protected[platonic] def addFormTaxonomy(
    specificForm : SpcForm,
    genericForm : SpcForm,
    label : String = SpcGraph.LABEL_KIND) : SpcTaxonomyEdge =
  {
    val edge = new SpcTaxonomyEdge(label)
    graph.formTaxonomy.addEdge(specificForm, genericForm, edge)
    edge
  }

  protected[platonic] def addFormAssoc(
    possessor : SpcForm,
    possessee : SpcForm,
    label : String) : SpcFormAssocEdge =
  {
    graph.formAssocs.getAllEdges(
      possessor, possessee).asScala.find(_.label == label) match
    {
      case Some(edge) => edge
      case _ => {
        val edge = new SpcFormAssocEdge(label)
        graph.formAssocs.addEdge(possessor, possessee, edge)
        edge
      }
    }
  }

  protected[platonic] def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    label : String) : SpcEntityAssocEdge =
  {
    graph.getFormAssoc(possessor.form, possessee.form, label) match {
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

  private def addEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    formAssocEdge : SpcFormAssocEdge) : SpcEntityAssocEdge =
  {
    getEntityAssocEdge(possessor, possessee, formAssocEdge.label) match {
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
    label : String) : Boolean =
  {
    !getEntityAssocEdge(possessor, possessee, label).isEmpty
  }

  def getEntityAssocEdge(
    possessor : SpcEntity,
    possessee : SpcEntity,
    label : String
  ) : Option[SpcEntityAssocEdge] =
  {
    graph.entityAssocs.getAllEdges(
      possessor, possessee).asScala.find(_.label == label)
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
    val creed = new SpcCreed(this)
    getFormAssocGraph.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = graph.getPossessorForm(formEdge)
        entities.values.filter(
          e => graph.isHyponym(e.form, form)).foreach(entity =>
          {
            val c = getEntityAssocGraph.outgoingEdgesOf(entity).asScala.
              count(_.label == formEdge.label)
            if ((c < constraint.lower) || (c > constraint.upper)) {
              throw new CardinalityExcn(
                creed.formAssociationBelief(formEdge))
            }
          }
        )
      }
    })
  }

  def resolveGenitive(
    possessor : SpcEntity,
    label : String)
      : Set[SpcEntity] =
  {
    getEntityAssocGraph.outgoingEdgesOf(possessor).
      asScala.filter(_.label == label).map(
        graph.getPossesseeEntity)
  }

  def isRole(name : SilWord) : Boolean =
  {
    roles.contains(name.lemma)
  }

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    forms.get(formSynonyms.resolveSynonym(lemma)) match {
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
          fail(s"unknown entity $lemma")
        } else {
          Success(ShlurdParseUtils.orderedSet(results))
        }
      }
    }
  }

  override def resolveProperty(
    entity : SpcEntity,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    resolveFormProperty(entity.form, lemma)
  }

  private def resolveFormProperty(
    form : SpcForm,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    graph.getHypernyms(form).foreach(hyperForm => {
      hyperForm.resolveProperty(lemma) match {
        case Some(pair) => return Success(pair)
        case _ =>
      }
    })
    fail(s"unknown property $lemma")
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
    val hypernymSet = graph.getHypernyms(entity.form).toSet
    val propertyEdgeNames = hypernymSet.flatMap { form =>
      getFormAssocGraph.outgoingEdgesOf(form).asScala.
        filter(propertyEdges.contains(_)).map(_.label).toSet
    }
    getEntityAssocGraph.outgoingEdgesOf(entity).asScala.
      filter(edge => propertyEdgeNames.contains(edge.label)).
      foreach(edge => {
        val propertyEntity = graph.getPossesseeEntity(edge)
        if (propertyEntity.form.properties.values.toSeq.contains(property)) {
          return evaluateEntityPropertyPredicate(
            propertyEntity,
            property,
            lemma)
        }
        resolveFormProperty(propertyEntity.form, lemma) match {
          case Success((underlyingProperty, stateName)) => {
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
        Success(Trilean.False)
      } else {
        val label = qualifiers.head
        Success(Trilean(isEntityAssoc(objRef, entity, label)))
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
    forms.get(formSynonyms.resolveSynonym(lemma)) match {
      case Some(form) => {
        if (graph.isHyponym(entity.form, form)) {
          if (roles.contains(lemma)) {
            Success(Trilean(
              getEntityAssocGraph.incomingEdgesOf(entity).asScala.
                exists(_.label == lemma)))
          } else {
            Success(Trilean.True)
          }
        } else {
          Success(Trilean.False)
        }
      }
      case _ => {
        fail(s"unknown entity $lemma")
      }
    }
  }

  override def normalizeState(
    entity : SpcEntity, originalState : SilState) =
  {
    graph.getHypernyms(entity.form).foldLeft(originalState) {
      case (state, form) => {
        form.normalizeState(state)
      }
    }
  }
}
