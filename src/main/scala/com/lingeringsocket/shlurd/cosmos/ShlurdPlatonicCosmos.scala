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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import org.jgrapht.graph._

import spire.math._

import scala.io._
import scala.util._
import scala.collection._
import scala.collection.JavaConverters._

import ShlurdEnglishLemmas._

class ShlurdPlatonicProperty(val name : String)
    extends ShlurdProperty with ShlurdNamedObject
{
  private[cosmos] val states =
    new mutable.LinkedHashMap[String, String]

  private var closed : Boolean = false

  override def getStates : Map[String, String] = states

  def isClosed = closed

  private[cosmos] def closeStates()
  {
    closed = true
  }

  def instantiateState(word : SilWord)
  {
    states.put(word.lemma, word.inflected)
  }

  override def toString = s"ShlurdPlatonicProperty($name)"
}

class ShlurdPlatonicForm(val name : String)
    extends ShlurdNamedObject
{
  private[cosmos] val properties =
    new mutable.LinkedHashMap[String, ShlurdPlatonicProperty]

  private val inflectedStateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  private val stateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  def getProperties : Map[String, ShlurdPlatonicProperty] = properties

  def instantiateProperty(name : SilWord) =
  {
    val property = name.lemma
    properties.getOrElseUpdate(property, new ShlurdPlatonicProperty(property))
  }

  def resolveStateSynonym(lemma : String) : String =
  {
    normalizeState(SilPropertyState(SilWord(lemma))) match {
      case SilPropertyState(word) => word.lemma
      case _ => lemma
    }
  }

  private[cosmos] def addStateNormalization(
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

  override def toString = s"ShlurdPlatonicForm($name)"
}

case class ShlurdPlatonicEntity(
  val name : String,
  val form : ShlurdPlatonicForm,
  val qualifiers : Set[String],
  val properName : String = "")
    extends ShlurdEntity with ShlurdNamedObject
{
}

object ShlurdPlatonicCosmos
{
  val DEFAULT_PROPERTY = "state"

  val LABEL_KIND = "aKindOf"

  val DEFAULT_PROPERTY_WORD = SilWord(DEFAULT_PROPERTY)

  case class CardinalityConstraint(lower : Int, upper : Int)
  {
  }

  protected[cosmos] class LabeledEdge(
    val label : String) extends DefaultEdge
  {
    override def hashCode() =
    {
      java.util.Objects.hash(getSource, getTarget, label)
    }

    override def equals(a : Any) =
    {
      a match {
        case that : LabeledEdge => {
          (this.getSource == that.getSource) &&
          (this.getTarget == that.getTarget) &&
          (this.label == that.label)
        }
        case _ => false
      }
    }
  }

  protected[cosmos] class FormAssocEdge(
    label : String) extends LabeledEdge(label)
  {
  }

  protected[cosmos] class FormTaxonomyEdge(
    label : String = LABEL_KIND) extends LabeledEdge(label)
  {
  }

  protected[cosmos] class EntityAssocEdge(
    label : String) extends LabeledEdge(label)
  {
  }

  private class ProbeFormEdge(
    val sourceForm : ShlurdPlatonicForm,
    val targetForm : ShlurdPlatonicForm,
    label : String) extends FormAssocEdge(label)
  {
    override def getSource = sourceForm

    override def getTarget = targetForm
  }

  private class ProbeEntityEdge(
    val sourceEntity : ShlurdPlatonicEntity,
    val targetEntity : ShlurdPlatonicEntity,
    label : String) extends EntityAssocEdge(label)
  {
    override def getSource = sourceEntity

    override def getTarget = targetEntity
  }
}

class ShlurdPlatonicCosmos
    extends ShlurdCosmos[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  import ShlurdPlatonicCosmos._

  private val forms =
    new mutable.LinkedHashMap[String, ShlurdPlatonicForm]

  private val roles =
    new mutable.LinkedHashSet[String]

  private val entities =
    new mutable.LinkedHashMap[String, ShlurdPlatonicEntity]

  private val formSynonyms = new ShlurdSynonymMap

  private val formTaxonomy =
    new DirectedAcyclicGraph[ShlurdPlatonicForm, FormTaxonomyEdge](
      classOf[FormTaxonomyEdge])

  private val formAssocs =
    new DirectedPseudograph[ShlurdPlatonicForm, FormAssocEdge](
      classOf[FormAssocEdge])

  private val assocConstraints =
    new mutable.LinkedHashMap[FormAssocEdge, CardinalityConstraint]

  private val propertyEdges =
    new mutable.LinkedHashSet[FormAssocEdge]

  private val entityAssocs =
    new DirectedPseudograph[ShlurdPlatonicEntity, EntityAssocEdge](
      classOf[EntityAssocEdge])

  private var nextId = 0

  init()

  private def init()
  {
    instantiateForm(SilWord(LEMMA_PERSON))
    formSynonyms.addSynonym(
      LEMMA_WHO, LEMMA_PERSON)
  }

  def getForms : Map[String, ShlurdPlatonicForm] = forms

  def getEntities : Map[String, ShlurdPlatonicEntity] = entities

  protected[cosmos] def getPropertyEdges
      : Set[FormAssocEdge] = propertyEdges

  protected[cosmos] def getAssocConstraints
      : Map[FormAssocEdge, CardinalityConstraint] = assocConstraints

  protected[cosmos] def annotateFormAssoc(
    edge : FormAssocEdge, constraint : CardinalityConstraint,
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
  }

  def instantiateForm(word : SilWord) =
  {
    val name = formSynonyms.resolveSynonym(word.lemma)
    forms.getOrElseUpdate(name, new ShlurdPlatonicForm(name))
  }

  protected[cosmos] def instantiateRole(word : SilWord) =
  {
    roles += word.lemma
    instantiateForm(word)
  }

  protected[cosmos] def getFormSynonyms =
    formSynonyms

  protected[cosmos] def getFormTaxonomyGraph =
    new AsUnmodifiableGraph(formTaxonomy)

  protected[cosmos] def getFormAssocGraph =
    new AsUnmodifiableGraph(formAssocs)

  protected[cosmos] def getEntityAssocGraph =
    new AsUnmodifiableGraph(entityAssocs)

  private def hasQualifiers(
    existing : ShlurdPlatonicEntity,
    form : ShlurdPlatonicForm,
    qualifiers : Set[String],
    overlap : Boolean) : Boolean =
  {
    (form == existing.form) &&
      (qualifiers.subsetOf(existing.qualifiers) ||
        (overlap && existing.qualifiers.subsetOf(qualifiers)))
  }

  protected[cosmos] def instantiateEntity(
    form : ShlurdPlatonicForm,
    qualifierString : Seq[SilWord],
    properName : String = "") : (ShlurdPlatonicEntity, Boolean) =
  {
    val qualifiers = qualifierSet(qualifierString)
    def redundantWith(existing : ShlurdPlatonicEntity) =
      (form == existing.form) && qualifiers.subsetOf(existing.qualifiers)
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
    val entity = new ShlurdPlatonicEntity(name, form, qualifiers, properName)
    addEntity(entity)
    (entity, true)
  }

  protected[cosmos] def addEntity(entity : ShlurdPlatonicEntity)
  {
    entities.put(entity.name, entity)
  }

  protected[cosmos] def getSpecificForm(edge : FormTaxonomyEdge) =
    formTaxonomy.getEdgeSource(edge)

  protected[cosmos] def getGenericForm(edge : FormTaxonomyEdge) =
    formTaxonomy.getEdgeTarget(edge)

  protected[cosmos] def getPossessorForm(edge : FormAssocEdge) =
    formAssocs.getEdgeSource(edge)

  protected[cosmos] def getPossessorEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

  protected[cosmos] def getPossesseeForm(edge : FormAssocEdge) =
    formAssocs.getEdgeTarget(edge)

  protected[cosmos] def getPossesseeEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  protected[cosmos] def addFormTaxonomy(
    specificForm : ShlurdPlatonicForm,
    genericForm : ShlurdPlatonicForm,
    label : String = LABEL_KIND) : FormTaxonomyEdge =
  {
    formTaxonomy.addVertex(specificForm)
    formTaxonomy.addVertex(genericForm)
    val edge = new FormTaxonomyEdge(label)
    formTaxonomy.addEdge(specificForm, genericForm, edge)
    edge
  }

  protected[cosmos] def addFormAssoc(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : FormAssocEdge =
  {
    formAssocs.addVertex(possessor)
    formAssocs.addVertex(possessee)
    val probe = new ProbeFormEdge(possessor, possessee, label)
    formAssocs.removeEdge(probe)
    val edge = new FormAssocEdge(label)
    formAssocs.addEdge(possessor, possessee, edge)
    edge
  }

  protected[cosmos] def addEntityAssoc(
    possessor : ShlurdPlatonicEntity,
    possessee : ShlurdPlatonicEntity,
    label : String) : EntityAssocEdge =
  {
    entityAssocs.addVertex(possessor)
    entityAssocs.addVertex(possessee)
    val probe = new ProbeEntityEdge(possessor, possessee, label)
    entityAssocs.removeEdge(probe)
    val edge = new EntityAssocEdge(label)
    entityAssocs.addEdge(
      possessor, possessee, edge)
    edge
  }

  protected[cosmos] def isFormAssoc(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : Boolean =
  {
    formAssocs.containsEdge(
      new ProbeFormEdge(possessor, possessee, label))
  }

  protected[cosmos] def getFormAssoc(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : Option[FormAssocEdge] =
  {
    if (formAssocs.containsVertex(possessor) &&
      formAssocs.containsVertex(possessee))
    {
      formAssocs.getAllEdges(possessor, possessee).asScala.
        find(_.label == label)
    } else {
      None
    }
  }

  protected[cosmos] def isEntityAssoc(
    possessor : ShlurdPlatonicEntity,
    possessee : ShlurdPlatonicEntity,
    label : String) : Boolean =
  {
    entityAssocs.containsEdge(
      new ProbeEntityEdge(possessor, possessee, label))
  }

  def loadBeliefs(source : Source)
  {
    val beliefs = source.getLines.mkString("\n")
    val sentences = ShlurdParser(beliefs).parseAll
    val interpreter = new ShlurdPlatonicBeliefInterpreter(this)
    sentences.foreach(interpreter.interpretBelief(_))
    validateBeliefs
  }

  def validateBeliefs()
  {
    val creed = new ShlurdPlatonicCreed(this)
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = getPossessorForm(formEdge)
        entities.values.filter(_.form == form).foreach(entity => {
          if (entityAssocs.containsVertex(entity)) {
            val c = entityAssocs.outgoingEdgesOf(entity).asScala.
              count(_.label == formEdge.label)
            if ((c < constraint.lower) || (c > constraint.upper)) {
              throw new CardinalityExcn(
                creed.formAssociationBelief(formEdge))
            }
          } else if (constraint.lower > 0) {
            throw new CardinalityExcn(
              creed.formAssociationBelief(formEdge))
          }
        })
      }
    })
  }

  def resolveGenitive(
    possessor : ShlurdPlatonicEntity,
    label : String)
      : Set[ShlurdPlatonicEntity] =
  {
    if (!entityAssocs.containsVertex(possessor)) {
      Set.empty
    } else {
      entityAssocs.outgoingEdgesOf(possessor).
        asScala.filter(_.label == label).map(
          getPossesseeEntity)
    }
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

  override def resolvePronoun(
    person : SilPerson,
    gender : SilGender,
    count : SilCount) : Try[Set[ShlurdPlatonicEntity]] =
  {
    fail("pronouns not supported")
  }

  override def resolveProperty(
    entity : ShlurdPlatonicEntity,
    lemma : String) : Try[(ShlurdPlatonicProperty, String)] =
  {
    resolveFormProperty(entity.form, lemma)
  }

  private def resolveFormProperty(
    form : ShlurdPlatonicForm,
    lemma : String) : Try[(ShlurdPlatonicProperty, String)] =
  {
    val stateName = form.resolveStateSynonym(lemma)
    form.properties.values.find(p => p.states.contains(stateName)) match {
      case Some(p) => return Success((p, stateName))
      case _ => fail(s"unknown property $lemma")
    }
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : SilDeterminer) =
  {
    val formName = entity.form.name
    def nounRef = SilNounReference(
      SilWord(formName), determiner)
    if (!entity.properName.isEmpty) {
      SilNounReference(
        SilWord(entity.properName), DETERMINER_UNSPECIFIED)
    } else {
      SilReference.qualified(
        nounRef, entity.qualifiers.map(q => SilWord(q)).toSeq)
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) : Try[Trilean] =
  {
    val propertyEdgeNames = {
      if (formAssocs.containsVertex(entity.form)) {
        formAssocs.outgoingEdgesOf(entity.form).asScala.
          filter(propertyEdges.contains(_)).map(_.label).toSet
      } else {
        Set.empty[String]
      }
    }
    if (entityAssocs.containsVertex(entity)) {
      entityAssocs.outgoingEdgesOf(entity).asScala.
        filter(edge => propertyEdgeNames.contains(edge.label)).
        foreach(edge => {
          val propertyEntity = getPossesseeEntity(edge)
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
    }
    Success(Trilean.Unknown)
  }

  override def evaluateEntityAdpositionPredicate(
    entity : ShlurdPlatonicEntity,
    objRef : ShlurdPlatonicEntity,
    adposition : SilAdposition,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    if (adposition == ADP_GENITIVE_OF) {
      if (!entityAssocs.containsVertex(entity) ||
        !entityAssocs.containsVertex(objRef) ||
        (qualifiers.size != 1))
      {
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
    entity : ShlurdPlatonicEntity,
    lemma : String,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    forms.get(formSynonyms.resolveSynonym(lemma)) match {
      case Some(form) => {
        if (entity.form == form) {
          val isRole = roles.contains(lemma) || (form.name == lemma)
          if (!formAssocs.containsVertex(form)) {
            Success(Trilean(isRole))
          } else {
            if (formAssocs.incomingEdgesOf(form).asScala.
              exists(_.label == lemma))
            {
              Success(Trilean(
                entityAssocs.incomingEdgesOf(entity).asScala.
                  exists(_.label == lemma)))
            } else {
              Success(Trilean(isRole))
            }
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
    entity : ShlurdPlatonicEntity, state : SilState) =
  {
    entity.form.normalizeState(state)
  }
}
