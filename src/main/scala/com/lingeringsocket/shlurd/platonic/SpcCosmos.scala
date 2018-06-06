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

import org.jgrapht.graph._
import org.jgrapht.alg.shortestpath._
import org.jgrapht.traverse._

import spire.math._

import scala.io._
import scala.util._
import scala.collection._
import scala.collection.JavaConverters._

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

object SpcCosmos
{
  val LABEL_KIND = "aKindOf"

  case class CardinalityConstraint(lower : Int, upper : Int)
  {
  }

  protected[platonic] class LabeledEdge(
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

  protected[platonic] class FormAssocEdge(
    label : String) extends LabeledEdge(label)
  {
  }

  protected[platonic] class FormTaxonomyEdge(
    label : String = LABEL_KIND) extends LabeledEdge(label)
  {
  }

  protected[platonic] class EntityAssocEdge(
    label : String) extends LabeledEdge(label)
  {
  }

  private class ProbeFormEdge(
    val sourceForm : SpcForm,
    val targetForm : SpcForm,
    label : String) extends FormAssocEdge(label)
  {
    override def getSource = sourceForm

    override def getTarget = targetForm
  }

  private class ProbeEntityEdge(
    val sourceEntity : SpcEntity,
    val targetEntity : SpcEntity,
    label : String) extends EntityAssocEdge(label)
  {
    override def getSource = sourceEntity

    override def getTarget = targetEntity
  }
}

class SpcCosmos
    extends ShlurdCosmos[SpcEntity, SpcProperty]
{
  import SpcCosmos._

  private val forms =
    new mutable.LinkedHashMap[String, SpcForm]

  private val roles =
    new mutable.LinkedHashSet[String]

  private val entities =
    new mutable.LinkedHashMap[String, SpcEntity]

  private val formSynonyms = new ShlurdSynonymMap

  private val formTaxonomy =
    new DirectedAcyclicGraph[SpcForm, FormTaxonomyEdge](
      classOf[FormTaxonomyEdge])

  private val formAssocs =
    new DirectedPseudograph[SpcForm, FormAssocEdge](
      classOf[FormAssocEdge])

  private val assocConstraints =
    new mutable.LinkedHashMap[FormAssocEdge, CardinalityConstraint]

  private val propertyEdges =
    new mutable.LinkedHashSet[FormAssocEdge]

  private val entityAssocs =
    new DirectedPseudograph[SpcEntity, EntityAssocEdge](
      classOf[EntityAssocEdge])

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

  protected[platonic] def getPropertyEdges
      : Set[FormAssocEdge] = propertyEdges

  protected[platonic] def getAssocConstraints
      : Map[FormAssocEdge, CardinalityConstraint] = assocConstraints

  protected[platonic] def annotateFormAssoc(
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
    forms.getOrElseUpdate(name, new SpcForm(name))
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

  protected[platonic] def getFormTaxonomyGraph =
    new AsUnmodifiableGraph(formTaxonomy)

  protected[platonic] def getFormAssocGraph =
    new AsUnmodifiableGraph(formAssocs)

  protected[platonic] def getEntityAssocGraph =
    new AsUnmodifiableGraph(entityAssocs)

  private def hasQualifiers(
    existing : SpcEntity,
    form : SpcForm,
    qualifiers : Set[String],
    overlap : Boolean) : Boolean =
  {
    (isHyponym(existing.form, form)) &&
      (qualifiers.subsetOf(existing.qualifiers) ||
        (overlap && existing.qualifiers.subsetOf(qualifiers)))
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
  }

  protected[platonic] def getSpecificForm(edge : FormTaxonomyEdge) =
    formTaxonomy.getEdgeSource(edge)

  protected[platonic] def getGenericForm(edge : FormTaxonomyEdge) =
    formTaxonomy.getEdgeTarget(edge)

  protected[platonic] def getPossessorForm(edge : FormAssocEdge) =
    formAssocs.getEdgeSource(edge)

  protected[platonic] def getPossessorEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

  protected[platonic] def getPossesseeForm(edge : FormAssocEdge) =
    formAssocs.getEdgeTarget(edge)

  protected[platonic] def getPossesseeEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  protected[platonic] def addFormTaxonomy(
    specificForm : SpcForm,
    genericForm : SpcForm,
    label : String = LABEL_KIND) : FormTaxonomyEdge =
  {
    formTaxonomy.addVertex(specificForm)
    formTaxonomy.addVertex(genericForm)
    val edge = new FormTaxonomyEdge(label)
    formTaxonomy.addEdge(specificForm, genericForm, edge)
    edge
  }

  protected[platonic] def addFormAssoc(
    possessor : SpcForm,
    possessee : SpcForm,
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

  protected[platonic] def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
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

  protected[platonic] def getFormAssoc(
    possessor : SpcForm,
    possessee : SpcForm,
    label : String) : Option[FormAssocEdge] =
  {
    val edges = formAssocs.edgeSet.asScala.filter(_.label == label)
    edges.foreach(edge => {
      val hyperPossessor = getPossessorForm(edge)
      val hyperPossessee = getPossesseeForm(edge)
      if (isHyponym(possessor, hyperPossessor) &&
        isHyponym(possessee, hyperPossessee))
      {
        return Some(edge)
      }
    })
    None
  }

  protected[platonic] def isEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    label : String) : Boolean =
  {
    entityAssocs.containsEdge(
      new ProbeEntityEdge(possessor, possessee, label))
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
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = getPossessorForm(formEdge)
        entities.values.filter(
          e => isHyponym(e.form, form)).foreach(entity =>
          {
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
    if (!entityAssocs.containsVertex(possessor)) {
      Set.empty
    } else {
      entityAssocs.outgoingEdgesOf(possessor).
        asScala.filter(_.label == label).map(
          getPossesseeEntity)
    }
  }

  def isRole(name : SilWord) : Boolean =
  {
    roles.contains(name.lemma)
  }

  def isHyponym(
    hyponymForm : SpcForm,
    hypernymForm : SpcForm) : Boolean =
  {
    if (hyponymForm == hypernymForm) {
      return true
    }
    if (!formTaxonomy.containsVertex(hyponymForm)) {
      return false
    }
    if (!formTaxonomy.containsVertex(hypernymForm)) {
      return false
    }
    val path = DijkstraShortestPath.findPathBetween(
      formTaxonomy, hyponymForm, hypernymForm)
    return (path != null)
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
    count : SilCount) : Try[Set[SpcEntity]] =
  {
    fail("pronouns not supported")
  }

  override def resolveProperty(
    entity : SpcEntity,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    resolveFormProperty(entity.form, lemma)
  }

  private def getHypernyms(
    form : SpcForm) : Iterator[SpcForm] =
  {
    if (formTaxonomy.containsVertex(form)) {
      new BreadthFirstIterator(formTaxonomy, form).asScala
    } else {
      Seq(form).iterator
    }
  }

  private def resolveFormProperty(
    form : SpcForm,
    lemma : String) : Try[(SpcProperty, String)] =
  {
    getHypernyms(form).foreach(hyperForm => {
      hyperForm.resolveProperty(lemma) match {
        case Some(pair) => return Success(pair)
        case _ =>
      }
    })
    fail(s"unknown property $lemma")
  }

  private def properReference(entity : SpcEntity) =
  {
    SilNounReference(
      SilWord(entity.properName), DETERMINER_UNSPECIFIED)
  }

  private def qualifiedReference(
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

    val hypernymSet = getHypernyms(entity.form).toSet
    val propertyEdgeNames = hypernymSet.flatMap { form =>
      if (formAssocs.containsVertex(form)) {
        formAssocs.outgoingEdgesOf(form).asScala.
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
    entity : SpcEntity,
    objRef : SpcEntity,
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
    entity : SpcEntity,
    lemma : String,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    forms.get(formSynonyms.resolveSynonym(lemma)) match {
      case Some(form) => {
        if (isHyponym(entity.form, form)) {
          if (roles.contains(lemma)) {
            Success(Trilean(
              entityAssocs.incomingEdgesOf(entity).asScala.
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
    getHypernyms(entity.form).foldLeft(originalState) {
      case (state, form) => {
        form.normalizeState(state)
      }
    }
  }
}
