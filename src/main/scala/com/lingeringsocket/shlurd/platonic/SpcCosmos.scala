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

  def isSynthetic = name.contains('_')

  override def toString = s"SpcProperty($name)"
}

sealed abstract class SpcIdeal(val name : String)
    extends ShlurdNamedObject
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

  private[platonic] val properties =
    new mutable.LinkedHashMap[String, SpcProperty]

  private val inflectedStateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  private val stateNormalizations =
    new mutable.LinkedHashMap[SilState, SilState]

  def isTentative = name.endsWith(TENTATIVE_SUFFIX)

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
  private val forms = new mutable.LinkedHashMap[String, SpcForm]

  private val roles = new mutable.LinkedHashMap[String, SpcRole]

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

  def getForms : Map[String, SpcForm] = forms

  def getRoles : Map[String, SpcRole] = roles

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
    forms.get(LEMMA_ENTITY) match {
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
    def checkRole = roles.get(name)
    def checkForm = forms.get(name)
    if (assumeRole) {
      checkRole.getOrElse(checkForm.getOrElse(instantiateRole(word)))
    } else {
      checkForm.getOrElse(checkRole.getOrElse(instantiateForm(word)))
    }
  }

  def resolveForm(name : String) = resolveIdeal(name, false)._1

  def resolveRole(name : String) = resolveIdeal(name, true)._2

  def instantiateForm(word : SilWord) =
  {
    val name = idealSynonyms.resolveSynonym(word.lemma)
    val ideal = forms.getOrElseUpdate(
      name, registerForm(new SpcForm(name)))
    ideal
  }

  def instantiateRole(word : SilWord) =
  {
    val name = idealSynonyms.resolveSynonym(word.lemma)
    val ideal = roles.getOrElseUpdate(
      name, registerRole(new SpcRole(name)))
    ideal
  }

  def forgetForm(form : SpcForm)
  {
    // FIXME remove synonyms?
    assert(!entities.values.exists(_.form == form))
    forms.remove(form.name)
    forgetIdeal(form)
  }

  def matchAssocs(oldForm : SpcForm, newForm : SpcForm)
      : Seq[(SpcFormAssocEdge, SpcFormAssocEdge)]=
  {
    val formAssocs = graph.formAssocs
    formAssocs.outgoingEdgesOf(oldForm).asScala.toSeq.flatMap(
      oldEdge => {
        assert(!inverseAssocEdges.contains(oldEdge))
        val possesseeForm = graph.getPossesseeForm(oldEdge).get
        val possesseeRole = graph.getPossesseeRole(oldEdge)
        graph.getFormAssocEdge(newForm, possesseeForm, possesseeRole).map(
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

  def replaceForm(oldForm : SpcForm, newForm : SpcForm)
  {
    assert(oldForm.isTentative)
    graph.replaceVertex(graph.formAssocs, oldForm, newForm)
    forgetForm(oldForm)
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
        if (old != entity) {
          graph.replaceVertex(graph.entityAssocs, old, entity)
          assert(graph.entityAssocs.degreeOf(old) == 0)
          graph.entityAssocs.removeVertex(old)
        }
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
    possessor : SpcForm,
    possessee : SpcRole) : SpcFormAssocEdge =
  {
    val roleName = possessee.name
    graph.formAssocs.getAllEdges(
      possessor, possessee).asScala.find(_.getRoleName == roleName) match
    {
      case Some(edge) => edge
      case _ => {
        val edge = new SpcFormAssocEdge(roleName)
        graph.formAssocs.addEdge(possessor, possessee, edge)
        edge
      }
    }
  }

  protected[platonic] def addEntityAssoc(
    possessor : SpcEntity,
    possessee : SpcEntity,
    roleName : String) : SpcEntityAssocEdge =
  {
    val role = roles(roleName)
    graph.getFormAssocEdge(possessor.form, possessee.form, role) match {
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
    val creed = new SpcCreed(this)
    getFormAssocGraph.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = graph.getPossessorForm(formEdge)
        entities.values.filter(
          e => graph.isHyponym(e.form, form)).foreach(entity =>
          {
            val entityEdges = getEntityAssocGraph.
              outgoingEdgesOf(entity).asScala
            val c = entityEdges.count(_.formEdge == formEdge)
            if ((c < constraint.lower) || (c > constraint.upper)) {
              throw new CardinalityExcn(
                creed.formAssociationBelief(formEdge))
            }
          }
        )
      }
    })
    assert(sanityCheck)
  }

  def sanityCheck() : Boolean =
  {
    assert(graph.sanityCheck)
    val ideals = forms.values.toSet ++ roles.values.toSet
    assert(ideals == graph.idealTaxonomy.vertexSet.asScala)
    assert(ideals == graph.formAssocs.vertexSet.asScala)
    assert(entities.values.toSet == graph.entityAssocs.vertexSet.asScala)
    forms.foreach(pair => assert(pair._1 == pair._2.name))
    entities.foreach(pair => assert(pair._1 == pair._2.name))
    forms.values.foreach(form => {
      assert((graph.formAssocs.outDegreeOf(form) ==
        graph.formAssocs.outgoingEdgesOf(form).
        asScala.map(_.getRoleName).toSet.size),
        form.toString)
    })
    true
  }

  def resolveGenitive(
    possessor : SpcEntity,
    roleName : String)
      : Set[SpcEntity] =
  {
    ShlurdParseUtils.orderedSet(getEntityAssocGraph.outgoingEdgesOf(possessor).
      asScala.toSeq.filter(_.getRoleName == roleName).map(
        graph.getPossesseeEntity))
  }

  private def resolveIdeal(
    lemma : String,
    assumeRole : Boolean) : (Option[SpcForm], Option[SpcRole]) =
  {
    val name = idealSynonyms.resolveSynonym(lemma)
    def checkRole = roles.get(name) match {
      case Some(role) => (graph.getFormForRole(role), Some(role))
      case _ => (None, None)
    }
    def checkForm = (forms.get(name), None)
    if (assumeRole) {
      val pair = checkRole
      if (pair._2.isEmpty) {
        checkForm
      } else {
        pair
      }
    } else {
      val pair = checkForm
      if (pair._1.isEmpty) {
        checkRole
      } else {
        pair
      }
    }
  }

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String]) =
  {
    val (formOpt, roleOpt) = resolveIdeal(lemma, false)
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
    resolveFormProperty(entity.form, lemma) match {
      case Some((property, stateName)) => Success((property, stateName))
      case _ => fail(s"unknown property $lemma")
    }
  }

  def resolveFormProperty(
    form : SpcForm,
    lemma : String) : Option[(SpcProperty, String)] =
  {
    graph.getFormHypernyms(form).foreach(hyperForm => {
      hyperForm.resolveProperty(lemma) match {
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

  def findProperty(
    form : SpcForm, name : String) : Option[SpcProperty] =
  {
    graph.getFormHypernyms(form).foreach(hyperForm => {
      hyperForm.properties.get(name) match {
        case Some(matchingProperty) => {
          return Some(matchingProperty)
        }
        case _ =>
      }
    })
    None
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
        if (propertyEntity.form.properties.values.toSeq.contains(property)) {
          return evaluateEntityPropertyPredicate(
            propertyEntity,
            property,
            lemma)
        }
        resolveFormProperty(propertyEntity.form, lemma) match {
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
    val (formOpt, roleOpt) = resolveIdeal(lemma, false)
    formOpt match {
      case Some(form) => {
        if (graph.isHyponym(entity.form, form)) {
          roleOpt match {
            case Some(role) => {
              Success(Trilean(
                getEntityAssocGraph.incomingEdgesOf(entity).asScala.
                  exists(edge =>
                    graph.isHyponym(
                      role,
                      graph.getPossesseeRole(edge.formEdge)))))
            }
            case _ => {
              Success(Trilean.True)
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
    entity : SpcEntity, originalState : SilState) =
  {
    graph.getFormHypernyms(entity.form).foldLeft(originalState) {
      case (state, form) => {
        form.normalizeState(state)
      }
    }
  }
}
