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
package com.lingeringsocket.shlurd.world

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
  private[world] val states =
    new mutable.LinkedHashMap[String, String]

  private var closed : Boolean = false

  override def getStates : Map[String, String] = states

  def isClosed = closed

  private[world] def closeStates()
  {
    closed = true
  }

  def instantiateState(word : ShlurdWord)
  {
    states.put(word.lemma, word.inflected)
  }

  override def toString = s"ShlurdPlatonicProperty($name)"
}

class ShlurdPlatonicForm(val name : String)
    extends ShlurdNamedObject
{
  private[world] val properties =
    new mutable.LinkedHashMap[String, ShlurdPlatonicProperty]

  private val stateSynonyms = new ShlurdSynonymMap

  private val stateNormalizations =
    new mutable.LinkedHashMap[ShlurdState, ShlurdState]

  def getStateSynonyms = stateSynonyms

  def getStateNormalizations = stateNormalizations

  def getProperties : Map[String, ShlurdPlatonicProperty] = properties

  def instantiateProperty(name : ShlurdWord) =
  {
    val property = name.lemma
    properties.getOrElseUpdate(property, new ShlurdPlatonicProperty(property))
  }

  override def toString = s"ShlurdPlatonicForm($name)"
}

case class ShlurdPlatonicEntity(
  val name : String,
  val form : ShlurdPlatonicForm,
  val qualifiers : Set[String])
    extends ShlurdEntity with ShlurdNamedObject
{
}

object ShlurdPlatonicWorld
{
  val DEFAULT_PROPERTY = "state"

  val DEFAULT_PROPERTY_WORD = ShlurdWord(DEFAULT_PROPERTY)

  abstract class RejectedBelief(
    belief : ShlurdSentence,
    cause : String) extends RuntimeException(cause)

  class IncomprehensibleBelief(belief : ShlurdSentence)
      extends RejectedBelief(belief,
        "can't understand this belief:  " + belief)
  {
  }

  class ContradictoryBelief(belief : ShlurdSentence)
      extends RejectedBelief(belief,
        "this belief contradicts previously accepted beliefs")
  {
  }

  class AmbiguousBelief(belief : ShlurdSentence)
      extends RejectedBelief(belief,
        "this belief introduces ambiguity with previously accepted beliefs")
  {
  }

  class InvalidBeliefs() extends RuntimeException(
        "accepted beliefs are invalid")
  {
  }

  case class CardinalityConstraint(lower : Int, upper : Int)
  {
  }

  protected class LabeledEdge(val label : String) extends DefaultEdge
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

  protected class FormGenitiveEdge(label : String) extends LabeledEdge(label)
  {
  }

  protected class EntityGenitiveEdge(label : String) extends LabeledEdge(label)
  {
  }

  private class ProbeFormEdge(
    val sourceForm : ShlurdPlatonicForm,
    val targetForm : ShlurdPlatonicForm,
    label : String) extends FormGenitiveEdge(label)
  {
    override def getSource = sourceForm

    override def getTarget = targetForm
  }

  private class ProbeEntityEdge(
    val sourceEntity : ShlurdPlatonicEntity,
    val targetEntity : ShlurdPlatonicEntity,
    label : String) extends EntityGenitiveEdge(label)
  {
    override def getSource = sourceEntity

    override def getTarget = targetEntity
  }
}

class ShlurdPlatonicWorld
    extends ShlurdWorld[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  import ShlurdPlatonicWorld._

  private val forms =
    new mutable.LinkedHashMap[String, ShlurdPlatonicForm]

  private val roles =
    new mutable.LinkedHashSet[String]

  private val entities =
    new mutable.LinkedHashMap[String, ShlurdPlatonicEntity]

  private val formSynonyms = new ShlurdSynonymMap
  formSynonyms.addSynonym(
    LEMMA_WHO, LEMMA_PERSON)

  private val formGenitives =
    new DirectedPseudograph[ShlurdPlatonicForm, FormGenitiveEdge](
      classOf[FormGenitiveEdge])

  private val genitiveConstraints =
    new mutable.LinkedHashMap[FormGenitiveEdge, CardinalityConstraint]

  private val propertyEdges =
    new mutable.LinkedHashSet[FormGenitiveEdge]

  private val entityGenitives =
    new DirectedPseudograph[ShlurdPlatonicEntity, EntityGenitiveEdge](
      classOf[EntityGenitiveEdge])

  private var nextId = 0

  def getForms : Map[String, ShlurdPlatonicForm] = forms

  def getEntities : Map[String, ShlurdPlatonicEntity] = entities

  protected def getPropertyEdges : Set[FormGenitiveEdge] = propertyEdges

  def clear()
  {
    entities.clear()
  }

  def instantiateForm(word : ShlurdWord) =
  {
    val name = formSynonyms.resolveSynonym(word.lemma)
    forms.getOrElseUpdate(name, new ShlurdPlatonicForm(name))
  }

  def instantiateRole(word : ShlurdWord) =
  {
    roles += word.lemma
    instantiateForm(word)
  }

  def getFormSynonyms = formSynonyms

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

  def instantiateEntity(
    sentence : ShlurdSentence,
    form : ShlurdPlatonicForm,
    qualifierString : Seq[ShlurdWord]) : ShlurdPlatonicEntity =
  {
    val qualifiers = qualifierSet(qualifierString)
    def redundantWith(existing : ShlurdPlatonicEntity) =
      (form == existing.form) && qualifiers.subsetOf(existing.qualifiers)
    if (entities.values.exists(hasQualifiers(_, form, qualifiers, true))) {
      throw new AmbiguousBelief(sentence)
    }
    val name =
      (qualifierString.map(_.lemma) ++
        Seq(form.name, nextId.toString)).mkString("_")
    nextId += 1
    val entity = new ShlurdPlatonicEntity(name, form, qualifiers)
    addEntity(entity)
    entity
  }

  protected def addEntity(entity : ShlurdPlatonicEntity)
  {
    entities.put(entity.name, entity)
  }

  protected def getPossessorForm(edge : FormGenitiveEdge) =
    formGenitives.getEdgeSource(edge)

  protected def getPossessorEntity(edge : EntityGenitiveEdge) =
    entityGenitives.getEdgeSource(edge)

  protected def getPossesseeForm(edge : FormGenitiveEdge) =
    formGenitives.getEdgeTarget(edge)

  protected def getPossesseeEntity(edge : EntityGenitiveEdge) =
    entityGenitives.getEdgeTarget(edge)

  protected def addFormGenitive(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : FormGenitiveEdge =
  {
    formGenitives.addVertex(possessor)
    formGenitives.addVertex(possessee)
    val probe = new ProbeFormEdge(possessor, possessee, label)
    formGenitives.removeEdge(probe)
    val edge = new FormGenitiveEdge(label)
    formGenitives.addEdge(possessor, possessee, edge)
    edge
  }

  protected def addEntityGenitive(
    possessor : ShlurdPlatonicEntity,
    possessee : ShlurdPlatonicEntity,
    label : String) : EntityGenitiveEdge =
  {
    entityGenitives.addVertex(possessor)
    entityGenitives.addVertex(possessee)
    val probe = new ProbeEntityEdge(possessor, possessee, label)
    entityGenitives.removeEdge(probe)
    val edge = new EntityGenitiveEdge(label)
    entityGenitives.addEdge(
      possessor, possessee, edge)
    edge
  }

  protected def isFormGenitive(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : Boolean =
  {
    formGenitives.containsEdge(
      new ProbeFormEdge(possessor, possessee, label))
  }

  protected def isEntityGenitive(
    possessor : ShlurdPlatonicEntity,
    possessee : ShlurdPlatonicEntity,
    label : String) : Boolean =
  {
    entityGenitives.containsEdge(
      new ProbeEntityEdge(possessor, possessee, label))
  }

  def loadBeliefs(source : Source)
  {
    val beliefs = source.getLines.mkString("\n")
    val sentences = ShlurdParser(beliefs).parseAll
    sentences.foreach(addBelief(_))
    validateBeliefs
  }

  def validateBeliefs()
  {
    formGenitives.edgeSet.asScala.foreach(formEdge => {
      val constraint = genitiveConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = getPossessorForm(formEdge)
        entities.values.filter(_.form == form).foreach(entity => {
          if (entityGenitives.containsVertex(entity)) {
            val c = entityGenitives.outgoingEdgesOf(entity).asScala.
              count(_.label == formEdge.label)
            if ((c < constraint.lower) || (c > constraint.upper)) {
              throw new InvalidBeliefs()
            }
          } else if (constraint.lower > 0) {
            throw new InvalidBeliefs()
          }
        })
      }
    })
  }

  private def extractQualifiedNoun(
    sentence : ShlurdSentence,
    reference : ShlurdReference,
    preQualifiers : Seq[ShlurdWord])
      : (ShlurdWord, Seq[ShlurdWord], ShlurdCount) =
  {
    reference match {
      case ShlurdNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (noun, preQualifiers, COUNT_SINGULAR)
        }
      case ShlurdNounReference(
        noun, DETERMINER_UNSPECIFIED, COUNT_PLURAL) =>
        {
          (noun, preQualifiers, COUNT_PLURAL)
        }
      case ShlurdStateSpecifiedReference(subRef, state) =>
        {
          extractQualifiedNoun(
            sentence, subRef,
            preQualifiers ++ ShlurdReference.extractQualifiers(state))
        }
      case ShlurdGenitiveReference(
        ShlurdNounReference(
          possessor, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        ShlurdNounReference(
          possession, DETERMINER_UNSPECIFIED, COUNT_SINGULAR)) =>
        {
          (possession, Seq(possessor), COUNT_SINGULAR)
        }
      case _ => throw new IncomprehensibleBelief(sentence)
    }
  }

  def addBelief(sentence : ShlurdSentence)
  {
    sentence match {
      case ShlurdPredicateSentence(predicate, mood, formality) => {
        if (mood.isNegative) {
          // FIXME:  interpret this as a constraint
          throw new IncomprehensibleBelief(sentence)
        }
        predicate match {
          case ShlurdStatePredicate(ref, state) => {
            val (noun, qualifiers, count) = extractQualifiedNoun(
              sentence, ref, Seq.empty)
            val form = instantiateForm(noun)
            ref match {
              case ShlurdStateSpecifiedReference(
                _, locState : ShlurdLocationState) =>
              {
                form.getStateNormalizations.put(locState, state)
              }
              case _ => {
                state match {
                  case ShlurdExistenceState() => {
                    addExistenceBelief(sentence, form, qualifiers, mood)
                  }
                  case _ => {
                    addPropertyBelief(sentence, form, qualifiers, state, mood)
                  }
                }
              }
            }
          }
          case ShlurdRelationshipPredicate(
            subject, complement, relationship) =>
          {
            val (complementNoun, qualifiers, count) = extractQualifiedNoun(
              sentence, complement, Seq.empty)
            val property = complement match {
              case ShlurdStateSpecifiedReference(
                _,
                ShlurdLocationState(locative, location)) =>
                {
                  if (locative != LOC_AS) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  location match {
                    case ShlurdNounReference(
                      ShlurdWord(LEMMA_PROPERTY, LEMMA_PROPERTY),
                      DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
                      COUNT_SINGULAR) =>
                      {
                        if (relationship == REL_ASSOCIATION) {
                          Some(complementNoun)
                        } else {
                          throw new IncomprehensibleBelief(sentence)
                        }
                      }
                    case _ => throw new IncomprehensibleBelief(sentence)
                  }
                }
              case _ => None
            }
            subject match {
              case ShlurdNounReference(
                subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
              ) => {
                if (!qualifiers.isEmpty) {
                  throw new IncomprehensibleBelief(sentence)
                }
                relationship match {
                  case REL_IDENTITY => {
                    // "a canine is a dog"
                    assert(count == COUNT_SINGULAR)
                    val form = instantiateForm(complementNoun)
                    formSynonyms.addSynonym(subjectNoun.lemma, form.name)
                  }
                  case REL_ASSOCIATION => {
                    // "a dog has an owner"
                    val upper = count match {
                      case COUNT_SINGULAR => 1
                      case COUNT_PLURAL => Int.MaxValue
                    }
                    val newConstraint = sentence.mood.getModality match {
                      case MODAL_NEUTRAL | MODAL_MUST | MODAL_EMPHATIC =>
                        CardinalityConstraint(1, 1)
                      case MODAL_MAY | MODAL_POSSIBLE |
                          MODAL_CAPABLE | MODAL_PERMITTED =>
                        CardinalityConstraint(0, upper)
                      case MODAL_SHOULD =>
                        throw new IncomprehensibleBelief(sentence)
                    }
                    val possessorForm = instantiateForm(subjectNoun)
                    val possesseeForm = instantiateRole(complementNoun)
                    val label = complementNoun.lemma
                    val edge = addFormGenitive(
                      possessorForm, possesseeForm, label)
                    val constraint = genitiveConstraints.get(edge) match {
                      case Some(oldConstraint) => CardinalityConstraint(
                        Math.max(oldConstraint.lower, newConstraint.lower),
                        Math.min(oldConstraint.upper, newConstraint.upper))
                      case _ => newConstraint
                    }
                    genitiveConstraints.put(edge, constraint)
                    property match {
                      case Some(name) => {
                        propertyEdges += edge
                      }
                      case _ =>
                    }
                  }
                }
              }
              case ShlurdNounReference(
                subjectNoun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
              ) => {
                // FIXME "Larry has a dog"
                assert(relationship == REL_IDENTITY)
                if (qualifiers.isEmpty) {
                  // "Fido is a dog"
                  val form = instantiateForm(complementNoun)
                  instantiateEntity(sentence, form, Seq(subjectNoun))
                } else {
                  // "Fido is Franny's pet"
                  if (qualifiers.size != 1) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  val possessorOpt = resolveUniqueName(qualifiers.head)
                  val possesseeOpt = resolveUniqueName(subjectNoun)
                  if (possessorOpt.isEmpty || possesseeOpt.isEmpty) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  val possessor = possessorOpt.get
                  val possessee = possesseeOpt.get
                  val label = complementNoun.lemma
                  if (!isFormGenitive(possessor.form, possessee.form, label)) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  addEntityGenitive(possessor, possessee, label)
                }
              }
              case _ => throw new IncomprehensibleBelief(sentence)
            }
          }
          case _ => throw new IncomprehensibleBelief(sentence)
        }
      }
      case _ => throw new IncomprehensibleBelief(sentence)
    }
  }

  private def addExistenceBelief(
    sentence : ShlurdSentence,
    form : ShlurdPlatonicForm,
    qualifiers : Seq[ShlurdWord],
    mood : ShlurdMood)
  {
    // FIXME:  interpret mood
    instantiateEntity(sentence, form, qualifiers)
  }

  private def addPropertyBelief(
    sentence : ShlurdSentence,
    form : ShlurdPlatonicForm,
    qualifiers : Seq[ShlurdWord],
    state : ShlurdState,
    mood : ShlurdMood)
  {
    if (qualifiers.size == 1) {
      // "a light that is lit is on"
      if (sentence.mood.getModality != MODAL_NEUTRAL) {
          throw new IncomprehensibleBelief(sentence)
      }
      state match {
        case ShlurdPropertyState(word) => {
          form.getStateSynonyms.addSynonym(
            qualifiers.head.lemma, word.lemma)
        }
        case _ => {
          throw new IncomprehensibleBelief(sentence)
        }
      }
      return
    } else if (!qualifiers.isEmpty) {
      // but maybe we should allow constraints on qualified entities?
      throw new IncomprehensibleBelief(sentence)
    }
    // "a light may be on or off"
    if (sentence.mood.getModality == MODAL_NEUTRAL) {
      throw new IncomprehensibleBelief(sentence)
    }
    val property = form.instantiateProperty(DEFAULT_PROPERTY_WORD)
    val newStates = state match {
      case ShlurdPropertyState(word) => {
        Seq(word)
      }
      case ShlurdConjunctiveState(determiner, states, _) => {
        // FIXME:  interpret determiner
        states.flatMap(_ match {
          case ShlurdPropertyState(word) => {
            Seq(word)
          }
          case _ => {
            throw new IncomprehensibleBelief(sentence)
          }
        })
      }
      case _ => {
        throw new IncomprehensibleBelief(sentence)
      }
    }
    if (property.isClosed) {
      if (!newStates.map(_.lemma).toSet.subsetOf(property.getStates.keySet)) {
        throw new ContradictoryBelief(sentence)
      }
    } else {
      newStates.foreach(property.instantiateState(_))
      if (mood.getModality == MODAL_MUST) {
        property.closeStates
      }
    }
  }

  private def resolveUniqueName(word : ShlurdWord)
      : Option[ShlurdPlatonicEntity] =
  {
    val candidates = entities.values.filter(
      _.qualifiers == Set(word.lemma))
    if (candidates.size > 1) {
      None
    } else {
      candidates.headOption
    }
  }

  def resolveGenitive(
    possessor : ShlurdPlatonicEntity,
    label : String)
      : Set[ShlurdPlatonicEntity] =
  {
    if (!entityGenitives.containsVertex(possessor)) {
      Set.empty
    } else {
      entityGenitives.outgoingEdgesOf(possessor).
        asScala.filter(_.label == label).map(
          getPossesseeEntity)
    }
  }

  override def resolveEntity(
    lemma : String,
    context : ShlurdReferenceContext,
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
    person : ShlurdPerson,
    gender : ShlurdGender,
    count : ShlurdCount) : Try[Set[ShlurdPlatonicEntity]] =
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
    val stateName = form.getStateSynonyms.resolveSynonym(lemma)
    form.properties.values.find(p => p.states.contains(stateName)) match {
      case Some(p) => return Success((p, stateName))
      case _ => {
        if (formGenitives.containsVertex(form)) {
          formGenitives.outgoingEdgesOf(form).asScala.
            filter(propertyEdges.contains(_)).foreach(edge => {
              val propertyForm = getPossesseeForm(edge)
              val attempt = resolveFormProperty(propertyForm, lemma)
              if (attempt.isSuccess) {
                return attempt
              }
            })
        }
      }
    }
    fail(s"unknown property $lemma")
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : ShlurdDeterminer) =
  {
    val formName = entity.form.name
    def nounRef = ShlurdNounReference(
      ShlurdWord(formName), determiner)
    if (entity.qualifiers.isEmpty) {
      nounRef
    } else if ((formName == LEMMA_PERSON) &&
      (entity.qualifiers.size == 1))
    {
      val name = ShlurdParseUtils.capitalize(entity.qualifiers.head)
      ShlurdNounReference(ShlurdWord(name), DETERMINER_UNSPECIFIED)
    } else {
      ShlurdReference.qualified(
        nounRef, entity.qualifiers.map(q => ShlurdWord(q)).toSeq)
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) : Try[Trilean] =
  {
    if (entityGenitives.containsVertex(entity)) {
      entityGenitives.outgoingEdgesOf(entity).asScala.foreach(edge => {
        val propertyEntity = getPossesseeEntity(edge)
        if (propertyEntity.form.properties.values.toSeq.contains(property)) {
          return evaluateEntityPropertyPredicate(
            propertyEntity,
            property,
            lemma)
        }
      })
    }
    Success(Trilean.Unknown)
  }

  override def evaluateEntityLocationPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    locative : ShlurdLocative,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    if (locative == LOC_GENITIVE_OF) {
      if (!entityGenitives.containsVertex(entity) ||
        !entityGenitives.containsVertex(location) ||
        (qualifiers.size != 1))
      {
        Success(Trilean.False)
      } else {
        val label = qualifiers.head
        Success(Trilean(isEntityGenitive(location, entity, label)))
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
          if (!formGenitives.containsVertex(form)) {
            Success(Trilean(isRole))
          } else {
            if (formGenitives.incomingEdgesOf(form).asScala.
              exists(_.label == lemma))
            {
              Success(Trilean(
                entityGenitives.incomingEdgesOf(entity).asScala.
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
    entity : ShlurdPlatonicEntity, state : ShlurdState) =
  {
    entity.form.getStateNormalizations.get(state).getOrElse(state)
  }
}
