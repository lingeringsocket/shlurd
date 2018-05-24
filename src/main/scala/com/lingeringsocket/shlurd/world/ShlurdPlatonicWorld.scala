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

  private val inflectedStateNormalizations =
    new mutable.LinkedHashMap[ShlurdState, ShlurdState]

  private val stateNormalizations =
    new mutable.LinkedHashMap[ShlurdState, ShlurdState]

  def getProperties : Map[String, ShlurdPlatonicProperty] = properties

  def instantiateProperty(name : ShlurdWord) =
  {
    val property = name.lemma
    properties.getOrElseUpdate(property, new ShlurdPlatonicProperty(property))
  }

  def resolveStateSynonym(lemma : String) : String =
  {
    normalizeState(ShlurdPropertyState(ShlurdWord(lemma))) match {
      case ShlurdPropertyState(word) => word.lemma
      case _ => lemma
    }
  }

  private[world] def addStateNormalization(
    state : ShlurdState, transformed : ShlurdState)
  {
    val normalized = normalizeState(transformed)
    inflectedStateNormalizations.put(state, normalized)
    stateNormalizations.put(foldState(state), normalized)
  }

  def normalizeState(state : ShlurdState) : ShlurdState =
  {
    inflectedStateNormalizations.get(state).getOrElse(
      stateNormalizations.get(foldState(state)).getOrElse(state))
  }

  private def foldState(state : ShlurdState) : ShlurdState =
  {
    // FIXME:  should fold compound states as well
    state match {
      case ShlurdPropertyState(word) =>
        ShlurdPropertyState(ShlurdWord(word.lemma))
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

  protected[world] class LabeledEdge(
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

  protected[world] class FormAssocEdge(
    label : String) extends LabeledEdge(label)
  {
  }

  protected[world] class EntityAssocEdge(
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

  def getForms : Map[String, ShlurdPlatonicForm] = forms

  def getEntities : Map[String, ShlurdPlatonicEntity] = entities

  protected[world] def getPropertyEdges = propertyEdges.toSet

  protected[world] def getAssocConstraints = assocConstraints.toMap

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

  protected[world] def getFormSynonyms =
    formSynonyms

  protected[world] def getFormAssocGraph =
    new AsUnmodifiableGraph(formAssocs)

  protected[world] def getEntityAssocGraph =
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

  private def instantiateEntity(
    sentence : ShlurdSentence,
    form : ShlurdPlatonicForm,
    qualifierString : Seq[ShlurdWord],
    properName : String = "") : ShlurdPlatonicEntity =
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
    val entity = new ShlurdPlatonicEntity(name, form, qualifiers, properName)
    addEntity(entity)
    entity
  }

  protected[world] def addEntity(entity : ShlurdPlatonicEntity)
  {
    entities.put(entity.name, entity)
  }

  protected[world] def getPossessorForm(edge : FormAssocEdge) =
    formAssocs.getEdgeSource(edge)

  protected[world] def getPossessorEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeSource(edge)

  protected[world] def getPossesseeForm(edge : FormAssocEdge) =
    formAssocs.getEdgeTarget(edge)

  protected[world] def getPossesseeEntity(edge : EntityAssocEdge) =
    entityAssocs.getEdgeTarget(edge)

  protected[world] def addFormAssoc(
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

  protected[world] def addEntityAssoc(
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

  protected[world] def isFormAssoc(
    possessor : ShlurdPlatonicForm,
    possessee : ShlurdPlatonicForm,
    label : String) : Boolean =
  {
    formAssocs.containsEdge(
      new ProbeFormEdge(possessor, possessee, label))
  }

  protected[world] def isEntityAssoc(
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
    sentences.foreach(addBelief(_))
    validateBeliefs
  }

  def validateBeliefs()
  {
    formAssocs.edgeSet.asScala.foreach(formEdge => {
      val constraint = assocConstraints(formEdge)
      if ((constraint.lower > 0) || (constraint.upper < Int.MaxValue)) {
        val form = getPossessorForm(formEdge)
        entities.values.filter(_.form == form).foreach(entity => {
          if (entityAssocs.containsVertex(entity)) {
            val c = entityAssocs.outgoingEdgesOf(entity).asScala.
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
    if (sentence.hasUnknown) {
      throw new IncomprehensibleBelief(sentence)
    }
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
                _, specifiedState @
                  (_ : ShlurdLocationState | _ : ShlurdPropertyState)) =>
              {
                // "a television that is on the blink is broken"
                // or "a television that is busted is broken"
                // or "a busted television is broken"
                if (sentence.mood.getModality != MODAL_NEUTRAL) {
                  throw new IncomprehensibleBelief(sentence)
                }
                state match {
                  case ps : ShlurdPropertyState => {
                    form.addStateNormalization(specifiedState, state)
                    return
                  }
                  case ShlurdExistenceState() =>
                  case _ => {
                    throw new IncomprehensibleBelief(sentence)
                  }
                }
              }
              case _ =>
            }
            state match {
              case ShlurdExistenceState() => {
                // "there is a television"
                addExistenceBelief(sentence, form, qualifiers, mood)
              }
              case _ => {
                addPropertyBelief(sentence, form, qualifiers, state, mood)
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
                  // "A television has a volume as a property"
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
                    val edge = addFormAssoc(
                      possessorForm, possesseeForm, label)
                    val constraint = assocConstraints.get(edge) match {
                      case Some(oldConstraint) => CardinalityConstraint(
                        Math.max(oldConstraint.lower, newConstraint.lower),
                        Math.min(oldConstraint.upper, newConstraint.upper))
                      case _ => newConstraint
                    }
                    assocConstraints.put(edge, constraint)
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
                  instantiateEntity(
                    sentence, form, Seq(subjectNoun), subjectNoun.lemmaUnfolded)
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
                  if (!isFormAssoc(possessor.form, possessee.form, label)) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  addEntityAssoc(possessor, possessee, label)
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
    if (!qualifiers.isEmpty) {
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
    val stateName = form.resolveStateSynonym(lemma)
    form.properties.values.find(p => p.states.contains(stateName)) match {
      case Some(p) => return Success((p, stateName))
      case _ => fail(s"unknown property $lemma")
    }
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : ShlurdDeterminer) =
  {
    val formName = entity.form.name
    def nounRef = ShlurdNounReference(
      ShlurdWord(formName), determiner)
    if (!entity.properName.isEmpty) {
      ShlurdNounReference(
        ShlurdWord(entity.properName), DETERMINER_UNSPECIFIED)
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

  override def evaluateEntityLocationPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    locative : ShlurdLocative,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    if (locative == LOC_GENITIVE_OF) {
      if (!entityAssocs.containsVertex(entity) ||
        !entityAssocs.containsVertex(location) ||
        (qualifiers.size != 1))
      {
        Success(Trilean.False)
      } else {
        val label = qualifiers.head
        Success(Trilean(isEntityAssoc(location, entity, label)))
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
    entity : ShlurdPlatonicEntity, state : ShlurdState) =
  {
    entity.form.normalizeState(state)
  }
}
