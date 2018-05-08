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
}

class ShlurdPlatonicForm(val name : String)
    extends ShlurdNamedObject
{
  private[world] val properties =
    new mutable.LinkedHashMap[String, ShlurdPlatonicProperty]

  private val stateSynonyms = new ShlurdSynonymMap

  def getStateSynonyms = stateSynonyms

  def getProperties : Map[String, ShlurdPlatonicProperty] = properties

  def instantiateProperty(word : ShlurdWord) =
  {
    val property = word.lemma
    properties.getOrElseUpdate(property, new ShlurdPlatonicProperty(property))
  }
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

  val DEFAULT_PROPERTY_WORD = ShlurdWord(DEFAULT_PROPERTY, DEFAULT_PROPERTY)

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
}

class LabeledEdge(val label : String) extends DefaultEdge
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

class ProbeEdge[V <: AnyRef](
  val sourceEntity : V,
  val targetEntity : V,
  label : String) extends LabeledEdge(label)
{
  override def getSource = sourceEntity

  override def getTarget = targetEntity
}

case class CardinalityConstraint(lower : Int, upper : Int)
{
}

class ShlurdPlatonicWorld
    extends ShlurdWorld[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  import ShlurdPlatonicWorld._

  private val forms =
    new mutable.LinkedHashMap[String, ShlurdPlatonicForm]

  private val entities =
    new mutable.LinkedHashMap[String, ShlurdPlatonicEntity]

  private val formSynonyms = new ShlurdSynonymMap
  formSynonyms.addSynonym(
    LEMMA_WHO, LEMMA_PERSON)

  private val formGenitives =
    new DirectedPseudograph[ShlurdPlatonicForm, LabeledEdge](
      classOf[LabeledEdge])

  private val genitiveConstraints =
    new mutable.LinkedHashMap[LabeledEdge, CardinalityConstraint]

  private val entityGenitives =
    new DirectedPseudograph[ShlurdPlatonicEntity, LabeledEdge](
      classOf[LabeledEdge])

  private var nextId = 0

  def getForms : Map[String, ShlurdPlatonicForm] = forms

  def getEntities : Map[String, ShlurdPlatonicEntity] = entities

  def clear()
  {
    entities.clear()
  }

  def instantiateForm(word : ShlurdWord) =
  {
    val name = formSynonyms.resolveSynonym(word.lemma)
    forms.getOrElseUpdate(name, new ShlurdPlatonicForm(name))
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
        val form = formGenitives.getEdgeSource(formEdge)
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

  private def extractQualifiedEntity(
    sentence : ShlurdSentence,
    reference : ShlurdReference,
    preQualifiers : Seq[ShlurdWord])
      : (ShlurdWord, Seq[ShlurdWord], ShlurdCount) =
  {
    reference match {
      case ShlurdEntityReference(
        entity, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (entity, preQualifiers, COUNT_SINGULAR)
        }
      case ShlurdEntityReference(
        entity, DETERMINER_UNSPECIFIED, COUNT_PLURAL) =>
        {
          (entity, preQualifiers, COUNT_PLURAL)
        }
      case ShlurdStateSpecifiedReference(subRef, state) =>
        {
          extractQualifiedEntity(
            sentence, subRef,
            preQualifiers ++ ShlurdReference.extractQualifiers(state))
        }
      case ShlurdGenitiveReference(
        ShlurdEntityReference(
          possessor, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        ShlurdEntityReference(
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
            val (entity, qualifiers, count) = extractQualifiedEntity(
              sentence, ref, Seq.empty)
            val form = instantiateForm(entity)
            state match {
              case ShlurdExistenceState() => {
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
            val (complementEntity, qualifiers, count) = extractQualifiedEntity(
              sentence, complement, Seq.empty)
            subject match {
              case ShlurdEntityReference(
                subjectEntity, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
              ) => {
                if (!qualifiers.isEmpty) {
                  throw new IncomprehensibleBelief(sentence)
                }
                relationship match {
                  case REL_IDENTITY => {
                    // "a canine is a dog"
                    assert(count == COUNT_SINGULAR)
                    val form = instantiateForm(complementEntity)
                    formSynonyms.addSynonym(subjectEntity.lemma, form.name)
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
                    val possessorForm = instantiateForm(subjectEntity)
                    val possesseeForm = instantiateForm(complementEntity)
                    formGenitives.addVertex(possessorForm)
                    formGenitives.addVertex(possesseeForm)
                    val label = complementEntity.lemma
                    val edge = new ProbeEdge(
                      possessorForm, possesseeForm, label)
                    if (!formGenitives.containsEdge(edge)) {
                      formGenitives.addEdge(
                        possessorForm, possesseeForm,
                        new LabeledEdge(label))
                    }
                    val constraint = genitiveConstraints.get(edge) match {
                      case Some(oldConstraint) => CardinalityConstraint(
                        Math.max(oldConstraint.lower, newConstraint.lower),
                        Math.min(oldConstraint.upper, newConstraint.upper))
                      case _ => newConstraint
                    }
                    genitiveConstraints.put(edge, constraint)
                  }
                }
              }
              case ShlurdEntityReference(
                subjectEntity, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
              ) => {
                // FIXME "Larry has a dog"
                assert(relationship == REL_IDENTITY)
                if (qualifiers.isEmpty) {
                  // "Fido is a dog"
                  val form = instantiateForm(complementEntity)
                  instantiateEntity(sentence, form, Seq(subjectEntity))
                } else {
                  // "Fido is Franny's pet"
                  if (qualifiers.size != 1) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  val possessorOpt = resolveUniqueName(qualifiers.head)
                  val possesseeOpt = resolveUniqueName(subjectEntity)
                  if (possessorOpt.isEmpty || possesseeOpt.isEmpty) {
                    throw new IncomprehensibleBelief(sentence)
                  }
                  val possessor = possessorOpt.get
                  val possessee = possesseeOpt.get
                  val label = complementEntity.lemma
                  if (!formGenitives.containsEdge(new ProbeEdge(
                    possessor.form,
                    possessee.form,
                    label)))
                  {
                    throw new IncomprehensibleBelief(sentence)
                  }

                  entityGenitives.addVertex(possessor)
                  entityGenitives.addVertex(possessee)
                  if (!entityGenitives.containsEdge(new ProbeEdge(
                    possessor, possessee, label)))
                  {
                    entityGenitives.addEdge(
                      possessor, possessee, new LabeledEdge(label))
                  }
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
          entityGenitives.getEdgeTarget)
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
    lemma : String) =
  {
    val form = entity.form
    val stateName = form.getStateSynonyms.resolveSynonym(lemma)
    form.properties.values.find(p => p.states.contains(stateName)) match {
      case Some(p) => Success((p, stateName))
      case _ => fail(s"unknown property $lemma")
    }
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : ShlurdDeterminer) =
  {
    val formName = entity.form.name
    def entityReference = ShlurdEntityReference(
      ShlurdWord(formName, formName), determiner)
    if (entity.qualifiers.isEmpty) {
      entityReference
    } else if ((formName == LEMMA_PERSON) &&
      (entity.qualifiers.size == 1))
    {
      val name = ShlurdParseUtils.capitalize(entity.qualifiers.head)
      ShlurdEntityReference(ShlurdWord(name, name), DETERMINER_UNSPECIFIED)
    } else {
      ShlurdReference.qualified(
        entityReference, entity.qualifiers.map(q => ShlurdWord(q, q)).toSeq)
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) : Try[Trilean] =
  {
    fail("FIXME")
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
        Success(Trilean(
          entityGenitives.containsEdge(
            new ProbeEdge(location, entity, label))))
      }
    } else {
      fail("FIXME")
    }
  }
}
