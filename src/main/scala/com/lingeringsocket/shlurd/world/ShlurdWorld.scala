// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

import scala.io._
import scala.util._
import scala.collection._

import spire.math._

sealed trait ShlurdReferenceContext
case object REF_SUBJECT extends ShlurdReferenceContext
case object REF_LOCATION extends ShlurdReferenceContext
case object REF_LOCATED extends ShlurdReferenceContext

trait ShlurdEntity
{
}

trait ShlurdProperty
{
  // lemma -> inflected
  def getStates() : Map[String, String]
}

trait ShlurdWorld[E<:ShlurdEntity, P<:ShlurdProperty]
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveEntity(
    lemma : String,
    context : ShlurdReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[E]]

  def resolveProperty(
    entity : E,
    lemma : String) : Try[P]

  def evaluateEntityPropertyPredicate(
    entity : E,
    property : P,
    lemma : String) : Try[Trilean]

  def evaluateEntityLocationPredicate(
    entity : E,
    location : E,
    locative : ShlurdLocative) : Try[Trilean]

  def specificReference(
    entity : E,
    determiner : ShlurdDeterminer) : ShlurdReference

  def qualifierSet(qualifiers : Seq[ShlurdWord]) =
    qualifiers.map(_.lemma).toSet
}

trait ShlurdNamedObject
{
  def name : String
}

class ShlurdSynonymMap
{
  private val map = new mutable.HashMap[String, String]

  def addSynonym(synonym : String, fundamental : String)
  {
    map.put(synonym, fundamental)
  }

  def resolveSynonym(synonym : String) : String =
  {
    map.get(synonym).getOrElse(synonym)
  }
}

class ShlurdPlatonicProperty(val name : String)
    extends ShlurdProperty with ShlurdNamedObject
{
  private[world] val states =
    new mutable.HashMap[String, String]

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
    new mutable.HashMap[String, ShlurdPlatonicProperty]

  def getProperties : Map[String, ShlurdPlatonicProperty] = properties

  def instantiateProperty(word : ShlurdWord) =
  {
    val property = word.lemma
    properties.getOrElseUpdate(property, new ShlurdPlatonicProperty(property))
  }
}

class ShlurdPlatonicEntity(
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
        "can't understand this belief")
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
}

class ShlurdPlatonicWorld
    extends ShlurdWorld[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  import ShlurdPlatonicWorld._

  private val forms =
    new mutable.HashMap[String, ShlurdPlatonicForm]

  private val entities =
    new mutable.HashMap[String, ShlurdPlatonicEntity]

  private val formSynonyms = new ShlurdSynonymMap

  private var nextId = 0

  def getForms : Map[String, ShlurdPlatonicForm] = forms

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
    entities.put(name, entity)
    entity
  }

  def loadBeliefs(source : Source)
  {
    val beliefs = source.getLines.mkString("\n")
    val sentences = ShlurdParser(beliefs).parseAll
    sentences.foreach(addBelief(_))
  }

  private def extractQualifiedEntity(
    sentence : ShlurdSentence,
    reference : ShlurdReference,
    preQualifiers : Seq[ShlurdWord])
      : (ShlurdWord, Seq[ShlurdWord]) =
  {
    reference match {
      case ShlurdEntityReference(
        entity, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (entity, preQualifiers)
        }
      case ShlurdQualifiedReference(subRef, qualifiers) =>
        {
          extractQualifiedEntity(
            sentence, subRef, preQualifiers ++ qualifiers)
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
            val (entity, qualifiers) = extractQualifiedEntity(
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
          case ShlurdIdentityPredicate(subject, complement) => {
            val (complementEntity, qualifiers) = extractQualifiedEntity(
              sentence, complement, Seq.empty)
            if (!qualifiers.isEmpty) {
              throw new IncomprehensibleBelief(sentence)
            }
            val form = instantiateForm(complementEntity)
            // FIXME:  cycle detection
            subject match {
              case ShlurdEntityReference(
                subjectEntity, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
              ) => {
                formSynonyms.addSynonym(subjectEntity.lemma, form.name)
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

  override def resolveEntity(
    lemma : String,
    context : ShlurdReferenceContext,
    qualifiers : Set[String]) =
  {
    forms.get(formSynonyms.resolveSynonym(lemma)) match {
      case Some(form) => {
        Success(entities.values.filter(
          hasQualifiers(_, form, qualifiers, false)).toSet)
      }
      case _ => {
        fail(s"unknown entity $lemma")
      }
    }
  }

  override def resolveProperty(
    entity : ShlurdPlatonicEntity,
    lemma : String) =
  {
    entity.form.properties.values.find(p => p.states.contains(lemma)) match {
      case Some(p) => Success(p)
      case _ => fail(s"unknown property $lemma")
    }
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : ShlurdDeterminer) =
  {
    val formName = entity.form.name
    val entityReference = ShlurdEntityReference(
      ShlurdWord(formName, formName), determiner)
    if (entity.qualifiers.isEmpty) {
      entityReference
    } else {
      ShlurdQualifiedReference(
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
    locative : ShlurdLocative) : Try[Trilean] =
  {
    fail("FIXME")
  }
}
