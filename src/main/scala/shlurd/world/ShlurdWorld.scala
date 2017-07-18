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
package shlurd.world

import shlurd.parser._

import scala.io._
import scala.util._
import scala.collection._

sealed trait ShlurdReferenceContext
case object REF_SUBJECT extends ShlurdReferenceContext
case object REF_LOCATION extends ShlurdReferenceContext
case object REF_LOCATED extends ShlurdReferenceContext

trait ShlurdEntity
{
}

trait ShlurdProperty
{
}

trait ShlurdWorld[E<:ShlurdEntity, P<:ShlurdProperty]
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveReference(
    reference : ShlurdReference,
    context : ShlurdReferenceContext) : Try[E]

  def resolveProperty(
    entity : E,
    lemma : String) : Try[P]

  def evaluateEntityPropertyPredicate(
    entity : E,
    property : P,
    lemma : String) : Try[Boolean]

  def evaluateEntityLocationPredicate(
    entity : E,
    location : E,
    locative : ShlurdLocative) : Try[Boolean]
}

trait ShlurdNamedObject
{
  def name : String
}

class ShlurdPlatonicProperty(val name : String)
    extends ShlurdProperty with ShlurdNamedObject
{
  private[world] val states =
    new mutable.HashSet[String]

  private var closed : Boolean = false

  def getStates : Set[String] = states

  def isClosed = closed

  private[world] def closeStates()
  {
    closed = true
  }

  def instantiateState(word : ShlurdWord)
  {
    states += word.lemma
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

class ShlurdPlatonicEntity(val name : String, val form : ShlurdPlatonicForm)
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
}

class ShlurdPlatonicWorld
    extends ShlurdWorld[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  import ShlurdPlatonicWorld._

  private val forms =
    new mutable.HashMap[String, ShlurdPlatonicForm]

  private val entities =
    new mutable.HashMap[String, ShlurdPlatonicEntity]

  def getForms : Map[String, ShlurdPlatonicForm] = forms

  def instantiateForm(word : ShlurdWord) =
  {
    val name = word.lemma
    forms.getOrElseUpdate(name, new ShlurdPlatonicForm(name))
  }

  def loadBeliefs(source : Source)
  {
    val beliefs = source.getLines.mkString("\n")
    val sentences = ShlurdParser(beliefs).parseAll
    sentences.foreach(addBelief(_))
  }

  def addBelief(sentence : ShlurdSentence)
  {
    sentence match {
      case ShlurdPredicateSentence(
        ShlurdStatePredicate(
          ShlurdEntityReference(
            entity, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
          state),
        mood, formality) =>
        {
          val form = instantiateForm(entity)
          val property = form.instantiateProperty(DEFAULT_PROPERTY_WORD)
          if (mood.isNegative) {
            // FIXME:  interpret this as a constraint
            throw new IncomprehensibleBelief(sentence)
          }
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
            if (!newStates.map(_.lemma).toSet.subsetOf(property.getStates)) {
              throw new ContradictoryBelief(sentence)
            }
          } else {
            newStates.foreach(property.instantiateState(_))
            if (mood.getModality == MODAL_MUST) {
              property.closeStates
            }
          }
        }
      case _ => {
        throw new IncomprehensibleBelief(sentence)
      }
    }
  }

  override def resolveReference(
    reference : ShlurdReference,
    context : ShlurdReferenceContext) =
  {
    fail("FIXME")
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

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) =
  {
    fail("FIXME")
  }

  override def evaluateEntityLocationPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    locative : ShlurdLocative) =
  {
    fail("FIXME")
  }
}
