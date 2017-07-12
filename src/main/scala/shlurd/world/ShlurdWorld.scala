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
}

class ShlurdPlatonicForm(val name : String)
    extends ShlurdNamedObject
{
  private[world] val properties =
    new mutable.HashMap[String, ShlurdPlatonicProperty]
}

class ShlurdPlatonicEntity(val name : String, val form : ShlurdPlatonicForm)
    extends ShlurdEntity with ShlurdNamedObject
{
}

class ShlurdPlatonicWorld
    extends ShlurdWorld[ShlurdPlatonicEntity, ShlurdPlatonicProperty]
{
  private val forms =
    new mutable.HashMap[String, ShlurdPlatonicForm]

  private val entities =
    new mutable.HashMap[String, ShlurdPlatonicEntity]

  def malformedBelief()
  {
    throw new RuntimeException("can't understand this belief")
  }

  def addBelief(sentence : ShlurdSentence)
  {
    sentence match {
      case ShlurdPredicateSentence(predicate, mood, formality) => {
        predicate match {
          case ShlurdStatePredicate(subject, state) => {
          }
          case _ => malformedBelief
        }
      }
      case _ => malformedBelief
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
