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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import spire.math._

import scala.util._
import scala.collection._

sealed trait SilReferenceContext
case object REF_SUBJECT extends SilReferenceContext
case object REF_COMPLEMENT extends SilReferenceContext
case object REF_DIRECT_OBJECT extends SilReferenceContext
case object REF_ADPOSITION_OBJ extends SilReferenceContext
case object REF_ADPOSITION_SUBJ extends SilReferenceContext

trait SmcEntity
{
  def isTentative : Boolean = false

  def getUniqueIdentifier : String
}

trait SmcProperty
{
}

trait SmcCosmos[EntityType<:SmcEntity, PropertyType<:SmcProperty]
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[EntityType]]

  def resolvePropertyState(
    entity : EntityType,
    lemma : String) : Try[(PropertyType, String)]

  def resolvePropertyName(
    entity : EntityType,
    propertyName : String) : Try[PropertyType] =
  {
    Failure(new UnsupportedOperationException)
  }

  def evaluateEntityPropertyPredicate(
    entity : EntityType,
    property : PropertyType,
    lemma : String) : Try[Trilean]

  def evaluateEntityProperty(
    entity : EntityType,
    propertyName : String,
    specific : Boolean = false) : Try[(Option[PropertyType], Option[String])]

  def qualifierSet(qualifiers : Seq[SilWord]) =
    SprUtils.orderedSet(qualifiers.flatMap(_.decomposed).map(_.lemma))

  def deriveName(lemma : String) : String =
  {
    lemma.replaceAllLiterally(" ", "_")
  }

  def deriveName(word : SilWord) : String =
  {
    deriveName(word.toLemma)
  }

  // lemma -> inflected
  def getPropertyStateMap(property : PropertyType) : Map[String, String]

  def uniqueEntity(
    result : Try[Set[EntityType]]) : Try[EntityType] =
  {
    result.flatMap(set => {
      if (set.size == 1) {
        Success(set.head)
      } else {
        fail("unique entity expected")
      }
    })
  }

  def newParser(input : String) = SprParser(input, SprContext())
}

trait SmcNamedObject
{
  def name : String
}
