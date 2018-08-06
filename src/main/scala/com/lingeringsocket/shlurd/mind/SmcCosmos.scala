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

import com.lingeringsocket.shlurd.parser._

import spire.math._

import scala.util._
import scala.collection._

sealed trait SilReferenceContext
case object REF_SUBJECT extends SilReferenceContext
case object REF_COMPLEMENT extends SilReferenceContext
case object REF_ADPOSITION_OBJ extends SilReferenceContext
case object REF_ADPOSITION_SUBJ extends SilReferenceContext

trait SmcCosmos[EntityType<:SmcEntity, PropertyType<:SmcProperty]
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[EntityType]]

  def resolveEntityAssoc(
    entity : EntityType,
    roleName : String) : Try[Set[EntityType]] =
  {
    Failure(new UnsupportedOperationException())
  }

  def resolveProperty(
    entity : EntityType,
    lemma : String) : Try[(PropertyType, String)]

  def evaluateEntityCategoryPredicate(
    entity : EntityType,
    lemma : String,
    qualifiers : Set[String] = Set.empty) : Try[Trilean] =
  {
    resolveQualifiedNoun(lemma, REF_SUBJECT, qualifiers).map(
      set => Trilean(set.contains(entity)))
  }

  def evaluateEntityPropertyPredicate(
    entity : EntityType,
    property : PropertyType,
    lemma : String) : Try[Trilean]

  def evaluateEntityAdpositionPredicate(
    entity : EntityType,
    objEntity : EntityType,
    adposition : SilAdposition,
    qualifiers : Set[String] = Set.empty) : Try[Trilean]

  def specificReference(
    entity : EntityType,
    determiner : SilDeterminer) : SilReference

  def specificReferences(
    entities : Set[EntityType],
    determiner : SilDeterminer) : SilReference =
  {
    assert(!entities.isEmpty)
    if (entities.size == 1) {
      specificReference(entities.head, DETERMINER_UNIQUE)
    } else {
      SilConjunctiveReference(
        DETERMINER_ALL,
        entities.toSeq.map(entity =>
          specificReference(
            entity, DETERMINER_UNIQUE)))
    }
  }

  def qualifierSet(qualifiers : Seq[SilWord]) =
    SprUtils.orderedSet(qualifiers.map(_.lemma))

  // lemma -> inflected
  def getPropertyStateMap(property : PropertyType) : Map[String, String]

  def normalizeState(entity : EntityType, state : SilState) : SilState = state

  def reifyRole(
    possessor : EntityType, roleName : String, onlyIfProven : Boolean)
  {
  }
}

trait SmcNamedObject
{
  def name : String
}
