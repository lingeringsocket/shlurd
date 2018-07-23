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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import spire.math._

import scala.util._
import scala.collection._

sealed trait SilReferenceContext
case object REF_SUBJECT extends SilReferenceContext
case object REF_COMPLEMENT extends SilReferenceContext
case object REF_ADPOSITION_OBJ extends SilReferenceContext
case object REF_ADPOSITION_SUBJ extends SilReferenceContext

trait ShlurdCosmos[E<:ShlurdEntity, P<:ShlurdProperty]
{
  def fail(msg : String) = Failure(new RuntimeException(msg))

  def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[E]]

  def resolveEntityAssoc(
    entity : E,
    roleName : String) : Try[Set[E]] =
  {
    Failure(new UnsupportedOperationException())
  }

  def resolveProperty(
    entity : E,
    lemma : String) : Try[(P, String)]

  def evaluateEntityCategoryPredicate(
    entity : E,
    lemma : String,
    qualifiers : Set[String] = Set.empty) : Try[Trilean] =
  {
    resolveQualifiedNoun(lemma, REF_SUBJECT, qualifiers).map(
      set => Trilean(set.contains(entity)))
  }

  def evaluateEntityPropertyPredicate(
    entity : E,
    property : P,
    lemma : String) : Try[Trilean]

  def evaluateEntityAdpositionPredicate(
    entity : E,
    objEntity : E,
    adposition : SilAdposition,
    qualifiers : Set[String] = Set.empty) : Try[Trilean]

  def specificReference(
    entity : E,
    determiner : SilDeterminer) : SilReference

  def qualifierSet(qualifiers : Seq[SilWord]) =
    ShlurdParseUtils.orderedSet(qualifiers.map(_.lemma))

  // lemma -> inflected
  def getPropertyStateMap(property : P) : Map[String, String]

  def normalizeState(entity : E, state : SilState) : SilState = state

  def reifyRole(possessor : E, roleName : String, onlyIfProven : Boolean)
  {
  }
}

trait ShlurdNamedObject
{
  def name : String
}
