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

import spire.math._

import scala.util._
import scala.collection._

sealed trait ShlurdReferenceContext
case object REF_SUBJECT extends ShlurdReferenceContext
case object REF_COMPLEMENT extends ShlurdReferenceContext
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

  def resolveQualifiedNoun(
    lemma : String,
    context : ShlurdReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[E]]

  def resolvePronoun(
    person : ShlurdPerson,
    gender : ShlurdGender,
    count : ShlurdCount) : Try[Set[E]]

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

  def evaluateEntityLocationPredicate(
    entity : E,
    location : E,
    locative : ShlurdLocative,
    qualifiers : Set[String] = Set.empty) : Try[Trilean]

  def specificReference(
    entity : E,
    determiner : ShlurdDeterminer) : ShlurdReference

  def qualifierSet(qualifiers : Seq[ShlurdWord]) =
    ShlurdParseUtils.orderedSet(qualifiers.map(_.lemma))

  def normalizeState(entity : E, state : ShlurdState) : ShlurdState = state
}

trait ShlurdNamedObject
{
  def name : String
}

class ShlurdSynonymMap
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

  def getAll = map
}
