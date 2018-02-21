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

import scala.util._
import scala.collection._

import spire.math._

abstract class ShlurdOpenhabWorld extends ShlurdPlatonicWorld
{
  private val locationFormName = "location"

  private val roomLemma = "room"

  private val groupMap = new mutable.HashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String]

  instantiateForm(ShlurdWord(locationFormName, locationFormName))

  override def resolveEntity(
    lemma : String,
    context : ShlurdReferenceContext,
    qualifiers : Set[String]) : Try[Set[ShlurdPlatonicEntity]] =
  {
    val rewrittenLemma = {
      if (lemma == roomLemma) {
        Set.empty[String]
      } else {
        Set(lemma)
      }
    }
    val rewrittenQualifiers = ((qualifiers - roomLemma) ++ rewrittenLemma)
    context match {
      case REF_LOCATION => {
        val result = super.resolveEntity(
          locationFormName, context, rewrittenQualifiers)
        if (result.isFailure) {
          result
        } else {
          if (result.get.isEmpty) {
            val any = super.resolveEntity(
              locationFormName, context, rewrittenLemma)
            if (any.isFailure) {
              result
            } else {
              if (any.get.isEmpty) {
                fail(s"unknown entity $lemma")
              } else {
                result
              }
            }
          } else {
            result
          }
        }
      }
      case _ => {
        val result = super.resolveEntity(
          lemma, context, (qualifiers - roomLemma))
        if (result.isFailure) {
          val any = super.resolveEntity(
            locationFormName, context, rewrittenLemma)
          if (any.isFailure) {
            result
          } else {
            if (any.get.isEmpty) {
              result
            } else {
              super.resolveEntity(
                locationFormName, context, rewrittenQualifiers)
            }
          }
        } else {
          result
        }
      }
    }
  }

  private def isAmbiguous(entity : ShlurdPlatonicEntity) : Boolean =
  {
    resolveEntity(entity.form.name, REF_SUBJECT, entity.qualifiers) match {
      case Success(set) => {
        set.size > 1
      }
      case _ => false
    }
  }

  private def getContainer(entity : ShlurdPlatonicEntity)
      : Option[ShlurdPlatonicEntity] =
  {
    groupMap.get(entity.name) match {
      case Some(groupNames) => {
        if (!groupNames.isEmpty) {
          getEntities.get(groupNames.head) match {
            case Some(groupEntity) => {
              Some(groupEntity)
            }
            case _ => None
          }
        } else {
          None
        }
      }
      case _ => None
    }
  }

  override def specificReference(
    entity : ShlurdPlatonicEntity,
    determiner : ShlurdDeterminer) =
  {
    if (entity.form.name == locationFormName) {
      val seq = entity.qualifiers.toSeq
      val realForm = seq.last
      val ref = ShlurdEntityReference(
        ShlurdWord(realForm, realForm),
        determiner)
      if (seq.size == 1) {
        ref
      } else {
        ShlurdReference.qualified(
          ref, seq.dropRight(1).map(x => ShlurdWord(x, x)))
      }
    } else {
      val ref = super.specificReference(entity, determiner)
      if (isAmbiguous(entity)) {
        getContainer(entity) match {
          case Some(containerEntity) => {
            getContainer(containerEntity) match {
              case Some(floorEntity) => {
                val floorRef = specificReference(floorEntity, DETERMINER_UNIQUE)
                ShlurdStateSpecifiedReference(
                  ref,
                  ShlurdLocationState(
                    LOC_ON,
                    floorRef))
              }
              case _ => ref
            }
          }
          case _ => ref
        }
      } else {
        ref
      }
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) : Try[Trilean] =
  {
    evaluateState(entity, entity.form.getStateSynonyms.resolveSynonym(lemma))
  }

  protected def evaluateState(
    entity : ShlurdPlatonicEntity, stateName : String) : Try[Trilean]

  override def evaluateEntityLocationPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    locative : ShlurdLocative) : Try[Trilean] =
  {
    Success(Trilean(evaluateLocationPredicate(entity, location, locative)))
  }

  def evaluateLocationPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    locative : ShlurdLocative) : Boolean =
  {
    groupMap.get(entity.name) match {
      case Some(groupNames) => {
        groupNames.contains(location.name) ||
          groupNames.exists(groupName =>
            getEntities.get(groupName).map(
              evaluateLocationPredicate(
                _, location, locative)).getOrElse(false))
      }
      case _ => false
    }
  }

  private def extractQualifiersFromLabel(label : String) =
  {
    label.split(" ").map(_.toLowerCase).filterNot(_ == roomLemma)
  }

  def addItem(
    itemName : String,
    itemLabel : String,
    isGroup : Boolean,
    itemGroupNames : Set[String])
  {
    val qualifiers = new mutable.LinkedHashSet[String]
    var trimmed = itemName
    itemGroupNames.foreach(groupName => {
      if (trimmed != groupName) {
        trimmed = trimmed.replaceAllLiterally(groupName, "")
      }
      if (groupName.startsWith("g") && (groupName.size > 1)
        && (groupName.drop(1).forall(_.isUpper)))
      {
        trimmed = trimmed.replaceAllLiterally(groupName.stripPrefix("g"), "")
      }
      getEntities.get(groupName) match {
        case Some(groupEntity) => {
          groupMap.addBinding(itemName, groupName)
          if (!isGroup) {
            qualifiers ++= groupEntity.qualifiers
          }
        }
        case _ =>
      }
      trimmed = trimmed.stripPrefix("_").stripSuffix("_")
    })
    if (trimmed.contains('_')) {
      trimmed = trimmed.replaceAllLiterally(itemLabel, "")
      trimmed = trimmed.stripPrefix("_").stripSuffix("_")
      trimmed = trimmed.replaceAllLiterally("_", "")
    }
    var formName = locationFormName
    if (isGroup) {
      //
    } else {
      formName = trimmed.toLowerCase
    }
    qualifiers ++= extractQualifiersFromLabel(itemLabel).
      filterNot(_ == formName)

    getForms.get(formName) match {
      case Some(form) => {
        val entity = new ShlurdPlatonicEntity(itemName, form, qualifiers)
        addEntity(entity)
      }
      case _ =>
    }
  }
}
