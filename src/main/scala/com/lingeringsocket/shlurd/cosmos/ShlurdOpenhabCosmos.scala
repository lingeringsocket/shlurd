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

import scala.util._
import scala.collection._

import spire.math._

abstract class ShlurdOpenhabCosmos extends ShlurdPlatonicCosmos
{
  private val locationFormName = "location"

  private val presenceFormName = "presence"

  private val roomLemma = "room"

  private val groupMap = new mutable.LinkedHashMap[String, mutable.Set[String]]
      with mutable.MultiMap[String, String] {
    override protected def makeSet = new mutable.LinkedHashSet[String]
  }

  private val roomyRooms = new mutable.LinkedHashSet[String]

  instantiateForm(SilWord(locationFormName))
  instantiateForm(SilWord(presenceFormName))

  override def resolveQualifiedNoun(
    lemma : String,
    context : SilReferenceContext,
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
      case REF_ADPOSITION_OBJ => {
        val result = super.resolveQualifiedNoun(
          locationFormName, context, rewrittenQualifiers)
        if (result.isFailure) {
          result
        } else {
          if (result.get.isEmpty) {
            val any = super.resolveQualifiedNoun(
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
        val result = super.resolveQualifiedNoun(
          lemma, context, (qualifiers - roomLemma))
        if (result.isFailure) {
          val any = super.resolveQualifiedNoun(
            locationFormName, context, rewrittenLemma)
          if (any.isFailure) {
            result
          } else {
            if (any.get.isEmpty) {
              result
            } else {
              super.resolveQualifiedNoun(
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
    resolveQualifiedNoun(
      entity.form.name, REF_SUBJECT, entity.qualifiers) match
    {
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
    determiner : SilDeterminer) =
  {
    if (entity.form.name == locationFormName) {
      val seq = entity.qualifiers.toSeq
      val specialRoom = roomyRooms.contains(entity.name)
      val realForm = {
        if (specialRoom) {
          roomLemma
        } else {
          seq.last
        }
      }
      val ref = SilNounReference(
        SilWord(realForm),
        determiner)
      if (specialRoom) {
          SilReference.qualified(
            ref, seq.map(x => SilWord(x)))
      } else {
        if (seq.size == 1) {
          ref
        } else {
          SilReference.qualified(
            ref, seq.dropRight(1).map(x => SilWord(x)))
        }
      }
    } else {
      val roomyEntity = {
        val roomyQualifiers = getRoomyQualifiers(entity)
        if (roomyQualifiers.isEmpty) {
          entity
        } else {
          val seq = entity.qualifiers.toSeq
          val i = seq.indexOfSlice(roomyQualifiers)
          if (i == -1) {
            entity
          } else {
            new ShlurdPlatonicEntity(
              entity.name, entity.form,
              ShlurdParseUtils.orderedSet(
                seq.patch(i + roomyQualifiers.size, Seq(roomLemma), 0)))
          }
        }
      }
      val ref = super.specificReference(roomyEntity, determiner)
      if (isAmbiguous(entity)) {
        getContainer(entity) match {
          case Some(containerEntity) => {
            getContainer(containerEntity) match {
              case Some(floorEntity) => {
                val floorRef =
                  specificReference(floorEntity, DETERMINER_UNIQUE)
                SilStateSpecifiedReference(
                  ref,
                  SilAdpositionalState(
                    ADP_ON,
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

  private def getRoomyQualifiers(entity : ShlurdPlatonicEntity) : Seq[String] =
  {
    getContainer(entity) match {
      case Some(containerEntity) => {
        if (roomyRooms.contains(containerEntity.name)) {
          Seq(containerEntity.qualifiers.last)
        } else {
          Seq.empty
        }
      }
      case _ => Seq.empty
    }
  }

  override def evaluateEntityPropertyPredicate(
    entity : ShlurdPlatonicEntity,
    property : ShlurdPlatonicProperty,
    lemma : String) : Try[Trilean] =
  {
    evaluateState(entity, lemma) match {
      case Success(Trilean.Unknown) => {
        super.evaluateEntityPropertyPredicate(entity, property, lemma)
      }
      case x => x
    }
  }

  protected def evaluateState(
    entity : ShlurdPlatonicEntity, stateName : String) : Try[Trilean]

  override def evaluateEntityAdpositionPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    adposition : SilAdposition,
    qualifiers : Set[String]) : Try[Trilean] =
  {
    if (adposition == ADP_GENITIVE_OF) {
      super.evaluateEntityAdpositionPredicate(
        entity, location, adposition, qualifiers)
    } else {
      assert(qualifiers.isEmpty)
      Success(Trilean(
        evaluateAdpositionPredicate(entity, location, adposition)))
    }
  }

  def evaluateAdpositionPredicate(
    entity : ShlurdPlatonicEntity,
    location : ShlurdPlatonicEntity,
    adposition : SilAdposition) : Boolean =
  {
    groupMap.get(entity.name) match {
      case Some(groupNames) => {
        groupNames.contains(location.name) ||
          groupNames.exists(groupName =>
            getEntities.get(groupName).map(
              evaluateAdpositionPredicate(
                _, location, adposition)).getOrElse(false))
      }
      case _ => false
    }
  }

  def addItem(
    itemName : String,
    itemLabel : String,
    isGroup : Boolean,
    itemGroupNames : Iterable[String])
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
    val formName = {
      if (isGroup) {
        locationFormName
      } else {
        trimmed.toLowerCase
      }
    }
    val labelComponents = itemLabel.split(" ").map(_.toLowerCase)
    if (labelComponents.contains(roomLemma)) {
      roomyRooms += itemName
    }
    val lastQualifier = {
      if (qualifiers.isEmpty) {
        ""
      } else {
        qualifiers.last
      }
    }
    qualifiers ++= labelComponents.filterNot(
      s => (s == roomLemma) || (s == formName) ||
        ((s + roomLemma) == lastQualifier))

    getForms.get(formName) match {
      case Some(form) => {
        val entity = new ShlurdPlatonicEntity(itemName, form, qualifiers)
        addEntity(entity)
        // for now we silently ignore mismatches...should probably
        // save up as warnings which can be nagged about
        var warning = false
        if (formName == presenceFormName) {
          qualifiers.lastOption match {
            case Some(personName) => {
              getPropertyEdges.find(_.label == presenceFormName) match {
                case Some(edge) => {
                  val personForm = getPossessorForm(edge)
                  resolveQualifiedNoun(
                    personForm.name, REF_SUBJECT, qualifiers.takeRight(1)) match
                  {
                    case Success(set) => {
                      if (set.size == 1) {
                        addEntityAssoc(
                          set.head, entity, presenceFormName)
                      } else {
                        warning = true
                      }
                    }
                    case _ => warning = true
                  }
                }
                case _ => warning = true
              }
            }
            case _ => warning = true
          }
        }
      }
      case _ =>
    }
  }
}
