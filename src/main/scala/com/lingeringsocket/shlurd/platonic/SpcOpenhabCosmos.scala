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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.jdk.CollectionConverters._
import scala.util._
import scala.io._

import spire.math._

object SpcOpenhabCosmos
{
  type GroupMap = mutable.MultiDict[String, String]

  val locationFormName = "location"

  val presenceFormName = "presence"

  val presenceRoleName = "ubiety"

  val roomLemma = "room"
}
import SpcOpenhabCosmos._

abstract class SpcOpenhabCosmos(
  graph : SpcGraph = SpcGraph(),
  val groupMap : GroupMap = mutable.MultiDict(),
  val roomyRooms : mutable.Set[String] = new mutable.LinkedHashSet[String],
  forkLevel : Int = 0,
  pool : SpcCosmicPool = new SpcCosmicPool
) extends SpcCosmos(graph, forkLevel, pool)
{
  import SpcOpenhabCosmos._

  private[platonic] var beliefsLoaded : Boolean = false

  if (forkLevel == 0) {
    SpcPrimordial.initCosmos(this)
    instantiateForm(SilWord(locationFormName))
    instantiateForm(SilWord(presenceFormName))
  }

  override def fork(detached : Boolean = false) : SpcOpenhabCosmos =
  {
    assert(!detached)
    val forkedGraph = {
      if (forkLevel > 0) {
        graph
      } else {
        SpcGraph.fork(graph)
      }
    }
    val forked = new SpcOpenhabDerivedCosmos(this, forkedGraph, forkLevel + 1)
    forked.meta.afterFork(meta)
    forked
  }

  override def asUnmodifiable : SpcOpenhabCosmos =
  {
    val frozen = new SpcOpenhabDerivedCosmos(this, getGraph, forkLevel)
    frozen.meta.afterFork(meta)
    frozen
  }

  override def resolveQualifiedNoun(
    lemmaOrig : String,
    context : SilReferenceContext,
    qualifiersOrig : Set[String]) : Try[Set[SpcEntity]] =
  {
    val (lemma, qualifiers) = {
      val split = lemmaOrig.split(" ")
      if (split.size == 1) {
        tupleN(lemmaOrig, qualifiersOrig)
      } else {
        tupleN(split.last, qualifiersOrig ++ split.dropRight(1))
      }
    }
    val rewrittenLemma = {
      if (lemma == roomLemma) {
        Set.empty[String]
      } else {
        Set(lemma)
      }
    }
    val rewrittenQualifiers = ((qualifiers.toSet - roomLemma) ++ rewrittenLemma)
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
          lemma, context, (qualifiers.toSet - roomLemma))
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

  def isAmbiguous(entity : SpcEntity) : Boolean =
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

  def getContainer(entity : SpcEntity)
      : Option[SpcEntity] =
  {
    groupMap.get(entity.name) match {
      case groupNames : Set[String] => {
        if (!groupNames.isEmpty) {
          getEntityBySynonym(groupNames.head) match {
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

  def getRoomyQualifiers(entity : SpcEntity) : Seq[String] =
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

  def evaluateAdpositionPredicate(
    entity : SpcEntity,
    location : SpcEntity,
    adposition : SilAdposition) : Boolean =
  {
    groupMap.get(entity.name) match {
      case groupNames : Set[String] => {
        groupNames.contains(location.name) ||
          groupNames.exists(groupName =>
            getEntityBySynonym(groupName).map(
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
    itemGroupNames : Iterable[String]) : Unit =
  {
    val qualifiers = new mutable.LinkedHashSet[String]
    var trimmed = itemName
    itemGroupNames.foreach(groupName => {
      if (trimmed != groupName) {
        trimmed = trimmed.replace(groupName, "")
      }
      if (groupName.startsWith("g") && (groupName.size > 1)
        && (groupName.drop(1).forall(_.isUpper)))
      {
        trimmed = trimmed.replace(groupName.stripPrefix("g"), "")
      }
      getEntityBySynonym(groupName).foreach(groupEntity => {
        groupMap += tupleN(itemName, groupName)
        if (!isGroup) {
          qualifiers ++= groupEntity.qualifiers
        }
      })
      trimmed = trimmed.stripPrefix("_").stripSuffix("_")
    })
    if (trimmed.contains('_')) {
      trimmed = trimmed.replace(itemLabel, "")
      trimmed = trimmed.stripPrefix("_").stripSuffix("_")
      trimmed = trimmed.replace("_", "")
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

    resolveForm(formName).foreach(form => {
      // for now we silently ignore mismatches...should probably
      // save up as warnings which can be nagged about
      var warning = false
      if (formName == presenceFormName) {
        val entity = SpcPersistentEntity(itemName, form, Set.empty)
        createOrReplaceEntity(entity)
        qualifiers.lastOption match {
          case Some(personName) => {
            getGraph.formAssocs.edgeSet.asScala.find(
              edge => (edge.getRoleName == presenceRoleName)
            ) match {
              case Some(edge) => {
                val personForm = getGraph.getPossessorForm(edge)
                resolveQualifiedNoun(
                  personForm.name, REF_SUBJECT,
                  qualifiers.takeRight(1)) match
                {
                  case Success(set) => {
                    if (set.size == 1) {
                      addEntityAssoc(
                        set.head, entity,
                        resolveRole(personForm, presenceRoleName).get)
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
      } else {
        val entity = SpcPersistentEntity(itemName, form, qualifiers)
        createOrReplaceEntity(entity)
      }
    })
  }
}

class SpcOpenhabDerivedCosmos(
  base : SpcOpenhabCosmos, graph : SpcGraph, forkLevel : Int)
    extends SpcOpenhabCosmos(
  graph, base.groupMap, base.roomyRooms,
      forkLevel, base.getPool
  )
{
  override def evaluateEntityProperty(
    entity : SpcEntity,
    propertyName : String,
    specific : Boolean = false) : Try[(Option[SpcProperty], Option[String])] =
  {
    base.evaluateEntityProperty(entity, propertyName, specific)
  }
}

abstract class SpcOpenhabDefaultCosmos extends SpcOpenhabCosmos
{
}

class SpcOpenhabMind(cosmos : SpcOpenhabCosmos)
    extends SpcMind(cosmos)
{
  private implicit val tongue = getTongue

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcOpenhabMind(newCosmos.asInstanceOf[SpcOpenhabCosmos])
    mind.initFrom(this)
    mind
  }

  override def loadBeliefs(source : Source, responder : SpcResponder) : Unit =
  {
    super.loadBeliefs(source, responder)
    cosmos.beliefsLoaded = true
  }

  override def evaluateEntityAdpositionPredicate(
    entity : SpcEntity,
    location : SpcEntity,
    adposition : SilAdposition,
    qualifiers : Set[SilWord]) : Try[Trilean] =
  {
    if (adposition == SprPredefAdposition(PD_GENITIVE_OF)) {
      super.evaluateEntityAdpositionPredicate(
        entity, location, adposition, qualifiers)
    } else {
      assert(qualifiers.isEmpty)
      Success(Trilean(
        cosmos.evaluateAdpositionPredicate(entity, location, adposition)))
    }
  }

  override def specificReference(
    annotator : AnnotatorType,
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    if (entity.form.name == SpcOpenhabCosmos.locationFormName) {
      val seq = entity.qualifiers.toSeq
      val specialRoom = cosmos.roomyRooms.contains(entity.name)
      val realForm = {
        if (specialRoom) {
          SpcOpenhabCosmos.roomLemma
        } else {
          seq.last
        }
      }
      val ref = annotator.determinedNounRef(
        SilWord(realForm),
        determiner)
      if (specialRoom) {
        annotator.qualifiedRef(
          ref, seq.map(x => SilWord(x)))
      } else {
        if (seq.size == 1) {
          ref
        } else {
          annotator.qualifiedRef(
            ref, seq.dropRight(1).map(x => SilWord(x)))
        }
      }
    } else {
      val roomyEntity = {
        val roomyQualifiers = cosmos.getRoomyQualifiers(entity)
        if (roomyQualifiers.isEmpty) {
          entity
        } else {
          val seq = entity.qualifiers.toSeq
          val i = seq.indexOfSlice(roomyQualifiers)
          if (i == -1) {
            entity
          } else {
            SpcPersistentEntity(
              entity.name, entity.form,
              SprUtils.orderedSet(
                seq.patch(i + roomyQualifiers.size,
                  Seq(SpcOpenhabCosmos.roomLemma), 0)))
          }
        }
      }
      val ref = super.specificReference(annotator, roomyEntity, determiner)
      if (cosmos.isAmbiguous(entity)) {
        cosmos.getContainer(entity) match {
          case Some(containerEntity) => {
            cosmos.getContainer(containerEntity) match {
              case Some(floorEntity) => {
                val floorRef =
                  specificReference(annotator, floorEntity, DETERMINER_DEFINITE)
                annotator.stateSpecifiedRef(
                  ref,
                  SilAdpositionalState(
                    SprPredefAdposition(PD_ON),
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
}
