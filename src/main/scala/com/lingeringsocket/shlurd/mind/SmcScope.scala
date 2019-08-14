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

import scala.collection._
import scala.util._

case class SmcScopeOutput[
  EntityType<:SmcEntity
](
  prior : Option[SilReference],
  entities : Set[EntityType]
)

trait SmcScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
]{
  def getMind : MindType

  def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]]

  protected def findMatchingPronounReference(
    referenceMap : Map[SilReference, Set[EntityType]],
    reference : SilPronounReference) : Option[Set[EntityType]] =
  {
    referenceMap.values.find(set => {
      getMind.thirdPersonReference(set) == Some(reference)
    })
  }
}

class SmcMindScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  mind : MindType
) extends SmcScope[EntityType, PropertyType, CosmosType, MindType]
{
  override def getMind = mind

  override def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    reference : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    val entityOpt = {
      if (reference.count == COUNT_SINGULAR) {
        reference.person match {
          case PERSON_FIRST => communicationContext.speakerEntity
          case PERSON_SECOND => communicationContext.listenerEntity
          case _ => None
        }
      } else {
        None
      }
    }
    val entitiesOpt = entityOpt.map(entity => Set(entity)).orElse {
      if (reference.distance != DISTANCE_UNSPECIFIED) {
        // FIXME proper resolution for this/that
        Some(Set.empty[EntityType])
      } else {
        // FIXME proper coreference resolution, including current
        // sentence; also, there should probably be some limit on how
        // far back to search.
        if (mind.isConversing) {
          mind.getConversation.getUtterances.reverseIterator.drop(1).flatMap(
            utterance => {
              findMatchingPronounReference(
                utterance.referenceMap, reference
              )
            }
          ).find(_ => true)
        } else {
          None
        }
      }
    }
    entitiesOpt match {
      case Some(entities) => {
        Success(SmcScopeOutput(None, entities))
      }
      case _ => {
        mind.getCosmos.fail("pronoun cannot be resolved")
      }
    }
  }
}

class SmcPhraseScope[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  referenceMap : Map[SilReference, Set[EntityType]],
  parent : SmcScope[EntityType, PropertyType, CosmosType, MindType]
) extends SmcScope[EntityType, PropertyType, CosmosType, MindType]
{
  override def getMind = parent.getMind

  override def resolvePronoun(
    communicationContext : SmcCommunicationContext[EntityType],
    ref : SilPronounReference
  ) : Try[SmcScopeOutput[EntityType]] =
  {
    val entitiesOpt = ref match {
      case SilPronounReference(PERSON_THIRD, _, _, DISTANCE_UNSPECIFIED) => {
        findMatchingPronounReference(referenceMap, ref)
      }
      case _ => None
    }
    entitiesOpt match {
      case Some(entities) => {
        Success(SmcScopeOutput(None, entities))
      }
      case _ => {
        parent.resolvePronoun(communicationContext, ref)
      }
    }
  }
}
