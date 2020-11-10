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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

class IndirectEntities[EntityType<:SmcEntity](
  var entities : Option[Set[EntityType]] = None
)
{
  override def toString = entities.toString
}

class SmcRefNote[EntityType<:SmcEntity](
  ref : SilAnnotatedReference
) extends SilBasicRefNote(ref)
{
  private var indirectEntities : Option[IndirectEntities[EntityType]] = None

  private var isAnswer : Boolean = false

  def isQueryAnswer = isAnswer

  def markQueryAnswer() : Unit =
  {
    isAnswer = true
  }

  def getEntities : Option[Set[EntityType]] =
    indirectEntities.flatMap(_.entities)

  def setEntities(newEntities : Set[EntityType]) : Unit =
  {
    indirectEntities match {
      case Some(ie) => {
        ie.entities = Some(newEntities)
      }
      case _ => {
        indirectEntities = Some(new IndirectEntities(Some(newEntities)))
      }
    }
  }

  def removeEntities() : Unit =
  {
    indirectEntities.foreach(ie => {
      ie.entities = None
    })
  }

  def unifyReferences(otherNote : SmcRefNote[EntityType]) : Unit =
  {
    otherNote.indirectEntities match {
      case Some(ie) if (!ie.entities.isDefined) => {
        otherNote.indirectEntities = indirectEntities
      }
      case Some(ie) => {
        assert(
          indirectEntities.isEmpty ||
            indirectEntities.get.entities.isEmpty ||
            ie.entities == indirectEntities.get.entities,
          tupleN(indirectEntities, otherNote.indirectEntities)
        )
        indirectEntities = Some(ie)
      }
      case _ => {
        val ie = indirectEntities match {
          case Some(old) => {
            old
          }
          case _ => {
            new IndirectEntities[EntityType]
          }
        }
        indirectEntities = Some(ie)
        otherNote.indirectEntities = Some(ie)
      }
    }
  }

  override def mergeFrom(
    oldNote : SilAbstractRefNote) : Unit =
  {
    super.mergeFrom(oldNote)
    oldNote matchPartial {
      case smc : SmcRefNote[EntityType] => {
        if (!indirectEntities.isDefined) {
          indirectEntities = smc.indirectEntities
        }
        if (!isAnswer) {
          isAnswer = smc.isAnswer
        }
      }
    }
  }
}

class SmcAnnotator[EntityType <: SmcEntity, NoteType <: SmcRefNote[EntityType]](
  noteSupplier : (SilAnnotatedReference) => NoteType
) extends SilTypedAnnotator[NoteType](noteSupplier)
{
}

object SmcAnnotator
{
  def apply[EntityType<:SmcEntity]() =
  {
    new SmcAnnotator[EntityType, SmcRefNote[EntityType]](
      (ref) => new SmcRefNote(ref))
  }

  def apply[EntityType<:SmcEntity](annotator : SilAnnotator) =
    annotator.asInstanceOf[SmcAnnotator[EntityType, SmcRefNote[EntityType]]]

  def unifyReferences[EntityType<:SmcEntity, NoteType<:SmcRefNote[EntityType]](
    annotator : SmcAnnotator[EntityType, NoteType],
    ref1 : SilAnnotatedReference,
    ref2 : SilAnnotatedReference
  ) : Unit =
  {
    if ((ref1.getAnnotator == annotator) && (ref2.getAnnotator == annotator)) {
      val note1 = annotator.getNote(ref1)
      val note2 = annotator.getNote(ref2)
      note1.unifyReferences(note2)
    }
  }
}
