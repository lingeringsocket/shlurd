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
package com.lingeringsocket.shlurd

import com.lingeringsocket.shlurd.ilang._

import spire.math._
import spire.algebra._

import scala.collection._

package object mind
{
  type SmcTimeInterval = Interval[SmcTimePoint]

  implicit def orderForTimePoint[T <: SmcTimePoint] : Order[T] =
    SmcTimePointOrder.asInstanceOf[Order[T]]

  type SmcRefMap[EntityType <: SmcEntity] =
    Map[SilReference, Set[EntityType]]

  type SmcMutableRefMap[EntityType <: SmcEntity] =
    mutable.Map[SilReference, Set[EntityType]]

  object SmcRefMap
  {
    trait SmcRefMapFromAnnotation[EntityType <: SmcEntity]
        extends Map[SilReference, Set[EntityType]]
    {
      def getAnnotator : SilTypedAnnotator[SmcRefNote[EntityType]]

      override def get(ref : SilReference) : Option[Set[EntityType]] =
      {
        ref match {
          case annotatedRef : SilAnnotatedReference => {
            getAnnotator.getNote(annotatedRef).getEntities
          }
          case _ => None
        }
      }

      override def iterator : Iterator[(SilReference, Set[EntityType])] =
      {
        getAnnotator.getMap.iterator.filter {
          case (_, note) => {
            note.getEntities.nonEmpty
          }
        }.map {
          case (_, note) => {
            tupleN((note.ref, note.getEntities.get))
          }
        }
      }
    }

    def apply[EntityType <: SmcEntity](
      elems : (SilReference, Set[EntityType])*) =
    {
      Map[SilReference, Set[EntityType]](elems:_*)
    }

    def fromAnnotation[EntityType <: SmcEntity](
      annotator : SilTypedAnnotator[SmcRefNote[EntityType]]
    ) =
    {
      new DefaultMap[SilReference, Set[EntityType]] with
          SmcRefMapFromAnnotation[EntityType]
      {
        override def getAnnotator = annotator
      }
    }
  }

  object SmcMutableRefMap
  {
    import SmcRefMap._

    def newByValue[EntityType <: SmcEntity]() =
    {
      new mutable.LinkedHashMap[SilReference, Set[EntityType]]
    }

    def newByIdentity[EntityType <: SmcEntity]() =
    {
      new IdentityLinkedHashMap[SilReference, Set[EntityType]]
    }

    def fromAnnotation[EntityType <: SmcEntity](
      annotator : SilTypedAnnotator[SmcRefNote[EntityType]]
    ) =
    {
      new mutable.AbstractMap[SilReference, Set[EntityType]] with
          SmcRefMapFromAnnotation[EntityType]
      {
        override def getAnnotator = annotator
        override def +=(kv : (SilReference, Set[EntityType])) =
        {
          kv._1 match {
            case annotatedRef : SilAnnotatedReference => {
              annotator.getNote(annotatedRef).setEntities(kv._2)
              this
            }
            case _ => throw new IllegalArgumentException
          }
        }

        override def -=(key : SilReference) =
        {
          key match {
            case annotatedRef : SilAnnotatedReference => {
              annotator.getNote(annotatedRef).removeEntities
              this
            }
            case _ => throw new IllegalArgumentException
          }
        }
      }
    }
  }


}
