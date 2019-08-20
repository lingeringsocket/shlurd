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
    def apply[EntityType <: SmcEntity](
      elems : (SilReference, Set[EntityType])*) =
    {
      Map[SilReference, Set[EntityType]](elems:_*)
    }
  }

  object SmcMutableRefMap
  {
    def newByValue[EntityType <: SmcEntity]() =
    {
      new mutable.LinkedHashMap[SilReference, Set[EntityType]]
    }

    def newByIdentity[EntityType <: SmcEntity]() =
    {
      new IdentityLinkedHashMap[SilReference, Set[EntityType]]
    }
  }
}
