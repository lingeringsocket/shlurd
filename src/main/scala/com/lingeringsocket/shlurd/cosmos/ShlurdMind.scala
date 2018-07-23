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

import scala.collection._
import scala.util._

class ShlurdMind[E<:ShlurdEntity, P<:ShlurdProperty](
  cosmos : ShlurdCosmos[E,P])
{
  private lazy val personFirst =
    uniqueEntity(resolvePronoun(PERSON_FIRST, GENDER_N, COUNT_SINGULAR))

  private lazy val personSecond =
    uniqueEntity(resolvePronoun(PERSON_SECOND, GENDER_N, COUNT_SINGULAR))

  def getCosmos = cosmos

  def resolvePronoun(
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    distance : SilDistance = DISTANCE_UNSPECIFIED) : Try[Set[E]] =
  {
    cosmos.fail("pronouns not supported")
  }

  def equivalentReferences(
    entity : E,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    pronounReference(entity, personFirst, PERSON_FIRST) ++
    pronounReference(entity, personSecond, PERSON_SECOND) ++
    Seq(cosmos.specificReference(entity, determiner))
  }

  def thirdPersonReference(entities : Set[E]) : Option[SilReference] =
  {
    None
  }

  private def pronounReference(
    entity : E, pronounEntity : Try[E],
    person : SilPerson)
      : Seq[SilReference] =
  {
    pronounEntity match {
      case Success(x) if (x == entity) => {
        Seq(SilPronounReference(person, GENDER_N, COUNT_SINGULAR))
      }
      case _ => Seq()
    }
  }

  protected def uniqueEntity(result : Try[Set[E]]) : Try[E] =
  {
    result.flatMap(set => {
      if (set.size == 1) {
        Success(set.head)
      } else {
        cosmos.fail("unique entity expected")
      }
    })
  }
}
