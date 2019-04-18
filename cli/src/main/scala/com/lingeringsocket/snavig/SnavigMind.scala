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
package com.lingeringsocket.snavig

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.cli._

import scala.collection._

class SnavigMind(
  cosmos : SpcCosmos,
  val entityFirst : SpcEntity,
  val entitySecond : SpcEntity,
  val perception : Option[SpcPerception]
) extends ShlurdCliMind(cosmos, entityFirst, entitySecond)
{
  private var timestamp = SpcTimestamp.ZERO

  def getTimestamp() = timestamp

  def startNewTurn()
  {
    timestamp = timestamp.successor
  }

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SnavigMind(
      newCosmos, entityFirst, entitySecond, perception)
    mind.initFrom(this)
    mind
  }

  override def equivalentReferences(
    entity : SpcEntity,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    val references = super.equivalentReferences(entity, determiner)
    if (entity.form.name == SnavigShell.INVENTORY_WORD) {
      val (nouns, others) =
        references.partition(_.isInstanceOf[SilNounReference])
      // prefer "the player's stuff" over "the player-inventory"
      others ++ nouns
    } else {
      references
    }
  }

  override protected def getFormName(form : SpcForm) : String =
  {
    synonymize(form, super.getFormName(form))
  }

  override protected def getPossesseeName(role : SpcRole) : String =
  {
    synonymize(role, super.getPossesseeName(role))
  }

  private def synonymize(ideal : SpcIdeal, name : String) : String =
  {
    def isHyphenized(s : String) = s.contains('-')
    if (isHyphenized(name)) {
      val synonyms = cosmos.getSynonymsForIdeal(ideal)
      synonyms.map(_.name).filterNot(isHyphenized).headOption.getOrElse(name)
    } else {
      name
    }
  }
}

