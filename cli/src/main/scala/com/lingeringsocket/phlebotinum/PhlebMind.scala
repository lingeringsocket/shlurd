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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.cli._

import scala.collection._

class PhlebMind(
  cosmos : SpcCosmos,
  val perception : Option[SpcPerception],
  val preferredSynonyms : mutable.Map[SpcIdeal, String]
) extends ShlurdCliMind(cosmos, preferredSynonyms)
{
  private var timestamp = SpcTimestamp.ZERO

  def getTimestamp() = timestamp

  def startNewTurn()
  {
    timestamp = timestamp.successor
  }

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new PhlebMind(
      newCosmos, perception, preferredSynonyms)
    mind.initFrom(this)
    mind
  }

  override def equivalentReferences(
    annotator : AnnotatorType,
    communicationContext : SmcCommunicationContext[SpcEntity],
    entity : SpcEntity,
    determiner : SilDeterminer)
      : Seq[SilReference] =
  {
    val references = super.equivalentReferences(
      annotator, communicationContext, entity, determiner)
    if (entity.form.name == PhlebShell.INVENTORY_WORD) {
      val (nouns, others) =
        references.partition(r =>
          r.isInstanceOf[SilNounReference] ||
            r.isInstanceOf[SilDeterminedReference]
        )
      // prefer "the player's stuff" over "the player-inventory"
      others ++ nouns
    } else {
      references
    }
  }

  override def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    val seq = super.resolveFormCandidates(noun)
    considerPreferredSynonym(seq, noun)
    seq
  }

  override def resolveForm(noun : SilWord) : Option[SpcForm] =
  {
    val opt = super.resolveForm(noun)
    considerPreferredSynonym(opt, noun)
    opt
  }

  override def resolveRole(
    form : SpcForm,
    noun : SilWord,
    includeHypernyms : Boolean = true) : Option[SpcRole] =
  {
    val opt = super.resolveRole(form, noun, includeHypernyms)
    considerPreferredSynonym(opt, noun)
    opt
  }

  private def considerPreferredSynonym(
    ideals : Iterable[SpcIdeal], noun : SilWord)
  {
    ideals.foreach(ideal => {
      if (!preferredSynonyms.contains(ideal)) {
        preferredSynonyms.put(ideal, noun.toNounLemma)
      }
    })
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
      synonyms.map(_.name.split(":").last).
        filterNot(isHyphenized).headOption.getOrElse(name)
    } else {
      name
    }
  }
}

