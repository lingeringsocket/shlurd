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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.nlang._

import scala.util._

object SpcPrimordial
{
  // FIXME can this ever be language-specific?
  private implicit val tongue = SnlUtils.defaultTongue

  // all the Whos down in Whoville
  private val synonyms = Map(
    PD_WHO.toLemma -> SmcIdeals.FORM_SOMEONE,
    PD_WHOM.toLemma -> SmcIdeals.FORM_SOMEONE,
    PD_WHAT.toLemma -> SpcMeta.ENTITY_METAFORM_NAME,
    PD_WHERE.toLemma -> (SmcIdeals.FORM_OBJECT + ":" + SmcIdeals.ROLE_CONTAINER)
  )

  private lazy val seedCosmos = initSeedCosmos

  private def initSeedCosmos =
  {
    val newCosmos = new SpcCosmos
    newCosmos.meta.enableBuffering()
    initCosmosFromBeliefs(newCosmos)
    newCosmos.meta.flush()
    newCosmos.clearWordLabeler()
    newCosmos
  }

  private def initCosmosFromBeliefs(cosmos : SpcCosmos) : Unit =
  {
    val mind = new SpcMind(cosmos)
    Using.resource(
      ResourceUtils.getResourceSource("/ontologies/primordial.txt")
    ) {
      source => mind.loadBeliefs(
        source,
        SpcResponder(
          mind,
          SpcBeliefParams(
            createTentativeIdeals = false,
            createTentativeEntities = false)))
    }
    synonyms.foreach(e => cosmos.addIdealSynonym(e._1, e._2))
  }

  def initCosmos(cosmos : SpcCosmos) : Unit =
  {
    cosmos.copyFrom(seedCosmos)
  }

  def isPrimordialSynonym(pair : (String, _)) : Boolean =
  {
    synonyms.contains(pair._1)
  }
}
