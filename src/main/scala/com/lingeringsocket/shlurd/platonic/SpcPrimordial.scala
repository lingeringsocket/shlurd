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

object SpcPrimordial
{
  // FIXME can this ever be language-specific?
  private implicit val tongue = SprContext.defaultTongue

  // all the Whos down in Whoville
  private val synonyms = Map(
    MW_WHO.toLemma -> SmcIdeals.FORM_SOMEONE,
    MW_WHOM.toLemma -> SmcIdeals.FORM_SOMEONE,
    MW_WHAT.toLemma -> SpcMeta.ENTITY_METAFORM_NAME,
    MW_WHERE.toLemma -> (SmcIdeals.FORM_OBJECT + ":" + SmcIdeals.ROLE_CONTAINER)
  )

  private lazy val seedCosmos = initSeedCosmos()

  private def initSeedCosmos() =
  {
    val newCosmos = new SpcCosmos
    newCosmos.meta.enableBuffering
    initCosmosFromBeliefs(newCosmos)
    newCosmos.meta.flush
    newCosmos
  }

  private def initCosmosFromBeliefs(cosmos : SpcCosmos)
  {
    val mind = new SpcMind(cosmos)
    mind.loadBeliefs(
      ResourceUtils.getResourceSource(
        "/ontologies/primordial.txt"),
      new SpcResponder(
        mind,
        SpcBeliefParams(
          createTentativeIdeals = false,
          createTentativeEntities = false))
    )
    synonyms.foreach(e => cosmos.addIdealSynonym(e._1, e._2))
  }

  def initCosmos(cosmos : SpcCosmos)
  {
    cosmos.copyFrom(seedCosmos)
  }

  def isPrimordialSynonym(pair : (String, _)) : Boolean =
  {
    synonyms.contains(pair._1)
  }
}
