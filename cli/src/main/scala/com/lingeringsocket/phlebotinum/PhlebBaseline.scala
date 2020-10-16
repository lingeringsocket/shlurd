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

import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._
import com.lingeringsocket.shlurd.cli._

import scala.collection._

import java.io._

object PhlebBaseline
{
  private var frozen : Option[SpcCosmos] = None

  def frozenCosmos() : SpcCosmos =
  {
    this.synchronized {
      frozen.getOrElse {
        val file = new File("run/phlebotinum-baseline.zip")
        val serializer = new ShlurdCliSerializer
        val cosmos = {
          if (file.exists) {
            serializer.loadCosmos(file)
          } else {
            val newCosmos = new SpcCosmos(SpcGraph("phlebotinum-baseline"))
            newCosmos.getPool.enableBulkLoad
            newCosmos.copyFrom(ShlurdPrincetonPrimordial.frozenCosmos)

            val preferredSynonyms = new mutable.LinkedHashMap[SpcIdeal, String]
            val mind = new PhlebMind(
              newCosmos, None, preferredSynonyms, new PhlebClock)
            mind.importBeliefs(
              "/phlebotinum/default-axioms.txt",
              SpcResponder(
                mind,
                PhlebShell.beliefParams,
                SmcResponseParams(reportExceptionCodes = true)))

            // hmm we kind of just conveniently forget preferredSynonyms
            // here

            serializer.saveCosmos(newCosmos, file)
            newCosmos
          }
        }.asUnmodifiable
        frozen = Some(cosmos)
        cosmos
      }
    }
  }

  def newMutableCosmos() : SpcCosmos =
  {
    val frozen = frozenCosmos
    frozen.fork(true)
  }
}

