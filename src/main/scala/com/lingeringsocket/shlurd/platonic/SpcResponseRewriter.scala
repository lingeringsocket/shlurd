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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.cosmos._

import scala.collection._
import scala.collection.JavaConverters._

class SpcResponseRewriter(cosmos : SpcCosmos)
    extends ShlurdResponseRewriter(cosmos)
{
  override protected def equivalentReferences(
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    val buf = new mutable.ArrayBuffer[SilReference]
    buf += cosmos.specificReference(entity, determiner)
    cosmos.getEntityAssocGraph.incomingEdgesOf(entity).asScala.foreach(
      edge => {
        val possessor = cosmos.getGraph.getPossessorEntity(edge)
        buf += SilGenitiveReference(
          cosmos.specificReference(possessor, determiner),
          SilNounReference(SilWord(edge.label)))
      }
    )
    if (!entity.properName.isEmpty) {
      buf += cosmos.qualifiedReference(entity, DETERMINER_NONSPECIFIC)
    }
    buf.toSeq
  }
}
