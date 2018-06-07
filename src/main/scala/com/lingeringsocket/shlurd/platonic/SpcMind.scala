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

import ShlurdEnglishLemmas._

class SpcMind(cosmos : SpcCosmos) extends ShlurdMind(cosmos)
{
  override def getCosmos = cosmos

  override def equivalentReferences(
    entity : SpcEntity,
    determiner : SilDeterminer) =
  {
    val assocGraph = cosmos.getEntityAssocGraph
    val genitives = assocGraph.incomingEdgesOf(entity).asScala.toSeq.map(
      edge => {
        val possessor = cosmos.getGraph.getPossessorEntity(edge)
        // we prefer more specific associations over less specific ones
        // e.g. "Larry's father" is better than "one of Pete's uncles"
        val count = assocGraph.outgoingEdgesOf(possessor).asScala.
          count(_.label == edge.label)
        val genitive = {
          if (count > 1) {
            // "one of Pete's uncles"
            SilStateSpecifiedReference(
              SilNounReference(SilWord(LEMMA_ONE)),
              SilAdpositionalState(
                ADP_OF,
                SilGenitiveReference(
                  cosmos.specificReference(possessor, determiner),
                  SilNounReference(SilWord("", edge.label),
                    DETERMINER_UNSPECIFIED,
                    COUNT_PLURAL))))
          } else {
            // "Larry's father"
            SilGenitiveReference(
              cosmos.specificReference(possessor, determiner),
              SilNounReference(SilWord(edge.label)))
          }
        }
        (genitive, count)
      }
    )
    val qualifiedSeq = {
      if (!entity.properName.isEmpty) {
        Seq(cosmos.qualifiedReference(entity, DETERMINER_NONSPECIFIC))
      } else {
        Seq.empty
      }
    }
    Seq(
      cosmos.specificReference(entity, determiner)
    ) ++ genitives.sortBy(_._2).map(_._1) ++ qualifiedSeq
  }
}
