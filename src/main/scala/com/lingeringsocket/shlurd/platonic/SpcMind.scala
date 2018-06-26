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

import spire.math._

import scala.collection._
import scala.collection.JavaConverters._
import scala.util._

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
        val graph = cosmos.getGraph
        val possessor = graph.getPossessorEntity(edge)
        val role = graph.getPossesseeRole(edge.formEdge)
        val specializedRole = graph.specializeRoleForForm(
          role,
          entity.form)
        // we prefer more specific associations over less specific ones
        // e.g. "Larry's father" is better than "one of Pete's uncles"
        val cardinality = assocGraph.outgoingEdgesOf(possessor).asScala.
          count(edge2 => {
            (edge2.getRoleName == edge.getRoleName) &&
            (role == specializedRole) || graph.isHyponym(
              specializedRole,
              graph.getPossesseeEntity(edge2).form)
          })
        val genitive = {
          if (cardinality > 1) {
            // "one of Pete's uncles"
            SilStateSpecifiedReference(
              SilNounReference(SilWord(LEMMA_ONE)),
              SilAdpositionalState(
                ADP_OF,
                SilGenitiveReference(
                  cosmos.specificReference(possessor, determiner),
                  SilNounReference(SilWord("", specializedRole.name),
                    DETERMINER_UNSPECIFIED,
                    COUNT_PLURAL))))
          } else {
            // "Larry's father"
            SilGenitiveReference(
              cosmos.specificReference(possessor, determiner),
              SilNounReference(SilWord(specializedRole.name)))
          }
        }
        (genitive, cardinality)
      }
    )
    val qualifiedSeq = {
      if (!entity.properName.isEmpty) {
        Seq(cosmos.qualifiedReference(entity, DETERMINER_NONSPECIFIC))
      } else {
        Seq.empty
      }
    }
    super.equivalentReferences(entity, determiner) ++
      genitives.sortBy(_._2).map(_._1) ++ qualifiedSeq
  }

  override def thirdPersonReference(entities : Set[SpcEntity])
      : Option[SilReference] =
  {
    val gender = {
      if (entities.size == 1) {
        val entity = entities.head
        cosmos.findProperty(entity.form, LEMMA_GENDER) match {
          case Some(genderProp) => {
            // FIXME add direct access to state value
            cosmos.evaluateEntityPropertyPredicate(
              entity, genderProp, LEMMA_FEMININE) match
            {
              case Success(Trilean.True) => {
                GENDER_F
              }
              case _ => {
                cosmos.evaluateEntityPropertyPredicate(
                  entity, genderProp, LEMMA_MASCULINE) match
                {
                  case Success(Trilean.True) => {
                    GENDER_M
                  }
                  case _ => {
                    GENDER_N
                  }
                }
              }
            }
          }
          case _ => GENDER_N
        }
      } else {
        // FIXME:  for languages like Spanish, need to be macho
        GENDER_N
      }
    }
    val count = {
      if (entities.size == 1) {
        COUNT_SINGULAR
      } else {
        COUNT_PLURAL
      }
    }
    Some(SilPronounReference(PERSON_THIRD, gender, count))
  }
}
