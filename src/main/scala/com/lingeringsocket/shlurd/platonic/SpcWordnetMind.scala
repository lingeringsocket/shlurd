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
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

import scala.collection._
import scala.util._

class SpcWordnetMind(cosmos : SpcCosmos, wordnetActive : Boolean)
    extends SpcMind(cosmos)
{
  private def getWordnet() = new SpcWordnet(cosmos)

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcWordnetMind(newCosmos, wordnetActive)
    mind.initFrom(this)
    mind
  }

  override def analyzeSense(sentence : SilSentence) =
  {
    if (wordnetActive) {
      val analyzer = new SilWordnetSenseAnalyzer
      analyzer.analyze(sentence)
    } else {
      super.analyzeSense(sentence)
    }
  }

  override def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    val seq = super.resolveFormCandidates(noun)
    if (!seq.isEmpty) {
      seq
    } else {
      if (wordnetActive) {
        val senses = ShlurdWordnet.findSenses(noun.senseId)
        val wordnet = getWordnet
        senses.toStream.flatMap(wordnet.getSynsetForm)
      } else {
        Seq.empty
      }
    }
  }

  override def resolveRole(
    possessorForm : SpcForm, noun : SilWord) : Option[SpcRole] =
  {
    val wordnetOpt = if (wordnetActive) {
      val senses = ShlurdWordnet.findSenses(noun.senseId)
      val wordnet = getWordnet
      val graph = cosmos.getGraph
      senses.toStream.flatMap(sense => {
        wordnet.getSynsetForm(sense)
      }).flatMap(possesseeForm => {
        graph.getRolesForForm(possesseeForm).filter(
          possesseeRole => {
            graph.getFormAssocEdge(possessorForm, possesseeRole).nonEmpty ||
            graph.getRolesForForm(possessorForm).exists(
              possessorRole => {
                graph.getFormAssocEdge(possessorRole, possesseeRole).nonEmpty
              }
            )
          }
        )
      }).headOption
    } else {
      None
    }
    wordnetOpt.orElse(super.resolveRole(possessorForm, noun))
  }

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[SpcEntity]] =
  {
    val superResult = super.resolveQualifiedNoun(noun, context, qualifiers)
    if (superResult.isSuccess || !wordnetActive) {
      superResult
    } else {
      resolveForm(noun) match {
        case Some(form) => {
          cosmos.resolveQualifiedNoun(form.name, context, qualifiers)
        }
        case _ => superResult
      }
    }
  }
}
