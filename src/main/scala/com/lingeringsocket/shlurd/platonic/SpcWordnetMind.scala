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
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.mind._

import scala.collection._
import scala.util._

object SpcWordnetMind
{
  def debug(s : String)
  {
    val analyzer = new SilWordnetSenseAnalyzer
    val sentence = SprParser(s).parseOne
    val analyzed = analyzer.analyze(sentence)
    println("ANALYSIS = " + analyzed)
  }
}

class SpcWordnetMind(
  cosmos : SpcCosmos,
  preferredSynonyms : Map[SpcIdeal, String] = Map.empty)
    extends SpcMind(cosmos)
{
  private def getWordnet() = new SpcWordnet(cosmos)

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcWordnetMind(newCosmos, preferredSynonyms)
    mind.initFrom(this)
    mind
  }

  override def analyzeSense[PhraseType <: SilPhrase](phrase : PhraseType) =
  {
    val analyzer = new SilWordnetSenseAnalyzer
    analyzer.analyze(phrase)
  }

  override def specificReference(
    entity : SpcEntity,
    determiner : SilDeterminer) : SilReference =
  {
    val ref = super.specificReference(entity, determiner)
    val analyzer = new SilWordnetSenseAnalyzer
    analyzer.analyze(ref)
  }

  override def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    val seq = super.resolveFormCandidates(noun)
    if (!seq.isEmpty) {
      seq
    } else {
      val senses = ShlurdWordnet.findSenses(noun.senseId)
      val wordnet = getWordnet
      senses.toStream.flatMap(wordnet.getSynsetForm)
    }
  }

  override def resolveRole(
    possessorForm : SpcForm, noun : SilWord) : Option[SpcRole] =
  {
    val pool = cosmos.getPool
    pool.accessCache(
      pool.roleCache,
      tupleN((possessorForm, noun)),
      pool.taxonomyTimestamp,
      {
        val wordnetOpt = {
          val senses = ShlurdWordnet.findSenses(noun.senseId)
          val wordnet = getWordnet
          val graph = cosmos.getGraph
          senses.toStream.flatMap(sense => {
            wordnet.getSynsetForm(sense)
          }).flatMap(possesseeForm => {
            cosmos.getRolesForForm(possesseeForm).filter(
              possesseeRole => {
                graph.getFormAssocEdge(possessorForm, possesseeRole).nonEmpty ||
                cosmos.getRolesForForm(possessorForm).exists(
                  possessorRole => {
                    graph.getFormAssocEdge(
                      possessorRole, possesseeRole).nonEmpty
                  }
                )
              }
            )
          }).headOption
        }
        wordnetOpt.orElse(super.resolveRole(possessorForm, noun))
      })
  }

  override def resolveQualifiedNoun(
    noun : SilWord,
    context : SilReferenceContext,
    qualifiers : Set[String] = Set.empty) : Try[Set[SpcEntity]] =
  {
    val superResult = super.resolveQualifiedNoun(noun, context, qualifiers)
    if (superResult.isSuccess) {
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

  override def isEquivalentVerb(
    verb1 : SilWord, verb2 : SilWord) : Boolean =
  {
    if (super.isEquivalentVerb(verb1, verb2)) {
      true
    } else {
      val sense1 = ShlurdWordnet.findSenses(verb1.senseId).headOption
      val sense2 = ShlurdWordnet.findSenses(verb2.senseId).headOption
      tupleN((sense1, sense2)) match {
        case (Some(s1), Some(s2)) => {
          s1 == s2
        }
        case _ => false
      }
    }
  }

  override protected def getFormName(form : SpcForm) : String =
  {
    preferredSynonyms.getOrElse(
      form,
      cosmos.decodeName(getWordnet.getNoun(form)))
  }

  override protected def getPossesseeName(role : SpcRole) : String =
  {
    preferredSynonyms.getOrElse(
      role,
      cosmos.decodeName(getWordnet.getPossesseeNoun(role)))
  }

  override protected def guessGender(entity : SpcEntity) : SilGender =
  {
    if (getWordnet.anyMatchingHypernym(
      entity.form, getWordnet.getFeminineForms)
    ) {
      GENDER_F
    } else {
      super.guessGender(entity)
    }
  }
}
