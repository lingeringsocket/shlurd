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
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

class SpcWordnetOntologyMind(
  tongue : SprTongue,
  cosmos : SpcCosmos,
  preferredSynonyms : Map[SpcIdeal, String] = Map.empty)
    extends SpcMind(cosmos)
{
  private val wordnet = tongue.getWordnet

  override def getTongue = tongue

  private def getOntology = new SpcWordnetOntology(wordnet, cosmos)

  override def spawn(newCosmos : SpcCosmos) =
  {
    val mind = new SpcWordnetOntologyMind(
      tongue, newCosmos, preferredSynonyms)
    mind.initFrom(this)
    mind
  }

  override def analyzeSense[PhraseType <: SilPhrase](
    annotator : AnnotatorType,
    phrase : PhraseType) =
  {
    val analyzer = new SprWordnetSenseAnalyzer(getTongue, annotator)
    analyzer.analyze(phrase)
  }

  override def specificReference(
    annotator : AnnotatorType,
    entity : SpcEntity,
    determiner : SilDeterminer) : SilReference =
  {
    val ref = super.specificReference(annotator, entity, determiner)
    val analyzer = new SprWordnetSenseAnalyzer(getTongue, annotator)
    analyzer.analyze(ref)
  }

  override def resolveFormCandidates(noun : SilWord) : Seq[SpcForm] =
  {
    val seq = super.resolveFormCandidates(noun)
    if (!seq.isEmpty) {
      seq
    } else {
      val senses = wordnet.findSenses(noun.senseId)
      senses.to(LazyList).flatMap(getOntology.getSynsetForm)
    }
  }

  override def resolveRole(
    possessorForm : SpcForm,
    noun : SilWord,
    includeHypernyms : Boolean = true) : Option[SpcRole] =
  {
    val pool = cosmos.getPool
    pool.accessCache(
      pool.roleCache,
      tupleN(possessorForm, noun, includeHypernyms),
      pool.taxonomyTimestamp,
      {
        val wordnetOpt = {
          val senses = wordnet.findSenses(noun.senseId)
          val ontology = getOntology
          val graph = cosmos.getGraph
          senses.to(LazyList).flatMap(sense => {
            ontology.getSynsetForm(sense)
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
        wordnetOpt.orElse(super.resolveRole(
          possessorForm, noun, includeHypernyms))
      }
    )
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
      val sense1 = wordnet.findSenses(verb1.senseId).headOption
      val sense2 = wordnet.findSenses(verb2.senseId).headOption
      tupleN(sense1, sense2) match {
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
      cosmos.decodeName(SpcWordnetOntology.getNoun(form)))
  }

  override protected def getPossesseeName(role : SpcRole) : String =
  {
    preferredSynonyms.getOrElse(
      role,
      cosmos.decodeName(SpcWordnetOntology.getPossesseeNoun(role)))
  }
}
