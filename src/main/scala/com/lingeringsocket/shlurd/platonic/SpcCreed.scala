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

import scala.collection.JavaConverters._

import SpcCosmos._
import ShlurdEnglishLemmas._

class SpcCreed(cosmos : SpcCosmos)
{
  def allBeliefs() : Iterable[SilSentence] =
  {
    cosmos.getFormSynonyms.getAll.filterNot(_._1 == LEMMA_WHO).map(entry => {
      if (cosmos.isRole(SilWord(entry._1))) {
        formRoleBelief(entry)
      } else {
        formAliasBelief(entry)
      }
    }) ++ (
      cosmos.getForms.values.flatMap(formBeliefs(_))
    ) ++ (
      cosmos.getEntities.values.flatMap(entityBeliefs(_))
    )
  }

  def formBeliefs(form : SpcForm) : Iterable[SilSentence] =
  {
    form.properties.values.map(
      formPropertyBelief(form, _)
    ) ++
    form.getInflectedStateNormalizations.map(
      formStateNormalizationBelief(form, _)
    ) ++ {
      val taxonomyGraph = cosmos.getFormTaxonomyGraph
      if (taxonomyGraph.containsVertex(form)) {
        taxonomyGraph.outgoingEdgesOf(form).asScala.toSeq.map(
          formTaxonomyBelief(_))
      } else {
        Iterable.empty
      }
    } ++ {
      val assocGraph = cosmos.getFormAssocGraph
      if (assocGraph.containsVertex(form)) {
        assocGraph.outgoingEdgesOf(form).asScala.toSeq.map(
          formAssociationBelief(_))
      } else {
        Iterable.empty
      }
    }
  }

  def entityBeliefs(entity : SpcEntity) : Iterable[SilSentence] =
  {
    Seq(entityFormBelief(entity)) ++ {
      val assocGraph = cosmos.getEntityAssocGraph
      if (assocGraph.containsVertex(entity)) {
        assocGraph.outgoingEdgesOf(entity).asScala.toSeq.map(
          entityAssociationBelief(_))
      } else {
        Iterable.empty
      }
    }
  }

  def formTaxonomyBelief(
    edge : FormTaxonomyEdge
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        formNoun(cosmos.getSpecificForm(edge)),
        SilStateSpecifiedReference(
          nounReference(LEMMA_KIND),
          SilAdpositionalState(
            ADP_OF,
            formNoun(cosmos.getGenericForm(edge)))),
        REL_IDENTITY))
  }

  def formRoleBelief(entry : (String, String)) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        nounReference(entry._1),
        nounReference(entry._2),
        REL_IDENTITY),
      SilIndicativeMood(true, MODAL_MUST))
  }

  def formAliasBelief(entry : (String, String)) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        nounReference(entry._1),
        nounReference(entry._2),
        REL_IDENTITY))
  }

  def formPropertyBelief(
    form : SpcForm,
    property : SpcProperty
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilStatePredicate(
        formNoun(form),
        if (property.states.size == 1) {
          propertyState(property.states.head)
        } else {
          SilConjunctiveState(
            DETERMINER_ANY,
            property.states.map(propertyState).toSeq,
            SEPARATOR_CONJOINED)
        }
      ),
      SilIndicativeMood(
        true,
        if (property.isClosed) MODAL_MUST else MODAL_MAY)
    )
  }

  def formStateNormalizationBelief(
    form : SpcForm,
    entry : (SilState, SilState)
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilStatePredicate(
        SilStateSpecifiedReference(
          formNoun(form),
          entry._1),
        entry._2
      )
    )
  }

  def formAssociationBelief(
    edge : FormAssocEdge
  ) : SilSentence =
  {
    val constraint = cosmos.getAssocConstraints(edge)
    val isProperty = cosmos.getPropertyEdges.contains(edge)
    val (count, determiner) = {
      if (constraint.upper == 1) {
        (COUNT_SINGULAR, SilIntegerDeterminer(constraint.upper))
      } else {
        (COUNT_PLURAL, DETERMINER_UNSPECIFIED)
      }
    }
    val possesseeNoun = nounReference(
      edge.label, count, determiner)
    val possessee = {
      if (isProperty) {
        SilStateSpecifiedReference(
          possesseeNoun,
          SilAdpositionalState(
            ADP_AS,
            nounReference(LEMMA_PROPERTY))
        )
      } else {
        possesseeNoun
      }
    }
    SilPredicateSentence(
      SilRelationshipPredicate(
        formNoun(cosmos.getPossessorForm(edge)),
        possessee,
        REL_ASSOCIATION),
      SilIndicativeMood(
        true,
        if (constraint.lower == 0) MODAL_MAY else MODAL_MUST)
    )
  }

  def entityFormBelief(entity : SpcEntity) : SilSentence =
  {
    val subject = cosmos.specificReference(entity, DETERMINER_NONSPECIFIC)
    val predicate = entity.properName match {
      case "" => {
        SilStatePredicate(
          subject,
          SilExistenceState())
      }
      case _ => {
        SilRelationshipPredicate(
          subject,
          formNoun(entity.form),
          REL_IDENTITY)
      }
    }
    SilPredicateSentence(predicate)
  }

  def entityAssociationBelief(
    edge : EntityAssocEdge
  ) : SilSentence =
  {
    val possessor = cosmos.specificReference(
      cosmos.getPossessorEntity(edge), DETERMINER_NONSPECIFIC)
    val possessee = cosmos.specificReference(
      cosmos.getPossesseeEntity(edge), DETERMINER_NONSPECIFIC)
    val role = nounReference(
      edge.label, COUNT_SINGULAR, DETERMINER_UNSPECIFIED)
    SilPredicateSentence(
      SilRelationshipPredicate(
        possessee,
        SilGenitiveReference(
          possessor,
          role),
        REL_IDENTITY)
    )
  }

  private def nounReference(
    noun : String, count : SilCount = COUNT_SINGULAR,
    determiner : SilDeterminer = DETERMINER_NONSPECIFIC) =
  {
    count match {
      case COUNT_SINGULAR => {
        SilNounReference(SilWord(noun), determiner)
      }
      case COUNT_PLURAL => {
        SilNounReference(
          SilWord("", noun), DETERMINER_UNSPECIFIED, COUNT_PLURAL)
      }
    }
  }

  private def formNoun(
    form : SpcForm, count : SilCount = COUNT_SINGULAR) =
  {
    nounReference(form.name, count)
  }

  private def propertyState(entry : (String, String)) =
  {
    SilPropertyState(SilWord(entry._2, entry._1))
  }
}
