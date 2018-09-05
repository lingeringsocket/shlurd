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

import scala.collection.JavaConverters._

import SprEnglishLemmas._

class SpcCreed(cosmos : SpcCosmos, includeMeta : Boolean = false)
{
  def allBeliefs() : Iterable[SilSentence] =
  {
    cosmos.getIdealSynonyms.filterNot(
      pair => SpcPrimordial.isPrimordialSynonym(pair) || isTrivialSynonym(pair)
    ).map(
      formAliasBelief
    ) ++ (
      cosmos.getRoles.flatMap(roleTaxonomyBeliefs)
    ) ++ (
      cosmos.getRoles.flatMap(idealAssociationBeliefs)
    ) ++ (
      cosmos.getForms.flatMap(formBeliefs)
    ) ++ (
      cosmos.getInverseAssocEdges.flatMap(
        entry => inverseAssocBelief(entry._1, entry._2))
    ) ++ (
      cosmos.getEntities.
        filterNot(e => SpcMeta.isMetaEntity(e) && !includeMeta).
        flatMap(entityBeliefs)
    ) ++ (
      cosmos.getTriggers
    )
  }

  def formBeliefs(form : SpcForm) : Iterable[SilSentence] =
  {
    cosmos.getFormPropertyMap(form).values.map(
      formPropertyBelief(form, _)
    ) ++
    cosmos.getInflectedStateNormalizations(form).map(
      formStateNormalizationBelief(form, _)
    ) ++ {
      cosmos.getIdealTaxonomyGraph.outgoingEdgesOf(form).asScala.toSeq.map(
        formTaxonomyBelief)
    } ++ {
      idealAssociationBeliefs(form)
    }
  }

  def idealAssociationBeliefs(ideal : SpcIdeal) : Iterable[SilSentence] =
  {
    cosmos.getFormAssocGraph.outgoingEdgesOf(ideal).asScala.toSeq.map(
      idealAssociationBelief)
  }

  def roleBeliefs(role : SpcRole) : Iterable[SilSentence] =
  {
    roleTaxonomyBeliefs(role) ++ idealAssociationBeliefs(role)
  }

  def roleTaxonomyBeliefs(role : SpcRole) : Iterable[SilSentence] =
  {
    cosmos.getIdealTaxonomyGraph.outgoingEdgesOf(role).asScala.toSeq.
      filterNot(isTrivialTaxonomy).map(
        roleTaxonomyBelief)
  }

  def entityBeliefs(entity : SpcEntity) : Iterable[SilSentence] =
  {
    Seq(entityFormBelief(entity)) ++ {
      cosmos.getEntityAssocGraph.outgoingEdgesOf(entity).asScala.toSeq.map(
        entityAssociationBelief)
    } ++ {
      cosmos.getEntityPropertyMap(entity).values.map(
        entityPropertyBelief(entity, _))
    }
  }

  def idealTaxonomyBelief(
    edge : SpcTaxonomyEdge
  ) : SilSentence =
  {
    cosmos.getGraph.getSubclassIdeal(edge) match {
      case _ : SpcForm => formTaxonomyBelief(edge)
      case _ : SpcRole => roleTaxonomyBelief(edge)
    }
  }

  def formTaxonomyBelief(
    edge : SpcTaxonomyEdge
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        idealNoun(cosmos.getGraph.getSubclassIdeal(edge)),
        SilStateSpecifiedReference(
          nounReference(LEMMA_KIND),
          SilAdpositionalState(
            SilAdposition.OF,
            idealNoun(cosmos.getGraph.getSuperclassIdeal(edge)))),
        REL_IDENTITY))
  }

  def roleTaxonomyBelief(
    edge : SpcTaxonomyEdge
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        idealNoun(cosmos.getGraph.getSubclassIdeal(edge)),
        idealNoun(cosmos.getGraph.getSuperclassIdeal(edge)),
        REL_IDENTITY),
      SilTam.indicative.withModality(MODAL_MUST))
  }

  def formAliasBelief(
    entry : (String, String)
  ) : SilSentence =
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
    val noun = {
      if (property.isSynthetic) {
        idealNoun(form)
      } else {
        SilGenitiveReference(
          idealNoun(form),
          nounReference(
            property.name, COUNT_SINGULAR, DETERMINER_UNSPECIFIED))
      }
    }
    SilPredicateSentence(
      SilStatePredicate(
        noun,
        {
          val propertyStates = cosmos.getPropertyStateMap(property)
          if (propertyStates.size == 1) {
            propertyState(propertyStates.head)
          } else {
            SilConjunctiveState(
              DETERMINER_ANY,
              propertyStates.map(propertyState).toSeq,
              SEPARATOR_CONJOINED)
          }
        }
      ),
      SilTam.indicative.withModality(
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
          idealNoun(form),
          entry._1),
        entry._2
      )
    )
  }

  def idealAssociationBelief(
    edge : SpcFormAssocEdge
  ) : SilSentence =
  {
    val constraint = edge.constraint
    val isProperty = edge.isProperty
    val (count, determiner) = {
      if (constraint.upper == 1) {
        tupleN((COUNT_SINGULAR, SilIntegerDeterminer(constraint.upper)))
      } else {
        tupleN((COUNT_PLURAL, DETERMINER_UNSPECIFIED))
      }
    }
    val possesseeNoun = nounReference(
      edge.getRoleName, count, determiner)
    val possessee = {
      if (isProperty) {
        SilStateSpecifiedReference(
          possesseeNoun,
          SilAdpositionalState(
            SilAdposition.AS,
            nounReference(LEMMA_PROPERTY))
        )
      } else {
        possesseeNoun
      }
    }
    SilPredicateSentence(
      SilRelationshipPredicate(
        idealNoun(cosmos.getGraph.getPossessorIdeal(edge)),
        possessee,
        REL_ASSOCIATION),
      SilTam.indicative.withModality(
        if (constraint.lower == 0) MODAL_MAY else MODAL_MUST)
    )
  }

  def entityFormBelief(
    entity : SpcEntity
  ) : SilSentence =
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
          idealNoun(entity.form),
          REL_IDENTITY)
      }
    }
    SilPredicateSentence(predicate)
  }

  def entityPropertyBelief(
    entity : SpcEntity,
    eps : SpcEntityPropertyState
  ) : SilSentence =
  {
    val subject = cosmos.specificReference(entity, DETERMINER_UNIQUE)
    val property = cosmos.resolvePropertyName(entity, eps.propertyName).get
    val propertyStates = cosmos.getPropertyStateMap(property)
    // FIXME be specific about property
    SilPredicateSentence(
      SilStatePredicate(
        subject,
        propertyState((eps.lemma, propertyStates(eps.lemma)))
      )
    )
  }

  def entityAssociationBelief(
    edge : SpcEntityAssocEdge
  ) : SilSentence =
  {
    val possessor = cosmos.specificReference(
      cosmos.getGraph.getPossessorEntity(edge), DETERMINER_UNIQUE)
    val possessee = cosmos.specificReference(
      cosmos.getGraph.getPossesseeEntity(edge), DETERMINER_UNIQUE)
    val role = nounReference(
      edge.getRoleName, COUNT_SINGULAR, DETERMINER_UNSPECIFIED)
    SilPredicateSentence(
      SilRelationshipPredicate(
        possessee,
        SilGenitiveReference(
          possessor,
          role),
        REL_IDENTITY)
    )
  }

  private def isTrivialTaxonomy(
    edge : SpcTaxonomyEdge) =
  {
    cosmos.getGraph.getSuperclassIdeal(edge).name.equals(
      cosmos.getGraph.getSubclassIdeal(edge).name)
  }

  private def isTrivialSynonym(
    pair : (String, String)) =
  {
    pair._1 == pair._2
  }

  def inverseAssocBelief(
    edge1 : SpcFormAssocEdge,
    edge2 : SpcFormAssocEdge
  ) : Option[SilSentence] =
  {
    val possessorIdeal = cosmos.getGraph.getPossessorIdeal(edge1) match {
      case form : SpcForm => form
      case role : SpcRole => {
        val candidates = cosmos.getGraph.getFormsForRole(role)
        val forms = {
          if (candidates.size > 1) {
            candidates.filterNot(_.name == SpcMeta.ENTITY_METAFORM_NAME)
          } else {
            candidates
          }
        }
        assert(forms.size < 2, (role, forms))
        if (forms.isEmpty) {
          return None
        } else {
          forms.head
        }
      }
    }
    val sentence =
      SilPredicateSentence(
        SilRelationshipPredicate(
          SilStateSpecifiedReference(
            idealNoun(possessorIdeal),
            SilAdpositionalState(
              SilAdposition.WITH,
              nounReference(edge1.getRoleName))),
          nounReference(edge2.getRoleName),
          REL_IDENTITY))
    Some(sentence)
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
          SilWord.uninflected(noun), DETERMINER_UNSPECIFIED, COUNT_PLURAL)
      }
    }
  }

  private def idealNoun(
    ideal : SpcIdeal, count : SilCount = COUNT_SINGULAR) =
  {
    nounReference(ideal.name, count)
  }

  private def propertyState(entry : (String, String)) =
  {
    SilPropertyState(SilWord(entry._2, entry._1))
  }
}
