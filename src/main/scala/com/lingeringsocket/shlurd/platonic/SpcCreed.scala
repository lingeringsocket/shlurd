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

import scala.collection.JavaConverters._

import SprEnglishLemmas._

class SpcCreed(cosmos : SpcCosmos, includeMeta : Boolean = false)
{
  private val mind = new SpcMind(cosmos)

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
      cosmos.getAssertions.map(_.toSentence)
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
        REL_PREDEF_IDENTITY.toVerb,
        SilStateSpecifiedReference(
          nounReference(LEMMA_KIND),
          SilAdpositionalState(
            SilAdposition.OF,
            idealNoun(cosmos.getGraph.getSuperclassIdeal(edge))))
      )
    )
  }

  def roleTaxonomyBelief(
    edge : SpcTaxonomyEdge
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        idealNoun(cosmos.getGraph.getSubclassIdeal(edge)),
        REL_PREDEF_IDENTITY.toVerb,
        idealNoun(cosmos.getGraph.getSuperclassIdeal(edge))
      ),
      SilTam.indicative.withModality(MODAL_MUST))
  }

  def formAliasBelief(
    entry : (String, String)
  ) : SilSentence =
  {
    SilPredicateSentence(
      SilRelationshipPredicate(
        nounReference(entry._1),
        REL_PREDEF_IDENTITY.toVerb,
        nounReference(entry._2)
      )
    )
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
    val predicate = property.domain match {
      case PROPERTY_OPEN_ENUM | PROPERTY_CLOSED_ENUM => {
        SilStatePredicate(
          noun,
          STATE_PREDEF_BE.toVerb,
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
        )
      }
      case _ => {
        SilRelationshipPredicate(
          noun,
          REL_PREDEF_IDENTITY.toVerb,
          SilNounReference(
            SilWord(property.domain.name), DETERMINER_NONSPECIFIC)
        )
      }
    }
    SilPredicateSentence(
      predicate,
      SilTam.indicative.withModality(
        if (property.domain == PROPERTY_OPEN_ENUM) MODAL_MAY else MODAL_MUST)
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
        STATE_PREDEF_BE.toVerb,
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
        REL_PREDEF_ASSOC.toVerb,
        possessee
      ),
      SilTam.indicative.withModality(
        if (constraint.lower == 0) MODAL_MAY else MODAL_MUST)
    )
  }

  def entityFormBelief(
    entity : SpcEntity
  ) : SilSentence =
  {
    val subject = mind.specificReference(entity, DETERMINER_NONSPECIFIC)
    val predicate = entity.properName match {
      case "" => {
        SilStatePredicate(
          subject,
          SilWord.uninflected(LEMMA_EXIST),
          SilExistenceState())
      }
      case _ => {
        SilRelationshipPredicate(
          subject,
          REL_PREDEF_IDENTITY.toVerb,
          idealNoun(entity.form)
        )
      }
    }
    SilPredicateSentence(predicate)
  }

  def entityPropertyBelief(
    entity : SpcEntity,
    eps : SpcEntityPropertyState
  ) : SilSentence =
  {
    val subject = mind.specificReference(entity, DETERMINER_UNIQUE)
    val property = cosmos.resolvePropertyName(entity, eps.propertyName).get
    val propertyStates = cosmos.getPropertyStateMap(property)
    val predicate = property.domain match {
      case PROPERTY_OPEN_ENUM | PROPERTY_CLOSED_ENUM => {
        val subjectRef = {
          if (property.name.contains('_')) {
            subject
          } else {
            SilGenitiveReference(
              subject,
              SilNounReference(SilWord(eps.propertyName))
            )
          }
        }
        SilStatePredicate(
          subjectRef,
          STATE_PREDEF_BE.toVerb,
          propertyState((eps.lemma, propertyStates(eps.lemma)))
        )
      }
      case _ => {
        SilRelationshipPredicate(
          SilGenitiveReference(
            subject,
            SilNounReference(SilWord(eps.propertyName))),
          REL_PREDEF_IDENTITY.toVerb,
          SilQuotationReference(eps.lemma)
        )
      }
    }
    SilPredicateSentence(predicate)
  }

  def entityAssociationBelief(
    edge : SpcEntityAssocEdge
  ) : SilSentence =
  {
    val possessor = mind.specificReference(
      cosmos.getGraph.getPossessorEntity(edge), DETERMINER_UNIQUE)
    val possessee = mind.specificReference(
      cosmos.getGraph.getPossesseeEntity(edge), DETERMINER_UNIQUE)
    val role = nounReference(
      edge.getRoleName, COUNT_SINGULAR, DETERMINER_UNSPECIFIED)
    SilPredicateSentence(
      SilRelationshipPredicate(
        possessee,
        REL_PREDEF_IDENTITY.toVerb,
        SilGenitiveReference(
          possessor,
          role)
      )
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
    val possessorForm =
      cosmos.getGraph.getPossessorIdeal(edge1).asInstanceOf[SpcForm]
    val possesseeForm =
      cosmos.getGraph.getPossessorIdeal(edge2).asInstanceOf[SpcForm]
    val (
        antecedentSubject, antecedentComplement,
        consequentSubject, consequentComplement
    ) = {
      if (possessorForm == possesseeForm) {
        tupleN((
          idealNoun(possesseeForm),
          SilGenitiveReference(
            SilStateSpecifiedReference(
              plainNoun(possessorForm),
              SilPropertyState(SilWord(LEMMA_ANOTHER))
            ),
            plainNoun(edge1.getRoleName)
          ),
          SilStateSpecifiedReference(
            idealNoun(possessorForm, COUNT_SINGULAR, DETERMINER_UNIQUE),
            SilPropertyState(SilWord(LEMMA_SECOND))
          ),
          SilGenitiveReference(
            SilStateSpecifiedReference(
              idealNoun(possesseeForm, COUNT_SINGULAR, DETERMINER_UNIQUE),
              SilPropertyState(SilWord(LEMMA_FIRST))
            ),
            plainNoun(edge2.getRoleName)
          )
        ))
      } else {
        tupleN((
          idealNoun(possesseeForm),
          SilGenitiveReference(
            idealNoun(possessorForm),
            plainNoun(edge1.getRoleName)
          ),
          idealNoun(possessorForm, COUNT_SINGULAR, DETERMINER_UNIQUE),
          SilGenitiveReference(
            idealNoun(possesseeForm, COUNT_SINGULAR, DETERMINER_UNIQUE),
            plainNoun(edge2.getRoleName)
          )
        ))
      }
    }
    val sentence = SilConditionalSentence(
      SilWord(LEMMA_IF),
      SilRelationshipPredicate(
        antecedentSubject,
        REL_PREDEF_IDENTITY.toVerb,
        antecedentComplement
      ),
      SilRelationshipPredicate(
        consequentSubject,
        REL_PREDEF_IDENTITY.toVerb,
        consequentComplement
      ),
      SilTam.indicative,
      SilTam.indicative,
      true
    )
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
    ideal : SpcIdeal, count : SilCount = COUNT_SINGULAR,
    determiner : SilDeterminer = DETERMINER_NONSPECIFIC) =
  {
    nounReference(ideal.name, count, determiner)
  }

  private def plainNoun(
    ideal : SpcIdeal) : SilReference =
  {
    plainNoun(ideal.name)
  }

  private def plainNoun(
    name : String) : SilReference =
  {
    nounReference(name, COUNT_SINGULAR, DETERMINER_UNSPECIFIED)
  }

  private def propertyState(entry : (String, String)) =
  {
    SilPropertyState(SilWord(entry._2, entry._1))
  }
}
