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

import scala.collection._
import scala.collection.JavaConverters._

import org.jgrapht.alg.shortestpath._

class SpcBeliefInterpreter(cosmos : SpcCosmos)
    extends SpcBeliefRecognizer(cosmos)
{
  type BeliefApplier = PartialFunction[SpcBelief, Unit]

  private val beliefAppliers = new mutable.ArrayBuffer[BeliefApplier]

  private lazy val allBeliefApplier = beliefAppliers.reduceLeft(_ orElse _)

  def interpretBelief(sentence : SilSentence)
  {
    recognizeBelief(sentence) match {
      case Some(belief) => {
        applyBelief(belief)
      }
      case _ => throw new IncomprehensibleBeliefExcn(sentence)
    }
  }

  def applyBelief(belief : SpcBelief)
  {
    allBeliefApplier.apply(belief)
  }

  private def resolveUniqueName(
    sentence : SilSentence,
    word : SilWord,
    determiner : SilDeterminer = DETERMINER_UNSPECIFIED)
      : SpcEntity =
  {
    determiner match {
      case DETERMINER_UNIQUE => {
        val form = cosmos.instantiateForm(word)
        val entities = cosmos.getEntities.filter(entity =>
          cosmos.getGraph.isHyponym(entity.form, form))
        if (entities.isEmpty) {
          val (entity, success) = cosmos.instantiateEntity(
            form, Seq.empty)
          assert(success)
          entity
        } else if (entities.size > 1) {
          val originalBelief = conjunctiveBelief(
            entities.map(creed.entityFormBelief(_)))
          throw new AmbiguousBeliefExcn(sentence, originalBelief)
        } else {
          entities.head
        }
      }
      case DETERMINER_UNSPECIFIED => {
        val candidates = cosmos.getEntities.filter(
          _.qualifiers == Set(word.lemma))
        assert(candidates.size < 2)
        candidates.headOption match {
          case Some(entity) => entity
          case _ => {
            val tentativeName = SpcForm.tentativeName(word)
            assert(cosmos.resolveForm(tentativeName.lemma).isEmpty)
            val newForm = cosmos.instantiateForm(tentativeName)
            val (entity, success) = cosmos.instantiateEntity(
              newForm, Seq(word), word.lemmaUnfolded)
            assert(success)
            entity
          }
        }
      }
      case _ => {
        throw new AssertionError("Unexpected determiner " + determiner)
      }
    }
  }

  private def validateEdgeCardinality(
    sentence : SilSentence,
    formAssocEdge : SpcFormAssocEdge,
    possessor : SpcEntity)
  {
    val constraint = formAssocEdge.constraint
    val entityAssocGraph = cosmos.getEntityAssocGraph
    val edges = entityAssocGraph.
      outgoingEdgesOf(possessor).asScala.
      filter(_.formEdge == formAssocEdge)

    val edgeCount = edges.size
    if (edgeCount >= constraint.upper) {
      val originalBelief = conjunctiveBelief(
        Seq(creed.idealAssociationBelief(formAssocEdge)) ++
          edges.map(creed.entityAssociationBelief(_)))
      throw new IncrementalCardinalityExcn(
        sentence, originalBelief)
    }
  }

  private def conjunctiveBelief(sentences : Seq[SilSentence]) : SilSentence =
  {
    SilConjunctiveSentence(
      DETERMINER_ALL,
      sentences,
      SEPARATOR_OXFORD_COMMA)
  }

  private def transmogrify(
    sentence : SilSentence, entity : SpcEntity, newForm : SpcForm)
  {
    // we are changing the form of an existing entity
    val oldForm = entity.form
    val newEntity = SpcEntity(
      entity.name, newForm, entity.qualifiers, entity.properName)
    val graph = cosmos.getGraph
    if (oldForm.isTentative) {
      val matchedAssocs = cosmos.matchAssocs(oldForm, newForm)
      matchedAssocs.foreach(pair => {
        // FIXME:  the "original belief" in this exception
        // isn't terribly helpful
        if (!cosmos.isValidMergeAssoc(pair._1, pair._2)) {
          throw new IncrementalCardinalityExcn(
            sentence,
            creed.idealAssociationBelief(pair._2))
        }
      })
      cosmos.createOrReplaceEntity(newEntity)
      matchedAssocs.foreach(pair => {
        cosmos.mergeAssoc(pair._1, pair._2)
      })
      cosmos.replaceForm(oldForm, newForm)
    } else {
      // the only form change allowed is a refinement
      assert(graph.isHyponym(newForm, oldForm))
      cosmos.createOrReplaceEntity(newEntity)
    }
  }

  private def findTentativePossessee(
    possessor : SpcEntity, formAssoc : SpcFormAssocEdge) : Option[SpcEntity] =
  {
    val set = cosmos.resolveGenitive(
      possessor, cosmos.getGraph.getPossesseeRole(formAssoc))
    if (set.size == 1) {
      val entity = set.head
      if (entity.isTentative) {
        Some(entity)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def addIdealTaxonomy(
    sentence : SilSentence,
    hyponymIdeal : SpcIdeal,
    hypernymIdeal : SpcIdeal)
  {
    val path = DijkstraShortestPath.findPathBetween(
      cosmos.getIdealTaxonomyGraph, hypernymIdeal, hyponymIdeal)
    if (path == null) {
      cosmos.addIdealTaxonomy(
        hyponymIdeal, hypernymIdeal)
    } else {
      val originalBelief = conjunctiveBelief(
        path.getEdgeList.asScala.toSeq.map(creed.idealTaxonomyBelief(_)))
      throw new ContradictoryBeliefExcn(
        sentence,
        originalBelief)
    }
  }

  private def beliefApplier(applier : BeliefApplier)
  {
    beliefAppliers += applier
  }

  beliefApplier {
    case UnimplementedBelief(
      sentence
    ) => {
      throw new UnimplementedBeliefExcn(sentence)
    }
  }

  beliefApplier {
    case StateEquivalenceBelief(
      sentence, formName, state1, state2
    ) => {
      val form = cosmos.instantiateForm(formName)
      cosmos.addStateNormalization(form, state1, state2)
    }
  }

  beliefApplier {
    case IdealAliasBelief(
      sentence, synonym, idealName
    ) => {
      val ideal = cosmos.instantiateIdeal(idealName)
      cosmos.addIdealSynonym(synonym.lemma, ideal.name)
    }
  }

  beliefApplier {
    case IdealTaxonomyBelief(
      sentence, hyponymIdealName, hypernymIdealName, hyponymIsRole
    ) => {
      // FIXME need to make sure all hypernyms are (and remain) compatible
      // FIXME also need to allow existing form to be refined
      val hypernymIdeal = cosmos.instantiateIdeal(
        hypernymIdealName, false)
      val hypernymIsRole = hypernymIdeal.isRole
      val hyponymIdeal = cosmos.instantiateIdeal(
        hyponymIdealName, hyponymIsRole || hypernymIsRole)
      if (hyponymIsRole || hypernymIsRole) {
        hyponymIdeal match {
          case form : SpcForm => {
            // FIXME:  thow a ContradictoryBeliefExcn pinpointing the reason
            // we believe hyponym to be a form, not a role
            throw new IncomprehensibleBeliefExcn(
              sentence)
          }
          case _ =>
        }
      }
      if (hyponymIsRole) {
        val entityAssocs = cosmos.getEntityAssocGraph
        entityAssocs.edgeSet.asScala.foreach(
          entityEdge => {
            val formEdge = entityEdge.formEdge
            val possesseeRole = cosmos.getGraph.getPossesseeRole(formEdge)
            if (possesseeRole == hyponymIdeal) {
              val possesseeEntity =
                cosmos.getGraph.getPossesseeEntity(entityEdge)
              if (!cosmos.getGraph.isHyponym(
                possesseeEntity.form, hypernymIdeal))
              {
                if (possesseeEntity.form.isTentative) {
                  addIdealTaxonomy(
                    sentence, possesseeEntity.form, hypernymIdeal)
                } else {
                  val formBelief = creed.entityFormBelief(possesseeEntity)
                  val assocBelief = creed.entityAssociationBelief(entityEdge)
                  throw new ContradictoryBeliefExcn(
                    sentence,
                    conjunctiveBelief(Seq(formBelief, assocBelief)))
                }
              }
            }
          }
        )
      }
      addIdealTaxonomy(sentence, hyponymIdeal, hypernymIdeal)
    }
  }

  beliefApplier {
    case FormAssocBelief(
      sentence, possessorIdealName, possesseeRoleNames,
      newConstraint, isProperty
    ) => {
      val possessorIdeal = cosmos.instantiateIdeal(possessorIdealName)
      possesseeRoleNames.foreach(possesseeRoleName => {
        val possesseeRole = cosmos.instantiateRole(possesseeRoleName)
        val edge = cosmos.addFormAssoc(
          possessorIdeal, possesseeRole)
        val oldConstraint = edge.constraint
        val constraint = SpcCardinalityConstraint(
          Math.max(oldConstraint.lower, newConstraint.lower),
          Math.min(oldConstraint.upper, newConstraint.upper))
        cosmos.annotateFormAssoc(edge, constraint, isProperty)
      })
    }
  }

  beliefApplier {
    case EntityExistenceBelief(
      sentence, formName, qualifiers, properName
    ) => {
      val form = cosmos.instantiateForm(formName)
      val (entity, success) = cosmos.instantiateEntity(
        form, qualifiers, properName)
      if (!success) {
        val graph = cosmos.getGraph
        val sameForm = (form == entity.form)
        if (!sameForm && graph.isHyponym(entity.form, form)) {
          // from "Bessie is a cow" to "Bessie is an animal"
          // so nothing to do
        } else if (!sameForm && graph.isHyponym(form, entity.form)) {
          // from "Bessie is an animal" to "Bessie is a cow"
          // so need to replace with refinement
          transmogrify(sentence, entity, form)
        } else {
          if (!sameForm && entity.form.isTentative) {
            // from "Bessie is a Bessie-form" to "Bessie is a cow"
            transmogrify(sentence, entity, form)
          } else {
            // from "Bessies is a dog" to "Bessie is a cow"
            // FIXME:  initiate dialog with user to sort it out,
            // e.g. "is a dog a kind of cow?"
            if (properName.isEmpty) {
              throw new AmbiguousBeliefExcn(
                sentence, creed.entityFormBelief(entity))
            } else {
              throw new ContradictoryBeliefExcn(
                sentence, creed.entityFormBelief(entity))
            }
          }
        }
      }
    }
  }

  beliefApplier {
    case EntityAssocBelief(
      sentence, possessorDeterminer, possessorName,
      possesseeDeterminer, possesseeName, roleName
    ) => {
      val possessor = resolveUniqueName(
        sentence, possessorName, possessorDeterminer)
      val possessee = resolveUniqueName(
        sentence, possesseeName, possesseeDeterminer)
      val role = cosmos.resolveRole(roleName.lemma) match {
        case Some(r) => r
        case _ => {
          val newRole = cosmos.instantiateRole(roleName)
          newRole
        }
      }
      val graph = cosmos.getGraph
      if (possessee.form.isTentative) {
        graph.getFormsForRole(role).foreach(form =>
          addIdealTaxonomy(sentence, possessee.form, form))
      }
      if (!graph.isFormCompatibleWithRole(possessee.form, role)) {
        val originalBelief = conjunctiveBelief(
          creed.roleTaxonomyBeliefs(role).toSeq)
        throw new ContradictoryBeliefExcn(
          sentence,
          originalBelief)
      }
      val candidates =
        Seq(possessor.form) ++ graph.getRolesForForm(possessor.form) ++ {
          if (possessor.form.isTentative) {
            graph.formAssocs.incomingEdgesOf(role).asScala.
              toSeq.map(graph.getPossessorIdeal)
          } else {
            Seq.empty
          }
        }
      val formAssocEdge = candidates.flatMap(hyponym =>
        graph.getFormAssocEdge(hyponym, role)).headOption match
      {
        case Some(formEdge) => formEdge
        case _ => {
          cosmos.addFormAssoc(possessor.form, role)
        }
      }
      val possessorIdeal = graph.getPossessorIdeal(formAssocEdge)
      if (!graph.isFormCompatibleWithIdeal(possessor.form, possessorIdeal)) {
        assert(possessor.form.isTentative)
        possessorIdeal match {
          case possessorForm : SpcForm => {
            addIdealTaxonomy(sentence, possessor.form, possessorForm)
          }
          case possessorRole : SpcRole => {
            graph.getFormsForRole(possessorRole).foreach(possessorForm =>
              addIdealTaxonomy(sentence, possessor.form, possessorForm))
          }
        }
      }
      // FIXME it may not be correct to assume same identity in the case
      // of a multi-valued association
      findTentativePossessee(possessor, formAssocEdge) match {
        case Some(tentativePossessee) => {
          cosmos.replaceEntity(tentativePossessee, possessee)
        }
        case _ => {
          validateEdgeCardinality(sentence, formAssocEdge, possessor)
        }
      }
      cosmos.getInverseAssocEdge(formAssocEdge) match {
        case Some(inverseAssocEdge) => {
          validateEdgeCardinality(sentence, inverseAssocEdge, possessee)
          cosmos.addEntityAssocEdge(
            possessor, possessee, formAssocEdge)
          cosmos.addEntityAssocEdge(
            possessee, possessor, inverseAssocEdge)
        }
        case _ => {
          cosmos.addEntityAssocEdge(
            possessor, possessee, formAssocEdge)
        }
      }
    }
  }

  beliefApplier {
    case InverseAssocBelief(
      sentence, possessorFormName, possessorRoleName, possesseeRoleNames
    ) => {
      val possessorForm = cosmos.instantiateForm(possessorFormName)
      val possessorRole = cosmos.instantiateRole(possessorRoleName)
      addIdealTaxonomy(sentence, possessorRole, possessorForm)
      possesseeRoleNames.foreach(possesseeRoleName => {
        val possesseeRole = cosmos.instantiateRole(possesseeRoleName)
        val edge = cosmos.addFormAssoc(
          possessorRole, possesseeRole)
        val inverseEdge = cosmos.addFormAssoc(
          possesseeRole, possessorRole)
        cosmos.connectInverseAssocEdges(edge, inverseEdge)
      })
    }
  }

  beliefApplier {
    case FormPropertyBelief(
      sentence, formName, newStates, isClosed, propertyNameOpt
    ) => {
      val form = cosmos.instantiateForm(formName)
      val property = propertyNameOpt match {
        case Some(propertyName) => {
          cosmos.instantiateProperty(form, propertyName)
        }
        case _ => {
          val properties = newStates.flatMap(
            w => cosmos.resolveFormProperty(form, w.lemma).map(_._1).toSeq)
          properties match {
            case Seq() => {
              cosmos.instantiateProperty(
                form,
                SilWord(formName.lemma + "_" +
                  newStates.map(_.lemma).mkString("_")))
            }
            case Seq(p) => {
              // FIXME:  if we add more states to an existing property,
              // we should rename the property too
              p
            }
            case _ => {
              // maybe unify multiple properties??
              throw new UnimplementedBeliefExcn(sentence)
            }
          }
        }
      }
      val baselineProperty = propertyNameOpt match {
        case Some(propertyName) => {
          cosmos.findProperty(form, propertyName.lemma).getOrElse(property)
        }
        case _ => {
          val hyperProperties = newStates.flatMap(
            w => cosmos.resolveHypernymProperty(form, w.lemma).
              map(_._1).toSeq)
          hyperProperties match {
            case Seq() => property
            case Seq(hyperProperty) => hyperProperty
            case _ => {
              throw new UnimplementedBeliefExcn(sentence)
            }
          }
        }
      }
      if (baselineProperty.isClosed) {
        if (!newStates.map(_.lemma).toSet.subsetOf(
          cosmos.getPropertyStateMap(baselineProperty).keySet))
        {
          throw new ContradictoryBeliefExcn(
            sentence,
            creed.formPropertyBelief(form, baselineProperty))
        }
      }
      val existingStates = cosmos.getPropertyStateMap(property)
      val statesToAdd = newStates.filterNot(
        word => existingStates.contains(word.lemma))
      statesToAdd.foreach(cosmos.instantiatePropertyState(property, _))
      if (isClosed || baselineProperty.isClosed) {
        cosmos.closePropertyStates(property)
      }
    }
  }

  beliefApplier {
    case ConsequenceBelief(
      sentence
    ) => {
      cosmos.addTrigger(sentence)
    }
  }
}
