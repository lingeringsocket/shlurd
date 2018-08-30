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

import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.parser._

import scala.collection._
import scala.collection.JavaConverters._

import org.jgrapht.alg.shortestpath._

class SpcBeliefInterpreter(
  cosmos : SpcCosmos,
  allowUpdates : Boolean = false,
  resultCollector : SmcResultCollector[SpcEntity] = SmcResultCollector())
    extends SpcBeliefRecognizer(cosmos, resultCollector)
{
  type BeliefApplier = PartialFunction[SpcBelief, Unit]

  private val beliefAppliers = new mutable.ArrayBuffer[BeliefApplier]

  private lazy val allBeliefApplier = beliefAppliers.reduceLeft(_ orElse _)

  private var finished = false

  def interpretBelief(sentence : SilSentence)
  {
    assert(!finished)
    finished = true
    recognizeBeliefs(sentence) match {
      case beliefs : Seq[SpcBelief] if (!beliefs.isEmpty) => {
        beliefs.foreach(applyBelief)
      }
      case _ => {
        throw new IncomprehensibleBeliefExcn(sentence)
      }
    }
  }

  def applyBelief(belief : SpcBelief)
  {
    allBeliefApplier.apply(belief)
  }

  private def getUniqueEntity(
    sentence : SilSentence,
    entities : Iterable[SpcEntity]) : Option[SpcEntity] =
  {
    if (entities.size == 1) {
      Some(entities.head)
    } else if (entities.size > 1) {
      val originalBelief = conjunctiveBelief(
        entities.toSeq.map(creed.entityFormBelief))
      // FIXME this conjunction may come out way too long, and may
      // also be phrased confusingly depending on what objects exist.
      throw new AmbiguousBeliefExcn(sentence, originalBelief)
    } else {
      None
    }
  }

  private def resolveReference(
    sentence : SilSentence,
    ref : SilReference) : SpcEntity =
  {
    resultCollector.referenceMap.get(ref).map(entities =>
      getUniqueEntity(sentence, entities).getOrElse(
        throw new IncomprehensibleBeliefExcn(sentence))).getOrElse(
      ref match {
        case SilNounReference(noun, determiner, _) => {
          resolveUniqueNameAndExistence(sentence, noun, determiner)._1
        }
        case _ => {
          throw new IncomprehensibleBeliefExcn(sentence)
        }
      }
    )
  }

  private def resolveUniqueNameAndExistence(
    sentence : SilSentence,
    noun : SilWord,
    determiner : SilDeterminer = DETERMINER_UNSPECIFIED)
      : (SpcEntity, Boolean) =
  {
    determiner match {
      case DETERMINER_UNIQUE => {
        val form = cosmos.instantiateForm(noun)
        // if there was an existing match, it would already
        // be cached
        assert(cosmos.getFormHyponymRealizations(form).isEmpty)
        val (entity, success) = cosmos.instantiateEntity(
          form, Seq.empty)
        assert(success)
        (entity, success)
      }
      case DETERMINER_UNSPECIFIED => {
        cosmos.getEntityBySynonym(noun.lemma) match {
          case Some(entity) => (entity, false)
          case _ => {
            val tentativeName = SpcForm.tentativeName(noun)
            assert(cosmos.resolveForm(tentativeName.lemma).isEmpty)
            val newForm = cosmos.instantiateForm(tentativeName)
            val (entity, success) = cosmos.instantiateEntity(
              newForm, Seq(noun), noun.lemmaUnfolded)
            assert(success)
            (entity, success)
          }
        }
      }
      case _ => {
        throw new IncomprehensibleBeliefExcn(sentence)
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
      outgoingEdgesOf(possessor).asScala.toSeq.
      filter(_.formEdge == formAssocEdge)

    val edgeCount = edges.size
    if (edgeCount >= constraint.upper) {
      if ((constraint.upper == 1) && allowUpdates) {
        // FIXME if the existence of this edge supports other beliefs
        // or inferences, then we have to deal with the fallout
        removeEntityAssocEdges(possessor, formAssocEdge, edges)
      } else {
        val originalBelief = conjunctiveBelief(
          Seq(creed.idealAssociationBelief(formAssocEdge)) ++
            edges.map(creed.entityAssociationBelief))
        throw new IncrementalCardinalityExcn(
          sentence, originalBelief)
      }
    }
  }

  private def conjunctiveBelief(sentences : Seq[SilSentence]) : SilSentence =
  {
    // FIXME use commas instead
    SilConjunctiveSentence(
      DETERMINER_ALL,
      sentences,
      SEPARATOR_CONJOINED)
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
    if (hyponymIdeal == hypernymIdeal) {
      return
    }
    val path = DijkstraShortestPath.findPathBetween(
      cosmos.getIdealTaxonomyGraph, hypernymIdeal, hyponymIdeal)
    if (path == null) {
      cosmos.addIdealTaxonomy(
        hyponymIdeal, hypernymIdeal)
    } else {
      val originalBelief = conjunctiveBelief(
        path.getEdgeList.asScala.toSeq.map(creed.idealTaxonomyBelief))
      throw new ContradictoryBeliefExcn(
        sentence,
        originalBelief)
    }
  }

  private def removeEntityAssocEdges(
    possessor : SpcEntity,
    formAssocEdge : SpcFormAssocEdge,
    edges : Seq[SpcEntityAssocEdge])
  {
    val entityAssocGraph = cosmos.getEntityAssocGraph
    cosmos.getInverseAssocEdge(formAssocEdge) match {
      case Some(inverseAssocEdge) => {
        val inverseEdges = entityAssocGraph.
          incomingEdgesOf(possessor).asScala.
          filter(_.formEdge == inverseAssocEdge)
        inverseEdges.foreach(cosmos.removeEntityAssocEdge)
      }
      case _ =>
    }
    edges.foreach(cosmos.removeEntityAssocEdge)
  }

  private def analyzeAssoc(
    sentence : SilSentence, possessor : SpcEntity, roleName : SilWord) =
  {
    val graph = cosmos.getGraph
    val role = cosmos.resolveRole(roleName.lemma) match {
      case Some(r) => r
      case _ => {
        cosmos.instantiateRole(roleName)
      }
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
    (formAssocEdge, possessorIdeal, role)
  }

  private def instantiatePropertyStates(
    sentence : SilSentence,
    form : SpcForm,
    newStates : Seq[SilWord],
    isClosed : Boolean,
    propertyNameOpt : Option[SilWord]) =
  {
    lazy val property = propertyNameOpt match {
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
              SilWord(form.name + "_" +
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
          w => cosmos.resolveHypernymPropertyState(form, w.lemma).
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
    property
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
            throw new IncomprehensibleBeliefExcn(sentence)
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
      sentence, entityRef, formName, qualifiers, properName, true
    ) => {
      val form = cosmos.instantiateForm(formName)
      val (entity, isNewEntity, determiner) =
        resultCollector.referenceMap.get(entityRef).map(entities =>
          getUniqueEntity(sentence, entities).map(
            entity => (entity, false, DETERMINER_UNIQUE)
          ).getOrElse(
            throw new IncomprehensibleBeliefExcn(sentence))
        ).getOrElse(
          entityRef match {
            case SilNounReference(noun, determiner, count) => {
              val (entity, isNewEntity) = {
                if (determiner == DETERMINER_UNIQUE) {
                  assert(properName.isEmpty)
                  resolveUniqueNameAndExistence(
                    sentence, noun, determiner)
                } else {
                  cosmos.instantiateEntity(
                    form, qualifiers, properName)
                }
              }
              (entity, isNewEntity, determiner)
            }
            case _ => {
              throw new IncomprehensibleBeliefExcn(sentence)
            }
          }
        )
      if (!isNewEntity) {
        val graph = cosmos.getGraph
        if (form == entity.form) {
          if (properName.isEmpty) {
            throw new AmbiguousBeliefExcn(
              sentence, creed.entityFormBelief(entity))
          }
        } else if (graph.isHyponym(entity.form, form)) {
          // from "Bessie is a cow" to "Bessie is an animal"
          // so nothing to do
        } else if (graph.isHyponym(form, entity.form)) {
          // from "Bessie is an animal" to "Bessie is a cow"
          // so need to replace with refinement
          transmogrify(sentence, entity, form)
        } else if (entity.form.isTentative) {
          // from "Bessie is a Bessie-form" to "Bessie is a cow"
          transmogrify(sentence, entity, form)
        } else {
          // from "Bessie is a dog" to "Bessie is a cow"
          // FIXME:  initiate dialog with user to sort it out,
          // e.g. "is a dog a kind of cow?"
          throw new ContradictoryBeliefExcn(
            sentence, creed.entityFormBelief(entity))
        }
      } else if (determiner == DETERMINER_UNIQUE) {
        // FIXME this has the unfortunate side-effect that
        // "the dog is a werewolf" implies "all dogs are werewolves"
        addIdealTaxonomy(sentence, entity.form, form)
      }
    }
  }

  beliefApplier {
    case EntityExistenceBelief(
      sentence, entityRef, _, _, _, false
    ) => {
      val entity = resolveReference(
        sentence, entityRef)
      cosmos.forgetEntity(entity)
    }
  }

  beliefApplier {
    case EntityNoAssocBelief(
      sentence, possessorRef, roleName
    ) => {
      val possessor = resolveReference(
        sentence, possessorRef)
      val (formAssocEdge, possessorIdeal, role) =
        analyzeAssoc(sentence, possessor, roleName)
      val entityAssocGraph = cosmos.getEntityAssocGraph
      val edges = entityAssocGraph.
        outgoingEdgesOf(possessor).asScala.toSeq.
        filter(_.formEdge == formAssocEdge)
      if (!edges.isEmpty && !allowUpdates) {
        // FIXME also, in the !allowUpdates case, henceforth we should
        // reject any attempt to add a matching edge
        val originalBelief = conjunctiveBelief(
          edges.map(creed.entityAssociationBelief))
        throw new ContradictoryBeliefExcn(
          sentence,
          originalBelief)
      }
      removeEntityAssocEdges(possessor, formAssocEdge, edges)
    }
  }

  beliefApplier {
    case EntityPropertyBelief(
      sentence, reference, propertyName, stateName
    ) => {
      val entity = resolveReference(sentence, reference)
      val form = entity.form
      val propertyOpt = propertyName match {
        case Some(word) => {
          cosmos.findProperty(form, word.lemma) match {
            case Some(property) => {
              if (cosmos.getPropertyStateObjMap(property).
                contains(stateName.lemma))
              {
                Some((property, stateName.lemma))
              } else {
                None
              }
            }
            case _ => None
          }
        }
        case _ => {
          cosmos.resolveHypernymPropertyState(form, stateName.lemma)
        }
      }
      val (property, actualState) = propertyOpt.getOrElse({
        val p = instantiatePropertyStates(
          sentence, form, Seq(stateName), false, propertyName)
        (p, stateName.lemma)
      })
      // FIXME need to honor allowUpdates
      cosmos.updateEntityProperty(entity, property, actualState)
    }
  }

  beliefApplier {
    case EntityAssocBelief(
      sentence, possessorRef, possesseeRef, roleName, positive
    ) => {
      val possessor = resolveReference(
        sentence, possessorRef)
      val possessee = resolveReference(
        sentence, possesseeRef)
      val (formAssocEdge, possessorIdeal, role) =
        analyzeAssoc(sentence, possessor, roleName)
      val graph = cosmos.getGraph
      if (positive) {
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
      }
      // FIXME if (!positive && !allowUpdates), then we
      // should fail if existing assoc, otherwise
      // prevent future constraint violations
      cosmos.getInverseAssocEdge(formAssocEdge) match {
        case Some(inverseAssocEdge) => {
          if (positive) {
            validateEdgeCardinality(sentence, inverseAssocEdge, possessee)
            cosmos.addEntityAssocEdge(
              possessor, possessee, formAssocEdge)
            cosmos.addEntityAssocEdge(
              possessee, possessor, inverseAssocEdge)
          } else {
            cosmos.removeEntityAssociation(
              possessor, possessee, formAssocEdge)
            cosmos.removeEntityAssociation(
              possessee, possessor, inverseAssocEdge)
          }
        }
        case _ => {
          if (positive) {
            cosmos.addEntityAssocEdge(
              possessor, possessee, formAssocEdge)
          } else {
            cosmos.removeEntityAssociation(
              possessor, possessee, formAssocEdge)
          }
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
      instantiatePropertyStates(
        sentence, form, newStates, isClosed, propertyNameOpt)
    }
  }

  beliefApplier {
    case ConsequenceBelief(
      sentence
    ) => {
      cosmos.addTrigger(sentence)
    }
  }

  beliefApplier {
    case EpsilonBelief(
      sentence
    ) => {
    }
  }
}
