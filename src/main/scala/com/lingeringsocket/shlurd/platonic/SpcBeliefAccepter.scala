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
import scala.collection.JavaConverters._
import scala.util._

import org.jgrapht.alg.shortestpath._

import SprEnglishLemmas._

object SpcBeliefAccepter
{
  def apply(
    responder : SpcResponder,
    params : SpcBeliefParams,
    resultCollector : SpcResultCollector)
      : SpcBeliefAccepter =
  {
    new SpcBeliefAccepter(
      responder,
      params,
      resultCollector)
  }
}

sealed trait SpcBeliefAcceptance
case object IGNORE_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_NO_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_NEW_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_MODIFIED_BELIEFS extends SpcBeliefAcceptance

case class SpcBeliefParams(
  acceptance : SpcBeliefAcceptance = ACCEPT_NEW_BELIEFS,
  createImplicitIdeals : Boolean = true,
  createTentativeIdeals : Boolean = true,
  createTentativeEntities : Boolean = true,
  createImplicitProperties : Boolean = true
)
{
}

class SpcBeliefAccepter private(
  responder : SpcResponder,
  params : SpcBeliefParams,
  resultCollector : SpcResultCollector)
    extends SpcBeliefRecognizer(responder, resultCollector)
{
  type BeliefApplier = PartialFunction[SpcBelief, Unit]

  private val mind = responder.getMind

  private val beliefAppliers = new mutable.ArrayBuffer[BeliefApplier]

  private lazy val allBeliefApplier = beliefAppliers.reduceLeft(_ orElse _)

  private var finished = false

  def processBelief(sentence : SilSentence)
  {
    assert(!finished)
    finished = true
    recognizeBeliefs(sentence) match {
      case beliefs : Seq[SpcBelief] if (!beliefs.isEmpty) => {
        beliefs.foreach(applyBelief)
      }
      case _ => {
        throw new IncomprehensibleBeliefExcn(
          ShlurdExceptionCode.IncomprehensibleBelief,
          sentence)
      }
    }
  }

  def applyBelief(belief : SpcBelief)
  {
    if (params.acceptance == ACCEPT_NO_BELIEFS) {
      throw new ProhibitedBeliefExcn(
        ShlurdExceptionCode.NewBeliefsProhibited,
        belief.sentence)
    }
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

      // FIXME better to use the same phrasing as normal responder

      throw new AmbiguousBeliefExcn(
        ShlurdExceptionCode.NotUnique,
        sentence, originalBelief)
    } else {
      None
    }
  }

  private def resolveReference(
    sentence : SilSentence,
    ref : SilReference) : SpcEntity =
  {
    resultCollector.lookup(ref).map(entities =>
      getUniqueEntity(sentence, entities).getOrElse(
        throw new IncomprehensibleBeliefExcn(
          ShlurdExceptionCode.NonExistent,
          sentence))).getOrElse(
      ref match {
        case SilOptionallyDeterminedReference(
          SilNounReference(noun),
          determiner
        ) => {
          resolveUniqueNameAndExistence(sentence, noun, determiner)._1
        }
        case _ => {
          throw new IncomprehensibleBeliefExcn(
            ShlurdExceptionCode.ReferenceNotYetImplemented,
            sentence)
        }
      }
    )
  }

  private def resolveUniqueNameAndExistence(
    sentence : SilSentence,
    noun : SilWord,
    determiner : SilDeterminer)
      : (SpcEntity, Boolean) =
  {
    determiner match {
      case DETERMINER_DEFINITE => {
        val form = instantiateForm(sentence, noun)
        val (entity, success) = cosmos.instantiateEntity(
          form, Seq.empty)
        tupleN((entity, success))
      }
      case DETERMINER_ABSENT => {
        getUniqueEntity(
          sentence,
          cosmos.getEntitiesBySynonym(
            cosmos.synthesizeEntitySynonym(noun.toNounLemma))
        ) match {
          case Some(entity) => {
            tupleN((entity, false))
          }
          case _ => {
            val tentativeName = SpcForm.tentativeName(noun)
            assert(mind.resolveForm(tentativeName).isEmpty)
            val newForm = instantiateForm(sentence, tentativeName)
            val (entity, success) = cosmos.instantiateEntity(
              newForm, Seq(noun), noun.toUnfoldedLemma)
            assert(success, tupleN((noun, entity)))
            tupleN((entity, success))
          }
        }
      }
      case _ => {
        throw new IncomprehensibleBeliefExcn(
          ShlurdExceptionCode.ReferenceNotYetImplemented,
          sentence)
      }
    }
  }

  def allowUpdates() : Boolean =
    (params.acceptance == ACCEPT_MODIFIED_BELIEFS)

  private def validateEdgeCardinality(
    sentence : SilSentence,
    formAssocEdge : SpcFormAssocEdge,
    possessor : SpcEntity,
    possessee : SpcEntity)
  {
    if (cosmos.isBulkLoad) {
      return
    }
    val constraint = formAssocEdge.constraint
    val entityAssocGraph = cosmos.getEntityAssocGraph
    val edges = entityAssocGraph.
      outgoingEdgesOf(possessor).asScala.toSeq.
      filter(_.getRoleName == formAssocEdge.getRoleName)

    val edgeCount = edges.count(edge =>
      (cosmos.getGraph.getPossesseeEntity(edge) != possessee))
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
          ShlurdExceptionCode.CardinalityConstraint,
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
    val newEntity = SpcPersistentEntity(
      entity.name, newForm, entity.qualifiers, entity.properName)
    if (oldForm.isTentative) {
      val matchedAssocs = cosmos.matchAssocs(oldForm, newForm)
      matchedAssocs.foreach(pair => {
        // FIXME:  the "original belief" in this exception
        // isn't terribly helpful
        if (!cosmos.isValidMergeAssoc(pair._1, pair._2)) {
          throw new IncrementalCardinalityExcn(
            ShlurdExceptionCode.CardinalityConstraint,
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
      assert(cosmos.isHyponym(newForm, oldForm))
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
        ShlurdExceptionCode.TaxonomyCycle,
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
    cosmos.getInverseAssocEdge(formAssocEdge) matchPartial {
      case Some(inverseAssocEdge) => {
        val inverseEdges = entityAssocGraph.
          incomingEdgesOf(possessor).asScala.
          filter(_.getRoleName == inverseAssocEdge.getRoleName)
        inverseEdges.foreach(cosmos.removeEntityAssocEdge)
      }
    }
    edges.foreach(cosmos.removeEntityAssocEdge)
  }

  private def analyzeAssoc(
    sentence : SilSentence, possessor : SpcEntity, roleName : SilWord) =
  {
    val graph = cosmos.getGraph
    val (role, isNewRole) = instantiateRole(
      sentence, possessor.form, roleName, true)
    val candidates =
      Seq(possessor.form) ++ cosmos.getRolesForForm(possessor.form) ++ {
        if (possessor.form.isTentative) {
          graph.formAssocs.incomingEdgesOf(role).asScala.
            toSeq.map(graph.getPossessorForm)
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
    val possessorForm = graph.getPossessorForm(formAssocEdge)
    if (!cosmos.isFormCompatibleWithIdeal(possessor.form, possessorForm)) {
      assert(possessor.form.isTentative)
      addIdealTaxonomy(sentence, possessor.form, possessorForm)
    }
    tupleN((formAssocEdge, possessorForm, role))
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
        val properties = newStates.flatMap(_.decomposed).flatMap(
          w => cosmos.resolveFormProperty(
            form, w.lemma).map(_._1).toSeq)
        properties match {
          case Seq() => {
            cosmos.instantiateProperty(
              form,
              SilWord(form.name + "_" +
                newStates.flatMap(_.decomposed).map(_.lemma).mkString("_")))
          }
          case Seq(p) => {
            // FIXME:  if we add more states to an existing property,
            // we should rename the property too
            p
          }
          case _ => {
            // maybe unify multiple properties??
            throw new UnimplementedBeliefExcn(
              ShlurdExceptionCode.OverlappingProperties,
              sentence)
          }
        }
      }
    }
    val baselineProperty = propertyNameOpt match {
      case Some(propertyName) => {
        cosmos.findProperty(
          form, cosmos.encodeName(propertyName)).getOrElse(property)
      }
      case _ => {
        val hyperProperties = newStates.flatMap(
          w => cosmos.resolveHypernymPropertyState(form, cosmos.encodeName(w)).
            map(_._1).toSeq)
        hyperProperties match {
          case Seq() => property
          case Seq(hyperProperty) => hyperProperty
          case _ => {
            throw new UnimplementedBeliefExcn(
              ShlurdExceptionCode.OverlappingProperties,
              sentence)
          }
        }
      }
    }
    val contradiction = baselineProperty.domain match {
      case PROPERTY_OPEN_ENUM => false
      case PROPERTY_CLOSED_ENUM => {
        !newStates.flatMap(_.decomposed).map(_.lemma).toSet.subsetOf(
          cosmos.getPropertyStateMap(baselineProperty).keySet)
      }
      case _ => true
    }
    if (contradiction) {
      throw new ContradictoryBeliefExcn(
        ShlurdExceptionCode.PropertyAlreadyClosed,
        sentence,
        creed.formPropertyBelief(form, baselineProperty))
    }
    val existingStates = cosmos.getPropertyStateMap(property)
    val statesToAdd = newStates.filterNot(
      word => existingStates.contains(cosmos.encodeName(word)))
    statesToAdd.foreach(cosmos.instantiatePropertyState(property, _))
    if (isClosed || baselineProperty.domain == PROPERTY_CLOSED_ENUM) {
      cosmos.closePropertyStates(property)
    }
    property
  }

  private def instantiateRole(
    sentence : SilSentence,
    possessorForm : SpcForm,
    idealName : SilWord,
    includeHypernyms : Boolean = false,
    isImplicit : Boolean = true) : (SpcRole, Boolean) =
  {
    mind.resolveRole(possessorForm, idealName, includeHypernyms) match {
      case Some(r) => tupleN((r, false))
      case _ => {
        if (isImplicit && !params.createImplicitIdeals) {
          throw new ProhibitedBeliefExcn(
            ShlurdExceptionCode.ImplicitIdealsProhibited,
            sentence)
        }
        tupleN((mind.instantiateRole(possessorForm, idealName), true))
      }
    }
  }

  private def instantiateForm(
    sentence : SilSentence, word : SilWord,
    isImplicit : Boolean = true) : SpcForm =
  {
    // FIXME pinpoint cause
    mind.resolveForm(word) match {
      case Some(form : SpcForm) => form
      case _ => {
        if (isImplicit && !params.createImplicitIdeals) {
          throw new ProhibitedBeliefExcn(
            ShlurdExceptionCode.ImplicitIdealsProhibited,
            sentence)
        }
        val newForm = mind.instantiateForm(word)
        if (newForm.isTentative && !params.createTentativeIdeals) {
          throw new ProhibitedBeliefExcn(
            ShlurdExceptionCode.TentativeIdealsProhibited,
            sentence)
        }
        newForm
      }
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
      throw new UnimplementedBeliefExcn(
        ShlurdExceptionCode.BeliefNotYetImplemented,
        sentence)
    }
  }

  beliefApplier {
    case InvalidBelief(
      sentence,
      exceptionCode
    ) => {
      throw new InvalidBeliefExcn(
        exceptionCode,
        sentence)
    }
  }

  beliefApplier {
    case IdealAliasBelief(
      sentence, synonym, idealName, possessorOpt
    ) => {
      possessorOpt match {
        case Some(possessorFormName) => {
          val possessorForm = mind.instantiateForm(possessorFormName)
          val (role, isNewRole) = instantiateRole(
            sentence, possessorForm, idealName, true)
          cosmos.addIdealSynonym(
            cosmos.synthesizeRoleSynonym(
              possessorForm, cosmos.encodeName(synonym)),
            role)
        }
        case _ => {
          val form = mind.resolveForm(
            idealName
          ).getOrElse {
            mind.instantiateForm(idealName)
          }
          cosmos.addIdealSynonym(cosmos.encodeName(synonym), form)
        }
      }
    }
  }

  beliefApplier {
    case FormTaxonomyBelief(
      sentence, hyponymFormName, hypernymFormName
    ) => {
      // FIXME need to make sure all hypernyms are (and remain) compatible
      // FIXME also need to allow existing form to be refined
      val hypernymForm = instantiateForm(sentence, hypernymFormName)
      val hyponymForm = instantiateForm(sentence, hyponymFormName, false)
      addIdealTaxonomy(sentence, hyponymForm, hypernymForm)
    }
  }

  beliefApplier {
    case RoleTaxonomyBelief(
      sentence, possessorFormName, hyponymRoleName, hypernymIdealName,
      isRefinement
    ) => {
      // FIXME need to make sure all hypernyms are (and remain) compatible
      val possessorForm = instantiateForm(sentence, possessorFormName)
      val hypernymIdeal = {
        if (isRefinement) {
          mind.resolveRole(possessorForm, hypernymIdealName).getOrElse {
            throw new IncomprehensibleBeliefExcn(
              ShlurdExceptionCode.RoleHypernymNonExistent,
              sentence)
          }
        } else {
          instantiateForm(sentence, hypernymIdealName)
        }
      }
      val existingFormOpt = mind.resolveForm(hyponymRoleName)
      existingFormOpt.foreach(form => {
        if (!cosmos.isHyponym(hypernymIdeal, form)) {
          throw new IncomprehensibleBeliefExcn(
            ShlurdExceptionCode.RoleHyponymConflictsWithForm,
            sentence)
        }
      })
      if (isRefinement) {
        if (mind.resolveRole(possessorForm, hyponymRoleName, false).nonEmpty) {
          // FIXME instead of failing, merge the associations
          throw new IncomprehensibleBeliefExcn(
            ShlurdExceptionCode.RoleHyponymAlreadyExists,
            sentence)
        }
      }
      val (hyponymRole, existingRole) = mind.resolveRole(
        possessorForm, hyponymRoleName, false
      ) match {
        case Some(r) => tupleN((r, true))
        case _ => tupleN((
          mind.instantiateRole(possessorForm, hyponymRoleName),
          false))
      }
      if (existingRole) {
        val entityAssocs = cosmos.getEntityAssocGraph
        // FIXME avoid iterating over all entity assocs!
        val entityAssocEdges = entityAssocs.edgeSet.asScala
        if (entityAssocEdges.size > 1000) {
          warn("BOOM:  " + entityAssocEdges.size)
        }
        entityAssocEdges.foreach(
          entityEdge => {
            val possesseeRole = cosmos.getPossesseeRole(entityEdge)
            if (possesseeRole == hyponymRole) {
              val possesseeEntity =
                cosmos.getGraph.getPossesseeEntity(entityEdge)
              if (!cosmos.isHyponym(
                possesseeEntity.form, hypernymIdeal))
              {
                if (possesseeEntity.form.isTentative) {
                  addIdealTaxonomy(
                    sentence, possesseeEntity.form, hypernymIdeal)
                } else {
                  val formBelief = creed.entityFormBelief(possesseeEntity)
                  val assocBelief = creed.entityAssociationBelief(entityEdge)
                  throw new ContradictoryBeliefExcn(
                    ShlurdExceptionCode.RoleTaxonomyIncompatible,
                    sentence,
                    conjunctiveBelief(Seq(formBelief, assocBelief)))
                }
              }
            }
          }
        )
      }
      addIdealTaxonomy(sentence, hyponymRole, hypernymIdeal)
      cosmos.addFormAssoc(possessorForm, hyponymRole)
    }
  }

  beliefApplier {
    case FormAssocBelief(
      sentence, possessorFormName, possesseeRoleNames,
      newConstraint
    ) => {
      val possessorForm = instantiateForm(sentence, possessorFormName)
      possesseeRoleNames.foreach(possesseeRoleName => {
        val existingHypernymOpt = mind.resolveForm(possesseeRoleName)
        val possibleRefinement =
          mind.resolveRole(possessorForm, possesseeRoleName, true).nonEmpty
        val (possesseeRole, isNewRole) = instantiateRole(
          sentence, possessorForm, possesseeRoleName,
          false, existingHypernymOpt.isEmpty)
        val edge = cosmos.addFormAssoc(
          possessorForm, possesseeRole)
        val oldConstraint = edge.constraint
        val constraint = SpcCardinalityConstraint(
          Math.max(oldConstraint.lower, newConstraint.lower),
          Math.min(oldConstraint.upper, newConstraint.upper))
        cosmos.annotateFormAssoc(edge, constraint)
        if (isNewRole && !possibleRefinement) {
          val hypernym = existingHypernymOpt.getOrElse {
            instantiateForm(sentence, possesseeRoleName)
          }
          addIdealTaxonomy(sentence, possesseeRole, hypernym)
        }
      })
    }
  }

  beliefApplier {
    case EntityExistenceBelief(
      sentence, entityRef, formName, qualifiers, properName, true
    ) => {
      val form = instantiateForm(sentence, formName, false)
      val (entity, isNewEntity, determiner) =
        resultCollector.lookup(entityRef).map(entities =>
          getUniqueEntity(sentence, entities).map(
            entity => (entity, false, DETERMINER_DEFINITE)
          ).getOrElse(
            throw new IncomprehensibleBeliefExcn(
              ShlurdExceptionCode.AmbiguousInterpretation,
              sentence))
        ).getOrElse(
          entityRef match {
            case SilOptionallyDeterminedReference(
              SilCountedNounReference(noun, count),
              determiner
            ) => {
              val (entity, isNewEntity) = {
                if (determiner == DETERMINER_DEFINITE) {
                  assert(properName.isEmpty)
                  resolveUniqueNameAndExistence(
                    sentence, noun, determiner)
                } else {
                  cosmos.instantiateEntity(
                    form, qualifiers, properName)
                }
              }
              tupleN((entity, isNewEntity, determiner))
            }
            case _ => {
              throw new IncomprehensibleBeliefExcn(
                ShlurdExceptionCode.ReferenceNotYetImplemented,
                sentence)
            }
          }
        )
      if (!isNewEntity) {
        if (form == entity.form) {
          if (properName.isEmpty) {
            throw new AmbiguousBeliefExcn(
              ShlurdExceptionCode.AmbiguousInterpretation,
              sentence, creed.entityFormBelief(entity))
          }
        } else if (!entity.form.isTentative &&
          cosmos.isHyponym(entity.form, form))
        {
          // from "Bessie is a cow" to "Bessie is an animal"
          // so nothing to do
        } else if (cosmos.isHyponym(form, entity.form)) {
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
            ShlurdExceptionCode.FormTaxonomyIncompatible,
            sentence, creed.entityFormBelief(entity))
        }
      } else if (determiner == DETERMINER_DEFINITE) {
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
        filter(_.getRoleName == formAssocEdge.getRoleName)
      if (!edges.isEmpty && !allowUpdates) {
        // FIXME also, in the !allowUpdates case, henceforth we should
        // reject any attempt to add a matching edge
        val originalBelief = conjunctiveBelief(
          edges.map(creed.entityAssociationBelief))
        throw new ContradictoryBeliefExcn(
          ShlurdExceptionCode.AbsenceConstraint,
          sentence,
          originalBelief)
      }
      removeEntityAssocEdges(possessor, formAssocEdge, edges)
    }
  }

  beliefApplier {
    case EntityPropertyBelief(
      sentence, reference, propertyName, Left(stateName)
    ) => {
      val entity = resolveReference(sentence, reference)
      val form = entity.form
      val encodedStateName = cosmos.encodeName(stateName)
      val propertyOpt = propertyName match {
        case Some(word) => {
          cosmos.findProperty(form, cosmos.encodeName(word)).map(
            property => tupleN((property, encodedStateName))
          )
        }
        case _ => {
          cosmos.resolveHypernymPropertyState(
            form, encodedStateName)
        }
      }
      val propertyOptFiltered = propertyOpt.filter {
        case (property, stateName) => {
          cosmos.getPropertyStateObjMap(property).contains(stateName)
        }
      }

      val (property, actualState) = propertyOptFiltered.getOrElse({
        if (propertyOpt.isEmpty && !params.createImplicitProperties) {
          throw new ProhibitedBeliefExcn(
            ShlurdExceptionCode.ImplicitPropertiesProhibited,
            sentence)
        }
        val p = instantiatePropertyStates(
          sentence, form, Seq(stateName), false, propertyName)
        tupleN((p, encodedStateName))
      })
      // FIXME need to honor allowUpdates
      cosmos.updateEntityProperty(entity, property, actualState)
    }
  }

  beliefApplier {
    case EntityPropertyBelief(
      sentence, reference, Some(propertyName), Right(propertyValue)
    ) => {
      // FIXME need to honor allowUpdates
      val entity = resolveReference(sentence, reference)
      val form = entity.form
      val property = cosmos.findProperty(
        form, cosmos.encodeName(propertyName)).getOrElse
      {
        // FIXME if allowed, create implicit property
        throw new ProhibitedBeliefExcn(
          ShlurdExceptionCode.ImplicitPropertiesProhibited,
          sentence)
      }
      // FIXME verify that propertyValue matches property's domain
      cosmos.updateEntityProperty(entity, property, propertyValue)
    }
  }

  beliefApplier {
    case EntityAssocBelief(
      sentence, possessorRef, possesseeRef, instantiation, roleName, positive
    ) => {
      val possessor = resolveReference(
        sentence, possessorRef)

      var newEntityRef = possesseeRef

      // FIXME for non-existing instantiation, we should check whether
      // an existing entity satisfies the state rather than assuming
      // that it specifies a new entity
      val (possesseeOpt, stateOpt) = {
        if (instantiation != ENTITY_ASSOC_EXISTING) {
          possesseeRef match {
            case SilOptionallyDeterminedReference(
              SilStateSpecifiedReference(ref, SilExistenceState(_)),
              _
            ) => {
              newEntityRef = ref
              tupleN((None, None))
            }
            case SilOptionallyDeterminedReference(
              SilStateSpecifiedReference(ref, state),
              _
            ) => {
              newEntityRef = ref
              tupleN((None, Some(state)))
            }
            case ref => {
              tupleN((Some(resolveReference(sentence, ref)), None))
            }
          }
        } else {
          tupleN((Some(resolveReference(sentence, possesseeRef)), None))
        }
      }
      val (formAssocEdge, possessorIdeal, role) =
        analyzeAssoc(sentence, possessor, roleName)
      val possessee = possesseeOpt.getOrElse {
        getUniqueEntity(
          sentence,
          cosmos.reifyRole(
            possessor, role, instantiation, false, stateOpt.nonEmpty)).get
      }

      if (!params.createTentativeEntities && possessee.isTentative) {
        throw new ProhibitedBeliefExcn(
          ShlurdExceptionCode.TentativeEntitiesProhibited,
          sentence)
      }
      val graph = cosmos.getGraph
      if (positive) {
        if (possessee.form.isTentative) {
          graph.getFormsForRole(role).foreach(form =>
            addIdealTaxonomy(sentence, possessee.form, form))
        }
        if (!cosmos.isFormCompatibleWithRole(possessee.form, role)) {
          val originalBelief = conjunctiveBelief(
            creed.roleTaxonomyBeliefs(role).toSeq)
          throw new ContradictoryBeliefExcn(
            ShlurdExceptionCode.FormRoleIncompatible,
            sentence,
            originalBelief)
        }

        // FIXME there is ambiguity here; if I mention Marito's aunt,
        // and then later in a different context mention that Julia is
        // Marito's aunt, do I mean that Marito has two aunts or just one?
        // Currently we assume two.
        if (formAssocEdge.constraint.upper > 1) {
          validateEdgeCardinality(
            sentence, formAssocEdge, possessor, possessee)
        } else {
          findTentativePossessee(possessor, formAssocEdge) match {
            case Some(tentativePossessee) => {
              cosmos.replaceEntity(tentativePossessee, possessee)
            }
            case _ => {
              validateEdgeCardinality(
                sentence, formAssocEdge, possessor, possessee)
            }
          }
        }
      }
      // FIXME if (!positive && !allowUpdates), then we
      // should fail if existing assoc, otherwise
      // prevent future constraint violations
      cosmos.getInverseAssocEdge(formAssocEdge) match {
        case Some(inverseAssocEdge) => {
          if (positive) {
            validateEdgeCardinality(
              sentence, inverseAssocEdge, possessee, possessor)
            cosmos.addEntityAssocEdge(
              possessor, possessee, role)
            // FIXME should do this by role instead of edge in order
            // to correctly handle refinements
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
              possessor, possessee, role)
          } else {
            cosmos.removeEntityAssociation(
              possessor, possessee, formAssocEdge)
          }
        }
      }
      stateOpt.foreach(state => {
        resultCollector.refMap.put(newEntityRef, Set(possessee))
        val statePredicate = SilStatePredicate(
          newEntityRef, STATE_PREDEF_BE.toVerb, state)
        val stateBeliefs = decomposeAndRecognizeStatePredicate(
          sentence,
          SilTam.indicative,
          statePredicate)
        assert(stateBeliefs.nonEmpty)
        stateBeliefs.foreach(applyBelief)
      })
    }
  }

  beliefApplier {
    case InverseAssocBelief(
      sentence,
      possessorFormName, possessorRoleName,
      possesseeFormName, possesseeRoleName
    ) => {
      val possessorForm = instantiateForm(sentence, possessorFormName)
      val possesseeForm = instantiateForm(sentence, possesseeFormName)
      val (possessorRole, isNewPossessor) = instantiateRole(
        sentence, possesseeForm, possessorRoleName)
      val (possesseeRole, isNewPossessee) = instantiateRole(
        sentence, possessorForm, possesseeRoleName)
      addIdealTaxonomy(sentence, possessorRole, possessorForm)
      addIdealTaxonomy(sentence, possesseeRole, possesseeForm)
      val edge = cosmos.addFormAssoc(
        possessorForm, possesseeRole)
      val inverseEdge = cosmos.addFormAssoc(
        possesseeForm, possessorRole)
      cosmos.connectInverseAssocEdges(edge, inverseEdge)
    }
  }

  beliefApplier {
    case FormEnumPropertyBelief(
      sentence, formName, newStates, isClosed, propertyNameOpt
    ) => {
      val form = instantiateForm(sentence, formName)
      instantiatePropertyStates(
        sentence, form, newStates, isClosed, propertyNameOpt)
    }
  }

  beliefApplier {
    case FormTypedPropertyBelief(
      sentence, formName, propertyName, domain
    ) => {
      val form = instantiateForm(sentence, formName)
      val property = cosmos.instantiateProperty(form, propertyName, domain)
      if (property.domain != domain) {
        throw new ContradictoryBeliefExcn(
          ShlurdExceptionCode.PropertyDomainIncompatible,
          sentence,
          creed.formPropertyBelief(form, property))
      }
    }
  }

  beliefApplier {
    case belief @ AssertionBelief(
      sentence,
      additionalConsequents,
      alternative
    ) => {
      val placeholderMapOpt = validateAssertion(belief)
      placeholderMapOpt.foreach(placeholderMap => {
        cosmos.addAssertion(
          SpcAssertion(
            sentence, additionalConsequents,
            alternative, placeholderMap))
      })
    }
  }

  def validateAssertion(
    belief : AssertionBelief) : Option[SpcRefMap] =
  {
    val implicationMapper = new SpcImplicationMapper(responder)
    belief.sentence match {
      case conditional : SilConditionalSentence => {
        val placeholderMap = implicationMapper.validateImplication(
          conditional, belief.additionalConsequents, belief.alternative)
        if (acceptSpecialAssertion(conditional, placeholderMap)) {
          None
        } else {
          if (
            (conditional.conjunction.toLemma == LEMMA_IF) &&
              isGenitiveRelationship(conditional.antecedent) &&
              isGenitiveRelationship(conditional.consequent)
          )
          {
            throw InvalidBeliefExcn(
              ShlurdExceptionCode.AssertionInvalidAssociation,
              belief.sentence)
          }
          Some(placeholderMap)
        }
      }
      case ps : SilPredicateSentence => {
        Some(implicationMapper.validateAssertionPredicate(
          SpcAnnotator(), belief.sentence, ps.predicate))
      }
      case _ => None
    }
  }

  def isAssertionValid(belief : AssertionBelief) : Boolean =
  {
    Try(validateAssertion(belief)).isSuccess
  }

  private def isGenitiveRelationship(predicate : SilPredicate) : Boolean =
  {
    predicate match {
      case rel : SilRelationshipPredicate => {
        if (rel.subject.isInstanceOf[SilGenitiveReference]
          || rel.complement.isInstanceOf[SilGenitiveReference])
        {
          (rel.verb.toLemma == REL_PREDEF_IDENTITY.toLemma) &&
          rel.modifiers.isEmpty
        } else {
          false
        }
      }
      case _ => false
    }
  }

  private def acceptSpecialAssertion(
    conditional : SilConditionalSentence,
    refMap : SpcRefMap) : Boolean =
  {
    conditional match {
      // "If a map-place is a map-connection's target-place,
      //   then equivalently the map-connection is
      //   the map-place's place-entrance."
      case SilConditionalSentence(
        SilWordLemma(LEMMA_IF),
        SilRelationshipPredicate(
          antecedentSubject,
          SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          antecedentComplement,
          Seq()),
        SilRelationshipPredicate(
          consequentSubject,
          SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          consequentComplement,
          verbModifiers),
        tam1,
        tam2,
        biconditional,
        _
      ) if (
        biconditional &&
        verbModifiers.isEmpty &&
          tam1 == SilTam.indicative &&
          tam2 == SilTam.indicative
      ) => {
        matchAssocInverse(
          antecedentSubject,
          antecedentComplement,
          consequentSubject,
          consequentComplement,
          refMap
        ) match {
          case Some(
            (possesseeForm, possessorForm,
              possesseeRole, possessorRole)
          ) => {
            applyBelief(InverseAssocBelief(
              conditional, possessorForm, possessorRole,
              possesseeForm, possesseeRole))
            true
          }
          case _ => false
        }
      }
      case _ => false
    }
  }

  private def matchAssocInverse(
    antecedentSubject : SilReference,
    antecedentComplement : SilReference,
    consequentSubject : SilReference,
    consequentComplement : SilReference,
    refMap : SpcRefMap
  ) : Option[(SilWord, SilWord, SilWord, SilWord)] =
  {
    val antecedentIdentity =
      matchAssocIdentity(antecedentSubject, antecedentComplement)
    val consequentIdentity =
      matchAssocIdentity(consequentSubject, consequentComplement)
    def samePlaceholders(ar : SilReference, cr : SilReference) : Boolean =
    {
      val ae = SpcImplicationMapper.extractPlaceholder(ar, refMap)
      val ce = SpcImplicationMapper.extractPlaceholder(cr, refMap)
      if (ae.nonEmpty) {
        ae == ce
      } else {
        false
      }
    }
    tupleN((antecedentIdentity, consequentIdentity)) match {
      case (Some((ar1, ar2, aw)), Some((cr1, cr2, cw))) => {
        if (samePlaceholders(ar1, cr2) &&
          samePlaceholders(ar2, cr1)
        ) {
          tupleN((
            SpcImplicationMapper.extractNoun(ar1),
            SpcImplicationMapper.extractNoun(ar2))
          ) match {
            case (Some(n1), Some(n2)) => {
              Some(tupleN((n1, n2, aw, cw)))
            }
            case _ => {
              None
            }
          }
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private def matchAssocIdentity(
    subject : SilReference,
    complement : SilReference,
    flipped : Boolean = false
  ) : Option[(SilReference, SilReference, SilWord)] =
  {
    (subject, complement) match {
      case (
        r,
        SilGenitiveReference(p, SilNounReference(w))
      ) => {
        Some((r, p, w))
      }
      case _ => {
        if (flipped) {
          None
        } else {
          matchAssocIdentity(complement, subject, true)
        }
      }
    }
  }

  beliefApplier {
    case UniquenessBelief(
      sentence,
      ref
    ) => {
      resolveReference(sentence, ref)
    }
  }

  beliefApplier {
    case IndirectBelief(
      sentence,
      resourceName
    ) => {
      mind.importBeliefs(resourceName, responder)
    }
  }

  beliefApplier {
    case EpsilonBelief(
      sentence
    ) => {
    }
  }
}
