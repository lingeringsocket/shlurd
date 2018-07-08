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

import ShlurdEnglishLemmas._

class SpcBeliefInterpreter(cosmos : SpcCosmos)
{
  type BeliefApplier = PartialFunction[SpcBelief, Unit]

  private val beliefAppliers = new mutable.ArrayBuffer[BeliefApplier]

  private lazy val allBeliefApplier = beliefAppliers.reduceLeft(_ orElse _)

  private val creed = new SpcCreed(cosmos)

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

  def recognizeBelief(sentence : SilSentence)
      : Option[SpcBelief] =
  {
    if (sentence.hasUnknown) {
      return None
    }
    if (!sentence.mood.isIndicative) {
      // FIXME support interrogative
      return None
    }
    sentence match {
      case SilPredicateSentence(predicate, mood, formality) => {
        if (mood.isNegative) {
          // FIXME:  interpret this as a constraint
          return Some(UnimplementedBelief(sentence))
        }
        predicate match {
          case SilStatePredicate(ref, state) => {
            val (noun, qualifiers, count, failed) = extractQualifiedNoun(
              sentence, ref, Seq.empty)
            if (failed) {
              return None
            }
            ref match {
              case SilStateSpecifiedReference(
                _, specifiedState @
                  (_ : SilAdpositionalState | _ : SilPropertyState)
              ) => {
                // "a television that is on the blink is broken"
                // or "a television that is busted is broken"
                // or "a busted television is broken"
                if (mood.getModality != MODAL_NEUTRAL) {
                  return Some(UnimplementedBelief(sentence))
                }
                state match {
                  case ps : SilPropertyState => {
                    return Some(StateEquivalenceBelief(
                      sentence, noun, specifiedState, state))
                  }
                  case SilExistenceState() =>
                  case _ => {
                    return Some(UnimplementedBelief(sentence))
                  }
                }
              }
              case _ =>
            }
            state match {
              case SilExistenceState() => {
                // "there is a television"
                // FIXME:  interpret mood
                return Some(EntityExistenceBelief(
                  sentence, noun, qualifiers, ""))
              }
              case _ => {
                if (!qualifiers.isEmpty) {
                  if ((qualifiers.size > 1) ||
                    !ref.isInstanceOf[SilGenitiveReference])
                  {
                    // maybe we should allow constraints on
                    // qualified entities?
                    return Some(UnimplementedBelief(sentence))
                  } else {
                    // "a cat's voracity must be carnivore"
                    return definePropertyBelief(
                      sentence, qualifiers.head, Some(noun), state, mood)
                  }
                }
                // "a lifeform may be either animal or vegetable"
                return definePropertyBelief(
                  sentence, noun, None, state, mood)
              }
            }
          }
          case SilRelationshipPredicate(
            subject, complement, relationship
          ) => {
            subject match {
              case SilNounReference(
                subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
              ) => {
                return interpretFormRelationship(
                  sentence, subjectNoun, complement, relationship)
              }
              case SilNounReference(
                subjectNoun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
              ) => {
                return interpretEntityRelationship(
                  sentence, subjectNoun, complement, relationship)
              }
              case gr : SilGenitiveReference => {
                complement match {
                  // "Will's dad is Lonnie"
                  case SilNounReference(
                    subjectNoun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
                  ) => {
                    return interpretEntityRelationship(
                      sentence, subjectNoun, gr, relationship)
                  }
                  case _ =>
                }
              }
              case SilStateSpecifiedReference(
                SilNounReference(
                  possessorFormName, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
                SilAdpositionalState(
                  ADP_WITH,
                  possesseeRef
                )
              ) => {
                // "a person with a child is a parent"
                if (mood.getModality != MODAL_NEUTRAL) {
                  return Some(UnimplementedBelief(sentence))
                }
                val possesseeRoleNames = possesseeRef match {
                  case SilNounReference(
                    possesseeRoleName, _, _
                  ) => {
                    Seq(possesseeRoleName)
                  }
                  case SilConjunctiveReference(_, references, _) => {
                    references.map({
                      case SilNounReference(possesseeRoleName, _, _) => {
                        possesseeRoleName
                      }
                      case _ => return Some(UnimplementedBelief(sentence))
                    })
                  }
                  case _ => return Some(UnimplementedBelief(sentence))
                }
                complement match {
                  case SilNounReference(
                    possessorRoleName, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
                  ) => {
                    return Some(InverseAssocBelief(
                      sentence,
                      possessorFormName, possessorRoleName,
                      possesseeRoleNames))
                  }
                  case _ =>
                }
              }
              case _ =>
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
    None
  }

  private def interpretFormRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
      : Option[SpcBelief] =
  {
    relationship match {
      case REL_IDENTITY => {
        val (complementNoun, qualifiers, count, failed) = extractQualifiedNoun(
          sentence, complement, Seq.empty)
        if (failed) {
          return None
        }
        if (!qualifiers.isEmpty) {
          return Some(UnimplementedBelief(sentence))
        }
        if (count != COUNT_SINGULAR) {
          return Some(UnimplementedBelief(sentence))
        }
        if (complementNoun.lemma == LEMMA_KIND) {
          // "a dog is a kind of canine"
          complement match {
            case SilStateSpecifiedReference(
              _,
              SilAdpositionalState(
                ADP_OF,
                SilNounReference(
                  hypernymIdealName,
                  DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
                  COUNT_SINGULAR))
            ) => {
              Some(IdealTaxonomyBelief(
                sentence, subjectNoun, hypernymIdealName, false))
            }
            case _ => None
          }
        } else {
          if (sentence.mood.getModality == MODAL_NEUTRAL) {
            // "a fridge is a refrigerator"
            Some(IdealAliasBelief(
              sentence, subjectNoun, complementNoun))
          } else {
            // "an owner must be a person"
            Some(IdealTaxonomyBelief(
              sentence, subjectNoun, complementNoun, true))
          }
        }
      }
      case REL_ASSOCIATION => {
        val (complementNouns, count) = complement match {
          // "a dog may have an owner and a groomer"
          case SilConjunctiveReference(_, refs, _) => {
            val pairs = refs.map(ref => {
              val (complementNoun, qualifiers, count, failed) =
                extractQualifiedNoun(
                  sentence, ref, Seq.empty)
              if (failed) {
                return None
              }
              if (!qualifiers.isEmpty) {
                return Some(UnimplementedBelief(sentence))
              }
              (complementNoun, count)
            })
            (pairs.map(_._1), pairs.map(_._2).maxBy(
              _ match {
                case COUNT_SINGULAR => 1
                case COUNT_PLURAL => 2
              })
            )
          }
          // "a dog has an owner"
          case ref => {
            val (complementNoun, qualifiers, count, failed) =
              extractQualifiedNoun(
                sentence, ref, Seq.empty)
            if (failed) {
              return None
            }
            if (!qualifiers.isEmpty) {
              return Some(UnimplementedBelief(sentence))
            }
            (Seq(complementNoun), count)
          }
        }
        val upper = count match {
          case COUNT_SINGULAR => 1
          case COUNT_PLURAL => Int.MaxValue
        }
        val newConstraint = sentence.mood.getModality match {
          case MODAL_NEUTRAL | MODAL_MUST | MODAL_EMPHATIC =>
            SpcCardinalityConstraint(1, 1)
          case MODAL_MAY | MODAL_POSSIBLE |
              MODAL_CAPABLE | MODAL_PERMITTED =>
            SpcCardinalityConstraint(0, upper)
          case MODAL_SHOULD | MODAL_ELLIPTICAL =>
            return Some(UnimplementedBelief(sentence))
        }
        isPropertyAssoc(sentence, complement, relationship).map(
          isProperty => {
            FormAssocBelief(
              sentence,
              subjectNoun, complementNouns, newConstraint,
              isProperty)
          }
        )
      }
    }
  }

  private def interpretEntityRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
      : Option[SpcBelief] =
  {
    if (sentence.mood.getModality != MODAL_NEUTRAL) {
      return Some(UnimplementedBelief(sentence))
    }
    // FIXME "Larry has a dog"
    if (relationship != REL_IDENTITY) {
      return Some(UnimplementedBelief(sentence))
    }
    val (complementNoun, qualifiers, count, failed) = extractQualifiedNoun(
      sentence, complement, Seq.empty)
    if (failed) {
      return None
    }
    if (qualifiers.isEmpty) {
      // "Fido is a dog"
      Some(EntityExistenceBelief(
        sentence,
        complementNoun, Seq(subjectNoun), subjectNoun.lemmaUnfolded))
    } else {
      // "Fido is Franny's pet"
      if (qualifiers.size != 1) {
        return Some(UnimplementedBelief(sentence))
      }
      Some(EntityAssocBelief(
        sentence,
        qualifiers.head, subjectNoun, complementNoun))
    }
  }

  private def definePropertyBelief(
    sentence : SilSentence,
    formName : SilWord,
    propertyName : Option[SilWord],
    state : SilState,
    mood : SilMood)
      : Option[SpcBelief] =
  {
    // "a light may be on or off"
    if (sentence.mood.getModality == MODAL_NEUTRAL) {
      return None
    }
    val newStates = state match {
      case SilPropertyState(word) => {
        Seq(word)
      }
      case SilConjunctiveState(determiner, states, _) => {
        // FIXME:  interpret determiner
        states.flatMap(_ match {
          case SilPropertyState(word) => {
            Seq(word)
          }
          case _ => {
            return None
          }
        })
      }
      case _ => {
        return None
      }
    }
    val isClosed = (mood.getModality == MODAL_MUST)
    Some(FormPropertyBelief(
      sentence, formName, newStates, isClosed, propertyName))
  }

  private def resolveUniqueName(word : SilWord)
      : SpcEntity =
  {
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

  private def isPropertyAssoc(
    sentence : SilSentence, complement : SilReference,
    relationship : SilRelationship) : Option[Boolean] =
  {
    complement match {
      case SilStateSpecifiedReference(
        _,
        SilAdpositionalState(adposition, objRef)) =>
        {
          if (adposition != ADP_AS) {
            return None
          }
          // "A television has a volume as a property"
          objRef match {
            case SilNounReference(
              SilWord(LEMMA_PROPERTY, LEMMA_PROPERTY),
              DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
              COUNT_SINGULAR) =>
              {
                if (relationship == REL_ASSOCIATION) {
                  Some(true)
                } else {
                  None
                }
              }
            case _ => None
          }
        }
      case _ => Some(false)
    }
  }

  private def extractQualifiedNoun(
    sentence : SilSentence,
    reference : SilReference,
    preQualifiers : Seq[SilWord])
      : (SilWord, Seq[SilWord], SilCount, Boolean) =
  {
    reference match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (noun, preQualifiers, COUNT_SINGULAR, false)
        }
      case SilNounReference(
        noun, DETERMINER_UNSPECIFIED, COUNT_PLURAL) =>
        {
          (noun, preQualifiers, COUNT_PLURAL, false)
        }
      case SilStateSpecifiedReference(subRef, state) =>
        {
          extractQualifiedNoun(
            sentence, subRef,
            preQualifiers ++ SilReference.extractQualifiers(state))
        }
      // FIXME:  be pickier about determiners here
      case SilGenitiveReference(
        SilNounReference(
          possessor, _, COUNT_SINGULAR),
        SilNounReference(
          possession, _, COUNT_SINGULAR)) =>
        {
          (possession, Seq(possessor), COUNT_SINGULAR, false)
        }
      case _ => (SilWord(""), Seq.empty, COUNT_SINGULAR, true)
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
          val creed = new SpcCreed(cosmos)
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

  def beliefApplier(applier : BeliefApplier)
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
                  // do we need to worry about cycles here?
                  cosmos.addIdealTaxonomy(
                    possesseeEntity.form, hypernymIdeal)
                } else {
                  val creed = new SpcCreed(cosmos)
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
      try {
        cosmos.addIdealTaxonomy(
          hyponymIdeal, hypernymIdeal)
      } catch {
        case ex : IllegalArgumentException => {
          // report detected cycle
          val path = DijkstraShortestPath.findPathBetween(
            cosmos.getIdealTaxonomyGraph, hypernymIdeal, hyponymIdeal)
          assert(path != null)
          val originalBelief = conjunctiveBelief(
            path.getEdgeList.asScala.toSeq.map(creed.idealTaxonomyBelief(_)))
          throw new ContradictoryBeliefExcn(
            sentence,
            originalBelief)
        }
      }
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
            val creed = new SpcCreed(cosmos)
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
      sentence, possessorEntityName, possesseeEntityName, roleName
    ) => {
      val possessor = resolveUniqueName(possessorEntityName)
      val possessee = resolveUniqueName(possesseeEntityName)
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
          cosmos.addIdealTaxonomy(possessee.form, form))
      }
      if (!graph.isFormCompatibleWithRole(possessee.form, role)) {
        val creed = new SpcCreed(cosmos)
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
            cosmos.addIdealTaxonomy(possessor.form, possessorForm)
          }
          case possessorRole : SpcRole => {
            graph.getFormsForRole(possessorRole).foreach(possessorForm =>
              cosmos.addIdealTaxonomy(possessor.form, possessorForm))
          }
        }
      }
      // FIXME it may not be correct to assume same identity in the case
      // of a multi-valued association
      findTentativePossessee(possessor, formAssocEdge) match {
        case Some(tentativePossessee) => {
          // FIXME in case of inverse assoc, should not be doing this until
          // after validateEdgeCardinality below (or else need rollback support)
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
      cosmos.addIdealTaxonomy(possessorRole, possessorForm)
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
}
