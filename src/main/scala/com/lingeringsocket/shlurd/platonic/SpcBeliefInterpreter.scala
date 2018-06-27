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
      : Option[SpcEntity] =
  {
    val candidates = cosmos.getEntities.values.filter(
      _.qualifiers == Set(word.lemma))
    if (candidates.size > 1) {
      None
    } else {
      candidates.headOption
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
    val constraint = cosmos.getAssocConstraints(formAssocEdge)
    val entityAssocGraph = cosmos.getEntityAssocGraph
    val edges = entityAssocGraph.
      outgoingEdgesOf(possessor).asScala.
      filter(_.formEdge == formAssocEdge)

    val edgeCount = edges.size
    if (edgeCount >= constraint.upper) {
      val originalBelief = SilConjunctiveSentence(
        DETERMINER_ALL,
        Seq(creed.formAssociationBelief(formAssocEdge)) ++
          edges.map(creed.entityAssociationBelief(_)),
        SEPARATOR_OXFORD_COMMA)
      throw new IncrementalCardinalityExcn(
        sentence, originalBelief)
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
      form.addStateNormalization(state1, state2)
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
      try {
        cosmos.addIdealTaxonomy(
          hyponymIdeal, hypernymIdeal)
      } catch {
        case ex : IllegalArgumentException => {
          // report detected cycle
          val path = DijkstraShortestPath.findPathBetween(
            cosmos.getIdealTaxonomyGraph, hypernymIdeal, hyponymIdeal)
          assert(path != null)
          val originalBelief = SilConjunctiveSentence(
            DETERMINER_ALL,
            path.getEdgeList.asScala.toSeq.map(creed.idealTaxonomyBelief(_)),
            SEPARATOR_OXFORD_COMMA)
          throw new ContradictoryBeliefExcn(
            sentence,
            originalBelief)
        }
      }
    }
  }

  beliefApplier {
    case FormAssocBelief(
      sentence, possessorFormName, possesseeRoleNames,
      newConstraint, isProperty
    ) => {
      val possessorForm = cosmos.instantiateForm(possessorFormName)
      possesseeRoleNames.foreach(possesseeRoleName => {
        val possesseeRole = cosmos.instantiateRole(possesseeRoleName)
        if (isProperty) {
          val possesseeForm = cosmos.instantiateForm(possesseeRoleName)
          cosmos.addIdealTaxonomy(possesseeRole, possesseeForm)
        }
        val edge = cosmos.addFormAssoc(
          possessorForm, possesseeRole)
        val constraint = cosmos.getAssocConstraints.get(edge) match {
          case Some(oldConstraint) => SpcCardinalityConstraint(
            Math.max(oldConstraint.lower, newConstraint.lower),
            Math.min(oldConstraint.upper, newConstraint.upper))
          case _ => newConstraint
        }
        cosmos.annotateFormAssoc(edge, constraint, isProperty)
      })
    }
  }

  beliefApplier {
    case EntityExistenceBelief(
      sentence, formName, qualifiers, properName
    ) => {
      // FIXME also need to allow existing form to be refined
      val form = cosmos.instantiateForm(formName)
      val (entity, success) = cosmos.instantiateEntity(
        form, qualifiers, properName)
      if (!success) {
        val creed = new SpcCreed(cosmos)
        throw new AmbiguousBeliefExcn(sentence, creed.entityFormBelief(entity))
      }
    }
  }

  beliefApplier {
    case EntityAssocBelief(
      sentence, possessorEntityName, possesseeEntityName, roleName
    ) => {
      val possessorOpt = resolveUniqueName(possessorEntityName)
      val possesseeOpt = resolveUniqueName(possesseeEntityName)
      if (possessorOpt.isEmpty) {
        throw new UnknownPossessorBeliefExcn(sentence)
      }
      if (possesseeOpt.isEmpty) {
        throw new UnknownPossesseeBeliefExcn(sentence)
      }
      val possessor = possessorOpt.get
      val possessee = possesseeOpt.get
      val role = cosmos.getRoles.get(roleName.lemma) match {
        case Some(r) => r
        case _ => {
          throw new MissingAssocBeliefExcn(sentence)
        }
      }
      cosmos.getGraph.getFormAssocEdge(
        possessor.form, possessee.form, role) match
      {
        case Some(formAssocEdge) => {
          validateEdgeCardinality(sentence, formAssocEdge, possessor)
          cosmos.getInverseAssocEdges.get(formAssocEdge) match {
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
        case _ => {
          throw new MissingAssocBeliefExcn(sentence)
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
        val possesseeForm = cosmos.getGraph.getFormForRole(possesseeRole) match
        {
          case Some(form) => form
          case _ => cosmos.instantiateForm(possesseeRoleName)
        }
        val edge = cosmos.addFormAssoc(
          possessorForm, possesseeRole)
        val inverseEdge = cosmos.addFormAssoc(
          possesseeForm, possessorRole)
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
          form.instantiateProperty(propertyName)
        }
        case _ => {
          val properties = newStates.flatMap(
            w => form.resolveProperty(w.lemma).map(_._1).toSeq)
          properties match {
            case Seq() => {
              form.instantiateProperty(
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
            w => cosmos.resolveFormProperty(form, w.lemma).
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
          baselineProperty.getStates.keySet))
        {
          throw new ContradictoryBeliefExcn(
            sentence,
            creed.formPropertyBelief(form, baselineProperty))
        }
      }
      newStates.foreach(property.instantiateState(_))
      if (isClosed || baselineProperty.isClosed) {
        property.closeStates
      }
    }
  }
}
