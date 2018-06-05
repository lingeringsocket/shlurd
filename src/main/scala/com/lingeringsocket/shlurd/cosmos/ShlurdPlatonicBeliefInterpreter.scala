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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._

import scala.collection._
import scala.collection.JavaConverters._

import ShlurdEnglishLemmas._
import ShlurdPlatonicCosmos._

class ShlurdPlatonicBeliefInterpreter(cosmos : ShlurdPlatonicCosmos)
{
  type BeliefApplier = PartialFunction[ShlurdPlatonicBelief, Unit]

  private val beliefAppliers = new mutable.ArrayBuffer[BeliefApplier]

  private lazy val allBeliefApplier = beliefAppliers.reduceLeft(_ orElse _)

  private val creed = new ShlurdPlatonicCreed(cosmos)

  def interpretBelief(sentence : SilSentence)
  {
    recognizeBelief(sentence) match {
      case Some(belief) => {
        applyBelief(belief)
      }
      case _ => throw new IncomprehensibleBeliefExcn(sentence)
    }
  }

  def applyBelief(belief : ShlurdPlatonicBelief)
  {
    allBeliefApplier.apply(belief)
  }

  def recognizeBelief(sentence : SilSentence)
      : Option[ShlurdPlatonicBelief] =
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
                  // maybe we should allow constraints on
                  // qualified entities?
                  return Some(UnimplementedBelief(sentence))
                }
                return definePropertyBelief(
                  sentence, noun, qualifiers, state, mood)
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
      : Option[ShlurdPlatonicBelief] =
  {
    val (complementNoun, qualifiers, count, failed) = extractQualifiedNoun(
      sentence, complement, Seq.empty)
    if (failed) {
      return None
    }
    if (!qualifiers.isEmpty) {
      return Some(UnimplementedBelief(sentence))
    }
    relationship match {
      case REL_IDENTITY => {
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
                  genericFormName,
                  DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
                  COUNT_SINGULAR))
            ) => {
              Some(FormTaxonomyBelief(
                sentence, subjectNoun, genericFormName))
            }
            case _ => None
          }
        } else {
          if (sentence.mood.getModality == MODAL_NEUTRAL) {
            // "a fridge is a refrigerator"
            Some(FormAliasBelief(
              sentence, subjectNoun, complementNoun))
          } else {
            // "an owner must be a person"
            Some(FormRoleBelief(
              sentence, subjectNoun, complementNoun))
          }
        }
      }
      case REL_ASSOCIATION => {
        // "a dog has an owner"
        val upper = count match {
          case COUNT_SINGULAR => 1
          case COUNT_PLURAL => Int.MaxValue
        }
        val newConstraint = sentence.mood.getModality match {
          case MODAL_NEUTRAL | MODAL_MUST | MODAL_EMPHATIC =>
            CardinalityConstraint(1, 1)
          case MODAL_MAY | MODAL_POSSIBLE |
              MODAL_CAPABLE | MODAL_PERMITTED =>
            CardinalityConstraint(0, upper)
          case MODAL_SHOULD =>
            return Some(UnimplementedBelief(sentence))
        }
        isPropertyAssoc(sentence, complement, relationship).map(
          isProperty => {
            FormAssocBelief(
              sentence,
              subjectNoun, complementNoun, newConstraint,
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
      : Option[ShlurdPlatonicBelief] =
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
    qualifiers : Seq[SilWord],
    state : SilState,
    mood : SilMood)
      : Option[ShlurdPlatonicBelief] =
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
      sentence, formName, newStates, isClosed))
  }

  private def resolveUniqueName(word : SilWord)
      : Option[ShlurdPlatonicEntity] =
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
      case SilGenitiveReference(
        SilNounReference(
          possessor, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        SilNounReference(
          possession, DETERMINER_UNSPECIFIED, COUNT_SINGULAR)) =>
        {
          (possession, Seq(possessor), COUNT_SINGULAR, false)
        }
      case _ => (SilWord(""), Seq.empty, COUNT_SINGULAR, true)
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
    case FormAliasBelief(
      sentence, synonym, formName
    ) => {
      val form = cosmos.instantiateForm(formName)
      cosmos.addFormSynonym(synonym.lemma, form.name, false)
    }
  }

  beliefApplier {
    case FormRoleBelief(
      sentence, role, formName
    ) => {
      val form = cosmos.instantiateForm(formName)
      // FIXME validation
      cosmos.addFormSynonym(role.lemma, form.name, true)
    }
  }

  beliefApplier {
    case FormTaxonomyBelief(
      sentence, specificFormName, genericFormName
    ) => {
      val specificForm = cosmos.instantiateForm(specificFormName)
      val genericForm = cosmos.instantiateForm(genericFormName)
      // FIXME:  handle cycle exceptions
      cosmos.addFormTaxonomy(
        specificForm, genericForm)
    }
  }

  beliefApplier {
    case FormAssocBelief(
      sentence, possessorFormName, possesseeFormName,
      newConstraint, isProperty
    ) => {
      val possessorForm = cosmos.instantiateForm(possessorFormName)
      if (!isProperty) {
        if (!cosmos.isRole(possesseeFormName)) {
          // FIXME:  maybe throw a more specific excn
          throw new UnknownPossesseeBeliefExcn(sentence)
        }
      }
      val possesseeForm = cosmos.instantiateForm(possesseeFormName)
      val label = possesseeFormName.lemma
      val edge = cosmos.addFormAssoc(
        possessorForm, possesseeForm, label)
      val constraint = cosmos.getAssocConstraints.get(edge) match {
        case Some(oldConstraint) => CardinalityConstraint(
          Math.max(oldConstraint.lower, newConstraint.lower),
          Math.min(oldConstraint.upper, newConstraint.upper))
        case _ => newConstraint
      }
      cosmos.annotateFormAssoc(edge, constraint, isProperty)
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
        val creed = new ShlurdPlatonicCreed(cosmos)
        throw new AmbiguousBeliefExcn(sentence, creed.entityFormBelief(entity))
      }
    }
  }

  beliefApplier {
    case EntityAssocBelief(
      sentence, possessorEntityName, possesseeEntityName, labelName
    ) => {
      val possessorOpt = resolveUniqueName(possessorEntityName)
      val possesseeOpt = resolveUniqueName(possesseeEntityName)
      val label = labelName.lemma
      if (possessorOpt.isEmpty) {
        throw new UnknownPossessorBeliefExcn(sentence)
      }
      if (possesseeOpt.isEmpty) {
        throw new UnknownPossesseeBeliefExcn(sentence)
      }
      val possessor = possessorOpt.get
      val possessee = possesseeOpt.get
      cosmos.getFormAssoc(
        possessor.form, possessee.form, label) match
      {
        case Some(formAssoc) => {
          val constraint = cosmos.getAssocConstraints(formAssoc)
          val entityAssocGraph = cosmos.getEntityAssocGraph
          if (entityAssocGraph.containsVertex(possessor)) {
            val edges = entityAssocGraph.
              outgoingEdgesOf(possessor).asScala.
              filter(_.label == label)

            val edgeCount = edges.size
            if (edgeCount >= constraint.upper) {
              val originalBelief = SilConjunctiveSentence(
                DETERMINER_ALL,
                Seq(creed.formAssociationBelief(formAssoc)) ++
                  edges.map(creed.entityAssociationBelief(_)),
                SEPARATOR_OXFORD_COMMA)
              throw new IncrementalCardinalityExcn(
                sentence, originalBelief)
            }
          }
          cosmos.addEntityAssoc(possessor, possessee, label)
        }
        case _ => {
          throw new MissingAssocBeliefExcn(sentence)
        }
      }
    }
  }

  beliefApplier {
    case FormPropertyBelief(
      sentence, formName, newStates, isClosed
    ) => {
      val form = cosmos.instantiateForm(formName)
      val properties = newStates.flatMap(
        w => form.resolveProperty(w.lemma).map(_._1).toSeq)
      val property = properties match {
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
      if (property.isClosed) {
        if (!newStates.map(_.lemma).toSet.subsetOf(property.getStates.keySet)) {
          throw new ContradictoryBeliefExcn(
            sentence,
            creed.formPropertyBelief(form, property))
        }
      } else {
        newStates.foreach(property.instantiateState(_))
        if (isClosed) {
          property.closeStates
        }
      }
    }
  }
}
