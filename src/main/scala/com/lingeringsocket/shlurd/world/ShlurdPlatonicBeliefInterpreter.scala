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
package com.lingeringsocket.shlurd.world

import com.lingeringsocket.shlurd.parser._

import scala.collection._

import ShlurdEnglishLemmas._
import ShlurdPlatonicWorld._

class ShlurdPlatonicBeliefInterpreter(world : ShlurdPlatonicWorld)
{
  def interpretBelief(sentence : SilSentence)
  {
    if (sentence.hasUnknown) {
      throw new IncomprehensibleBelief(sentence)
    }
    sentence match {
      case SilPredicateSentence(predicate, mood, formality) => {
        if (mood.isNegative) {
          // FIXME:  interpret this as a constraint
          throw new IncomprehensibleBelief(sentence)
        }
        predicate match {
          case SilStatePredicate(ref, state) => {
            val (noun, qualifiers, count) = extractQualifiedNoun(
              sentence, ref, Seq.empty)
            val form = world.instantiateForm(noun)
            ref match {
              case SilStateSpecifiedReference(
                _, specifiedState @
                  (_ : SilAdpositionalState | _ : SilPropertyState)
              ) => {
                // "a television that is on the blink is broken"
                // or "a television that is busted is broken"
                // or "a busted television is broken"
                if (mood.getModality != MODAL_NEUTRAL) {
                  throw new IncomprehensibleBelief(sentence)
                }
                state match {
                  case ps : SilPropertyState => {
                    form.addStateNormalization(specifiedState, state)
                    return
                  }
                  case SilExistenceState() =>
                  case _ => {
                    throw new IncomprehensibleBelief(sentence)
                  }
                }
              }
              case _ =>
            }
            state match {
              case SilExistenceState() => {
                // "there is a television"
                addExistenceBelief(sentence, form, qualifiers, mood)
              }
              case _ => {
                addPropertyBelief(sentence, form, qualifiers, state, mood)
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
                interpretFormRelationship(
                  sentence, subjectNoun, complement, relationship)
              }
              case SilNounReference(
                subjectNoun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
              ) => {
                interpretEntityRelationship(
                  sentence, subjectNoun, complement, relationship)
              }
              case _ => throw new IncomprehensibleBelief(sentence)
            }
          }
          case _ => throw new IncomprehensibleBelief(sentence)
        }
      }
      case _ => throw new IncomprehensibleBelief(sentence)
    }
  }

  private def interpretFormRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
  {
    val (complementNoun, qualifiers, count) = extractQualifiedNoun(
      sentence, complement, Seq.empty)
    if (!qualifiers.isEmpty) {
      throw new IncomprehensibleBelief(sentence)
    }
    relationship match {
      case REL_IDENTITY => {
        // "a canine is a dog"
        assert(count == COUNT_SINGULAR)
        val form = world.instantiateForm(complementNoun)
        world.getFormSynonyms.addSynonym(subjectNoun.lemma, form.name)
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
            throw new IncomprehensibleBelief(sentence)
        }
        val possessorForm = world.instantiateForm(subjectNoun)
        val possesseeForm = world.instantiateRole(complementNoun)
        val label = complementNoun.lemma
        val edge = world.addFormAssoc(
          possessorForm, possesseeForm, label)
        val constraint = world.getAssocConstraint(edge) match {
          case Some(oldConstraint) => CardinalityConstraint(
            Math.max(oldConstraint.lower, newConstraint.lower),
            Math.min(oldConstraint.upper, newConstraint.upper))
          case _ => newConstraint
        }
        world.setAssocConstraint(edge, constraint)
        if (isPropertyAssoc(sentence, complement, relationship)) {
          world.getPropertyEdges += edge
        }
      }
    }
  }

  private def interpretEntityRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
  {
    // FIXME "Larry has a dog"
    assert(relationship == REL_IDENTITY)
    val (complementNoun, qualifiers, count) = extractQualifiedNoun(
      sentence, complement, Seq.empty)
    if (qualifiers.isEmpty) {
      // "Fido is a dog"
      val form = world.instantiateForm(complementNoun)
      world.instantiateEntity(
        sentence, form, Seq(subjectNoun), subjectNoun.lemmaUnfolded)
    } else {
      // "Fido is Franny's pet"
      if (qualifiers.size != 1) {
        throw new IncomprehensibleBelief(sentence)
      }
      val possessorOpt = resolveUniqueName(qualifiers.head)
      val possesseeOpt = resolveUniqueName(subjectNoun)
      if (possessorOpt.isEmpty || possesseeOpt.isEmpty) {
        throw new IncomprehensibleBelief(sentence)
      }
      val possessor = possessorOpt.get
      val possessee = possesseeOpt.get
      val label = complementNoun.lemma
      if (!world.isFormAssoc(
        possessor.form, possessee.form, label))
      {
        throw new IncomprehensibleBelief(sentence)
      }
      world.addEntityAssoc(possessor, possessee, label)
    }
  }

  private def addExistenceBelief(
    sentence : SilSentence,
    form : ShlurdPlatonicForm,
    qualifiers : Seq[SilWord],
    mood : SilMood)
  {
    // FIXME:  interpret mood
    world.instantiateEntity(sentence, form, qualifiers)
  }

  private def addPropertyBelief(
    sentence : SilSentence,
    form : ShlurdPlatonicForm,
    qualifiers : Seq[SilWord],
    state : SilState,
    mood : SilMood)
  {
    if (!qualifiers.isEmpty) {
      // but maybe we should allow constraints on qualified entities?
      throw new IncomprehensibleBelief(sentence)
    }
    // "a light may be on or off"
    if (sentence.mood.getModality == MODAL_NEUTRAL) {
      throw new IncomprehensibleBelief(sentence)
    }
    val property = form.instantiateProperty(DEFAULT_PROPERTY_WORD)
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
            throw new IncomprehensibleBelief(sentence)
          }
        })
      }
      case _ => {
        throw new IncomprehensibleBelief(sentence)
      }
    }
    if (property.isClosed) {
      if (!newStates.map(_.lemma).toSet.subsetOf(property.getStates.keySet)) {
        throw new ContradictoryBelief(sentence)
      }
    } else {
      newStates.foreach(property.instantiateState(_))
      if (mood.getModality == MODAL_MUST) {
        property.closeStates
      }
    }
  }
  private def resolveUniqueName(word : SilWord)
      : Option[ShlurdPlatonicEntity] =
  {
    val candidates = world.getEntities.values.filter(
      _.qualifiers == Set(word.lemma))
    if (candidates.size > 1) {
      None
    } else {
      candidates.headOption
    }
  }

  private def isPropertyAssoc(
    sentence : SilSentence, complement : SilReference,
    relationship : SilRelationship) : Boolean =
  {
    complement match {
      case SilStateSpecifiedReference(
        _,
        SilAdpositionalState(adposition, objRef)) =>
        {
          if (adposition != ADP_AS) {
            throw new IncomprehensibleBelief(sentence)
          }
          // "A television has a volume as a property"
          objRef match {
            case SilNounReference(
              SilWord(LEMMA_PROPERTY, LEMMA_PROPERTY),
              DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
              COUNT_SINGULAR) =>
              {
                if (relationship == REL_ASSOCIATION) {
                  true
                } else {
                  throw new IncomprehensibleBelief(sentence)
                }
              }
            case _ => throw new IncomprehensibleBelief(sentence)
          }
        }
      case _ => false
    }
  }

  private def extractQualifiedNoun(
    sentence : SilSentence,
    reference : SilReference,
    preQualifiers : Seq[SilWord])
      : (SilWord, Seq[SilWord], SilCount) =
  {
    reference match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (noun, preQualifiers, COUNT_SINGULAR)
        }
      case SilNounReference(
        noun, DETERMINER_UNSPECIFIED, COUNT_PLURAL) =>
        {
          (noun, preQualifiers, COUNT_PLURAL)
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
          (possession, Seq(possessor), COUNT_SINGULAR)
        }
      case _ => throw new IncomprehensibleBelief(sentence)
    }
  }
}
