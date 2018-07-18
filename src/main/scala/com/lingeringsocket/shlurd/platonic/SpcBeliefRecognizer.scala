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

import ShlurdEnglishLemmas._

class SpcBeliefRecognizer(val cosmos : SpcCosmos)
{
  protected val creed = new SpcCreed(cosmos)

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
    if (sentence.mood.isNegative) {
      // FIXME:  interpret this as a constraint
      return None
    }
    sentence match {
      case SilPredicateSentence(predicate, mood, formality) => {
        if (!predicate.getModifiers.isEmpty) {
          return None
        }
        predicate match {
          case statePredicate : SilStatePredicate => {
            return recognizeStatePredicateBelief(
              sentence, statePredicate, mood)
          }
          case relationshipPredicate : SilRelationshipPredicate => {
            return recognizeRelationshipPredicateBelief(
              sentence, relationshipPredicate, mood)
          }
          case _ =>
        }
      }
      case conditionalSentence : SilConditionalSentence => {
        return recognizeConsequenceBelief(conditionalSentence)
      }
      case _ =>
    }
    None
  }

  private def recognizeStatePredicateBelief(
    sentence : SilSentence,
    predicate : SilStatePredicate,
    mood : SilMood) : Option[SpcBelief] =
  {
    val ref = predicate.subject
    val state = predicate.state
    // FIXME we should not be allowing genitives here except
    // in certain cases
    val (noun, qualifiers, count, determiner, failed) =
      extractQualifiedNoun(sentence, ref, Seq.empty, true)
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
        // FIXME assert something about qualifiers here
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
        Some(EntityExistenceBelief(
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
        definePropertyBelief(
          sentence, noun, None, state, mood)
      }
    }
  }

  private def recognizeRelationshipPredicateBelief(
    sentence : SilSentence,
    predicate : SilRelationshipPredicate,
    mood : SilMood) : Option[SpcBelief] =
  {
    val subject = predicate.subject
    val complement = predicate.complement
    val relationship = predicate.relationship
    subject match {
      case SilNounReference(
        subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        return interpretFormRelationship(
          sentence, subjectNoun, complement, relationship)
      }
      case SilNounReference(
        subjectNoun, subjectDeterminer, COUNT_SINGULAR
      ) => {
        return interpretEntityRelationship(
          sentence, subjectDeterminer, subjectNoun,
          complement, relationship)
      }
      case gr : SilGenitiveReference => {
        complement match {
          // "Will's dad is Lonnie"
          case SilNounReference(
            subjectNoun, subjectDeterminer, COUNT_SINGULAR
          ) => {
            return interpretEntityRelationship(
              sentence, subjectDeterminer, subjectNoun,
              gr, relationship)
          }
          case _ =>
        }
      }
      case SilStateSpecifiedReference(
        SilNounReference(
          possessorFormName, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
        SilAdpositionalState(
          SilAdposition.WITH,
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
    None
  }

  private def recognizeConsequenceBelief(sentence : SilConditionalSentence)
      : Option[SpcBelief] =
  {
    val antecedent = sentence.antecedent
    val consequent = sentence.consequent
    antecedent match {
      case _ : SilActionPredicate => {
      }
      case _ => return None
    }
    consequent match {
      case rp @ SilRelationshipPredicate(
        subject, _ : SilGenitiveReference, REL_IDENTITY, _
      ) => {
        var invalid = false
        val querier = new SilPhraseRewriter
        def validateReferences = querier.queryMatcher {
          case SilNounReference(_, determiner, _) => {
            determiner match {
              case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED =>
              case _ => {
                invalid = true
              }
            }
          }
        }
        querier.query(validateReferences, rp)
        if (invalid) {
          return None
        }
      }
      case _ => return None
    }
    Some(ConsequenceBelief(sentence))
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
        val (complementNoun, qualifiers, count, determiner, failed) =
          extractQualifiedNoun(sentence, complement, Seq.empty)
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
                SilAdposition.OF,
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
              val (complementNoun, qualifiers, count, determiner, failed) =
                extractQualifiedNoun(sentence, ref, Seq.empty)
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
            val (complementNoun, qualifiers, count, determiner, failed) =
              extractQualifiedNoun(sentence, ref, Seq.empty)
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
    subjectDeterminer : SilDeterminer,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
      : Option[SpcBelief] =
  {
    subjectDeterminer match {
      case DETERMINER_UNSPECIFIED | DETERMINER_UNIQUE =>
      case _ => {
        return None
      }
    }
    if (sentence.mood.getModality != MODAL_NEUTRAL) {
      return Some(UnimplementedBelief(sentence))
    }
    // FIXME "Larry has a dog"
    if (relationship != REL_IDENTITY) {
      return Some(UnimplementedBelief(sentence))
    }
    val (complementNoun, qualifiers, count, complementDeterminer, failed) =
      extractQualifiedNoun(sentence, complement, Seq.empty, true)
    if (failed) {
      return None
    }
    if (qualifiers.isEmpty) {
      if (complementDeterminer != DETERMINER_NONSPECIFIC) {
        // FIXME this should be easy to implement
        // "Oz is the werewolf"
        return Some(UnimplementedBelief(sentence))
      }
      if (subjectDeterminer != DETERMINER_UNSPECIFIED) {
        // FIXME this should be easy to implement
        // "The dog is a werewolf"
        return Some(UnimplementedBelief(sentence))
      }
      // "Fido is a dog"
      Some(EntityExistenceBelief(
        sentence,
        complementNoun, Seq(subjectNoun), subjectNoun.lemmaUnfolded))
    } else {
      // "Fido is Franny's pet"
      complementDeterminer match {
        case DETERMINER_UNSPECIFIED | DETERMINER_UNIQUE => {
          if (qualifiers.size != 1) {
            return None
          }
        }
        case _ => return None
      }
      Some(EntityAssocBelief(
        sentence,
        complementDeterminer, qualifiers.head,
        subjectDeterminer, subjectNoun, complementNoun))
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
  private def isPropertyAssoc(
    sentence : SilSentence, complement : SilReference,
    relationship : SilRelationship) : Option[Boolean] =
  {
    complement match {
      case SilStateSpecifiedReference(
        _,
        SilAdpositionalState(adposition, objRef)) =>
        {
          if (adposition != SilAdposition.AS) {
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
    preQualifiers : Seq[SilWord],
    allowGenitive : Boolean = false)
      : (SilWord, Seq[SilWord], SilCount, SilDeterminer, Boolean) =
  {
    reference match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) =>
        {
          (noun, preQualifiers, COUNT_SINGULAR, DETERMINER_NONSPECIFIC, false)
        }
      case SilNounReference(
        noun, DETERMINER_UNSPECIFIED, COUNT_PLURAL) =>
        {
          (noun, preQualifiers, COUNT_PLURAL, DETERMINER_UNSPECIFIED, false)
        }
      case SilStateSpecifiedReference(subRef, state) =>
        {
          extractQualifiedNoun(
            sentence, subRef,
            preQualifiers ++ SilReference.extractQualifiers(state))
        }
      case SilGenitiveReference(
        SilNounReference(
          possessor, possessorDeterminer, COUNT_SINGULAR),
        SilNounReference(
          possession, DETERMINER_UNSPECIFIED, COUNT_SINGULAR)) =>
        {
          val failed = possessorDeterminer match {
            case DETERMINER_UNSPECIFIED => false
            case DETERMINER_NONSPECIFIC => false
            case DETERMINER_UNIQUE => false
            case _ => true
          }
          (possession, Seq(possessor),
            COUNT_SINGULAR, possessorDeterminer, failed || !allowGenitive)
        }
      case _ => (SilWord(""), Seq.empty, COUNT_SINGULAR,
        DETERMINER_UNSPECIFIED, true)
    }
  }
}
