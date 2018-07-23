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
import com.lingeringsocket.shlurd.print._
import com.lingeringsocket.shlurd.cosmos._

import scala.collection._

import ShlurdEnglishLemmas._

class SpcBeliefRecognizer(val cosmos : SpcCosmos)
{
  protected val creed = new SpcCreed(cosmos)

  def recognizeBeliefs(sentence : SilSentence)
      : Seq[SpcBelief] =
  {
    if (sentence.hasUnknown) {
      return Seq.empty
    }
    if (!sentence.mood.isIndicative) {
      // FIXME support interrogative
      return Seq.empty
    }
    if (sentence.mood.isNegative) {
      // FIXME:  interpret this as a constraint
      return Seq.empty
    }
    sentence match {
      case SilPredicateSentence(predicate, mood, formality) => {
        if (!predicate.getModifiers.isEmpty) {
          return Seq.empty
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
    Seq.empty
  }

  private def recognizeStatePredicateBelief(
    sentence : SilSentence,
    predicate : SilStatePredicate,
    mood : SilMood) : Seq[SpcBelief] =
  {
    val ref = predicate.subject
    val state = predicate.state
    state match {
      case SilAdpositionalState(SilAdposition.IN, container) => {
        return recognizeRelationshipPredicateBelief(
          sentence,
          SilRelationshipPredicate(
            SilGenitiveReference(
              ref,
              SilNounReference(SilWord(LEMMA_CONTAINER),
                DETERMINER_UNSPECIFIED,
                COUNT_SINGULAR)),
            container,
            REL_IDENTITY
          ),
          sentence.mood)
      }
      case _ =>
    }
    // FIXME we should not be allowing genitives here except
    // in certain cases
    val (noun, qualifiers, count, determiner, failed) =
      extractQualifiedNoun(sentence, ref, Seq.empty, true)
    if (failed) {
      return Seq.empty
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
          return Seq(UnimplementedBelief(sentence))
        }
        // FIXME assert something about qualifiers here
        state match {
          case ps : SilPropertyState => {
            return Seq(StateEquivalenceBelief(
              sentence, noun, specifiedState, state))
          }
          case SilExistenceState() =>
          case _ => {
            return Seq(UnimplementedBelief(sentence))
          }
        }
      }
      case _ =>
    }
    state match {
      case SilExistenceState() => {
        // "there is a television"
        // FIXME:  interpret mood
        Seq(EntityExistenceBelief(
          sentence, noun, DETERMINER_NONSPECIFIC, qualifiers, ""))
      }
      case _ => {
        if (!qualifiers.isEmpty) {
          if ((qualifiers.size > 1) ||
            !ref.isInstanceOf[SilGenitiveReference])
          {
            // maybe we should allow constraints on
            // qualified entities?
            return Seq(UnimplementedBelief(sentence))
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
    mood : SilMood) : Seq[SpcBelief] =
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
        // "Lonnie is Will's dad"
        return interpretEntityRelationship(
          sentence, subjectDeterminer, subjectNoun,
          complement, relationship)
      }
      case _ : SilGenitiveReference => {
        complement match {
          // "Will's dad is Lonnie"
          case SilNounReference(
            complementNoun, complementDeterminer, COUNT_SINGULAR
          ) => {
            // flip subject/complement to match "Lonnie is Will's dad"
            return interpretEntityRelationship(
              sentence, complementDeterminer, complementNoun,
              subject, relationship)
          }
          case _ : SilGenitiveReference => {
            // "Will's dad is Joyce's ex-husband": resolve "Joyce's ex-husband"
            // to "Lonnie" and then proceed flipping subject/complement
            return interpretIndirectEntityRelationship(
              sentence, subject, complement, relationship)
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
          return Seq(UnimplementedBelief(sentence))
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
              case _ => return Seq(UnimplementedBelief(sentence))
            })
          }
          case _ => return Seq(UnimplementedBelief(sentence))
        }
        complement match {
          case SilNounReference(
            possessorRoleName, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
          ) => {
            return Seq(InverseAssocBelief(
              sentence,
              possessorFormName, possessorRoleName,
              possesseeRoleNames))
          }
          case _ =>
        }
      }
      case _ =>
    }
    Seq.empty
  }

  private def interpretResolvedReference(
    sentence : SilSentence,
    ref : SilReference,
    interpretation : (SpcEntity) => Seq[SpcBelief]) : Seq[SpcBelief] =
  {
    val rewriter =
      new ShlurdReferenceRewriter[SpcEntity, SpcProperty](
        cosmos,
        new SilSentencePrinter,
        ResultCollector[SpcEntity],
        false)
    rewriter.rewrite(rewriter.rewriteReferences, ref) match {
      case SilResolvedReference(
        set : Set[_], _, _
      ) => {
        if (set.isEmpty) {
          // FIXME for single-valued associations, not sure we
          // should be doing this if the association is already
          // bound
          return Seq(EpsilonBelief(sentence))
        }
        set.toSeq.flatMap(_ match {
          case entity : SpcEntity => {
            val seq = interpretation(entity)
            if (seq.isEmpty) {
              return seq
            }
            seq
          }
          case _ => return Seq.empty
        })
      }
      case _ => Seq.empty
    }
  }

  private def interpretIndirectEntityRelationship(
    sentence : SilSentence,
    complement : SilReference,
    subject : SilReference,
    relationship : SilRelationship) : Seq[SpcBelief] =
  {
    interpretResolvedReference(sentence, subject, entity => {
      val (subjectDeterminer, subjectNoun) = {
        if (entity.properName.isEmpty) {
          // FIXME we may be losing important qualifiers here
          (DETERMINER_UNIQUE, SilWord(entity.form.name))
        } else {
          (DETERMINER_UNSPECIFIED, SilWord(entity.properName))
        }
      }
      interpretEntityRelationship(
        sentence, subjectDeterminer, subjectNoun,
        complement, relationship)
    })
  }

  private def recognizeConsequenceBelief(sentence : SilConditionalSentence)
      : Seq[SpcBelief] =
  {
    val antecedent = sentence.antecedent
    val consequent = sentence.consequent
    antecedent match {
      case _ : SilActionPredicate => {
      }
      case _ => return Seq.empty
    }
    var invalid = false
    val querier = new SilPhraseRewriter
    def validateReferences = querier.queryMatcher {
      case SilNounReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED | DETERMINER_NONE =>
          case _ => {
            invalid = true
          }
        }
      }
    }
    querier.query(validateReferences, consequent)
    if (invalid) {
      Seq.empty
    } else {
      Seq(ConsequenceBelief(sentence))
    }
  }

  private def interpretFormRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
      : Seq[SpcBelief] =
  {
    relationship match {
      case REL_IDENTITY => {
        val (complementNoun, qualifiers, count, determiner, failed) =
          extractQualifiedNoun(sentence, complement, Seq.empty)
        if (failed) {
          return Seq.empty
        }
        if (!qualifiers.isEmpty) {
          return Seq(UnimplementedBelief(sentence))
        }
        if (count != COUNT_SINGULAR) {
          return Seq(UnimplementedBelief(sentence))
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
              Seq(IdealTaxonomyBelief(
                sentence, subjectNoun, hypernymIdealName, false))
            }
            case _ => Seq.empty
          }
        } else {
          if (sentence.mood.getModality == MODAL_NEUTRAL) {
            // "a fridge is a refrigerator"
            Seq(IdealAliasBelief(
              sentence, subjectNoun, complementNoun))
          } else {
            // "an owner must be a person"
            Seq(IdealTaxonomyBelief(
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
                return Seq.empty
              }
              if (!qualifiers.isEmpty) {
                return Seq(UnimplementedBelief(sentence))
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
              return Seq.empty
            }
            if (!qualifiers.isEmpty) {
              return Seq(UnimplementedBelief(sentence))
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
            return Seq(UnimplementedBelief(sentence))
        }
        isPropertyAssoc(sentence, complement, relationship).map(
          isProperty => {
            FormAssocBelief(
              sentence,
              subjectNoun, complementNouns, newConstraint,
              isProperty)
          }
        ).toSeq
      }
    }
  }

  private def interpretEntityRelationship(
    sentence : SilSentence,
    subjectDeterminer : SilDeterminer,
    subjectNoun : SilWord,
    complement : SilReference,
    relationship : SilRelationship)
      : Seq[SpcBelief] =
  {
    subjectDeterminer match {
      case DETERMINER_UNSPECIFIED | DETERMINER_UNIQUE =>
      case _ => {
        return Seq.empty
      }
    }
    if (sentence.mood.getModality != MODAL_NEUTRAL) {
      return Seq(UnimplementedBelief(sentence))
    }
    relationship match {
      case REL_ASSOCIATION => {
        complement match {
          case SilNounReference(
            roleNoun,
            DETERMINER_NONE,
            _
          ) => {
            // "Larry has no pets"
            return Seq(EntityNoAssocBelief(
              sentence,
              subjectDeterminer,
              subjectNoun,
              roleNoun
            ))
          }
          case _ => {
            // FIXME "Larry has a dog"
            return Seq(UnimplementedBelief(sentence))
          }
        }
      }
      case REL_IDENTITY =>
    }
    complement match {
      case SilGenitiveReference(gr : SilGenitiveReference, possessee) => {
        // "Lurch is Morticia's children's butler" =>
        // "Lurch is Wednesday's butler", "Lurch is Pugsley's butler"
        return interpretResolvedReference(
          sentence,
          gr,
          entity => {
            val flattenedComplement = SilGenitiveReference(
              cosmos.specificReference(entity, DETERMINER_UNIQUE),
              possessee)
            interpretEntityRelationship(
              sentence, subjectDeterminer, subjectNoun, flattenedComplement,
              relationship)
          })
      }
      case _ =>
    }
    val (complementNoun, qualifiers, count, complementDeterminer, failed) =
      extractQualifiedNoun(sentence, complement, Seq.empty, true)
    if (failed) {
      return Seq.empty
    }
    if (qualifiers.isEmpty) {
      if (complementDeterminer != DETERMINER_NONSPECIFIC) {
        // FIXME this should be easy to implement
        // "Oz is the werewolf"
        return Seq(UnimplementedBelief(sentence))
      }
      subjectDeterminer match {
        case DETERMINER_UNSPECIFIED => {
          // "Fido is a dog"
          Seq(EntityExistenceBelief(
            sentence,
            complementNoun,
            subjectDeterminer, Seq(subjectNoun), subjectNoun.lemmaUnfolded))
        }
        case DETERMINER_UNIQUE => {
          // "The boss is a werewolf"
          Seq(EntityExistenceBelief(
            sentence,
            complementNoun,
            subjectDeterminer, Seq(subjectNoun), ""))
        }
        case _ => {
          // We don't yet support stuff like "all dogs are werewolves"
          return Seq(UnimplementedBelief(sentence))
        }
      }
    } else {
      // "Fido is Franny's pet"
      complementDeterminer match {
        case DETERMINER_UNSPECIFIED | DETERMINER_UNIQUE => {
          if (qualifiers.size != 1) {
            return Seq.empty
          }
        }
        case _ => return Seq.empty
      }
      Seq(EntityAssocBelief(
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
      : Seq[SpcBelief] =
  {
    // "a light may be on or off"
    if (sentence.mood.getModality == MODAL_NEUTRAL) {
      return Seq.empty
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
            return Seq.empty
          }
        })
      }
      case _ => {
        return Seq.empty
      }
    }
    val isClosed = (mood.getModality == MODAL_MUST)
    Seq(FormPropertyBelief(
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
    def failedResult = (SilWord(""), Seq.empty, COUNT_SINGULAR,
      DETERMINER_UNSPECIFIED, true)
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
          possession, DETERMINER_UNSPECIFIED, count)) =>
        {
          val failed = possessorDeterminer match {
            case DETERMINER_UNSPECIFIED => false
            case DETERMINER_NONSPECIFIC => false
            case DETERMINER_UNIQUE => false
            case _ => true
          }
          (possession, Seq(possessor),
            count, possessorDeterminer, failed || !allowGenitive)
        }
      case _ => failedResult
    }
  }
}
