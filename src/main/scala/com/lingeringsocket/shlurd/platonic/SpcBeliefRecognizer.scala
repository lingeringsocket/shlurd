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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

import SprEnglishLemmas._

class SpcBeliefRecognizer(
  val cosmos : SpcCosmos,
  resultCollector : SmcResultCollector[SpcEntity] = SmcResultCollector())
{
  protected val creed = new SpcCreed(cosmos)

  private var finished = false

  def recognizeBeliefs(sentence : SilSentence)
      : Seq[SpcBelief] =
  {
    val beliefs = recognizeBeliefsImpl(sentence)
    if (sentence.tam.isNegative) {
      if (beliefs.forall(_ match {
        case (_ : EntityAssocBelief | _ : AssertionBelief) => {
          true
        }
        case _ => {
          false
        }
      })) {
        beliefs
      } else {
        Seq(UnimplementedBelief(sentence))
      }
    } else {
      beliefs
    }
  }

  private def recognizeBeliefsImpl(sentence : SilSentence)
      : Seq[SpcBelief] =
  {
    assert(!finished)
    finished = true
    if (sentence.hasUnknown) {
      return Seq.empty
    }
    if (sentence.tam.isImperative) {
      sentence match {
        case SilPredicateSentence(
          SilActionPredicate(
            _, action, Some(SilQuotationReference(quotation)), _),
          tam, formality
        ) => {
          return recognizeDirective(sentence, action, quotation)
        }
        case _ => return Seq.empty
      }
    }
    if (!sentence.tam.isIndicative) {
      // FIXME support interrogative
      return Seq.empty
    }
    sentence match {
      case SilPredicateSentence(predicate, tam, formality) => {
        if (predicate.getModifiers.filterNot(isIgnorableModifier).isEmpty) {
          predicate match {
            case statePredicate : SilStatePredicate => {
              return recognizeStatePredicateBelief(
                sentence, statePredicate, tam)
            }
            case relationshipPredicate : SilRelationshipPredicate => {
              return recognizeRelationshipPredicateBelief(
                sentence, relationshipPredicate, tam)
            }
            case _ =>
          }
        }
        recognizeAssertionBelief(sentence)
      }
      case SilConjunctiveSentence(
        DETERMINER_UNSPECIFIED,
        Seq(
          assertionSentence : SilConditionalSentence,
          alternative : SilPredicateSentence),
        SEPARATOR_SEMICOLON
      ) => {
        recognizeAssertionBelief(
          assertionSentence, Some(alternative))
      }
      case _ => {
        recognizeAssertionBelief(sentence)
      }
    }
  }

  private def recognizeStatePredicateBelief(
    sentence : SilSentence,
    predicate : SilStatePredicate,
    tam : SilTam) : Seq[SpcBelief] =
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
              SilNounReference(
                SilWord(SmcLemmas.LEMMA_CONTAINER),
                DETERMINER_UNSPECIFIED,
                COUNT_SINGULAR)),
            container,
            REL_IDENTITY
          ),
          sentence.tam)
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
    if (determiner != DETERMINER_NONSPECIFIC) {
      state match {
        case SilPropertyState(stateName) => {
          val (rr, isGenitive) = ref match {
            case SilGenitiveReference(possessor, _) => {
              resultCollector.referenceMap.get(ref) match {
                // interpret as association, e.g. "the boss's minions"
                case Some(entities) => {
                  tupleN((ref, false))
                }
                // interpret as property, e.g. "the boss's mood"
                case _ => {
                  tupleN((possessor, true))
                }
              }
            }
            case _ => (ref, false)
          }
          return processResolvedReference(sentence, rr, {
            entityRef => {
              if (!isGenitive) {
                Seq(EntityPropertyBelief(
                  sentence,
                  entityRef,
                  None, stateName
                ))
              } else {
                Seq(EntityPropertyBelief(
                  sentence,
                  entityRef,
                  Some(noun), stateName
                ))
              }
            }
          })
        }
        case _ => {
          return Seq.empty
        }
      }
    }
    ref match {
      case SilStateSpecifiedReference(
        _, specifiedState @
          (_ : SilAdpositionalState | _ : SilPropertyState)
      ) => {
        // "a television that is on the blink is broken"
        // or "a television that is busted is broken"
        // or "a busted television is broken"
        if (tam.modality != MODAL_NEUTRAL) {
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
        // FIXME:  interpret tam
        Seq(EntityExistenceBelief(
          sentence,
          SilNounReference(noun, DETERMINER_NONSPECIFIC),
          noun, qualifiers, ""))
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
              sentence, qualifiers.head, Some(noun), state, tam)
          }
        }
        // "a lifeform may be either animal or vegetable"
        definePropertyBelief(
          sentence, noun, None, state, tam)
      }
    }
  }

  private def recognizeRelationshipPredicateBelief(
    sentence : SilSentence,
    predicate : SilRelationshipPredicate,
    tam : SilTam) : Seq[SpcBelief] =
  {
    val subjectRef = predicate.subject
    val complementRef = predicate.complement
    val relationship = predicate.relationship
    subjectRef match {
      case SilNounReference(
        subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        return processFormRelationship(
          sentence, subjectNoun, complementRef, relationship)
      }
      case _ : SilGenitiveReference => {
        complementRef match {
          // "Will's dad is Lonnie"
          case SilNounReference(
            _, _, COUNT_SINGULAR
          ) => {
            // flip subject/complement to match "Lonnie is Will's dad"
            return processEntityRelationship(
              sentence, complementRef,
              subjectRef, relationship)
          }
          case _ : SilGenitiveReference => {
            // "Will's dad is Joyce's ex-husband": resolve "Joyce's ex-husband"
            // to "Lonnie" and then proceed flipping subject/complement
            return processIndirectEntityRelationship(
              sentence, subjectRef, complementRef, relationship)
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
        if (tam.modality != MODAL_NEUTRAL) {
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
        complementRef match {
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
      case _ => {
        // "Lonnie is Will's dad"
        return processEntityRelationship(
          sentence, subjectRef,
          complementRef, relationship)
      }
    }
    Seq.empty
  }

  private def processResolvedReference(
    sentence : SilSentence,
    ref : SilReference,
    processor : (SilReference) => Seq[SpcBelief])
      : Seq[SpcBelief] =
  {
    resultCollector.referenceMap.get(ref) match {
      case Some(set) => {
        if (set.isEmpty) {
          // FIXME for single-valued associations, not sure we
          // should be doing this if the association is already
          // bound
          return Seq(EpsilonBelief(sentence))
        }
        set.toSeq.flatMap(entity => {
          val entityRef = SilNounReference(SilWord(entity.name))
          val seq = processor(entityRef)
          if (seq.isEmpty) {
            return seq
          }
          seq
        })
      }
      case _ => {
        Seq.empty
      }
    }
  }

  private def processIndirectEntityRelationship(
    sentence : SilSentence,
    complementRef : SilReference,
    subjectRef : SilReference,
    relationship : SilRelationship) : Seq[SpcBelief] =
  {
    processResolvedReference(sentence, subjectRef, {
      entityRef => {
        processEntityRelationship(
          sentence,
          entityRef,
          complementRef, relationship)
      }
    })
  }

  private def recognizeAssertionBelief(
    assertionSentence : SilSentence,
    alternative : Option[SilPredicateSentence] = None)
      : Seq[SpcBelief] =
  {
    var invalid = false
    val querier = new SilPhraseRewriter
    assertionSentence match {
      case conditional : SilConditionalSentence => {
        val consequent = conditional.consequent
        def validateConsequent = querier.queryMatcher {
          case SilStateSpecifiedReference(_, _ : SilAdpositionalState) => {
            invalid = true
          }
          case SilNounReference(_, determiner, _) => {
            determiner match {
              case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED |
                  DETERMINER_NONE =>
              case _ => {
                invalid = true
              }
            }
          }
        }
        if (conditional.tamConsequent.modality == MODAL_NEUTRAL) {
          querier.query(validateConsequent, consequent)
        }
        alternative.foreach(alternativeSentence => {
          if (conditional.tamConsequent.modality == MODAL_NEUTRAL) {
            invalid = true
          }
          if (!alternativeSentence.predicate.getModifiers.exists(
            _ match {
              case SilBasicVerbModifier(
                SilWordLemma(LEMMA_OTHERWISE), _) => true
              case _ => false
            }
          )) {
            invalid = true
          }
        })
      }
      case SilPredicateSentence(predicate : SilActionPredicate, tam, _) => {
        def validateAssertion = querier.queryMatcher {
          case SilStateSpecifiedReference(_, _ : SilAdpositionalState) => {
            invalid = true
          }
        }
        tam.modality match {
          case MODAL_MAY | MODAL_POSSIBLE | MODAL_CAPABLE | MODAL_PERMITTED => {
          }
          case _ => {
            invalid = true
          }
        }
        querier.query(validateAssertion, predicate)
      }
      case _ => {
        invalid = true
      }
    }
    if (invalid) {
      Seq.empty
    } else {
      Seq(AssertionBelief(assertionSentence, alternative))
    }
  }

  private def recognizeDirective(
    sentence : SilSentence,
    action : SilWord,
    argument : String) : Seq[SpcBelief] =
  {
    if (action.toLemma == LEMMA_IMPORT) {
      Seq(IndirectBelief(sentence, argument))
    } else {
      Seq.empty
    }
  }

  private def processFormRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complementRef : SilReference,
    relationship : SilRelationship)
      : Seq[SpcBelief] =
  {
    relationship match {
      case REL_IDENTITY => {
        val (complementNoun, qualifiers, count, determiner, failed) =
          extractQualifiedNoun(sentence, complementRef, Seq.empty)
        if (failed) {
          return Seq.empty
        }
        if (!qualifiers.isEmpty) {
          return Seq(UnimplementedBelief(sentence))
        }
        if (count != COUNT_SINGULAR) {
          return Seq(UnimplementedBelief(sentence))
        }
        if (complementNoun.toNounLemma == LEMMA_KIND) {
          // "a dog is a kind of canine"
          complementRef match {
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
          if (sentence.tam.modality == MODAL_NEUTRAL) {
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
        val (complementNouns, count) = complementRef match {
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
              tupleN((complementNoun, count))
            })
            tupleN((pairs.map(_._1), pairs.map(_._2).maxBy(
              _ match {
                case COUNT_SINGULAR => 1
                case COUNT_PLURAL => 2
              })
            ))
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
            tupleN((Seq(complementNoun), count))
          }
        }
        val upper = count match {
          case COUNT_SINGULAR => 1
          case COUNT_PLURAL => Int.MaxValue
        }
        if (sentence.tam.isProgressive) {
          return Seq(UnimplementedBelief(sentence))
        }
        val newConstraint = sentence.tam.modality match {
          case MODAL_NEUTRAL | MODAL_MUST | MODAL_EMPHATIC =>
            // FIXME honor plural here
            SpcCardinalityConstraint(1, 1)
          case MODAL_MAY | MODAL_POSSIBLE |
              MODAL_CAPABLE | MODAL_PERMITTED =>
            SpcCardinalityConstraint(0, upper)
          case MODAL_SHOULD | MODAL_ELLIPTICAL =>
            return Seq(UnimplementedBelief(sentence))
        }
        isPropertyAssoc(sentence, complementRef, relationship).map(
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

  private def processEntityRelationship(
    sentence : SilSentence,
    subjectRef : SilReference,
    complementRef : SilReference,
    relationship : SilRelationship)
      : Seq[SpcBelief] =
  {
    if (sentence.tam.modality != MODAL_NEUTRAL) {
      return Seq(UnimplementedBelief(sentence))
    }
    relationship match {
      case REL_ASSOCIATION => {
        complementRef match {
          case SilNounReference(
            roleNoun,
            DETERMINER_NONE,
            _
          ) => {
            // "Larry has no pets"
            return Seq(EntityNoAssocBelief(
              sentence,
              subjectRef,
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
    complementRef match {
      case SilGenitiveReference(
        sub @ (_ : SilGenitiveReference
          | SilConjunctiveReference(DETERMINER_ALL, _, _)),
        possessee
      ) => {
        // "Lurch is Morticia's children's butler" =>
        // "Lurch is Wednesday's butler", "Lurch is Pugsley's butler"
        //
        // or likewise for
        //
        // "Lurch is (Wednesday and Pugsley)'s butler
        return processResolvedReference(
          sentence,
          sub,
          {
            entityRef => {
              val flattenedComplement = SilGenitiveReference(
                entityRef,
                possessee)
              processEntityRelationship(
                sentence, subjectRef,
                flattenedComplement,
                relationship)
            }
          })
      }
      case _ =>
    }

    complementRef match {
      case SilGenitiveReference(
        possessorRef, SilNounReference(roleNoun, DETERMINER_UNSPECIFIED, _)
      ) => {
        // "Fido is Franny's pet"
        return Seq(EntityAssocBelief(
          sentence,
          possessorRef,
          subjectRef,
          roleNoun,
          sentence.tam.isPositive))
      }
      case _ =>
    }

    val (subjectNoun, subjectDeterminer) = subjectRef match {
      case SilNounReference(noun, determiner, _) => {
        tupleN((noun, determiner))
      }
      case _ => return Seq.empty
    }

    subjectDeterminer match {
      case DETERMINER_UNSPECIFIED | DETERMINER_UNIQUE =>
      case _ => {
        return Seq.empty
      }
    }

    val (complementNoun, qualifiers, count, complementDeterminer, failed) =
      extractQualifiedNoun(sentence, complementRef, Seq.empty)
    if (failed || !qualifiers.isEmpty) {
      return Seq.empty
    }
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
          subjectRef,
          complementNoun,
          Seq(subjectNoun), subjectNoun.toUnfoldedLemma))
      }
      case DETERMINER_UNIQUE => {
        // "The boss is a werewolf"
        Seq(EntityExistenceBelief(
          sentence,
          subjectRef,
          complementNoun,
          Seq.empty, ""))
      }
      case _ => {
        // We don't yet support stuff like "all dogs are werewolves"
        return Seq(UnimplementedBelief(sentence))
      }
    }
  }

  private def definePropertyBelief(
    sentence : SilSentence,
    formName : SilWord,
    propertyName : Option[SilWord],
    state : SilState,
    tam : SilTam)
      : Seq[SpcBelief] =
  {
    // "a light may be on or off"
    if (sentence.tam.modality == MODAL_NEUTRAL) {
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
    val isClosed = (tam.modality == MODAL_MUST)
    Seq(FormPropertyBelief(
      sentence, formName, newStates, isClosed, propertyName))
  }

  private def isPropertyAssoc(
    sentence : SilSentence, complementRef : SilReference,
    relationship : SilRelationship) : Option[Boolean] =
  {
    complementRef match {
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
              SilWordLemma(LEMMA_PROPERTY),
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

  private def isIgnorableModifier(modifier : SilVerbModifier) : Boolean =
  {
    modifier match {
      // "after this | that"
      case SilAdpositionalVerbModifier(
        SilAdposition.AFTER,
        SilPronounReference(
          PERSON_THIRD, GENDER_N, COUNT_SINGULAR,
          DISTANCE_HERE | DISTANCE_THERE)
      ) => true
      case _ => false
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
        noun,
        determiner @ (DETERMINER_NONSPECIFIC | DETERMINER_UNIQUE |
          DETERMINER_UNSPECIFIED),
        COUNT_SINGULAR
      ) => {
        tupleN((noun, preQualifiers, COUNT_SINGULAR, determiner, false))
      }
      case SilNounReference(
        noun, DETERMINER_UNSPECIFIED, COUNT_PLURAL
      ) => {
        tupleN((noun, preQualifiers, COUNT_PLURAL,
          DETERMINER_UNSPECIFIED, false))
      }
      case SilStateSpecifiedReference(
        subRef, state
      ) => {
        extractQualifiedNoun(
          sentence, subRef,
          preQualifiers ++ SilReference.extractQualifiers(state))
      }
      case SilGenitiveReference(
        SilNounReference(
          possessor, possessorDeterminer, COUNT_SINGULAR),
        SilNounReference(
          possession, DETERMINER_UNSPECIFIED, count)
      ) => {
        val failed = possessorDeterminer match {
          case DETERMINER_UNSPECIFIED => false
          case DETERMINER_NONSPECIFIC => false
          case DETERMINER_UNIQUE => false
          case _ => true
        }
        tupleN((possession, Seq(possessor),
          count, possessorDeterminer, failed || !allowGenitive))
      }
      case _ => failedResult
    }
  }
}
