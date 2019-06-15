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

import org.slf4j._

import SprEnglishLemmas._

object SpcBeliefRecognizer
{
  private val logger =
    LoggerFactory.getLogger(classOf[SpcBeliefRecognizer])
}

class SpcBeliefRecognizer(
  val cosmos : SpcCosmos,
  resultCollector : SmcResultCollector[SpcEntity] = SmcResultCollector())
    extends SmcDebuggable(new SmcDebugger(SpcBeliefRecognizer.logger))
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
    sentence match {
      case SilPredicateSentence(
        SilActionPredicate(
          _, verb, Some(SilQuotationReference(quotation)), _),
        tam, formality
      ) => {
        return recognizeDirective(sentence, verb, quotation)
      }
      case _ =>
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
        Seq(conditional : SilConditionalSentence, additional @ _*),
        SEPARATOR_SEMICOLON
      ) if (additional.forall(_.isInstanceOf[SilPredicateSentence])) => {
        recognizeAssertionBelief(
          conditional, additional.map(_.asInstanceOf[SilPredicateSentence]))
      }
      case _ => {
        recognizeAssertionBelief(sentence)
      }
    }
  }

  protected def recognizeStatePredicateBelief(
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
            REL_PREDEF_IDENTITY.toVerb,
            container
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
          val (rr, isPropertyName) = ref match {
            case SilGenitiveReference(possessor, _) => {
              resultCollector.referenceMap.get(ref) match {
                // interpret as association, e.g. "the boss's minions"
                case Some(entities) if (
                  !entities.exists(_.isInstanceOf[SpcTransientEntity])
                ) => {
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
              if (!isPropertyName) {
                // "the cat is angry "
                Seq(EntityPropertyBelief(
                  sentence,
                  entityRef,
                  None, Left(stateName)
                ))
              } else {
                // "the cat's mood is angry "
                Seq(EntityPropertyBelief(
                  sentence,
                  entityRef,
                  Some(noun), Left(stateName)
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
          case SilExistenceState(_) =>
          case _ => {
            return Seq(UnimplementedBelief(sentence))
          }
        }
      }
      case _ =>
    }
    state match {
      case SilExistenceState(_) => {
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
            return defineEnumPropertyBelief(
              sentence, qualifiers.head, Some(noun), state, tam)
          }
        }
        // "a lifeform may be either animal or vegetable"
        defineEnumPropertyBelief(
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
    val verb = predicate.verb
    subjectRef match {
      case SilNounReference(
        subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        return processFormRelationship(
          sentence, subjectNoun, complementRef, verb)
      }
      case SilGenitiveReference(possessor, possessee) => {
        complementRef match {
          case SilNounReference(
            complementNoun, determiner, COUNT_SINGULAR
          ) => {
            if (determiner == DETERMINER_NONSPECIFIC) {
              val formNoun = possessor match {
                // "a thermometer's reading must be a number"
                case SilNounReference(
                  noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
                ) => noun
                case _ => {
                  // "Mary's cat is a pest"
                  return Seq(EntityExistenceBelief(
                    sentence, subjectRef, complementNoun,
                    Seq.empty, ""))
                }
              }
              val propertyNoun = possessee match {
                case SilNounReference(
                  noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
                ) => noun
                case _ => return Seq.empty
              }
              return defineTypedPropertyBelief(
                sentence,
                formNoun,
                propertyNoun,
                complementNoun,
                tam)
            } else {
              // "Will's dad is Lonnie"
              // flip subject/complement to match "Lonnie is Will's dad"
              return processEntityRelationship(
                sentence, complementRef,
                subjectRef, verb)
            }
          }
          case _ : SilGenitiveReference => {
            // "Will's dad is Joyce's ex-husband": resolve "Joyce's ex-husband"
            // to "Lonnie" and then proceed flipping subject/complement
            return processIndirectEntityRelationship(
              sentence, subjectRef, complementRef, verb)
          }
          case SilQuotationReference(quotation) => {
            // "Arnie's catchphrase is <<I'll be back>>"
            return processQuotation(
              sentence, possessor, possessee, quotation, verb)
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
        complementRef match {
          case SilNounReference(
            complementNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
          ) if (subjectRef.isInstanceOf[SilStateSpecifiedReference]) => {
            // "The cat in the hat is a pest"
            // FIXME more constraints ont the subjectRef
            return Seq(EntityExistenceBelief(
              sentence, subjectRef, complementNoun,
              Seq.empty, ""))
          }
          case _ => {
            // "Lonnie is Will's dad"
            return processEntityRelationship(
              sentence, subjectRef,
              complementRef, verb)
          }
        }
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
    verb : SilWord) : Seq[SpcBelief] =
  {
    processResolvedReference(sentence, subjectRef, {
      entityRef => {
        processEntityRelationship(
          sentence,
          entityRef,
          complementRef, verb)
      }
    })
  }

  private def processQuotation(
    sentence : SilSentence,
    entityRef : SilReference,
    propertyRef : SilReference,
    quotation : String,
    verb : SilWord
  ) : Seq[SpcBelief] =
  {
    if (SilRelationshipPredef(verb) != REL_PREDEF_IDENTITY) {
      return Seq.empty
    }
    propertyRef match {
      case SilNounReference(
        propertyName, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
      ) => {
        Seq(EntityPropertyBelief(
          sentence, entityRef, Some(propertyName), Right(quotation)))
      }
      case _ => {
        Seq.empty
      }
    }
  }

  private def extractBasicModifierLemmas(
    predicate : SilPredicate) : Seq[String] =
  {
    predicate.getModifiers.flatMap(
      _ match {
        case SilBasicVerbModifier(
          SilWordLemma(lemma), _) => Some(lemma)
        case _ => None
      }
    )
  }

  private def isConsequentValid(
    isBefore : Boolean,
    isSubsequently : Boolean,
    consequent : SilPredicate,
    biconditional : Boolean,
    antecedentAction : Boolean,
    checkPatterns : Boolean = true) : Boolean =
  {
    val querier = new SilPhraseRewriter
    var invalid = false
    if (isBefore && isSubsequently) {
      trace("BEFORE INCOMPATIBLE WITH SUBSEQUENTLY")
      invalid = true
    }
    def validateConsequent() = querier.queryMatcher {
      case SilNounReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED |
              DETERMINER_NONE | DETERMINER_NONSPECIFIC =>
          case _ => {
            trace(s"UNSUPPORTED DETERMINER $determiner")
            invalid = true
          }
        }
      }
      case sp : SilStatePredicate => {
        val predef = SilStatePredef(sp.verb)
        if (biconditional) {
          if ((predef == STATE_PREDEF_BECOME) || isSubsequently) {
            trace("CONDITION EXPECTED")
            invalid = true
          }
        } else {
          if (antecedentAction &&
            (predef != STATE_PREDEF_BECOME) && !isSubsequently)
          {
            trace("EVENT EXPECTED")
            invalid = true
          }
        }
      }
      case rp : SilRelationshipPredicate => {
        val predef = SilRelationshipPredef(rp.verb)
        if (biconditional) {
          if ((predef == REL_PREDEF_BECOME) || isSubsequently) {
            trace("CONDITION EXPECTED")
            invalid = true
          }
        } else {
          if (antecedentAction &&
            (predef == REL_PREDEF_IDENTITY) && !isSubsequently)
          {
            trace("EVENT EXPECTED")
            invalid = true
          }
        }
      }
    }
    if (checkPatterns) {
      querier.query(validateConsequent, consequent)
    }
    !invalid
  }

  private def recognizeAssertionBelief(
    assertionSentence : SilSentence,
    additionalSentences : Seq[SilPredicateSentence] = Seq.empty)
      : Seq[SpcBelief] =
  {
    var invalid = false
    var ignored = false
    val additionalConsequents = new mutable.ArrayBuffer[SilPredicateSentence]
    var alternative : Option[SilPredicateSentence] = None
    assertionSentence match {
      case conditional : SilConditionalSentence => {
        val antecedentAction =
          conditional.antecedent.isInstanceOf[SilActionPredicate]
        val consequent = conditional.consequent
        val isAfter = (conditional.conjunction.toLemma == LEMMA_AFTER)
        val isBefore = (conditional.conjunction.toLemma == LEMMA_BEFORE)
        val isConsequentSubsequently = isAfter ||
          extractBasicModifierLemmas(consequent).
            contains(LEMMA_SUBSEQUENTLY)
        val consequentNonModal =
          (conditional.tamConsequent.unemphaticModality == MODAL_NEUTRAL)
        if (consequentNonModal) {
          if (isBefore) {
            trace("BEFORE REQUIRES CONSTRAINT")
            invalid = true
          }
          if (conditional.conjunction.toLemma != LEMMA_IF) {
            if (conditional.biconditional) {
              trace("EQUIVALENTLY REQUIRES IF")
              invalid = true
            }
          }
        } else {
          if (isAfter) {
            // FIXME implement this as a postcondition
            trace("AFTER INCOMPATIBLE WITH MODAL")
            invalid = true
          }
          if (conditional.biconditional) {
            trace("MODAL PROHIBITED")
            invalid = true
          }
        }
        if (!isConsequentValid(
          isBefore,
          isConsequentSubsequently,
          consequent,
          conditional.biconditional,
          antecedentAction,
          consequentNonModal
        )) {
          invalid = true
        }
        additionalSentences.foreach(additionalSentence => {
          val modifiers = extractBasicModifierLemmas(
            additionalSentence.predicate)
          val isOtherwise = modifiers.contains(LEMMA_OTHERWISE)
          val isAlso = modifiers.contains(LEMMA_ALSO)
          val isSubsequently = modifiers.contains(LEMMA_SUBSEQUENTLY)
          if (!isConsequentValid(
            isBefore,
            isSubsequently || isConsequentSubsequently,
            additionalSentence.predicate,
            conditional.biconditional,
            antecedentAction
          )) {
            invalid = true
          }
          if (isOtherwise && isAlso) {
            trace("OTHERWISE INCOMPATIBLE WITH ALSO")
            invalid = true
          } else if (isOtherwise) {
            if (conditional.biconditional) {
              trace("OTHERWISE INCOMPATIBLE WITH EQUIVALENTLY")
              invalid = true
            }
            if (conditional.tamConsequent.unemphaticModality == MODAL_NEUTRAL) {
              trace("MODAL REQUIRED")
              invalid = true
            }
            if (additionalConsequents.nonEmpty) {
              trace("OTHERWISE MUST BE LAST")
              invalid = true
            }
            if (alternative.nonEmpty) {
              trace("ONLY ONE OTHERWISE ALLOWED")
              invalid = true
            } else {
              alternative = Some(additionalSentence)
            }
          } else if (isAlso) {
            if (conditional.biconditional) {
              trace("ALSO INCOMPATIBLE WITH EQUIVALENTLY")
              invalid = true
            }
            if (alternative.nonEmpty) {
              trace("OTHERWISE MUST BE LAST")
              invalid = true
            }
            if (additionalSentence.tam.unemphaticModality != MODAL_NEUTRAL) {
              trace("MODAL PROHIBITED")
              invalid = true
            }
            additionalConsequents += additionalSentence
          } else {
            trace("OTHERWISE OR ALSO EXPECTED")
            invalid = true
          }
        })
      }
      case SilPredicateSentence(predicate : SilActionPredicate, tam, _) => {
        if (additionalSentences.nonEmpty) {
          trace("ALSO INCOMPATIBLE WITH CONSTRAINT")
          invalid = true
        }
        val querier = new SilPhraseRewriter
        def validateAssertion = querier.queryMatcher {
          case SilStateSpecifiedReference(_, _ : SilAdpositionalState) => {
            ignored = true
          }
        }
        querier.query(validateAssertion, predicate)
        tam.modality match {
          case MODAL_MAY | MODAL_POSSIBLE | MODAL_CAPABLE | MODAL_PERMITTED => {
          }
          case _ => {
            ignored = true
          }
        }
      }
      case _ => {
        trace(s"UNRECOGNIZED ASSERTION $assertionSentence")
        invalid = true
      }
    }
    if (invalid) {
      Seq(InvalidBelief(assertionSentence))
    } else if (ignored) {
      Seq.empty
    } else {
      Seq(AssertionBelief(
        assertionSentence, additionalConsequents, alternative))
    }
  }

  private def recognizeDirective(
    sentence : SilSentence,
    verb : SilWord,
    argument : String) : Seq[SpcBelief] =
  {
    if (verb.toLemma == LEMMA_IMPORT) {
      Seq(IndirectBelief(sentence, argument))
    } else {
      Seq.empty
    }
  }

  private def processFormRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    complementRef : SilReference,
    verb : SilWord)
      : Seq[SpcBelief] =
  {
    SilRelationshipPredef(verb) match {
      case REL_PREDEF_IDENTITY => {
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
      case REL_PREDEF_BECOME => {
        Seq.empty
      }
      case REL_PREDEF_ASSOC => {
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
        isPropertyAssoc(sentence, complementRef, verb).map(
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
    verb : SilWord)
      : Seq[SpcBelief] =
  {
    if (sentence.tam.modality != MODAL_NEUTRAL) {
      return Seq(UnimplementedBelief(sentence))
    }
    SilRelationshipPredef(verb) match {
      case REL_PREDEF_ASSOC => {
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
          case SilStateSpecifiedReference(
            SilNounReference(
              roleNoun,
              DETERMINER_NONSPECIFIC,
              _
            ),
            state
          ) => {
            // "Larry has a dirty dog"
            return indefiniteEntityAssocBelief(
              sentence, subjectRef,
              state,
              roleNoun)
          }
          case SilNounReference(
            roleNoun,
            DETERMINER_NONSPECIFIC,
            _
          ) => {
            // "Larry has a dog"
            return indefiniteEntityAssocBelief(
              sentence, subjectRef,
              SilExistenceState(),
              roleNoun)
          }
          case _ => {
            // FIXME other interesting cases such as
            // "Larry has dogs", "Larry has the dogs"
            return Seq(UnimplementedBelief(sentence))
          }
        }
      }
      case _ =>
    }
    complementRef match {
      case SilGenitiveReference(
        sub @ (_ : SilStateSpecifiedReference | _ : SilGenitiveReference |
          SilConjunctiveReference(DETERMINER_ALL, _, _)),
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
                verb)
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
          false,
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

  private def indefiniteEntityAssocBelief(
    sentence : SilSentence,
    subjectRef : SilReference,
    state : SilState,
    roleNoun : SilWord) =
  {
    Seq(EntityAssocBelief(
      sentence,
      subjectRef,
      SilStateSpecifiedReference(
        SilNounReference(roleNoun, DETERMINER_UNIQUE),
        state),
      true,
      roleNoun
    ))
  }

  private def defineTypedPropertyBelief(
    sentence : SilSentence,
    formName : SilWord,
    propertyName : SilWord,
    domainName : SilWord,
    tam : SilTam) : Seq[SpcBelief] =
  {
    if (tam.modality != MODAL_MUST) {
      return Seq.empty
    }
    val domain = SpcPropertyDomain(domainName.toNounLemma).getOrElse {
      return Seq(UnimplementedBelief(sentence))
    }
    Seq(FormTypedPropertyBelief(
      sentence, formName, propertyName, domain))
  }

  private def defineEnumPropertyBelief(
    sentence : SilSentence,
    formName : SilWord,
    propertyName : Option[SilWord],
    state : SilState,
    tam : SilTam) : Seq[SpcBelief] =
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
    Seq(FormEnumPropertyBelief(
      sentence, formName, newStates, isClosed, propertyName))
  }

  private def isPropertyAssoc(
    sentence : SilSentence, complementRef : SilReference,
    verb : SilWord) : Option[Boolean] =
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
                if (SilRelationshipPredef(verb) == REL_PREDEF_ASSOC) {
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
