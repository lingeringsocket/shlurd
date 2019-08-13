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
import SprPennTreebankLabels._
import ShlurdExceptionCode._

class SubjectConjunction(determiner : SilDeterminer)
{
  private var checked = false

  private var checkFailed = false

  def fail() : Boolean =
  {
    checkFailed = true
    false
  }

  def check() : SilDeterminer =
  {
    checked = true
    determiner
  }

  def checkAnd() : Boolean =
  {
    check match {
      case DETERMINER_ALL | DETERMINER_UNSPECIFIED => true
      case _ => fail
    }
  }

  def checkAndPluralOrSingular(count : SilCount) : Boolean =
  {
    check match {
      // "a dog or a cat is a kind of pet"
      case DETERMINER_ANY => (count == COUNT_SINGULAR)
      // "dogs and cats are kinds of pet"
      case DETERMINER_ALL => (count == COUNT_PLURAL)
      // "a dog is a kind of pet"
      // "dogs are a kind of pet"
      case DETERMINER_UNSPECIFIED => true
      case _ => fail
    }
  }

  def checkFinal(seq : Seq[SpcBelief]) : Seq[SpcBelief] =
  {
    if (checkFailed || (!checked && (determiner != DETERMINER_UNSPECIFIED))) {
      Seq.empty
    } else {
      seq
    }
  }
}

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
    assert(!finished)
    finished = true
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
    if (sentence.hasUnknown) {
      return Seq.empty
    }
    sentence matchPartial {
      case SilPredicateSentence(
        SilActionPredicate(
          _, verb, Some(SilQuotationReference(quotation)), _),
        tam, formality
      ) => {
        return recognizeDirective(sentence, verb, quotation)
      }
    }
    if (!sentence.tam.isIndicative) {
      // FIXME support interrogative
      return Seq.empty
    }
    sentence match {
      case SilPredicateSentence(predicate, tam, formality) => {
        if (predicate.getModifiers.filterNot(isIgnorableModifier).isEmpty) {
          predicate matchPartial {
            case statePredicate : SilStatePredicate => {
              val (components, conjunction) =
                decomposeSubject(statePredicate)
              return conjunction.checkFinal(
                components.flatMap(component => {
                  recognizeStatePredicateBelief(
                    sentence, component, tam, conjunction)
                })
              )
            }
            case relationshipPredicate : SilRelationshipPredicate => {
              if (recognizeWordRule(sentence).isEmpty) {
                val (components, conjunction) =
                  decomposeSubject(relationshipPredicate)
                return conjunction.checkFinal(
                  components.flatMap(component => {
                    recognizeRelationshipPredicateBelief(
                      sentence, component, tam, conjunction)
                  })
                )
              }
            }
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
      case SilConjunctiveSentence(
        DETERMINER_ALL,
        sentences,
        _
      ) => {
        sentences.flatMap(recognizeBeliefsImpl)
      }
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
          tam2 == SilTam.indicative &&
          matchAssocInverse(
            antecedentSubject,
            antecedentComplement,
            consequentSubject,
            consequentComplement
          ).nonEmpty
      ) => {
        val (
          possesseeForm, possessorForm,
          possesseeRole, possessorRole
        ) = matchAssocInverse(
          antecedentSubject,
          antecedentComplement,
          consequentSubject,
          consequentComplement
        ).get
        Seq(InverseAssocBelief(
          sentence, possessorForm, possessorRole,
          possesseeForm, possesseeRole))
      }
      case _ => {
        recognizeAssertionBelief(sentence)
      }
    }
  }

  private def decomposeSubject[PredicateType <: SilPredicate](
    predicate : PredicateType) : (Seq[PredicateType], SubjectConjunction) =
  {
    predicate.getSubject match {
      case SilConjunctiveReference(
        determiner @ (DETERMINER_ANY | DETERMINER_ALL), references, _) => {
        tupleN((
          references.map(reference => {
            predicate.withNewSubject(reference).asInstanceOf[PredicateType]
          }),
          new SubjectConjunction(determiner)
        ))
      }
      case _ => tupleN((
        Seq(predicate),
        new SubjectConjunction(DETERMINER_UNSPECIFIED)))
    }
  }

  private def matchAssocInverse(
    antecedentSubject : SilReference,
    antecedentComplement : SilReference,
    complementSubject : SilReference,
    consequentComplement : SilReference
  ) : Option[(SilWord, SilWord, SilWord, SilWord)] =
  {
    val antecedent = matchAssocIdentity(
      antecedentSubject,
      antecedentComplement,
      DETERMINER_NONSPECIFIC,
      false)
    val consequent = matchAssocIdentity(
      complementSubject,
      consequentComplement,
      DETERMINER_UNIQUE,
      antecedent.map(_._4).getOrElse(false))
    // FIXME also need to verify that FIRST/SECOND line up correctly
    tupleN((antecedent, consequent)) match {
      case (
        Some((af1, af2, ar, _)),
        Some((cf1, cf2, cr, _))) => {
        if ((af1 == cf2) && (af2 == cf1)) {
          Some(tupleN((af1, af2, ar, cr)))
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private def matchAssocIdentity(
    r1 : SilReference,
    r2 : SilReference,
    expectedDeterminer : SilDeterminer,
    otherFlipped : Boolean,
    flipped : Boolean = false
  ) : Option[(SilWord, SilWord, SilWord, Boolean)] =
  {
    tupleN((
      matchNoun(r1, expectedDeterminer, flipped),
      matchGenitive(r2, expectedDeterminer, !flipped)
    )) match {
      case (Some((w1, q1)), Some((w2, w3, q2))) => {
        val valid = {
          expectedDeterminer match {
            case DETERMINER_NONSPECIFIC => {
              val qualifiers = Seq(q1, q2).flatten
              if (w1.toLemma == w2.toLemma) {
                if ((flipped && q2.nonEmpty) || (!flipped && q1.nonEmpty)) {
                  false
                } else {
                  (qualifiers.size == 1) &&
                    (qualifiers.head.toLemma == LEMMA_ANOTHER)
                }
              } else {
                qualifiers.isEmpty
              }
            }
            case DETERMINER_UNIQUE => {
              val qualifiers = Seq(q1, q2).flatten
              if (w1.toLemma == w2.toLemma) {
                val requiredOrder = {
                  if (flipped != otherFlipped) {
                    Seq(LEMMA_FIRST, LEMMA_SECOND)
                  } else {
                    Seq(LEMMA_SECOND, LEMMA_FIRST)
                  }
                }
                qualifiers.map(_.toLemma).sameElements(requiredOrder)
              } else {
                qualifiers.isEmpty
              }
            }
            case _ => false
          }
        }
        if (valid) {
          Some(tupleN((w1, w2, w3, flipped)))
        } else {
          None
        }
      }
      case _ => {
        if (!flipped) {
          matchAssocIdentity(r2, r1, expectedDeterminer, otherFlipped, true)
        } else {
          None
        }
      }
    }
  }

  private def matchNoun(
    ref : SilReference, expectedDeterminer : SilDeterminer,
    isComplement : Boolean
  ) : Option[(SilWord, Option[SilWord])] =
  {
    ref match {
      case SilNounReference(
        noun, determiner, COUNT_SINGULAR
      ) if (
        determiner == expectedDeterminer
      )  => {
        Some((noun, None))
      }
      case SilStateSpecifiedReference(
        SilNounReference(noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        SilPropertyState(
          qualifier @ SilWordLemma(LEMMA_ANOTHER)
        )
      ) if (
        isComplement &&
          (expectedDeterminer == DETERMINER_NONSPECIFIC)
      ) => {
        Some((noun, Some(qualifier)))
      }
      case SilStateSpecifiedReference(
        SilNounReference(noun, DETERMINER_UNIQUE, COUNT_SINGULAR),
        SilPropertyState(
          qualifier @ (
            SilWordLemma(LEMMA_FIRST) |
              SilWordLemma(LEMMA_SECOND)
          )
        )
      ) if (
        expectedDeterminer == DETERMINER_UNIQUE
      ) => {
        Some((noun, Some(qualifier)))
      }
      case _ => None
    }
  }

  private def matchGenitive(
    ref : SilReference, expectedDeterminer : SilDeterminer,
    isComplement : Boolean
  ) : Option[(SilWord, SilWord, Option[SilWord])] =
  {
    val intermediate = ref match {
      case SilGenitiveReference(
        SilNounReference(possessor, determiner, COUNT_SINGULAR),
        possessee
      ) if (
        determiner == expectedDeterminer
      ) => {
        Some(tupleN((possessor, possessee, None)))
      }
      case SilGenitiveReference(
        SilStateSpecifiedReference(
          SilNounReference(possessor, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
          SilPropertyState(
            qualifier @ SilWordLemma(LEMMA_ANOTHER)
          )
        ),
        possessee
      ) if (
        isComplement &&
          (expectedDeterminer == DETERMINER_NONSPECIFIC)
      ) => {
        Some(tupleN((possessor, possessee, Some(qualifier))))
      }
      case SilGenitiveReference(
        SilStateSpecifiedReference(
          SilNounReference(possessor, DETERMINER_UNIQUE, COUNT_SINGULAR),
          SilPropertyState(
            qualifier @ (
              SilWordLemma(LEMMA_FIRST) |
                SilWordLemma(LEMMA_SECOND)
            )
          )
        ),
        possessee
      ) if (
        expectedDeterminer == DETERMINER_UNIQUE
      ) => {
        Some(tupleN((possessor, possessee, Some(qualifier))))
      }
      case _ => None
    }
    intermediate.flatMap {
      case (
        possessor,
        SilNounReference(possessee, DETERMINER_UNSPECIFIED, COUNT_SINGULAR),
        qualifier
      ) => {
        Some(tupleN((possessor, possessee, qualifier)))
      }
      case _ => None
    }
  }

  protected def recognizeStatePredicateBelief(
    sentence : SilSentence,
    predicate : SilStatePredicate,
    tam : SilTam,
    subjectConjunction : SubjectConjunction) : Seq[SpcBelief] =
  {
    val ref = predicate.subject
    val state = predicate.state
    state matchPartial {
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
          sentence.tam,
          subjectConjunction)
      }
    }
    val specifiedStateOpt = ref match {
      case SilStateSpecifiedReference(
        _, specifiedState @
          (_ : SilAdpositionalState | _ : SilPropertyState)
      ) => Some(specifiedState)
      case _ => None
    }
    val allowAdpositions = specifiedStateOpt.nonEmpty
    // FIXME we should not be allowing genitives here except
    // in certain cases
    val (noun, qualifiers, count, determiner, failed) =
      extractQualifiedNoun(sentence, ref, Seq.empty, true, allowAdpositions)
    if (failed) {
      return Seq.empty
    }
    if (determiner != DETERMINER_NONSPECIFIC) {
      state match {
        case SilPropertyState(stateName) => {
          val (rr, isPropertyName) = ref match {
            case SilGenitiveReference(possessor, _) => {
              resultCollector.lookup(ref) match {
                // interpret as association, e.g. "the boss's minions"
                case Some(entities) if (
                  entities.nonEmpty &&
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
          val prechecks = {
            if ((determiner == DETERMINER_UNIQUE) &&
              (count == COUNT_SINGULAR))
            {
              Seq(UniquenessBelief(sentence, rr))
            } else {
              Seq.empty
            }
          }
          return prechecks ++ processResolvedReference(sentence, rr, {
            entityRef => {
              if (!isPropertyName) {
                // "the cat is angry "
                subjectConjunction.checkAnd
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
        case SilExistenceState(_) if (isProperSubject(noun, count)) => {
          // "Beelzebub exists"
          subjectConjunction.checkAnd
          return Seq(EntityExistenceBelief(
            sentence,
            ref,
            SpcForm.tentativeName(noun),
            Seq(noun),
            noun.toUnfoldedLemma))
        }
        case _ => {
          return Seq.empty
        }
      }
    }
    specifiedStateOpt.foreach(specifiedState => {
      // "a television that is on the blink is broken"
      // or "a television that is busted is broken"
      // or "a busted television is broken"
      if (tam.modality != MODAL_NEUTRAL) {
        return Seq(UnimplementedBelief(sentence))
      }
      // FIXME assert something about qualifiers here
      state match {
        case SilExistenceState(_) =>
        case _ => {
          return Seq(UnimplementedBelief(sentence))
        }
      }
    })
    state match {
      case SilExistenceState(_) => {
        subjectConjunction.checkAnd
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
        subjectConjunction.checkAndPluralOrSingular(count)
        defineEnumPropertyBelief(
          sentence, noun, None, state, tam)
      }
    }
  }

  private def recognizeRelationshipPredicateBelief(
    sentence : SilSentence,
    predicate : SilRelationshipPredicate,
    tam : SilTam,
    subjectConjunction : SubjectConjunction) : Seq[SpcBelief] =
  {
    val subjectRef = predicate.subject
    val complementRef = predicate.complement
    val verb = predicate.verb
    if ((SilRelationshipPredef(verb) == REL_PREDEF_IDENTITY) &&
      sentence.tam.unemphaticModality == MODAL_NEUTRAL
    ) {
      val (kindOpt, aliasOpt) = complementRef match {
        case SilStateSpecifiedReference(
          SilNounReference(
            SilWordLemma(LEMMA_KIND),
            kindDeterminer, kindCount),
          SilAdpositionalState(
            SilAdposition.OF,
            SilNounReference(
              hypernymIdealName,
              DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED,
              _))
        ) if (
          (kindDeterminer == DETERMINER_NONSPECIFIC) ||
            ((kindDeterminer == DETERMINER_UNSPECIFIED) &&
              (kindCount == COUNT_PLURAL))
        ) => {
          tupleN((Some(hypernymIdealName), None))
        }
        case SilStateSpecifiedReference(
          SilNounReference(
            SilWordLemma(LEMMA_SAME),
            DETERMINER_UNIQUE, COUNT_SINGULAR),
          SilAdpositionalState(
            SilAdposition.AS,
            SilNounReference(
              idealName,
              determiner,
              count))
        ) if (compatibleDeterminerAndCount(determiner, count)) => {
          tupleN((None, Some(idealName)))
        }
        case _ => tupleN((None, None))
      }
      if (kindOpt.nonEmpty || aliasOpt.nonEmpty) {
        subjectRef matchPartial {
          case nr : SilNounReference if (
            compatibleDeterminerAndCount(nr.determiner, nr.count)
          ) => {
            subjectConjunction.checkAndPluralOrSingular(nr.count)
            kindOpt.foreach(hypernymIdealName => {
              // "a dog is a kind of canine"
              return Seq(FormTaxonomyBelief(
                sentence, nr.noun, hypernymIdealName))
            })
            aliasOpt.foreach(idealName => {
              // "a fridge is the same as a refrigerator"
              return Seq(IdealAliasBelief(
                sentence, nr.noun, idealName))
            })
          }
          case SilGenitiveReference(
            SilNounReference(
              possessorNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR),
            SilNounReference(
              roleNoun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR)
          ) => {
            kindOpt.foreach(hypernymIdealName => {
              // "a person's brother is a kind of sibling"
              return Seq(RoleTaxonomyBelief(
                sentence, possessorNoun, roleNoun, hypernymIdealName, true))
            })
            aliasOpt.foreach(idealName => {
              // "a person's auntie is the same as an aunt"
              // FIXME we should support "his/her/its aunt" also
              return Seq(IdealAliasBelief(
                sentence, roleNoun, idealName, Some(possessorNoun)))
            })
          }
        }
      }
    }
    subjectRef match {
      case SilNounReference(
        subjectNoun, subjectDeterminer, subjectCount
      ) if (compatibleDeterminerAndCount(subjectDeterminer, subjectCount)) => {
        return processIdealRelationship(
          sentence, subjectNoun, subjectCount,
          complementRef, verb, subjectConjunction)
      }
      case SilGenitiveReference(possessor, possessee) => {
        complementRef matchPartial {
          case SilNounReference(
            complementNoun, determiner, COUNT_SINGULAR
          ) => {
            if (determiner == DETERMINER_NONSPECIFIC) {
              val formNoun = possessor match {
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
              val possesseeNoun = possessee match {
                case SilNounReference(
                  noun, DETERMINER_UNSPECIFIED, COUNT_SINGULAR
                ) => noun
                case _ => return Seq.empty
              }
              if (SpcPropertyDomain(complementNoun.toNounLemma).nonEmpty) {
                // "a thermometer's reading must be a number"
                return defineTypedPropertyBelief(
                  sentence,
                  formNoun,
                  possesseeNoun,
                  complementNoun,
                  tam)
              } else {
                // "an informant's handler must be a spy"
                return processRoleTaxonomy(
                  sentence,
                  formNoun,
                  possesseeNoun,
                  complementNoun,
                  verb
                )
              }
            } else {
              // "Will's dad is Lonnie"
              // flip subject/complement to match "Lonnie is Will's dad"
              return processEntityRelationship(
                sentence, complementRef,
                subjectRef, verb, subjectConjunction)
            }
          }
          case _ : SilGenitiveReference => {
            // "Will's dad is Joyce's ex-husband": resolve "Joyce's ex-husband"
            // to "Lonnie" and then proceed flipping subject/complement
            return processIndirectEntityRelationship(
              sentence, subjectRef, complementRef, verb, subjectConjunction)
          }
          case SilQuotationReference(quotation) => {
            // "Arnie's catchphrase is <<I'll be back>>"
            return processQuotation(
              sentence, possessor, possessee, quotation, verb)
          }
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
              complementRef, verb, subjectConjunction)
          }
        }
      }
    }
    Seq.empty
  }

  private def compatibleDeterminerAndCount(
    determiner : SilDeterminer,
    count : SilCount) : Boolean =
  {
    tupleN((determiner, count)) match {
      case (DETERMINER_UNSPECIFIED, COUNT_PLURAL) => true
      case (DETERMINER_NONSPECIFIC, COUNT_SINGULAR) => true
      case _ => false
    }
  }

  private def processResolvedReference(
    sentence : SilSentence,
    ref : SilReference,
    processor : (SilReference) => Seq[SpcBelief])
      : Seq[SpcBelief] =
  {
    resultCollector.lookup(ref) match {
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
    verb : SilWord,
    subjectConjunction : SubjectConjunction) : Seq[SpcBelief] =
  {
    processResolvedReference(sentence, subjectRef, {
      entityRef => {
        processEntityRelationship(
          sentence,
          entityRef,
          complementRef, verb, subjectConjunction)
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
        case SilBasicVerbModifier(SilWordLemma(lemma)) => Some(lemma)
        case _ => None
      }
    )
  }

  private def validateConsequent(
    isBefore : Boolean,
    isUnidirectional : Boolean,
    consequent : SilPredicate,
    biconditional : Boolean,
    antecedentEvent : Boolean,
    checkPatterns : Boolean = true) : Option[ShlurdExceptionCode] =
  {
    val querier = new SilPhraseRewriter
    var exceptionCode : Option[ShlurdExceptionCode] = None
    def reportException(code : ShlurdExceptionCode)
    {
      trace(s"INVALID ASSERTION:  $code")
      if (exceptionCode.isEmpty) {
        exceptionCode = Some(code)
      }
    }
    if (isBefore && isUnidirectional) {
      reportException(AssertionModifiersIncompatible)
    }
    def visitConsequent() = querier.queryMatcher {
      case SilNounReference(_, determiner, _) => {
        determiner match {
          case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED |
              DETERMINER_NONE | DETERMINER_NONSPECIFIC =>
          case _ => {
            reportException(QuantifierNotYetImplemented)
          }
        }
      }
      case sp : SilStatePredicate => {
        val predef = SilStatePredef(sp.verb)
        if (biconditional) {
          if ((predef == STATE_PREDEF_BECOME) || isUnidirectional) {
            reportException(ConsequentConditionExpected)
          }
        } else {
          if (antecedentEvent &&
            (predef != STATE_PREDEF_BECOME) && !isUnidirectional)
          {
            reportException(ConsequentEventExpected)
          }
        }
      }
      case rp : SilRelationshipPredicate => {
        val predef = SilRelationshipPredef(rp.verb)
        if (biconditional) {
          if ((predef == REL_PREDEF_BECOME) || isUnidirectional) {
            reportException(ConsequentConditionExpected)
          }
        } else {
          if (antecedentEvent &&
            (predef == REL_PREDEF_IDENTITY) && !isUnidirectional)
          {
            reportException(ConsequentEventExpected)
          }
        }
      }
    }
    if (checkPatterns) {
      querier.query(visitConsequent, consequent)
    }
    exceptionCode
  }

  private def recognizeAssertionBelief(
    assertionSentence : SilSentence,
    additionalSentences : Seq[SilPredicateSentence] = Seq.empty)
      : Seq[SpcBelief] =
  {
    var exceptionCode : Option[ShlurdExceptionCode] = None
    def reportException(code : ShlurdExceptionCode)
    {
      trace(s"INVALID ASSERTION:  $code")
      if (exceptionCode.isEmpty) {
        exceptionCode = Some(code)
      }
    }
    var ignored = false
    val additionalConsequents = new mutable.ArrayBuffer[SilPredicateSentence]
    var alternative : Option[SilPredicateSentence] = None
    assertionSentence match {
      case conditional : SilConditionalSentence => {
        val antecedentEvent = conditional.antecedent match {
          case _ : SilActionPredicate => true
          case rp : SilRelationshipPredicate if (
            SilRelationshipPredef(rp.verb) == REL_PREDEF_BECOME
          ) => true
          case sp : SilStatePredicate if (
            SilStatePredef(sp.verb) == STATE_PREDEF_BECOME
          ) => true
          case _ => false
        }
        val consequent = conditional.consequent
        val isAfter = (conditional.conjunction.toLemma == LEMMA_AFTER)
        val isBefore = (conditional.conjunction.toLemma == LEMMA_BEFORE)
        val consequentModifiers = extractBasicModifierLemmas(consequent)
        val isConsequentOtherwise =
          consequentModifiers.contains(LEMMA_OTHERWISE)
        val isConsequentAlso = consequentModifiers.contains(LEMMA_ALSO)
        val isConsequentSubsequently = isAfter ||
          consequentModifiers.contains(LEMMA_SUBSEQUENTLY)
        val isConsequentImplication =
          consequentModifiers.contains(LEMMA_CONSEQUENTLY)
        if (!antecedentEvent && !conditional.biconditional &&
          !isConsequentImplication)
        {
          reportException(AntecedentEventExpected)
        }
        if (isConsequentOtherwise || isConsequentAlso) {
          reportException(AssertionModifierSequence)
        }
        if (isConsequentImplication && isConsequentSubsequently) {
          reportException(AssertionModifiersIncompatible)
        }
        val consequentNonModal =
          (conditional.tamConsequent.unemphaticModality == MODAL_NEUTRAL)
        if (consequentNonModal) {
          def isGenitiveRelationship(predicate : SilPredicate) : Boolean =
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
          if (
            (conditional.conjunction.toLemma == LEMMA_IF) &&
            isGenitiveRelationship(conditional.antecedent) &&
              isGenitiveRelationship(conditional.consequent)
          )
          {
            reportException(AssertionInvalidAssociation)
          }
          if (isBefore) {
            reportException(ConsequentConstraintExpected)
          }
          if (conditional.conjunction.toLemma != LEMMA_IF) {
            if (conditional.biconditional) {
              reportException(EquivalenceIfExpected)
            }
          }
        } else {
          if (isAfter) {
            // FIXME implement this as a postcondition
            reportException(PostConstraintNotYetImplemented)
          }
          if (conditional.biconditional) {
            reportException(AssertionModalProhibited)
          }
        }
        validateConsequent(
          isBefore,
          isConsequentSubsequently || isConsequentImplication,
          consequent,
          conditional.biconditional,
          antecedentEvent,
          consequentNonModal
        ).foreach(code => reportException(code))
        additionalSentences.foreach(additionalSentence => {
          val modifiers = extractBasicModifierLemmas(
            additionalSentence.predicate)
          val isOtherwise = modifiers.contains(LEMMA_OTHERWISE)
          val isAlso = modifiers.contains(LEMMA_ALSO)
          val isSubsequently = modifiers.contains(LEMMA_SUBSEQUENTLY)
          val isImplication = modifiers.contains(LEMMA_CONSEQUENTLY)
          if (isSubsequently && isImplication) {
            reportException(AssertionModifiersIncompatible)
          }
          validateConsequent(
            isBefore,
            isSubsequently || isConsequentSubsequently ||
              isConsequentImplication || isImplication,
            additionalSentence.predicate,
            conditional.biconditional,
            antecedentEvent
          ).foreach(code => reportException(code))
          if (isOtherwise && isAlso) {
            reportException(AssertionModifiersIncompatible)
          } else if (isOtherwise) {
            if (conditional.biconditional) {
              reportException(AssertionModifiersIncompatible)
            }
            if (conditional.tamConsequent.unemphaticModality == MODAL_NEUTRAL) {
              reportException(ConsequentConstraintExpected)
            }
            if (additionalConsequents.nonEmpty) {
              reportException(AssertionModifierSequence)
            }
            if (alternative.nonEmpty) {
              reportException(AssertionModifierSequence)
            } else {
              alternative = Some(additionalSentence)
            }
          } else if (isAlso) {
            if (conditional.biconditional) {
              reportException(AssertionModifiersIncompatible)
            }
            if (alternative.nonEmpty) {
              reportException(AssertionModifierSequence)
            }
            if (additionalSentence.tam.unemphaticModality != MODAL_NEUTRAL) {
              reportException(AssertionModalProhibited)
            }
            additionalConsequents += additionalSentence
          } else {
            reportException(AssertionModifierSequence)
          }
        })
      }
      case SilPredicateSentence(predicate : SilActionPredicate, tam, _) => {
        if (additionalSentences.nonEmpty) {
          reportException(AssertionModifiersIncompatible)
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
        if (recognizeWordRule(assertionSentence).isEmpty) {
          trace(s"UNRECOGNIZED ASSERTION $assertionSentence")
          reportException(ShlurdExceptionCode.InvalidBelief)
        }
      }
    }
    if (exceptionCode.nonEmpty) {
      Seq(InvalidBelief(assertionSentence, exceptionCode.get))
    } else if (ignored) {
      Seq.empty
    } else {
      Seq(AssertionBelief(
        assertionSentence, additionalConsequents, alternative))
    }
  }

  def recognizeWordRule(
    sentence : SilSentence
  ) : Option[SprWordRule] =
  {
    sentence match {
      case SilPredicateSentence(
        SilRelationshipPredicate(
          quotation : SilQuotationReference,
          SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          interpretation,
          Seq()
        ),
        tam,
        _
      ) => {
        if (
          tam.isNegative || !tam.isIndicative || !tam.isPresent ||
          (tam.aspect != ASPECT_SIMPLE)
        ) {
          None
        } else {
          val labels = recognizeWordLabels(interpretation)
          if (labels.nonEmpty) {
            val isClosed = tam.modality match {
              case MODAL_MUST => true
              case _ => false
            }
            Some(SprWordRule(
              tokenizeQuotation(quotation),
              labels, isClosed))
          } else {
            None
          }
        }
      }
      case _ => None
    }
  }

  private def tokenizeQuotation(quotation : SilQuotationReference)
      : Seq[String] =
  {
    val tokenizedSentences = SprParser.tokenize(quotation.quotation)
    if (tokenizedSentences.size != 1) {
      Seq.empty
    } else {
      tokenizedSentences.head.tokens.map(_.text)
    }
  }

  private def recognizeWordLabels(
    interpretation : SilReference
  ) : Seq[String] =
  {
    interpretation match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        recognizeWordLabel(Seq(noun)).toSeq
      }
      case SilStateSpecifiedReference(
        SilNounReference(
          noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
        ),
        SilPropertyState(qualifier)
      ) => {
        recognizeWordLabel(Seq(qualifier, noun)).toSeq
      }
      case SilConjunctiveReference(
        DETERMINER_ANY,
        references,
        _
      ) => {
        val subs = references.map(ref =>
          recognizeWordLabels(ref)
        )
        if (subs.exists(_.isEmpty)) {
          Seq.empty
        } else {
          subs.flatten
        }
      }
      case _ => Seq.empty
    }
  }

  private def recognizeWordLabel(
    words : Seq[SilWord]) : Option[String] =
  {
    words.flatMap(_.decomposed) match {
      case Seq(SilWordLemma("noun")) => Some(LABEL_NN)
      case Seq(SilWordLemma("common"), SilWordLemma("noun")) => Some(LABEL_NN)
      case Seq(SilWordLemma("proper"), SilWordLemma("noun")) => Some(LABEL_NNP)
      case Seq(SilWordLemma("verb")) => Some(LABEL_VB)
      case Seq(SilWordLemma("adjective")) => Some(LABEL_JJ)
      case Seq(SilWordLemma("adverb")) => Some(LABEL_RB)
      case Seq(SilWordLemma("pronoun")) => Some(LABEL_PRP)
      case Seq(SilWordLemma("possessive"), SilWordLemma("pronoun")) =>
        Some(LABEL_PRP_POS)
      case Seq(SilWordLemma("conjunction")) => Some(LABEL_CC)
      case Seq(SilWordLemma("preposition")) => Some(LABEL_IN)
      case _ => None
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

  private def processRoleTaxonomy(
    sentence : SilSentence,
    formNoun : SilWord,
    roleNoun : SilWord,
    complementNoun : SilWord,
    verb : SilWord)
      : Seq[SpcBelief] =
  {
    SilRelationshipPredef(verb) match {
      case REL_PREDEF_IDENTITY if (sentence.tam.modality == MODAL_MUST) => {
        Seq(RoleTaxonomyBelief(
          sentence, formNoun, roleNoun, complementNoun, false))
      }
      case _ => {
        Seq.empty
      }
    }
  }

  private def processIdealRelationship(
    sentence : SilSentence,
    subjectNoun : SilWord,
    subjectCount : SilCount,
    complementRef : SilReference,
    verb : SilWord,
    subjectConjunction : SubjectConjunction)
      : Seq[SpcBelief] =
  {
    SilRelationshipPredef(verb) match {
      case REL_PREDEF_BECOME | REL_PREDEF_IDENTITY => {
        Seq.empty
      }
      case REL_PREDEF_ASSOC => {
        subjectConjunction.checkAndPluralOrSingular(subjectCount)
        val (complementNouns, count) = complementRef match {
          // "a dog may have an owner and a groomer"
          case SilConjunctiveReference(_, refs, _) => {
            val pairs = refs.map(ref => {
              val (complementNoun, qualifiers, count, determiner, failed) =
                extractQualifiedNoun(sentence, ref, Seq.empty,
                  false, false)
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
              extractQualifiedNoun(
                sentence, ref, Seq.empty, false, false)
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
            SpcCardinalityConstraint(1, upper)
          case MODAL_MAY | MODAL_POSSIBLE |
              MODAL_CAPABLE | MODAL_PERMITTED =>
            SpcCardinalityConstraint(0, upper)
          case MODAL_SHOULD | MODAL_ELLIPTICAL =>
            return Seq(UnimplementedBelief(sentence))
        }
        Seq(
          FormAssocBelief(
            sentence,
            subjectNoun, complementNouns, newConstraint)
        )
      }
    }
  }

  private def processEntityRelationship(
    sentence : SilSentence,
    subjectRef : SilReference,
    complementRef : SilReference,
    verb : SilWord,
    subjectConjunction : SubjectConjunction)
      : Seq[SpcBelief] =
  {
    if (sentence.tam.modality != MODAL_NEUTRAL) {
      return Seq(UnimplementedBelief(sentence))
    }
    SilRelationshipPredef(verb) matchPartial {
      case REL_PREDEF_ASSOC => {
        subjectConjunction.checkAnd
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
    }
    complementRef matchPartial {
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
                verb, subjectConjunction)
            }
          })
      }
    }

    complementRef matchPartial {
      case SilGenitiveReference(
        possessorRef, SilNounReference(roleNoun, DETERMINER_UNSPECIFIED, _)
      ) => {
        // "Fido is Franny's pet"
        subjectConjunction.checkAnd
        return Seq(EntityAssocBelief(
          sentence,
          possessorRef,
          subjectRef,
          false,
          roleNoun,
          sentence.tam.isPositive))
      }
    }

    val (subjectNoun, subjectDeterminer, subjectCount) = subjectRef match {
      case SilNounReference(noun, determiner, count) => {
        tupleN((noun, determiner, count))
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
    subjectConjunction.checkAnd
    tupleN((subjectConjunction.check, complementDeterminer)) match {
      // "Spot is a canine"
      case (DETERMINER_UNSPECIFIED, DETERMINER_NONSPECIFIC) => ;
      // "Spot and Tiger are canines"
      case (DETERMINER_ALL, DETERMINER_UNSPECIFIED) => ;
      case _ => {
        // FIXME some other cases might make sense, e.g.
        // "Oz is the werewolf"
        return Seq(UnimplementedBelief(sentence))
      }
    }
    subjectDeterminer match {
      case DETERMINER_UNSPECIFIED if (
        isProperSubject(subjectNoun, subjectCount)
      ) => {
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

  private def isProperSubject(word : SilWord, count : SilCount) : Boolean =
  {
    // FIXME the singular part is a kludge for the fact that we
    // currently lose the proper noun status of "e e cummings"
    // when it becomes a SilWord
    word.isProper || (count == COUNT_SINGULAR)
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
    allowGenitives : Boolean = false,
    allowAdpositions : Boolean = false)
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
      ) if (allowAdpositions || !state.isInstanceOf[SilAdpositionalState]) => {
        extractQualifiedNoun(
          sentence, subRef,
          preQualifiers ++ SilUtils.extractQualifiers(state))
      }
      case SilGenitiveReference(
        SilStateSpecifiedReference(
          sub,
          state),
        possessee
      ) => {
        extractQualifiedNoun(
          sentence,
          SilGenitiveReference(sub, possessee),
          preQualifiers ++ SilUtils.extractQualifiers(state),
          allowGenitives)
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
        tupleN((possession, preQualifiers :+ possessor,
          count, possessorDeterminer, failed || !allowGenitives))
      }
      case _ => failedResult
    }
  }
}
