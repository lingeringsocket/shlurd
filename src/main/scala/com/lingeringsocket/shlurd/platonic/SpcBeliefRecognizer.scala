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

  def recognizeWordLabel(
    words : Seq[SilWord]) : Option[String] =
  {
    words.flatMap(_.decomposed) match {
      case Seq(SilWordLemma("noun")) => Some(LABEL_NN)
      case Seq(SilWordLemma("plural"), SilWordLemma("noun")) => Some(LABEL_NNS)
      case Seq(SilWordLemma("common"), SilWordLemma("noun")) => Some(LABEL_NN)
      case Seq(SilWordLemma("plural"), SilWordLemma("common"),
        SilWordLemma("noun")) => Some(LABEL_NNS)
      case Seq(SilWordLemma("proper"), SilWordLemma("noun")) => Some(LABEL_NNP)
      case Seq(SilWordLemma("plural"), SilWordLemma("proper"),
        SilWordLemma("noun")) => Some(LABEL_NNP)
      case Seq(SilWordLemma("verb")) => Some(LABEL_VB)
      case Seq(SilWordLemma("adjective")) => Some(LABEL_JJ)
      case Seq(SilWordLemma("adverb")) => Some(LABEL_RB)
      case Seq(SilWordLemma("pronoun")) =>
        Some(LABEL_PRP)
      case Seq(SilWordLemma("nominative"), SilWordLemma("pronoun")) =>
        Some(LABEL_PRP)
      case Seq(SilWordLemma("objective"), SilWordLemma("pronoun")) =>
        Some(LABEL_PRP_OBJ)
      case Seq(SilWordLemma("reflexive"), SilWordLemma("pronoun")) =>
        Some(LABEL_PRP_REFLEXIVE)
      case Seq(SilWordLemma("possessive"), SilWordLemma("pronoun")) =>
        Some(LABEL_PRP_POS)
      case Seq(SilWordLemma("conjunction")) => Some(LABEL_CC)
      case Seq(SilWordLemma("preposition")) => Some(LABEL_IN)
      case _ => None
    }
  }

  def recognizeWordRule(
    sentence : SilSentence
  ) : Seq[SprWordRule] =
  {
    sentence match {
      case SilPredicateSentence(
        SilRelationshipPredicate(
          ref,
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
          Seq.empty
        } else {
          val quotations = ref match {
            case quotation : SilQuotationReference => {
              Seq(quotation)
            }
            case SilConjunctiveReference(DETERMINER_ALL, refs, _) => {
              if (refs.forall(_.isInstanceOf[SilQuotationReference])) {
                refs.map(_.asInstanceOf[SilQuotationReference])
              } else {
                Seq.empty
              }
            }
            case _ => {
              Seq.empty
            }
          }
          val expectedCount = {
            if (quotations.size > 1) {
              COUNT_PLURAL
            } else {
              COUNT_SINGULAR
            }
          }
          val labels = recognizeWordLabels(interpretation, expectedCount)
          if (labels.nonEmpty) {
            quotations.map(quotation => {
              val isClosed = tam.unemphaticModality match {
                case MODAL_MUST | MODAL_NEUTRAL => true
                case _ => false
              }
              SprWordRule(
                tokenizeQuotation(quotation),
                labels, isClosed)
            })
          } else {
            Seq.empty
          }
        }
      }
      case _ => Seq.empty
    }
  }

  private def recognizeWordLabels(
    interpretation : SilReference,
    expectedCount : SilCount
  ) : Seq[String] =
  {
    interpretation match {
      case SilDeterminedReference(
        SilMandatorySingular(noun),
        DETERMINER_NONSPECIFIC
      ) if (expectedCount == COUNT_SINGULAR) => {
        recognizeWordLabel(Seq(noun)).toSeq
      }
      case SilMandatoryPlural(
        noun
      ) if (expectedCount == COUNT_PLURAL) => {
        recognizeWordLabel(Seq(noun)).toSeq
      }
      case SilDeterminedReference(
        SilStateSpecifiedReference(
          SilMandatorySingular(noun),
          SilPropertyState(qualifier)
        ),
        DETERMINER_NONSPECIFIC
      ) if (expectedCount == COUNT_SINGULAR) => {
        recognizeWordLabel(Seq(qualifier, noun)).toSeq
      }
      case SilConjunctiveReference(
        DETERMINER_ANY,
        references,
        _
      ) => {
        val subs = references.map(ref =>
          recognizeWordLabels(ref, expectedCount)
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

  private def tokenizeQuotation(quotation : SilQuotationReference)
      : Seq[String] =
  {
    val tokenizedSentences = SprParser.tokenize(quotation.quotation)
    if (tokenizedSentences.size != 1) {
      Seq.empty
    } else {
      tokenizedSentences.head.tokens.map(_.text.toLowerCase)
    }
  }
}

class SpcBeliefRecognizer(
  responder : SpcResponder,
  resultCollector : SpcResultCollector)
    extends SmcDebuggable(new SmcDebugger(SpcBeliefRecognizer.logger))
{
  import SpcBeliefRecognizer._

  private val mind = responder.getMind

  val cosmos = mind.getCosmos

  private val annotator = resultCollector.annotator

  protected val creed = new SpcCreed(annotator, cosmos)

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
          _, verb, Some(SilQuotationReference(quotation, _)), _),
        tam, formality
      ) if (tam.isImperative) => {
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
            annotator.genitiveRef(
              ref,
              annotator.nounRef(
                SilWord(SmcLemmas.LEMMA_CONTAINER),
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
            case SilGenitiveReference(possessor, possessee) => {
              possessee match {
                case SilNounReference(attribute) => {
                  val possessorForm =
                    responder.deriveType(
                      annotator, possessor,
                      SmcResultCollector.newAnnotationRefMap(annotator))
                  val propertyOpt = cosmos.findProperty(
                    possessorForm, attribute.toNounLemma)
                  if (propertyOpt.nonEmpty) {
                    // interpret as property, e.g. "the boss's mood"
                    tupleN((possessor, true))
                  } else {
                    // interpret as association, e.g. "the boss's minions"
                    tupleN((ref, false))
                  }
                }
                case _ => {
                  tupleN((possessor, true))
                }
              }
            }
            case _ => tupleN((ref, false))
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
          annotator.determinedNounRef(noun, DETERMINER_NONSPECIFIC),
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
        case SilOptionallyDeterminedReference(
          SilStateSpecifiedReference(
            SilCountedNounReference(
              SilWordLemma(LEMMA_KIND),
              kindCount),
            SilAdpositionalState(
              SilAdposition.OF,
              SilOptionallyDeterminedReference(
                SilNounReference(hypernymIdealName),
                DETERMINER_NONSPECIFIC | DETERMINER_UNSPECIFIED
              ))),
          kindDeterminer
        ) if (
          (kindDeterminer == DETERMINER_NONSPECIFIC) ||
            ((kindDeterminer == DETERMINER_UNSPECIFIED) &&
              (kindCount == COUNT_PLURAL))
        ) => {
          tupleN((Some(hypernymIdealName), None))
        }
        case SilDeterminedReference(
          SilStateSpecifiedReference(
            SilMandatorySingular(
              SilWordLemma(LEMMA_SAME)
            ),
            SilAdpositionalState(
              SilAdposition.AS,
              SilOptionallyDeterminedReference(
                SilCountedNounReference(idealName, count),
                determiner
              ))),
          DETERMINER_UNIQUE
        ) if (compatibleDeterminerAndCount(determiner, count)) => {
          tupleN((None, Some(idealName)))
        }
        case _ => tupleN((None, None))
      }
      if (kindOpt.nonEmpty || aliasOpt.nonEmpty) {
        subjectRef matchPartial {
          case SilOptionallyDeterminedReference(
            SilCountedNounReference(noun, count),
            determiner
          ) if (
            compatibleDeterminerAndCount(determiner, count)
          ) => {
            subjectConjunction.checkAndPluralOrSingular(count)
            kindOpt.foreach(hypernymIdealName => {
              // "a dog is a kind of canine"
              return Seq(FormTaxonomyBelief(
                sentence, noun, hypernymIdealName))
            })
            aliasOpt.foreach(idealName => {
              // "a fridge is the same as a refrigerator"
              return Seq(IdealAliasBelief(
                sentence, noun, idealName))
            })
          }
          case SilGenitiveReference(
            SilDeterminedReference(
              SilMandatorySingular(possessorNoun),
              DETERMINER_NONSPECIFIC
            ),
            SilMandatorySingular(
              roleNoun
            )
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
      case SilOptionallyDeterminedReference(
        SilCountedNounReference(subjectNoun, subjectCount),
        subjectDeterminer
      ) if (
        compatibleDeterminerAndCount(
          subjectDeterminer, subjectCount, exactPlural = true)
      ) => {
        return processIdealRelationship(
          sentence, subjectNoun, subjectCount,
          complementRef, verb, subjectConjunction)
      }
      case SilGenitiveReference(possessor, possessee) => {
        complementRef matchPartial {
          case SilOptionallyDeterminedReference(
            SilMandatorySingular(complementNoun),
            determiner
          ) => {
            if (determiner == DETERMINER_NONSPECIFIC) {
              val formNoun = possessor match {
                case SilDeterminedReference(
                  SilMandatorySingular(noun),
                  DETERMINER_NONSPECIFIC
                ) => noun
                case _ => {
                  // "Mary's cat is a pest"
                  return Seq(EntityExistenceBelief(
                    sentence, subjectRef, complementNoun,
                    Seq.empty, ""))
                }
              }
              val possesseeNoun = possessee match {
                case SilMandatorySingular(
                  noun
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
          case SilQuotationReference(quotation, _) => {
            // "Arnie's catchphrase is <<I'll be back>>"
            return processQuotation(
              sentence, possessor, possessee, quotation, verb)
          }
        }
      }
      case _ => {
        complementRef match {
          case SilDeterminedReference(
            SilMandatorySingular(complementNoun),
            DETERMINER_NONSPECIFIC
          ) if (subjectRef.isInstanceOf[SilStateSpecifiedReference]) => {
            // "The cat in the hat is a pest"
            // FIXME more constraints on the subjectRef
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
    count : SilCount,
    exactPlural : Boolean = false) : Boolean =
  {
    tupleN((determiner, count)) match {
      case (DETERMINER_UNSPECIFIED, COUNT_PLURAL) => true
      case (DETERMINER_UNSPECIFIED, _) if (!exactPlural) => true
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
          val entityRef = annotator.nounRef(SilWord(entity.name))
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
      case SilMandatorySingular(
        propertyName
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
    exceptionReporter : ExceptionReporter,
    isBefore : Boolean,
    isUnidirectional : Boolean,
    consequent : SilPredicate,
    biconditional : Boolean,
    antecedentEvent : Boolean,
    checkPatterns : Boolean = true)
  {
    val querier = new SilPhraseQuerier
    def reportException(code : ShlurdExceptionCode)
    {
      exceptionReporter.reportException(code)
    }
    if (isBefore && isUnidirectional) {
      reportException(AssertionModifiersIncompatible)
    }
    def visitConsequent() = querier.queryMatcher {
      case SilOptionallyDeterminedReference(
        _ : SilNounReference, determiner
      ) => {
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
  }

  private def recognizeAssertionBelief(
    assertionSentence : SilSentence,
    additionalSentences : Seq[SilPredicateSentence] = Seq.empty)
      : Seq[SpcBelief] =
  {
    val exceptionReporter = new ExceptionReporter
    def reportException(code : ShlurdExceptionCode)
    {
      exceptionReporter.reportException(code)
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
        if (!conditional.biconditional) {
          conditional.antecedent.getSubject matchPartial {
            case _ : SilGenitiveReference => {
              reportException(AssertionInvalidVariable)
            }
          }
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
          exceptionReporter,
          isBefore,
          isConsequentSubsequently || isConsequentImplication,
          consequent,
          conditional.biconditional,
          antecedentEvent,
          consequentNonModal
        )
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
            exceptionReporter,
            isBefore,
            isSubsequently || isConsequentSubsequently ||
              isConsequentImplication || isImplication,
            additionalSentence.predicate,
            conditional.biconditional,
            antecedentEvent
          )
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
        val querier = new SilPhraseQuerier
        def validateCapability = querier.queryMatcher {
          case SilStateSpecifiedReference(_, _ : SilAdpositionalState) => {
            ignored = true
          }
        }
        querier.query(validateCapability, predicate)
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
    val exceptionCode = exceptionReporter.getCode
    if (ignored) {
      Seq.empty
    } else if (exceptionCode.nonEmpty) {
      Seq(InvalidBelief(assertionSentence, exceptionCode.get))
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
    if (verb.toLemma == LEMMA_BELIEVE) {
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
            // FIXME special casing for COUNT_MASS/COUNT_ZERO_PLURAL
            tupleN((pairs.map(_._1), pairs.map(_._2).maxBy(
              _ match {
                case COUNT_PLURAL => 2
                case _ => 1
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
          case COUNT_PLURAL => Int.MaxValue
          case _ => 1
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
          case SilDeterminedReference(
            SilNounReference(roleNoun),
            DETERMINER_NONE
          ) => {
            // "Larry has no pets"
            return Seq(EntityNoAssocBelief(
              sentence,
              subjectRef,
              roleNoun
            ))
          }
          case SilDeterminedReference(
            SilStateSpecifiedReference(
              SilNounReference(
                roleNoun
              ),
              state
            ),
            DETERMINER_NONSPECIFIC
          ) => {
            // "Larry has a dirty dog"
            return indefiniteEntityAssocBelief(
              sentence, subjectRef,
              state,
              roleNoun)
          }
          case SilDeterminedReference(
            SilNounReference(roleNoun),
            DETERMINER_NONSPECIFIC
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
              val flattenedComplement = annotator.genitiveRef(
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
        possessorRef, SilNounReference(roleNoun)
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
      case SilOptionallyDeterminedReference(
        SilCountedNounReference(noun, count), determiner
      ) => {
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
      annotator.determinedRef(
        annotator.stateSpecifiedRef(
          annotator.nounRef(roleNoun),
          state),
        DETERMINER_UNIQUE),
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
        pr : SilPronounReference
      ) if (pr.isDemonstrative) => true
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
      case SilOptionallyDeterminedReference(
        SilMandatorySingular(noun),
        determiner @ (DETERMINER_NONSPECIFIC | DETERMINER_UNIQUE |
          DETERMINER_UNSPECIFIED)
      ) => {
        tupleN((noun, preQualifiers, COUNT_SINGULAR, determiner, false))
      }
      case SilDeterminedReference(sub, determiner) => {
        val (w, s, c, d, b) = extractQualifiedNoun(
          sentence, sub, preQualifiers, allowGenitives, allowAdpositions)
        assert(d == DETERMINER_UNSPECIFIED)
        tupleN((w, s, c, determiner, b))
      }
      case SilMandatoryPlural(
        noun
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
        SilOptionallyDeterminedReference(
          SilStateSpecifiedReference(
            sub,
            state),
          determiner),
        possessee
      ) => {
        extractQualifiedNoun(
          sentence,
          annotator.determinedRef(
            annotator.genitiveRef(sub, possessee), determiner),
          preQualifiers ++ SilUtils.extractQualifiers(state),
          allowGenitives)
      }
      case SilGenitiveReference(
        SilOptionallyDeterminedReference(
          SilMandatorySingular(
            possessor
          ),
          possessorDeterminer),
        SilCountedNounReference(
          possession, count)
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

  class ExceptionReporter()
  {
    private var exceptionCode : Option[ShlurdExceptionCode] = None

    def reportException(code : ShlurdExceptionCode)
    {
      trace(s"INVALID ASSERTION:  $code")
      if (exceptionCode.isEmpty) {
        exceptionCode = Some(code)
      }
    }

    def getCode() = exceptionCode
  }
}
