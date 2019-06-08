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
import scala.util._

import spire.math._

import SprEnglishLemmas._

sealed trait SpcBeliefAcceptance
case object ACCEPT_NO_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_NEW_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_MODIFIED_BELIEFS extends SpcBeliefAcceptance

sealed trait SpcAssertionApplicability
case object APPLY_CONSTRAINTS_ONLY extends SpcAssertionApplicability
case object APPLY_TRIGGERS_ONLY extends SpcAssertionApplicability
case object APPLY_ALL_ASSERTIONS extends SpcAssertionApplicability

sealed trait SpcAssertionResultStrength
case object ASSERTION_PASS extends SpcAssertionResultStrength
case object ASSERTION_INAPPLICABLE extends SpcAssertionResultStrength
case object ASSERTION_STRONG_FAILURE extends SpcAssertionResultStrength
case object ASSERTION_WEAK_FAILURE extends SpcAssertionResultStrength

case class SpcAssertionResult(
  predicate : Option[SilPredicate],
  message : String,
  strength : SpcAssertionResultStrength)
{
}

class SpcContextualScorer(responder : SpcResponder)
    extends SmcContextualScorer(responder)
{
  override protected def computeBoost(
    sentence : SilSentence,
    resultCollector : ResultCollectorType) : SilPhraseScore =
  {
    val cosmos = responder.getMind.getCosmos
    val recognizer = new SpcBeliefRecognizer(
      cosmos,
      resultCollector)
    val beliefs = recognizer.recognizeBeliefs(sentence)
    val beliefBoost = beliefs match {
      case Seq() => SilPhraseScore.neutral
      case _ => SilPhraseScore.proBig
    }
    var propBoost = SilPhraseScore.neutral
    val querier = new SilPhraseRewriter
    def detectBoosts = querier.queryMatcher {
      case SilStatePredicate(
        subject,
        _,
        SilPropertyState(SilWordLemma(lemma)),
        _
      ) => {
        val detected = resultCollector.referenceMap.get(subject) match {
          case Some(entities) => {
            entities.exists(entity => {
              cosmos.resolvePropertyState(entity, lemma).isSuccess
            })
          }
          case _ => {
            val form = responder.deriveType(subject)
            cosmos.resolveHypernymPropertyState(form, lemma).nonEmpty
          }
        }
        if (detected) {
          propBoost = SilPhraseScore.proBig
        }
      }
    }
    querier.query(detectBoosts, sentence)
    super.computeBoost(sentence, resultCollector) + beliefBoost + propBoost
  }
}

class SpcResponder(
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
  params : SmcResponseParams = SmcResponseParams(),
  executor : SmcExecutor[SpcEntity] = new SmcExecutor[SpcEntity],
  communicationContext : SmcCommunicationContext[SpcEntity] =
    SmcCommunicationContext()
) extends SmcResponder[
  SpcEntity, SpcProperty, SpcCosmos, SpcMind
](
  mind, params, executor, communicationContext
)
{
  private val already = new mutable.HashSet[SilPredicate]

  private val typeMemo = new mutable.LinkedHashMap[SilReference, SpcForm]

  private val assertionMapper = new SpcAssertionMapper(
    mind, communicationContext, inputRewriter)

  override protected def spawn(subMind : SpcMind) =
  {
    new SpcResponder(subMind, beliefAcceptance, params,
      executor, communicationContext)
  }

  override def newParser(input : String) =
  {
    val context = SprContext(scorer = new SpcContextualScorer(this))
    SprParser(input, context)
  }

  override protected def newPredicateEvaluator() =
    new SmcPredicateEvaluator[SpcEntity, SpcProperty, SpcCosmos, SpcMind](
      mind, sentencePrinter, params.existenceAssumption,
      communicationContext, debugger)
  {
    override protected def evaluateActionPredicate(
      predicate : SilActionPredicate,
      resultCollector : ResultCollectorType) : Try[Trilean] =
    {
      checkCycle(predicate, already, resultCollector.referenceMap) match {
        case f @ Failure(err) => {
          return f.map(Trilean(_))
        }
        case Success(true) => {
          return Success(Trilean.Unknown)
        }
        case _ =>
      }
      getBiconditionalImplications.foreach(conditionalSentence => {
        assertionMapper.matchImplication(
          "IMPLIES",
          mind.getCosmos,
          conditionalSentence,
          predicate,
          resultCollector.referenceMap,
          Some(resultCollector.referenceMap)) match
        {
          case Some(newPredicate) => {
            return super.evaluatePredicate(newPredicate, resultCollector)
          }
          case _ =>
        }
      })
      super.evaluateActionPredicate(predicate, resultCollector)
    }

    override protected def normalizePredicate(
      predicate : SilPredicate,
      referenceMap : Map[SilReference, Set[SpcEntity]]) : SilPredicate =
    {
      val stateNormalizedPredicate = predicate match {
        case SilStatePredicate(subject, verb, state, modifiers) => {
          val normalizedState = mind.getCosmos.normalizeHyperFormState(
            deriveType(subject), state)
          val normalized = SilStatePredicate(
            subject, verb, normalizedState, modifiers)
          if (normalizedState.isInstanceOf[SilPropertyState]) {
            return normalized
          }
          normalized
        }
        case _ => predicate
      }
      // FIXME this could cause the predicate to become
      // inconsisent with the answer inflection.  Also, when there
      // are multiple matches, we should be conjoining them.
      val triggers = mind.getCosmos.getTriggers.filter(
        _.conditionalSentence.biconditional)
      val replacements = getBiconditionalImplications.flatMap(
        conditionalSentence => {
          assertionMapper.matchImplication(
            "IMPLIES",
            mind.getCosmos,
            conditionalSentence,
            stateNormalizedPredicate,
            referenceMap,
            None)
        }
      ).filter(acceptReplacement)
      replacements.headOption.getOrElse(stateNormalizedPredicate)
    }

    private def acceptReplacement(sil : SilPhrase) : Boolean =
    {
      var accepted = true
      val querier = new SilPhraseRewriter
      def checkPhrase = querier.queryMatcher {
        case SilGenitiveReference(
          SilNounReference(_, DETERMINER_ANY, _),
          _
        ) => {
          accepted = false
        }
      }
      querier.query(checkPhrase, sil)
      accepted
    }

    override protected def evaluatePropertyStatePredicate(
      entity : SpcEntity,
      entityRef : SilReference,
      state : SilWord,
      resultCollector : ResultCollectorType)
        : Try[Trilean] =
    {
      entity match {
        case SpcTransientEntity(_, value, _) => {
          // maybe we need some type-checking here too?
          resultCollector.states += state
          Success(Trilean(state.toLemma == value))
        }
        case _ => {
          super.evaluatePropertyStatePredicate(
            entity, entityRef, state, resultCollector)
        }
      }
    }
  }

  private def getBiconditionalImplications() : Seq[SilConditionalSentence] =
  {
    val triggers = mind.getCosmos.getTriggers.filter(
      _.conditionalSentence.biconditional)
    triggers.flatMap(getTriggerImplications)
  }

  private def getTriggerImplications(
    trigger : SpcTrigger) : Seq[SilConditionalSentence] =
  {
    val cs = trigger.conditionalSentence
    if (cs.biconditional) {
      assert(trigger.additionalConsequents.isEmpty)
      assert(trigger.alternative.isEmpty)
      val variableLemmas = new mutable.LinkedHashSet[String]
      val querier = new SilPhraseRewriter
      def findVariables = querier.queryMatcher {
        case SilNounReference(
          noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
        ) => {
          variableLemmas += noun.toLemma
        }
      }
      querier.query(findVariables, cs.antecedent)
      Seq(
        cs,
        cs.copy(
          antecedent = flipVariables(cs.consequent, variableLemmas),
          consequent = flipVariables(cs.antecedent, variableLemmas),
          tamAntecedent = cs.tamConsequent,
          tamConsequent = cs.tamAntecedent
        )
      )
    } else {
      Seq(trigger.conditionalSentence)
    }
  }

  private def flipVariables(
    predicate : SilPredicate,
    variableLemmas : Set[String]) : SilPredicate =
  {
    val rewriter = new SilPhraseRewriter
    def replaceReferences = rewriter.replacementMatcher(
      "flipVariables", {
        case SilNounReference(
          noun, DETERMINER_NONSPECIFIC, count
        ) if (variableLemmas.contains(noun.toLemma)) => {
          SilNounReference(noun, DETERMINER_UNIQUE, count)
        }
        case SilNounReference(
          noun, DETERMINER_UNIQUE, count
        ) if (variableLemmas.contains(noun.toLemma)) => {
          SilNounReference(noun, DETERMINER_NONSPECIFIC, count)
        }
      }
    )
    rewriter.rewrite(replaceReferences, predicate)
  }

  override protected def imagine(
    alternateCosmos : SpcCosmos) =
  {
    mind.spawn(alternateCosmos.fork())
  }

  override protected def responderMatchers(
    resultCollector : ResultCollectorType
  ) =
  {
    (attemptResponse(resultCollector) _) #::
      super.responderMatchers(resultCollector)
  }

  override protected def processImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    try {
      super.processImpl(sentence, resultCollector)
    } finally {
      already.clear
      typeMemo.clear
    }
  }

  private def attemptResponse(
    resultCollector : ResultCollectorType)(sentence : SilSentence)
      : Option[(SilSentence, String)] =
  {
    if ((beliefAcceptance != ACCEPT_NO_BELIEFS) &&
      sentence.tam.isIndicative)
    {
      val (interval, predicateOpt, baselineCosmos, temporal) = sentence match {
        case SilPredicateSentence(predicate, _, _) => {
          val temporalRefs = predicate.getModifiers.map(
            _ match {
              case SilAdpositionalVerbModifier(
                SilAdposition.ADVERBIAL_TMP,
                ref
              ) => {
                Some(ref)
              }
              case _ => {
                None
              }
            }
          )
          val iTemporal = temporalRefs.indexWhere(!_.isEmpty)
          val (interval, predicateOpt, baselineCosmos, temporal) = {
            if (iTemporal < 0) {
              tupleN((SmcTimeInterval.NEXT_INSTANT,
                predicate, mind.getCosmos, false))
            } else {
              val interval = Interval.point[SmcTimePoint](
                SmcRelativeTimePoint(
                  temporalRefs(iTemporal).get))
              val temporalCosmos = mind.getTemporalCosmos(interval)
              tupleN((interval,
                predicate.withNewModifiers(
                  predicate.getModifiers.patch(iTemporal, Seq.empty, 1)),
                temporalCosmos,
                true))
            }
          }
          tupleN((interval, Some(predicateOpt), baselineCosmos, temporal))
        }
        case _ => {
          tupleN((SmcTimeInterval.NEXT_INSTANT, None, mind.getCosmos, false))
        }
      }

      val forkedCosmos = baselineCosmos.fork()
      val inputSentence =
        predicateOpt.map(
          SilPredicateSentence(_, sentence.tam)).getOrElse(sentence)
      processBeliefOrAction(
        forkedCosmos, inputSentence, resultCollector, 0
      ) match {
        case Some(result) => {
          if (result != sentencePrinter.sb.respondCompliance) {
            return Some(wrapResponseText(result))
          }
          if (mind.hasNarrative) {
            predicateOpt.foreach(predicate => {
              val updatedCosmos = freezeCosmos(forkedCosmos)
              try {
                updateNarrative(
                  interval,
                  updatedCosmos,
                  predicate,
                  resultCollector.referenceMap)
              } catch {
                case CausalityViolationExcn(cause) => {
                  return Some(wrapResponseText(cause))
                }
              }
            })
          }
          if (!temporal) {
            forkedCosmos.applyModifications
          }
          return Some(wrapResponseText(result))
        }
        case _ =>
      }
    }
    already.clear
    None
  }

  private def applyAssertion(
    forkedCosmos : SpcCosmos,
    assertion : SpcAssertion,
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]],
    applicability : SpcAssertionApplicability,
    triggerDepth : Int)
      : SpcAssertionResult =
  {
    val resultCollector = new SmcResultCollector[SpcEntity](
      SmcResultCollector.modifiableReferenceMap(referenceMap))
    spawn(imagine(forkedCosmos)).resolveReferences(
      predicate, resultCollector, false, true)

    def inapplicable = SpcAssertionResult(None, "", ASSERTION_INAPPLICABLE)

    assertion.asTrigger match {
      case Some(trigger) => {
        if (applicability == APPLY_CONSTRAINTS_ONLY) {
          inapplicable
        } else {
          applyTrigger(
            forkedCosmos, trigger, predicate, resultCollector, triggerDepth
          ) match {
            case Some(message) => {
              val strength = {
                if (message == sentencePrinter.sb.respondCompliance) {
                  ASSERTION_PASS
                } else {
                  ASSERTION_STRONG_FAILURE
                }
              }
              SpcAssertionResult(None, message, strength)
            }
            case _ => inapplicable
          }
        }
      }
      case _ if (applicability != APPLY_TRIGGERS_ONLY) => {
        val assertionPredicate = assertion.sentence match {
          case ps : SilPredicateSentence => {
            ps.tam.modality match {
              case MODAL_MAY | MODAL_POSSIBLE |
                  MODAL_CAPABLE | MODAL_PERMITTED => ps.predicate
              case _ => return inapplicable
            }
          }
          case _ => return inapplicable
        }
        def isGenerally(m : SilVerbModifier) : Boolean = {
          m match {
            case SilBasicVerbModifier(
              SilWordLemma(LEMMA_GENERALLY),
              _
            ) => true
            case _ => false
          }
        }
        val (generally, requirement) = {
          if (assertionPredicate.getModifiers.exists(isGenerally)) {
            tupleN((true,
              assertionPredicate.withNewModifiers(
                assertionPredicate.getModifiers.filterNot(isGenerally))))
          } else {
            tupleN((false, assertionPredicate))
          }
        }
        if (isSubsumption(
          forkedCosmos, requirement, predicate, referenceMap)
        ) {
          if (assertion.sentence.tam.isPositive) {
            SpcAssertionResult(
              Some(requirement),
              sentencePrinter.sb.respondCompliance,
              ASSERTION_PASS)
          } else {
            if (generally) {
              val action = sentencePrinter.printPredicateCommand(
                requirement, SilTam.imperative)
              SpcAssertionResult(
                Some(requirement),
                sentencePrinter.sb.respondUnable(action),
                ASSERTION_WEAK_FAILURE)
            } else {
              SpcAssertionResult(
                None,
                SprUtils.capitalize(
                  sentencePrinter.print(assertion.sentence)),
                ASSERTION_STRONG_FAILURE)
            }
          }
        } else {
          inapplicable
        }
      }
      case _ => inapplicable
    }
  }

  private def applyTrigger(
    forkedCosmos : SpcCosmos,
    trigger : SpcTrigger,
    predicate : SilPredicate,
    resultCollector : ResultCollectorType,
    triggerDepth : Int)
      : Option[String] =
  {
    getTriggerImplications(trigger).toStream.flatMap(conditionalSentence => {
      applyTriggerImpl(
        forkedCosmos, trigger, conditionalSentence,
        predicate, resultCollector, triggerDepth)
    }).headOption
  }

  private def applyTriggerImpl(
    forkedCosmos : SpcCosmos,
    trigger : SpcTrigger,
    conditionalSentence : SilConditionalSentence,
    predicate : SilPredicate,
    resultCollector : ResultCollectorType,
    triggerDepth : Int)
      : Option[String] =
  {
    val (isTest, isPrecondition) =
      conditionalSentence.tamConsequent.modality match
      {
        case MODAL_MAY | MODAL_POSSIBLE => tupleN((true, false))
        case MODAL_MUST | MODAL_SHOULD => tupleN((false, true))
        case _ => tupleN((false, false))
      }
    val operator = {
      if (isPrecondition) {
        "REQUIRES"
      } else if (isTest) {
        "CHECKS"
      } else {
        "TRIGGERS"
      }
    }
    assertionMapper.matchImplicationPlusAlternative(
      operator,
      forkedCosmos, conditionalSentence.antecedent,
      conditionalSentence.consequent, predicate,
      trigger.additionalConsequents, trigger.alternative,
      resultCollector.referenceMap,
      Some(resultCollector.referenceMap),
      triggerDepth) match
    {
      case (Some(newPredicate), newAdditionalConsequents, newAlternative) => {
        val newConsequents = (
          SilPredicateSentence(newPredicate) +: newAdditionalConsequents
        ).map(
          removeBasicVerbModifier(_, Set(LEMMA_ALSO, LEMMA_SUBSEQUENTLY))
        )
        newConsequents.foreach(sentence => {
          checkCycle(
            sentence.predicate, already,
            resultCollector.referenceMap, isPrecondition || isTest
          ) match {
            case Failure(err) => {
              return Some(err.getMessage)
            }
            case Success(true) => {
              return None
            }
            case _ =>
          }
        })
        if (isPrecondition || isTest) {
          // FIXME
          assert(newAdditionalConsequents.isEmpty)

          spawn(imagine(forkedCosmos)).resolveReferences(
            newConsequents.head, resultCollector, false, true)

          val newTam = SilTam.indicative.withPolarity(
            conditionalSentence.tamConsequent.polarity)
          evaluateTamPredicate(
            newPredicate, newTam, resultCollector) match
          {
            case Success(Trilean.True) if (newTam.isPositive) => {
              None
            }
            case Success(Trilean.False) if (newTam.isNegative) => {
              None
            }
            case Failure(e) => {
              e match {
                case _ : NonExistentException => {
                  None
                }
                case _ => {
                  // FIXME better handling
                  throw e
                }
              }
            }
            case _ => {
              newAlternative.foreach(alternativeSentence => {
                val recoverySentence = removeBasicVerbModifier(
                  alternativeSentence,
                  Set(LEMMA_OTHERWISE, LEMMA_SUBSEQUENTLY))
                checkCycle(recoverySentence.predicate,
                  already, resultCollector.referenceMap
                ) match {
                  case Failure(err) => {
                    return Some(err.getMessage)
                  }
                  case Success(true) => {
                    return None
                  }
                  case _ =>
                }
                spawn(imagine(forkedCosmos)).resolveReferences(
                  recoverySentence, resultCollector, false, true)
                // FIXME use recoveryResult somehow
                val recoveryResult = processBeliefOrAction(
                  forkedCosmos, recoverySentence, resultCollector,
                  triggerDepth + 1, false)
              })
              // FIXME i18n
              if (isPrecondition) {
                Some("But " + sentencePrinter.printPredicateStatement(
                  newPredicate, SilTam.indicative.negative) + ".")
              } else {
                None
              }
            }
          }
        } else {
          val results = newConsequents.toStream.flatMap(newSentence => {
            spawn(imagine(forkedCosmos)).resolveReferences(
              newSentence, resultCollector, false, true)
            processBeliefOrAction(
              forkedCosmos, newSentence, resultCollector,
              triggerDepth + 1, false)
          })
          if (results.isEmpty) {
            Some(sentencePrinter.sb.respondCompliance)
          } else {
            val nonCompliant =
              results.filterNot(_ == sentencePrinter.sb.respondCompliance)
            if (nonCompliant.isEmpty) {
              Some(sentencePrinter.sb.respondCompliance)
            } else {
              nonCompliant.headOption
            }
          }
        }
      }
      case _ => None
    }
  }

  private def removeBasicVerbModifier(
    sentence : SilPredicateSentence, lemmas : Set[String]) =
  {
    sentence.copy(
      predicate = sentence.predicate.withNewModifiers(
        sentence.predicate.getModifiers.filterNot(
          _ match {
            case SilBasicVerbModifier(
              SilWordLemma(lemma), _) if (lemmas.contains(lemma)) => true
            case _ => false
          })))
  }

  override protected def rewriteQuery(
    predicate : SilPredicate, question : SilQuestion,
    originalAnswerInflection : SilInflection,
    resultCollector : ResultCollectorType)
      : (SilPredicate, SilInflection) =
  {
    val (rewritten, answerInflection) = super.rewriteQuery(
      predicate, question, originalAnswerInflection, resultCollector)
    if (question == QUESTION_WHICH) {
      rewritten match {
        case SilRelationshipPredicate(
          SilNounReference(SilWordLemma(lemma), DETERMINER_ANY, count),
          SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          complement,
          modifiers
        ) => {
          val form = deriveType(complement)
          if (mind.getCosmos.formHasProperty(form, lemma)) {
            val statePredicate = SilStatePredicate(
              complement,
              STATE_PREDEF_BE.toVerb,
              SilPropertyQueryState(lemma),
              modifiers
            )
            return tupleN((statePredicate, INFLECT_COMPLEMENT))
          }
        }
        case _ =>
      }
    }
    tupleN((rewritten, answerInflection))
  }

  override protected def newQueryRewriter(
    question : SilQuestion,
    answerInflection : SilInflection) =
  {
    new SpcQueryRewriter(question, answerInflection)
  }

  private def updateReferenceMap(
    sentence : SilSentence,
    cosmos : SpcCosmos,
    resultCollector : ResultCollectorType)
  {
    // we may have modified cosmos (e.g. with new entities) by this
    // point, so run another full reference resolution pass to pick
    // them up
    resultCollector.referenceMap.clear
    spawn(imagine(cosmos)).resolveReferences(
      sentence, resultCollector)
    rememberSentenceAnalysis(resultCollector)
  }

  override protected def freezeCosmos(mutableCosmos : SpcCosmos) =
  {
    // FIXME use smart deltas instead of blindly flattening
    mutableCosmos.newClone(true).asUnmodifiable
  }

  private def processBeliefOrAction(
    forkedCosmos : SpcCosmos,
    sentence : SilSentence,
    resultCollector : ResultCollectorType,
    triggerDepth : Int,
    flagErrors : Boolean = true)
      : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    val beliefAccepter =
      new SpcBeliefAccepter(
        spawn(mind.spawn(forkedCosmos)),
        (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS),
        resultCollector)
    attemptAsBelief(beliefAccepter, sentence, triggerDepth).foreach(
      result => {
        if (result != compliance) {
          return Some(result)
        } else {
          matched = true
        }
      }
    )
    var earlyReturn : Option[String] = None
    if (sentence.tam.unemphaticModality == MODAL_NEUTRAL) {
      sentence match {
        case SilPredicateSentence(predicate, _, _) => {
          if (flagErrors && predicate.isInstanceOf[SilActionPredicate]) {
            resultCollector.referenceMap.clear
            val resolutionResult =
              spawn(imagine(forkedCosmos)).resolveReferences(
                predicate, resultCollector,
                true, false)
            resolutionResult match {
              case Failure(ex) => {
                earlyReturn = Some(ex.getMessage)
              }
              case _ =>
            }
          }
          if (earlyReturn.isEmpty) {
            val applicability = {
              if (matched) {
                APPLY_TRIGGERS_ONLY
              } else {
                APPLY_ALL_ASSERTIONS
              }
            }
            val result = processTriggerablePredicate(
              forkedCosmos, predicate,
              resultCollector.referenceMap, applicability,
              triggerDepth, flagErrors && !matched)
            if (!result.isEmpty) {
              earlyReturn = result
            }
          }
        }
        case _ => {
        }
      }
    }
    // defer until this point so that any newly created entities etc will
    // already have taken effect
    if (triggerDepth == 0) {
      updateReferenceMap(sentence, forkedCosmos, resultCollector)
    }
    earlyReturn.orElse {
      if (matched) {
        Some(compliance)
      } else {
        None
      }
    }
  }

  protected def publishBelief(belief : SpcBelief)
  {
  }

  private def attemptAsBelief(
    beliefAccepter : SpcBeliefAccepter,
    sentence : SilSentence,
    triggerDepth : Int) : Option[String] =
  {
    beliefAccepter.recognizeBeliefs(sentence) match {
      case beliefs : Seq[SpcBelief] if (!beliefs.isEmpty) => {
        beliefs.foreach(belief => {
          debug(s"TRYING TO BELIEVE : $belief")
          publishBelief(belief)
          try {
            beliefAccepter.applyBelief(belief)
          } catch {
            case ex : RejectedBeliefExcn => {
              debug("NEW BELIEF REJECTED", ex)
              if (params.throwRejectedBeliefs) {
                throw ex
              }
              return Some(respondRejection(ex))
            }
          }
          debug("NEW BELIEF ACCEPTED")
        })
        Some(sentencePrinter.sb.respondCompliance)
      }
      case _ => {
        if (params.throwRejectedBeliefs) {
          throw new IncomprehensibleBeliefExcn(sentence)
        }
        None
      }
    }
  }

  private def isSubsumption(
    forkedCosmos : SpcCosmos,
    generalOpt : Option[SilPredicate],
    specificOpt : Option[SilPredicate],
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Boolean =
  {
    // maybe we should maintain this relationship in the graph
    // for efficiency?

    tupleN((generalOpt, specificOpt)) match {
      case (Some(general), Some(specific)) => {
        isSubsumption(forkedCosmos, general, specific, referenceMap)
      }
      case _ => false
    }
  }

  private def isSubsumption(
    forkedCosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Boolean =
  {
    assertionMapper.matchSubsumption(
      forkedCosmos,
      general,
      specific,
      referenceMap)
  }

  def processTriggerablePredicate(
    viewedCosmos : SpcCosmos,
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]],
    applicability : SpcAssertionApplicability,
    triggerDepth : Int,
    flagErrors : Boolean)
      : Option[String] =
  {
    val results = mind.getCosmos.getAssertions.map(assertion => {
      val result = applyAssertion(
        viewedCosmos, assertion, predicate, referenceMap,
        applicability, triggerDepth
      )
      if (result.strength == ASSERTION_STRONG_FAILURE) {
        return Some(result.message)
      }
      result
    })

    val grouped = results.groupBy(_.strength)
    val weakFailures = grouped.getOrElse(ASSERTION_WEAK_FAILURE, Seq.empty)
    val passes = grouped.getOrElse(ASSERTION_PASS, Seq.empty)

    weakFailures.find(
      w => !passes.exists(
        p => isSubsumption(
          viewedCosmos, w.predicate, p.predicate, referenceMap))) match
    {
      case Some(result) => {
        return Some(result.message)
      }
      case _ =>
    }

    if (applicability != APPLY_CONSTRAINTS_ONLY) {
      predicate match {
        case ap : SilActionPredicate => {
          val executorResponse = executor.executeAction(ap, referenceMap)
          if (executorResponse.nonEmpty) {
            return executorResponse
          }
        }
        case _ => {
        }
      }
    }

    if (passes.nonEmpty) {
      Some(sentencePrinter.sb.respondCompliance)
    } else {
      if (flagErrors) {
        Some(sentencePrinter.sb.respondIrrelevant)
      } else {
        None
      }
    }
  }

  protected def checkCycle(
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    referenceMap : Map[SilReference, Set[SpcEntity]],
    isPrecondition : Boolean = false) : Try[Boolean] =
  {
    val boundPredicate = bindPredicate(predicate, referenceMap)
    if (seen.size > 100) {
      fail(sentencePrinter.sb.respondTriggerLimit)
    } else if (seen.contains(boundPredicate)) {
      Success(true)
    } else {
      seen += boundPredicate
      Success(false)
    }
  }

  private def bindPredicate(
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]]) =
  {
    val rewriter = new SilPhraseRewriter
    def replaceReferences = rewriter.replacementMatcher(
      "bindReferences", {
        case ref : SilReference => {
          referenceMap.get(ref) match {
            case Some(entities) => {
              SilMappedReference(
                entities.map(_.name).toSeq.sorted.mkString("+"),
                DETERMINER_UNSPECIFIED)
            }
            case _ => ref
          }
        }
      }
    )
    rewriter.rewrite(replaceReferences, predicate)
  }

  // FIXME:  i18n
  private def respondRejection(ex : RejectedBeliefExcn) : String =
  {
    val beliefString = printBelief(ex.belief)
    ex match {
      case UnimplementedBeliefExcn(belief) => {
        s"I am not yet capable of processing the belief that ${beliefString}."
      }
      case InvalidBeliefExcn(belief) => {
        s"The belief that ${beliefString} is not valid in the given context."
      }
      case IncomprehensibleBeliefExcn(belief) => {
        s"I am unable to understand the belief that ${beliefString}."
      }
      case ContradictoryBeliefExcn(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"The belief that ${beliefString} contradicts " +
        s"the belief that ${originalBeliefString}."
      }
      case AmbiguousBeliefExcn(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}.  So there is" +
          s" an ambiguous reference in the belief that ${beliefString}."
      }
      case IncrementalCardinalityExcn(belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}." +
          s"  So it does not add up when I hear that ${beliefString}."
      }
    }
  }

  private def printBelief(belief : SilSentence) : String =
  {
    val punctuated = belief.maybeSyntaxTree match {
      case Some(syntaxTree) => syntaxTree.toWordString
      case _ => sentencePrinter.print(belief)
    }
    // FIXME:  need a cleaner way to omit full stop
    punctuated.dropRight(1).trim
  }

  private def unknownType() : SpcForm =
  {
    mind.instantiateForm(SilWord(SpcMeta.ENTITY_METAFORM_NAME))
  }

  private[platonic] def deriveType(
    ref : SilReference) : SpcForm =
  {
    def cosmos = mind.getCosmos
    typeMemo.getOrElseUpdate(ref, {
      ref match {
        case SilConjunctiveReference(_, refs, _) => {
          lcaType(refs.map(deriveType).toSet)
        }
        case SilGenitiveReference(possessor, SilNounReference(noun, _, _)) => {
          val possessorType = deriveType(possessor)
          mind.resolveRole(possessorType, noun) match {
            case Some(role) => {
              lcaType(cosmos.getGraph.getFormsForRole(role).toSet)
            }
            case _ => {
              cosmos.findProperty(
                possessorType, cosmos.encodeName(noun.toLemma)) match
              {
                case Some(property) => {
                  mind.resolveForm(
                    SilWord(property.domain.name)).getOrElse(unknownType)
                }
                case _ => unknownType
              }
            }
          }
        }
        case SilNounReference(noun, _, _) => {
          // FIXME resolve roles as well?
          if (noun.isProper) {
            cosmos.getEntityBySynonym(cosmos.encodeName(noun)).map(_.form).
              getOrElse(unknownType)
          } else {
            mind.resolveForm(noun).getOrElse(unknownType)
          }
        }
        case pr : SilPronounReference => {
          mind.resolvePronoun(communicationContext, pr) match {
            case Success(entities) => {
              lcaType(entities.map(_.form))
            }
            case _ => unknownType
          }
        }
        case SilStateSpecifiedReference(sub, state) => {
          deriveType(sub)
        }
        case _ => unknownType
      }
    })
  }

  private def lcaType(forms : Set[SpcForm]) : SpcForm =
  {
    if (forms.isEmpty) {
      unknownType
    } else {
      def lcaPair(o1 : Option[SpcForm], o2 : Option[SpcForm])
          : Option[SpcForm] =
      {
        (o1, o2) match {
          case (Some(f1), Some(f2)) => {
            mind.getCosmos.getGraph.closestCommonHypernym(f1, f2).
              map(_.asInstanceOf[SpcForm])
          }
          case _ => None
        }
      }
      forms.map(Some(_)).reduce(lcaPair).getOrElse(unknownType)
    }
  }

  override protected def matchActions(
    eventActionPredicate : SilPredicate,
    queryActionPredicate : SilPredicate,
    eventReferenceMap : Map[SilReference, Set[SpcEntity]],
    resultCollector : ResultCollectorType,
    applyBindings : Boolean) : Try[Boolean] =
  {
    val modifiableReferenceMap =
      SmcResultCollector.modifiableReferenceMap(eventReferenceMap)
    val queue = new mutable.Queue[SilPredicate]
    queue.enqueue(eventActionPredicate)
    val seen = new mutable.HashSet[SilPredicate]
    while (!queue.isEmpty) {
      val predicate = queue.dequeue
      checkCycle(predicate, seen, resultCollector.referenceMap) match {
        case f @ Failure(err) => {
          return f
        }
        case Success(true) => {
          return Success(false)
        }
        case _ =>
      }
      // FIXME need to attempt trigger rewrite in both directions
      val superMatch = super.matchActions(
        predicate, queryActionPredicate,
        modifiableReferenceMap, resultCollector, applyBindings)
      if (superMatch.isFailure) {
        return superMatch
      }
      if (superMatch.get) {
        return Success(true)
      } else {
        mind.getCosmos.getTriggers.foreach(trigger => {
          assertionMapper.matchImplication(
            "IMPLIES",
            mind.getCosmos, trigger.conditionalSentence,
            predicate,
            modifiableReferenceMap,
            Some(modifiableReferenceMap)
          ) match {
            case Some(newPredicate) => {
              queue.enqueue(newPredicate)
            }
            case _ =>
          }
        })
      }
    }
    Success(false)
  }
}
