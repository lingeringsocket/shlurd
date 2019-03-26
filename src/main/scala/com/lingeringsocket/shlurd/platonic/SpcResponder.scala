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

class SpcResponder(
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
  params : SmcResponseParams = SmcResponseParams(),
  executor : SmcExecutor[SpcEntity] = new SmcExecutor[SpcEntity]
) extends SmcResponder[
  SpcEntity, SpcProperty, SpcCosmos, SpcMind
](
  mind, params, executor
)
{
  private val already = new mutable.HashSet[SilPredicate]

  private var referenceMapOpt
      : Option[mutable.Map[SilReference, Set[SpcEntity]]] = None

  private val typeMemo = new mutable.LinkedHashMap[SilReference, SpcForm]

  private val triggerExecutor = new SpcTriggerExecutor(mind, inputRewriter)

  override protected def spawn(subMind : SpcMind) =
  {
    new SpcResponder(subMind, beliefAcceptance, params)
  }

  override protected def newPredicateEvaluator() =
    new SmcPredicateEvaluator[SpcEntity, SpcProperty, SpcCosmos, SpcMind](
      mind, sentencePrinter, params.existenceAssumption, debugger)
  {
    override protected def evaluateActionPredicate(
      predicate : SilActionPredicate,
      resultCollector : ResultCollectorType) : Try[Trilean] =
    {
      if (checkCycle(predicate, already)) {
        return fail(sentencePrinter.sb.circularAction)
      }
      mind.getCosmos.getTriggers.filter(
        _.conditionalSentence.biconditional).foreach(trigger => {
          triggerExecutor.matchTrigger(
            mind.getCosmos,
            trigger.conditionalSentence,
            predicate,
            referenceMapOpt.get) match
          {
            case Some(newPredicate) => {
              return super.evaluatePredicate(newPredicate, resultCollector)
            }
            case _ =>
          }
        }
      )
      super.evaluateActionPredicate(predicate, resultCollector)
    }

    override protected def normalizePredicate(
      predicate : SilPredicate,
      referenceMap : Map[SilReference, Set[SpcEntity]]) : SilPredicate =
    {
      val stateNormalized = predicate match {
        case SilStatePredicate(subject, state, modifiers) => {
          val normalizedState = mind.getCosmos.normalizeHyperFormState(
            deriveType(subject), state)
          SilStatePredicate(subject, normalizedState, modifiers)
        }
        case _ => predicate
      }
      // FIXME this could cause the predicate to become
      // inconsisent with the answer inflection.  Also, when there
      // are multiple matches, we should be conjoining them.
      val triggers = mind.getCosmos.getTriggers.filter(
        _.conditionalSentence.biconditional)
      val modifiableReferenceMap =
        SmcResultCollector.modifiableReferenceMap(referenceMap)
      val replacements = triggers.flatMap(trigger => {
        triggerExecutor.matchTrigger(
          mind.getCosmos,
          trigger.conditionalSentence,
          stateNormalized,
          modifiableReferenceMap)
      }).filter(acceptReplacement)
      replacements.headOption.getOrElse(stateNormalized)
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
  }

  override protected def imagine(
    alternateCosmos : SpcCosmos) =
  {
    mind.spawn(alternateCosmos.fork)
  }

  override protected def processImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    try {
      attemptResponse(sentence, resultCollector)
    } finally {
      already.clear
      referenceMapOpt = None
      typeMemo.clear
    }
  }

  private def attemptResponse(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
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

      val forkedCosmos = baselineCosmos.fork
      val inputSentence =
        predicateOpt.map(
          SilPredicateSentence(_, sentence.tam)).getOrElse(sentence)
      processBeliefOrAction(
        forkedCosmos, inputSentence, resultCollector
      ) match {
        case Some(result) => {
          if (result != sentencePrinter.sb.respondCompliance) {
            return wrapResponseText(result)
          }
          if (mind.hasNarrative) {
            predicateOpt.foreach(predicate => {
              val updatedCosmos = freezeCosmos(forkedCosmos)
              try {
                updateNarrative(
                  interval,
                  updatedCosmos,
                  predicate,
                  referenceMapOpt.get)
              } catch {
                case CausalityViolationExcn(cause) => {
                  return wrapResponseText(cause)
                }
              }
            })
          }
          if (!temporal) {
            forkedCosmos.applyModifications
          }
          return wrapResponseText(result)
        }
        case _ =>
      }
    }
    already.clear
    // in case we haven't done this already, need to do it now
    // in case evaluateActionPredicate is called by super
    saveReferenceMap(sentence, mind.getCosmos, resultCollector)
    super.processImpl(sentence, resultCollector)
  }

  private def applyTrigger(
    forkedCosmos : SpcCosmos,
    trigger : SpcTrigger,
    predicate : SilPredicate)
      : Option[String] =
  {
    val resultCollector = new SmcResultCollector[SpcEntity](referenceMapOpt.get)
    spawn(imagine(forkedCosmos)).resolveReferences(
      predicate, resultCollector, false, true)
    val conditionalSentence = trigger.conditionalSentence
    triggerExecutor.matchTrigger(
      forkedCosmos, conditionalSentence, predicate, referenceMapOpt.get) match
    {
      case Some(newPredicate) => {
        val isPrecondition =
          (conditionalSentence.tamConsequent.modality == MODAL_MUST)
        if (checkCycle(
          newPredicate, already, isPrecondition)
        ) {
          return Some(sentencePrinter.sb.circularAction)
        }
        val newSentence = SilPredicateSentence(newPredicate)
        spawn(imagine(forkedCosmos)).resolveReferences(
          newSentence, resultCollector, false, true)
        if (isPrecondition) {
          evaluateTamPredicate(
            newPredicate, SilTam.indicative, resultCollector) match
          {
            case Success(Trilean.True) => {
              None
            }
            case Failure(e) => {
              // FIXME we should be pickier about the error
              None
            }
            case _ => {
              trigger.alternative.foreach(alternativeSentence => {
                val recoverySentence = alternativeSentence.copy(
                  predicate = alternativeSentence.predicate.withNewModifiers(
                    alternativeSentence.predicate.getModifiers.filterNot(
                      _ match {
                        case SilBasicVerbModifier(
                          Seq(SilWordLemma(LEMMA_OTHERWISE)), _) => true
                        case _ => false
                      })))
                spawn(imagine(forkedCosmos)).resolveReferences(
                  recoverySentence, resultCollector, false, true)
                // FIXME use recoveryResult somehow
                val recoveryResult = processBeliefOrAction(
                  forkedCosmos, recoverySentence, resultCollector)
              })
              // FIXME i18n
              Some("But " + sentencePrinter.printPredicateStatement(
                newPredicate, SilTam.indicative.negative) + ".")
            }
          }
        } else {
          val result = processBeliefOrAction(
            forkedCosmos, newSentence, resultCollector)
          if (result.isEmpty) {
            Some(sentencePrinter.sb.respondCompliance)
          } else {
            result
          }
        }
      }
      case _ => None
    }
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
          complement,
          REL_IDENTITY,
          modifiers
        ) => {
          val form = deriveType(complement)
          if (mind.getCosmos.formHasProperty(form, lemma)) {
            val statePredicate = SilStatePredicate(
              complement,
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

  private def saveReferenceMap(
    sentence : SilSentence,
    cosmos : SpcCosmos,
    resultCollector : ResultCollectorType)
  {
    if (!referenceMapOpt.isEmpty) {
      return
    }

    // we may have modified cosmos (e.g. with new entities) by this
    // point, so run another full reference resolution pass to pick
    // them up
    resultCollector.referenceMap.clear
    spawn(imagine(cosmos)).resolveReferences(
      sentence, resultCollector)

    mind.rememberSentenceAnalysis(resultCollector.referenceMap)
    referenceMapOpt = Some(resultCollector.referenceMap)
  }

  override protected def freezeCosmos(mutableCosmos : SpcCosmos) =
  {
    // FIXME use smart deltas instead of wholesale copy
    val frozenCosmos = new SpcCosmos
    frozenCosmos.copyFrom(mutableCosmos)
    frozenCosmos.asUnmodifiable
  }

  private def processBeliefOrAction(
    forkedCosmos : SpcCosmos,
    sentence : SilSentence,
    resultCollector : ResultCollectorType)
      : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    val beliefAccepter =
      new SpcBeliefAccepter(
        mind.spawn(forkedCosmos),
        (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS),
        resultCollector)
    attemptAsBelief(beliefAccepter, sentence).foreach(result => {
      if (result != compliance) {
        return Some(result)
      } else {
        matched = true
      }
    })
    // defer until this point so that any newly created entities etc will
    // already have taken effect
    saveReferenceMap(sentence, forkedCosmos, resultCollector)
    sentence match {
      case SilPredicateSentence(predicate, _, _) => {
        val result = processTriggerablePredicate(forkedCosmos, predicate)
        if (!result.isEmpty) {
          return result
        }
      }
      case _ => {
      }
    }
    if (matched) {
      Some(compliance)
    } else {
      None
    }
  }

  protected def publishBelief(belief : SpcBelief)
  {
  }

  private def attemptAsBelief(
    beliefAccepter : SpcBeliefAccepter,
    sentence : SilSentence) : Option[String] =
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

  private def processTriggerablePredicate(
    forkedCosmos : SpcCosmos,
    predicate : SilPredicate) : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    mind.getCosmos.getTriggers.foreach(trigger => {
      applyTrigger(
        forkedCosmos, trigger, predicate
      ).foreach(result => {
        if (result != compliance) {
          return Some(result)
        } else {
          matched = true
        }
      })
    })
    predicate match {
      case ap : SilActionPredicate => {
        val executorResponse = executor.executeAction(ap)
        if (executorResponse.nonEmpty) {
          return executorResponse
        }
      }
      case _ =>
    }
    if (matched) {
      Some(compliance)
    } else {
      None
    }
  }

  protected def checkCycle(
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    isPrecondition : Boolean = false) : Boolean =
  {
    // FIXME make limit configurable and add test
    if (seen.contains(predicate) || (seen.size > 100)) {
      true
    } else {
      seen += predicate
      false
    }
  }

  // FIXME:  i18n
  private def respondRejection(ex : RejectedBeliefExcn) : String =
  {
    val beliefString = printBelief(ex.belief)
    ex match {
      case UnimplementedBeliefExcn(belief) => {
        s"I am not yet capable of processing the belief that ${beliefString}."
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
          // FIXME need to handle properties
          mind.resolveRole(deriveType(possessor), noun) match {
            case Some(role) => {
              lcaType(cosmos.getGraph.getFormsForRole(role).toSet)
            }
            case _ => unknownType
          }
        }
        case SilNounReference(noun, _, _) => {
          // FIXME resolve roles as well?
          if (noun.isProper) {
            cosmos.getEntityBySynonym(cosmos.deriveName(noun)).map(_.form).
              getOrElse(unknownType)
          } else {
            mind.resolveForm(noun).getOrElse(unknownType)
          }
        }
        case pr : SilPronounReference => {
          mind.resolvePronoun(pr) match {
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
      if (checkCycle(predicate, seen)) {
        return fail(sentencePrinter.sb.circularAction)
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
          triggerExecutor.matchTrigger(
            mind.getCosmos, trigger.conditionalSentence,
            predicate, modifiableReferenceMap
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