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

class SpcContextualScorer(
  responder : SpcResponder,
  annotator : SpcAnnotator)
    extends SmcContextualScorer(responder, annotator)
{
  override protected def computeBoost(
    sentence : SilSentence,
    resultCollector : ResultCollectorType) : SilPhraseScore =
  {
    val beliefAccepter = SpcBeliefAccepter(
      responder,
      SpcBeliefParams(
        createImplicitIdeals = false,
        createTentativeIdeals = false,
        createTentativeEntities = false,
        createImplicitProperties = false
      ),
      resultCollector)
    val beliefs = beliefAccepter.recognizeBeliefs(sentence)
    val beliefBoost = {
      if (beliefs.isEmpty) {
        SilPhraseScore.neutral
      } else {
        if (beliefs.exists(_ match {
          case _ : InvalidBelief => true
          case ab : AssertionBelief => {
            !beliefAccepter.isAssertionValid(ab)
          }
          case _ => false
        })) {
          SilPhraseScore.conSmall
        } else {
          if (beliefs.exists(_.isInstanceOf[UnimplementedBelief])) {
            SilPhraseScore.neutral
          } else {
            SilPhraseScore.proBig
          }
        }
      }
    }
    var propBoost = SilPhraseScore.neutral
    val querier = new SilPhraseQuerier
    val cosmos = responder.getMind.getCosmos
    def detectBoosts = querier.queryMatcher {
      case SilStatePredicate(
        subject,
        _,
        SilPropertyState(SilWordLemma(lemma)),
        _
      ) => {
        val detected = resultCollector.lookup(subject) match {
          case Some(entities) => {
            entities.exists(entity => {
              entity.isInstanceOf[SpcTransientEntity] ||
                cosmos.resolvePropertyState(entity, lemma).isSuccess
            })
          }
          case _ => {
            val form = responder.deriveType(
              SpcAnnotator(resultCollector.annotator),
              subject, resultCollector.refMap)
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

class SpcRefNote(
  ref : SilReference
) extends SmcRefNote[SpcEntity](ref)
{
  private var form : Option[SpcForm] = None

  def maybeForm() : Option[SpcForm] = form

  protected def copyFrom(oldNote : SpcRefNote)
  {
    super.copyFrom(oldNote)
    form = oldNote.form
  }

  override def updateRef(newRef : SilReference) : SpcRefNote =
  {
    val newNote = new SpcRefNote(newRef)
    newNote.copyFrom(this)
    newNote
  }

  def setForm(newForm : SpcForm)
  {
    form = Some(newForm)
  }
}

class SpcResultCollector private(
  val spcAnnotator : SpcAnnotator,
  refMap : SpcMutableRefMap
) extends SmcResultCollector[SpcEntity](
  spcAnnotator,
  refMap
)
{
  override protected def preSpawn() =
  {
    SpcResultCollector(spcAnnotator, refMap)
  }
}

object SpcResultCollector
{
  def apply(annotator : SpcAnnotator) : SpcResultCollector =
  {
    val refMap = SmcResultCollector.newAnnotationRefMap(
      SmcAnnotator[SpcEntity](annotator))
    new SpcResultCollector(annotator, refMap)
  }

  def apply(
    annotator : SpcAnnotator,
    refMap : SmcMutableRefMap[SpcEntity]
  ) : SpcResultCollector =
  {
    new SpcResultCollector(annotator, refMap)
  }

  def apply(
    resultCollector : SmcResultCollector[SpcEntity]
  ) : SpcResultCollector =
  {
    resultCollector.asInstanceOf[SpcResultCollector]
  }
}

class SpcAnnotator extends SmcAnnotator[SpcEntity, SpcRefNote](
  (ref) => new SpcRefNote(ref))
{
}

object SpcAnnotator
{
  def apply() : SpcAnnotator = new SpcAnnotator

  def apply(annotator : SilAnnotator) : SpcAnnotator =
    annotator.asInstanceOf[SpcAnnotator]
}

class SpcResponder(
  mind : SpcMind,
  beliefParams : SpcBeliefParams =
    SpcBeliefParams(ACCEPT_NO_BELIEFS),
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

  private def newAssertionMapper(
    annotator : SpcAnnotator
  ) = new SpcAssertionMapper(
    mind, communicationContext, newInputRewriter(annotator),
    sentencePrinter)

  override protected def spawn(subMind : SpcMind) =
  {
    new SpcResponder(subMind, beliefParams, params,
      executor, communicationContext)
  }

  override def newParser(input : String) =
  {
    val contextAnnotator = SpcAnnotator()
    val context = SprContext(
      mind.getCosmos.getWordLabeler,
      scorer = new SpcContextualScorer(this, contextAnnotator),
      annotator = contextAnnotator)
    SprParser(input, context)
  }

  override protected def newAnnotator() =
  {
    SpcAnnotator()
  }

  override def newResultCollector(
    annotator : AnnotatorType
  ) : SpcResultCollector =
  {
    SpcResultCollector(SpcAnnotator(annotator))
  }


  override protected def newPredicateEvaluator(
    annotator : AnnotatorType,
    scope : ScopeType = mindScope
  ) =
    new SmcPredicateEvaluator[SpcEntity, SpcProperty, SpcCosmos, SpcMind](
      SmcAnnotator(annotator), scope, params.existenceAssumption,
      communicationContext, debugger)
  {
    override protected def reifyRole(
      possessor : SpcEntity, roleName : SilWord, onlyIfProven : Boolean)
        : Set[SpcEntity] =
    {
      if (beliefParams.createTentativeEntities) {
        super.reifyRole(
          possessor, roleName, onlyIfProven)
      } else {
        mind.resolveGenitive(possessor, roleName).get
      }
    }

    override protected def evaluateActionPredicate(
      predicate : SilActionPredicate,
      resultCollector : ResultCollectorType) : Try[Trilean] =
    {
      val spcAnnotator = SpcAnnotator(annotator)
      checkCycle(
        spcAnnotator, predicate, already,
        resultCollector.refMap
      ) matchPartial {
        case f @ Failure(err) => {
          return f.map(Trilean(_))
        }
        case Success(true) => {
          return Success(Trilean.Unknown)
        }
      }
      getBiconditionalImplications(
        spcAnnotator
      ).foreach {
        case (conditionalSentence, placeholderMap) => {
          newAssertionMapper(
            spcAnnotator
          ).matchImplication(
            "IMPLIES",
            mind.getCosmos,
            conditionalSentence,
            predicate,
            SpcAssertionBinding(
              spcAnnotator,
              resultCollector.refMap,
              Some(resultCollector.refMap),
              Some(placeholderMap)
            )
          ) matchPartial {
            case Some(newPredicate) => {
              return super.evaluatePredicate(newPredicate, resultCollector)
            }
          }
        }
      }
      super.evaluateActionPredicate(predicate, resultCollector)
    }

    override protected def normalizePredicate(
      annotator : AnnotatorType,
      predicate : SilPredicate,
      refMap : SpcRefMap
    ) : SilPredicate =
    {
      val spcAnnotator = SpcAnnotator(annotator)
      if (scoreEquivalentPredicate(spcAnnotator, predicate, refMap) == 1) {
        // the original predicate is something that we want to
        // keep no matter what
        return predicate
      }
      // FIXME this could cause the predicate to become
      // inconsistent with the answer inflection.  Also, when there
      // are multiple matches, we should be conjoining them.
      val replacements = getBiconditionalImplications(spcAnnotator).flatMap {
        case (conditionalSentence, placeholderMap) => {
          newAssertionMapper(spcAnnotator).matchImplication(
            "IMPLIES",
            mind.getCosmos,
            conditionalSentence,
            predicate,
            SpcAssertionBinding(
              spcAnnotator,
              refMap,
              None,
              Some(placeholderMap)
            )
          )
        }
      }.map(p => optimizeEquivalentPredicate(spcAnnotator, p, refMap))
      replacements.filter(_._2 >= 0).sortBy(_._2).map(_._1).headOption.
        getOrElse(predicate)
    }

    private def scoreEquivalentPredicate(
      annotator : SpcAnnotator,
      predicate : SilPredicate,
      refMap : SpcRefMap) : Int =
    {
      predicate match {
        case SilStatePredicate(
          subject, _, SilPropertyState(SilWordLemma(lemma)), _
        ) => {
          val form = deriveType(annotator, subject, refMap)
          mind.getCosmos.resolveHypernymPropertyState(
            form, lemma).map(_ => 1).getOrElse(-1)
        }
        case _ => 0
      }
    }

    private def optimizeEquivalentPredicate(
      annotator : SpcAnnotator,
      sil : SilPredicate,
      refMap : SpcRefMap
    ) : (SilPredicate, Int) =
    {
      val rewriter = new SilPhraseRewriter(annotator)
      var score = 0
      def optimizePredicate = rewriter.replacementMatcher(
        "optimizePredicate", {
          case sp : SilStatePredicate => {
            if (scoreEquivalentPredicate(annotator, sp, refMap) == -1) {
              score = -1
            }
            sp
          }
          case SilGenitiveReference(
            possessor @ SilDeterminedReference(
              _ : SilNounReference, DETERMINER_ANY
            ),
            possessee : SilNounReference
          ) => {
            if (score == 0) {
              score = 1
            }
            annotator.genitiveRef(
              possessor,
              annotator.determinedRef(possessee, DETERMINER_ANY)
            )
          }
        }
      )
      tupleN((rewriter.rewrite(optimizePredicate, sil), score))
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

  def getBiconditionalImplications(annotator : SpcAnnotator)
      : Seq[(SilConditionalSentence, SpcRefMap)] =
  {
    val triggers = getTriggers(annotator).filter(
      _.conditionalSentence.biconditional)
    triggers.flatMap(getTriggerImplications(annotator, _))
  }

  private def reannotate[PhraseType <: SilPhrase](
    annotator : SpcAnnotator,
    phrase : PhraseType) : PhraseType =
  {
    annotator.copy(phrase, SilPhraseCopyOptions(preserveNotes = true))
  }

  def getAssertions(annotator : SpcAnnotator) : Seq[SpcAssertion] =
  {
    mind.getCosmos.getAssertions.map(assertion => {
      SpcAssertion(
        reannotate(annotator, assertion.sentence),
        assertion.additionalConsequents.map(reannotate(annotator, _)),
        assertion.alternative.map(reannotate(annotator, _)),
        assertion.placeholderMap
      )
    })
  }

  def getTriggers(annotator : SpcAnnotator) : Seq[SpcTrigger] =
  {
    mind.getCosmos.getTriggers.map(trigger => {
      SpcTrigger(
        reannotate(annotator, trigger.conditionalSentence),
        trigger.additionalConsequents.map(reannotate(annotator, _)),
        trigger.alternative.map(reannotate(annotator, _)),
        trigger.placeholderMap
      )
    })
  }

  def getTriggerImplications(
    annotator : SpcAnnotator,
    trigger : SpcTrigger) : Seq[(SilConditionalSentence, SpcMutableRefMap)] =
  {
    val cs = trigger.conditionalSentence
    val placeholderMap = SmcMutableRefMap.newByValue[SpcEntity]
    placeholderMap ++= trigger.getPlaceholderMap
    val standardized = cs.copy(
      antecedent = standardizeVariables(
        annotator, cs.antecedent, placeholderMap),
      consequent = standardizeVariables(
        annotator, cs.consequent, placeholderMap))
    val seq = {
      if (cs.biconditional) {
        assert(trigger.additionalConsequents.isEmpty)
        assert(trigger.alternative.isEmpty)

        Seq(
          standardized,
          cs.copy(
            antecedent = flipVariables(
              annotator, standardized.consequent, placeholderMap),
            consequent = flipVariables(
              annotator, cs.antecedent, placeholderMap),
            tamAntecedent = cs.tamConsequent,
            tamConsequent = cs.tamAntecedent
          )
        )
      } else {
        Seq(standardized)
      }
    }
    seq.map(cs => tupleN((cs, placeholderMap)))
  }

  private def standardizeVariables[PhraseType <: SilPhrase](
    annotator : SpcAnnotator,
    phrase : PhraseType,
    placeholderMap : SpcMutableRefMap
  ) : PhraseType =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    def standardizeOne(ref : SilReference) : SilReference =
    {
      SpcImplicationMapper.findPlaceholderCorrespondence(
        annotator, ref, Some(placeholderMap)
      ) match {
        case (true, correspondingRefs) if (!correspondingRefs.isEmpty) => {
          val newRef = SpcImplicationMapper.flipVariable(
            annotator, sentencePrinter, correspondingRefs.head, ref)
          placeholderMap.put(newRef, placeholderMap(ref))
          newRef
        }
        case _ => ref
      }
    }
    def replaceReferences = rewriter.replacementMatcher(
      "standardizeVariables", {
        case ar @ SilAppositionalReference(primary, _) => {
          placeholderMap.put(primary, placeholderMap(ar))
          primary
        }
        case dr @ SilDeterminedReference(
          _, determiner
        ) => {
          if (determiner == DETERMINER_UNIQUE) {
            standardizeOne(dr)
          } else {
            dr
          }
        }
        case sr : SilStateSpecifiedReference => {
          sr
        }
        case ref : SilReference => {
          standardizeOne(ref)
        }
      }
    )
    rewriter.rewrite(
      replaceReferences, phrase, SilRewriteOptions(topDown = true))
  }

  private def flipVariables(
    annotator : SpcAnnotator,
    predicate : SilPredicate,
    placeholderMap : SpcMutableRefMap
  ) : SilPredicate =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    def replaceReferences = rewriter.replacementMatcher(
      "flipVariables", {
        case ref : SilReference => {
          SpcImplicationMapper.findPlaceholderCorrespondence(
            annotator, ref, Some(placeholderMap)
          ) match {
            case (true, correspondingRefs) if (!correspondingRefs.isEmpty) => {
              val newRef = SpcImplicationMapper.flipVariable(
                annotator, sentencePrinter, ref, correspondingRefs.head)
              placeholderMap.put(newRef, placeholderMap(ref))
              newRef
            }
            case _ => ref
          }
        }
      }
    )
    rewriter.rewrite(
      replaceReferences, predicate, SilRewriteOptions(topDown = true))
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
    }
  }

  private def attemptResponse(
    resultCollector : SpcResultCollector)(sentence : SilSentence)
      : Option[(SilSentence, String)] =
  {
    if (sentence.tam.isIndicative &&
      (beliefParams.acceptance != IGNORE_BELIEFS))
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
      ) matchPartial {
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
                  resultCollector)
              } catch {
                case e @ ShlurdException(
                  ShlurdExceptionCode.CausalityViolation, message) => {
                  return Some(wrapResponseText(message))
                }
              }
            })
          }
          if (!temporal) {
            forkedCosmos.applyModifications
          }
          return Some(wrapResponseText(result))
        }
      }
    }
    already.clear
    None
  }

  private def applyAssertion(
    annotator : SpcAnnotator,
    forkedCosmos : SpcCosmos,
    assertion : SpcAssertion,
    predicate : SilPredicate,
    refMap : SpcRefMap,
    applicability : SpcAssertionApplicability,
    triggerDepth : Int)
      : SpcAssertionResult =
  {
    val resultCollector = SpcResultCollector(
      annotator, SmcResultCollector.modifiableRefMap(refMap))
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
            case SilBasicVerbModifier(SilWordLemma(LEMMA_GENERALLY)) => true
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
          annotator, forkedCosmos, requirement, predicate, refMap)
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
    resultCollector : SpcResultCollector,
    triggerDepth : Int)
      : Option[String] =
  {
    val annotator = resultCollector.spcAnnotator
    getTriggerImplications(
      annotator, trigger
    ).toStream.flatMap {
      case (conditionalSentence, placeholderMap) => {
        applyTriggerImpl(
          forkedCosmos, trigger, placeholderMap, conditionalSentence,
          trigger.additionalConsequents.map(
            s => standardizeVariables(
              annotator, s, placeholderMap)),
          trigger.getAlternative.map(
            s => standardizeVariables(
              annotator, s, placeholderMap)),
          predicate, resultCollector, triggerDepth)
      }
    }.headOption
  }

  private def applyTriggerImpl(
    forkedCosmos : SpcCosmos,
    trigger : SpcTrigger,
    placeholderMap : SpcRefMap,
    conditionalSentence : SilConditionalSentence,
    additionalConsequents : Seq[SilPredicateSentence],
    alternative : Option[SilPredicateSentence],
    predicate : SilPredicate,
    resultCollector : SpcResultCollector,
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
    val annotator = resultCollector.spcAnnotator
    newAssertionMapper(
      annotator
    ).matchImplicationPlusAlternative(
      operator,
      forkedCosmos, conditionalSentence,
      predicate,
      additionalConsequents, alternative,
      SpcAssertionBinding(
        annotator,
        resultCollector.refMap,
        Some(resultCollector.refMap),
        Some(placeholderMap)),
      triggerDepth) match
    {
      case (Some(newPredicate), newAdditionalConsequents, newAlternative) => {
        val newConsequents = (
          SilPredicateSentence(newPredicate) +: newAdditionalConsequents
        ).map(
          removeBasicVerbModifier(
            _, Set(LEMMA_ALSO, LEMMA_SUBSEQUENTLY, LEMMA_CONSEQUENTLY)
          )
        )
        newConsequents.foreach(sentence => {
          checkCycle(
            annotator,
            sentence.predicate, already,
            resultCollector.refMap, isPrecondition || isTest
          ) matchPartial {
            case Failure(err) => {
              return Some(wrapResponseMessage(err))
            }
            case Success(true) => {
              return None
            }
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
                case ShlurdException(ShlurdExceptionCode.NonExistent, _) => {
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
                  Set(LEMMA_OTHERWISE, LEMMA_SUBSEQUENTLY, LEMMA_CONSEQUENTLY))
                checkCycle(
                  annotator,
                  recoverySentence.predicate,
                  already, resultCollector.refMap
                ) matchPartial {
                  case Failure(err) => {
                    return Some(wrapResponseMessage(err))
                  }
                  case Success(true) => {
                    return None
                  }
                }
                spawn(imagine(forkedCosmos)).resolveReferences(
                  recoverySentence, resultCollector, false, true)
                // FIXME use return value of this invocation somehow
                processBeliefOrAction(
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
              SilWordLemma(lemma)) if (lemmas.contains(lemma)) => true
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
      rewritten matchPartial {
        case SilRelationshipPredicate(
          SilDeterminedReference(
            SilCountedNounReference(SilWordLemma(lemma), count),
            DETERMINER_ANY),
          SilRelationshipPredefVerb(REL_PREDEF_IDENTITY),
          complement,
          modifiers
        ) => {
          val form = deriveType(
            SpcAnnotator(resultCollector.annotator),
            complement, resultCollector.refMap)
          if (mind.getCosmos.findProperty(form, lemma).nonEmpty) {
            val statePredicate = SilStatePredicate(
              complement,
              STATE_PREDEF_BE.toVerb,
              SilPropertyQueryState(lemma),
              modifiers
            )
            return tupleN((statePredicate, INFLECT_COMPLEMENT))
          }
        }
      }
    }
    tupleN((rewritten, answerInflection))
  }

  override protected def newQueryRewriter(
    annotator : AnnotatorType,
    question : SilQuestion,
    answerInflection : SilInflection) =
  {
    new SpcQueryRewriter(SpcAnnotator(annotator), question, answerInflection)
  }

  private def updateRefMap(
    sentence : SilSentence,
    cosmos : SpcCosmos,
    resultCollector : SpcResultCollector)
  {
    // we may have modified cosmos (e.g. with new entities) by this
    // point, so run another full reference resolution pass to pick
    // them up
    resultCollector.refMap.clear
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
    resultCollector : SpcResultCollector,
    triggerDepth : Int,
    flagErrors : Boolean = true)
      : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    val spawned = spawn(mind.spawn(forkedCosmos))
    val refMap = SmcResultCollector.snapshotRefMap(resultCollector.refMap)
    val beliefAccepter =
      SpcBeliefAccepter(
        spawned,
        beliefParams,
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
      sentence matchPartial {
        case SilPredicateSentence(predicate, _, _) => {
          if (flagErrors && predicate.isInstanceOf[SilActionPredicate]) {
            resultCollector.refMap.clear
            val resolutionResult =
              spawn(imagine(forkedCosmos)).resolveReferences(
                predicate, resultCollector,
                true, false)
            resolutionResult matchPartial {
              case Failure(ex) => {
                earlyReturn = Some(wrapResponseMessage(ex))
              }
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
              resultCollector.spcAnnotator,
              forkedCosmos, predicate,
              refMap, applicability,
              triggerDepth, flagErrors && !matched)
            if (!result.isEmpty) {
              earlyReturn = result
            }
          }
        }
      }
    }
    // defer until this point so that any newly created entities etc will
    // already have taken effect
    if (triggerDepth == 0) {
      updateRefMap(sentence, forkedCosmos, resultCollector)
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
          throw new IncomprehensibleBeliefExcn(
            ShlurdExceptionCode.IncomprehensibleBelief,
            sentence)
        }
        None
      }
    }
  }

  private def isSubsumption(
    annotator : SpcAnnotator,
    forkedCosmos : SpcCosmos,
    generalOpt : Option[SilPredicate],
    specificOpt : Option[SilPredicate],
    refMap : SpcRefMap) : Boolean =
  {
    // maybe we should maintain this relationship in the graph
    // for efficiency?

    tupleN((generalOpt, specificOpt)) match {
      case (Some(general), Some(specific)) => {
        isSubsumption(annotator, forkedCosmos, general, specific, refMap)
      }
      case _ => false
    }
  }

  private def isSubsumption(
    annotator : SpcAnnotator,
    forkedCosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    refMap : SpcRefMap) : Boolean =
  {
    newAssertionMapper(annotator).matchSubsumption(
      annotator,
      forkedCosmos,
      general,
      specific,
      refMap)
  }

  def processTriggerablePredicate(
    annotator : SpcAnnotator,
    viewedCosmos : SpcCosmos,
    predicate : SilPredicate,
    refMap : SpcRefMap,
    applicability : SpcAssertionApplicability,
    triggerDepth : Int,
    flagErrors : Boolean)
      : Option[String] =
  {
    val results = getAssertions(annotator).map(assertion => {
      val result = applyAssertion(
        annotator,
        viewedCosmos, assertion, predicate, refMap,
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
          annotator, viewedCosmos,
          w.predicate, p.predicate, refMap))
    ) matchPartial {
      case Some(result) => {
        return Some(result.message)
      }
    }

    if (applicability != APPLY_CONSTRAINTS_ONLY) {
      predicate matchPartial {
        case ap : SilActionPredicate => {
          val executorResponse = executor.executeAction(ap, refMap)
          if (executorResponse.nonEmpty) {
            return executorResponse
          }
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
    annotator : SpcAnnotator,
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    refMap : SpcRefMap,
    isPrecondition : Boolean = false) : Try[Boolean] =
  {
    val boundPredicate = bindPredicate(annotator, predicate, refMap)
    if (seen.size > 100) {
      mind.getCosmos.fail(
        ShlurdExceptionCode.TriggerLimit,
        sentencePrinter.sb.respondTriggerLimit)
    } else if (seen.contains(boundPredicate)) {
      Success(true)
    } else {
      seen += boundPredicate
      Success(false)
    }
  }

  private def bindPredicate(
    annotator : SpcAnnotator,
    predicate : SilPredicate,
    refMap : SpcRefMap) =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    def replaceReferences = rewriter.replacementMatcher(
      "bindReferences", {
        case ref : SilReference => {
          refMap.get(ref) match {
            case Some(entities) => {
              annotator.mappedRef(
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

  private def wrapResponseMessage(ex : Throwable) : String =
  {
    wrapResponseText(ex)._2
  }

  // FIXME:  i18n
  private def respondRejection(ex : RejectedBeliefExcn) : String =
  {
    val beliefString = printBelief(ex.belief)
    val msg = ex match {
      case UnimplementedBeliefExcn(code, belief) => {
        s"I am not yet capable of processing the belief that ${beliefString}."
      }
      case InvalidBeliefExcn(code, belief) => {
        s"I am unable to validate the belief that ${beliefString}."
      }
      case UnacceptableBeliefExcn(code, cause, belief) => {
        cause
      }
      case ProhibitedBeliefExcn(code, belief) => {
        s"The belief that ${beliefString} is prohibited in the given context."
      }
      case IncomprehensibleBeliefExcn(code, belief) => {
        s"I am unable to understand the belief that ${beliefString}."
      }
      case ContradictoryBeliefExcn(code, belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"The belief that ${beliefString} contradicts " +
        s"the belief that ${originalBeliefString}."
      }
      case AmbiguousBeliefExcn(code, belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}.  So there is" +
          s" an ambiguous reference in the belief that ${beliefString}."
      }
      case IncrementalCardinalityExcn(code, belief, originalBelief) => {
        val originalBeliefString = printBelief(originalBelief)
        s"Previously I was told that ${originalBeliefString}." +
          s"  So it does not add up when I hear that ${beliefString}."
      }
    }
    wrapResponseText(ex.getCode, msg)._2
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

  def unknownType() : SpcForm =
  {
    mind.instantiateForm(SilWord(SpcMeta.ENTITY_METAFORM_NAME))
  }

  private[platonic] def deriveType(
    annotator : SpcAnnotator,
    ref : SilReference,
    refMap : SpcRefMap) : SpcForm =
  {
    ref match {
      case annotatedRef : SilAnnotatedReference => {
        val note = annotator.getNote(annotatedRef)
        note.maybeForm.getOrElse {
          val form = deriveTypeImpl(annotator, ref, refMap)
          note.setForm(form)
          form
        }
      }
      case _ => {
        unknownType
      }
    }
  }

  private def deriveTypeImpl(
    annotator : SpcAnnotator,
    ref : SilReference,
    refMap : SpcRefMap) : SpcForm =
  {
    def cosmos = mind.getCosmos
    ref match {
      case SilConjunctiveReference(_, refs, _) => {
        lcaType(refs.map(r => deriveType(annotator, r, refMap)).toSet)
      }
      case SilGenitiveReference(
        possessor,
        SilOptionallyDeterminedReference(SilNounReference(noun), _)
      ) => {
        val possessorType = deriveType(annotator, possessor, refMap)
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
      case SilDeterminedReference(sub, _) => {
        deriveType(annotator, sub, refMap)
      }
      case SilAppositionalReference(primary, _) => {
        deriveType(annotator, primary, refMap)
      }
      case SilOptionallyDeterminedReference(SilNounReference(noun), _) => {
        // FIXME resolve roles as well?
        if (noun.isProper) {
          lcaType(
            cosmos.getEntitiesBySynonym(
              cosmos.synthesizeEntitySynonym(noun.toNounLemma)
            ).map(_.form).toSet)
        } else {
          mind.resolveForm(noun).getOrElse(unknownType)
        }
      }
      case pr : SilPronounReference => {
        // FIXME in case no entities are resolved, try prior
        // reference instead
        val scope = new SmcPhraseScope(
          refMap, mindScope)
        scope.resolvePronoun(annotator, communicationContext, pr) match {
          case Success(SmcScopeOutput(_, entities)) => {
            lcaType(entities.map(_.form))
          }
          case _ => unknownType
        }
      }
      case SilStateSpecifiedReference(sub, state) => {
        deriveType(annotator, sub, refMap)
      }
      case _ => unknownType
    }
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
    eventRefMap : SpcRefMap,
    resultCollector : ResultCollectorType,
    applyBindings : Boolean) : Try[Boolean] =
  {
    val spcAnnotator = SpcAnnotator(resultCollector.annotator)
    val modifiableRefMap =
      SmcResultCollector.modifiableRefMap(eventRefMap)
    val queue = new mutable.Queue[SilPredicate]
    queue.enqueue(eventActionPredicate)
    val seen = new mutable.HashSet[SilPredicate]
    while (!queue.isEmpty) {
      val predicate = queue.dequeue
      checkCycle(
        spcAnnotator,
        predicate, seen, resultCollector.refMap
      ) matchPartial {
        case f @ Failure(err) => {
          return f
        }
        case Success(true) => {
          return Success(false)
        }
      }
      // FIXME need to attempt trigger rewrite in both directions
      val superMatch = super.matchActions(
        predicate, queryActionPredicate,
        modifiableRefMap, resultCollector, applyBindings)
      if (superMatch.isFailure) {
        return superMatch
      }
      if (superMatch.get) {
        return Success(true)
      } else {
        getTriggers(spcAnnotator).foreach(trigger => {
          newAssertionMapper(spcAnnotator).matchImplication(
            "IMPLIES",
            mind.getCosmos, trigger.conditionalSentence,
            predicate,
            SpcAssertionBinding(
              spcAnnotator,
              modifiableRefMap,
              Some(modifiableRefMap),
              Some(trigger.getPlaceholderMap))
          ).foreach(newPredicate => queue.enqueue(newPredicate))
        })
      }
    }
    Success(false)
  }
}
