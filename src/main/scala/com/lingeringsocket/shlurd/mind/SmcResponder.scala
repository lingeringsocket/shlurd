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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.util._

import spire.math._

import scala.collection._

import org.slf4j._

case class SmcStateChangeInvocation[EntityType<:SmcEntity](
  entities : Set[EntityType],
  state : SilWord)
{
}

case class CausalityViolationExcn(cause : String)
    extends RuntimeException(cause)
{
}

sealed trait SmcResponseVerbosity
case object RESPONSE_TERSE extends SmcResponseVerbosity
case object RESPONSE_ELLIPSIS extends SmcResponseVerbosity
case object RESPONSE_COMPLETE extends SmcResponseVerbosity

sealed trait SmcExistenceAssumption
case object EXISTENCE_ASSUME_NOTHING extends SmcExistenceAssumption
case object EXISTENCE_ASSUME_UNKNOWN extends SmcExistenceAssumption

object SmcResponseParams
{
  def standard() = SmcResponseParams()
}

case class SmcResponseParams(
  listLimit : Int = 3,
  thirdPersonPronouns : Boolean = true,
  verbosity : SmcResponseVerbosity = RESPONSE_COMPLETE,
  existenceAssumption : SmcExistenceAssumption = EXISTENCE_ASSUME_NOTHING,
  throwRejectedBeliefs : Boolean = false
)
{
  def neverSummarize = (listLimit == Int.MaxValue)

  def alwaysSummarize = (listLimit == 0)
}

class SmcResultCollector[EntityType<:SmcEntity](
  val referenceMap : mutable.Map[SilReference, Set[EntityType]])
{
  val entityMap = new mutable.LinkedHashMap[EntityType, Trilean]
  val states = new mutable.LinkedHashSet[SilWord]
  var isCategorization = false
  var suppressWildcardExpansion = 0
  var swapSpeakerListener = false
  var resolvingReferences = false

  val refEquivalence =
    new IdentityLinkedHashMap[SilReference, SilReference]

  def spawn() = {
    val newCollector = new SmcResultCollector[EntityType](referenceMap)
    newCollector.suppressWildcardExpansion = suppressWildcardExpansion
    newCollector.swapSpeakerListener = swapSpeakerListener
    newCollector.resolvingReferences = resolvingReferences
    newCollector
  }

  def lookup(ref : SilReference) : Option[Set[EntityType]] =
  {
    val entitiesOpt = referenceMap.get(ref)
    entitiesOpt.orElse {
      refEquivalence.get(ref).flatMap(lookup)
    }
  }
}

object SmcResultCollector
{
  def apply[EntityType<:SmcEntity]() =
    new SmcResultCollector(newReferenceMap[EntityType])

  // we use an identity hash map since the same expression (e.g.
  // the pronoun "it") may appear in a phrase multiple times with
  // different referents
  def newReferenceMap[EntityType<:SmcEntity]() =
  {
    new IdentityLinkedHashMap[SilReference, Set[EntityType]]
  }

  def modifiableReferenceMap[EntityType<:SmcEntity](
    map : Map[SilReference, Set[EntityType]]) =
  {
    val newMap = newReferenceMap[EntityType]
    newMap ++= map
    newMap
  }
}

class SmcExecutor[EntityType<:SmcEntity]
{
  def executeAction(
    predicate : SilActionPredicate,
    referenceMap : Map[SilReference, Set[EntityType]]) : Option[String] =
  {
    None
  }

  def executeImperative(
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[EntityType]]) : Option[String] =
  {
    None
  }

  def executeInvocation(
    invocation : SmcStateChangeInvocation[EntityType],
    referenceMap : Map[SilReference, Set[EntityType]]) : Option[String] =
  {
    None
  }
}

object SmcResponder
{
  private val logger =
    LoggerFactory.getLogger(
      classOf[SmcResponder[_, _, _, _]])
}

class SmcContextualScorer[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](responder : SmcResponder[EntityType, PropertyType, CosmosType, MindType])
    extends SilWordnetScorer
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  protected def computeBoost(
    sentence : SilSentence,
    resultCollector : ResultCollectorType) : SilPhraseScore =
  {
    SilPhraseScore.numeric(
      3*resultCollector.referenceMap.values.count(_.nonEmpty))
  }

  override def computeGlobalScore(phrase : SilPhrase) : SilPhraseScore =
  {
    val boost = phrase match {
      case sentence : SilSentence => {
        val analyzed = responder.getMind.analyzeSense(sentence)
        val resultCollector = SmcResultCollector[EntityType]
        val result = responder.resolveReferences(analyzed, resultCollector)
        if (result.isFailure) {
          return SilPhraseScore.conBig
        }
        computeBoost(analyzed, resultCollector)
      }
      case _ => SilPhraseScore.neutral
    }
    super.computeGlobalScore(phrase) + boost
  }
}

case class SmcCommunicationContext[EntityType<:SmcEntity](
  speakerEntity : Option[EntityType] = None,
  listenerEntity : Option[EntityType] = None
)

class SmcResponder[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  mind : MindType,
  generalParams : SmcResponseParams = SmcResponseParams(),
  executor : SmcExecutor[EntityType] = new SmcExecutor[EntityType],
  communicationContext : SmcCommunicationContext[EntityType] =
    SmcCommunicationContext()
) extends SmcDebuggable(new SmcDebugger(SmcResponder.logger))
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  type PartialSentenceResponder =
    PartialFunction[SilSentence, (SilSentence, String)]
  type SentenceResponder = (SilSentence) => Option[(SilSentence, String)]

  private def cosmos = mind.getCosmos

  protected val inputRewriter = new SmcInputRewriter(mind)

  private val responseRewriter =
    new SmcResponseRewriter(mind, communicationContext)

  val sentencePrinter = new SilSentencePrinter

  lazy protected val predicateEvaluator = newPredicateEvaluator

  protected def responderMatchers(
    resultCollector : ResultCollectorType
  ) = Stream(
    processPredicateQuery(resultCollector),
    processPredicateSentence(resultCollector),
    processUnsupportedSentence(resultCollector)
  )

  def fail(msg : String) = cosmos.fail(msg)

  def getMind = mind

  protected def newPredicateEvaluator() =
    new SmcPredicateEvaluator[EntityType, PropertyType, CosmosType, MindType](
      mind, sentencePrinter, generalParams.existenceAssumption,
      communicationContext, debugger)

  def newParser(input : String) =
  {
    val context = SprContext(scorer = new SmcContextualScorer(this))
    SprParser(input, context)
  }

  def process(sentence : SilSentence, input : String = "") : String =
  {
    debug("-----------------------------")
    if (!input.isEmpty) {
      debug(s"INPUT TEXT : $input")
    }
    debugger.setContext({
      if (input.nonEmpty) {
        input
      } else {
        sentence.toString
      }
    })
    debug(s"INPUT SENTENCE : $sentence")
    SilPhraseValidator.validatePhrase(sentence)
    val analyzed = mind.analyzeSense(sentence)
    mind.rememberSpeakerSentence(
      SmcConversation.SPEAKER_NAME_PERSON, analyzed, input)
    val resultCollector = SmcResultCollector[EntityType]
    resolveReferencesImpl(analyzed, resultCollector)
    rememberSentenceAnalysis(resultCollector)
    val (responseSentence, responseText) =
      processResolved(analyzed, resultCollector)
    debug(s"RESPONSE TEXT : $responseText")
    debug(s"RESPONSE SENTENCE : $responseSentence")
    if (mind.isConversing) {
      // perhaps we should synthesize referenceMap as we go instead
      // of attempting to reconstruct it here
      val responseResultCollector = SmcResultCollector[EntityType]
      responseResultCollector.swapSpeakerListener = true
      resolveReferences(
        responseSentence, responseResultCollector)
      mind.rememberSpeakerSentence(
        SmcConversation.SPEAKER_NAME_SHLURD,
        responseSentence, responseText, responseResultCollector.referenceMap)
    }
    responseText
  }

  def resolveReferences(
    phrase : SilPhrase,
    resultCollector : ResultCollectorType,
    throwFailures : Boolean = false,
    reify : Boolean = false) : Try[Trilean] =
  {
    debugger.setContext(phrase.toString)
    resolveReferencesImpl(phrase, resultCollector, throwFailures, reify)
  }

  private def resolveReferencesImpl(
    phrase : SilPhrase,
    resultCollector : ResultCollectorType,
    throwFailures : Boolean = false,
    reify : Boolean = false) : Try[Trilean] =
  {
    predicateEvaluator.resolveReferences(
      phrase, resultCollector, throwFailures, reify)
  }

  private def processResolved(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    val normalizedInput = inputRewriter.normalizeInput(sentence)
    if (normalizedInput != sentence) {
      trace(s"REWRITTEN INPUT : $normalizedInput")
    }
    if (normalizedInput.isUninterpretable) {
      val unrecognized = responseRewriter.rewrite(
        responseRewriter.swapPronounsSpeakerListener(
          resultCollector.referenceMap),
        normalizedInput)
      val responder = new SmcUnrecognizedResponder(sentencePrinter)
      wrapResponseText(responder.respond(unrecognized))
    } else {
      processImpl(normalizedInput, resultCollector)
    }
  }

  protected def processImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    responderMatchers(resultCollector).flatMap(_(sentence)).
      headOption.getOrElse {
        debug("UNKNOWN SENTENCE")
        wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
      }
  }

  protected def updateNarrative(
    interval : SmcTimeInterval,
    updatedCosmos : CosmosType,
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[EntityType]])
  {
    // FIXME deal with tense/aspect/mood
    if (mind.hasNarrative) {
      def cosmosMutator(
        eventPredicate : SilPredicate,
        eventCosmos : CosmosType) : CosmosType =
      {
        val sentence = SilPredicateSentence(eventPredicate)
        val eventMind = imagine(eventCosmos)
        val eventResponder = spawn(eventMind)
        val result = eventResponder.process(sentence)
        if (result != sentencePrinter.sb.respondCompliance) {
          throw new CausalityViolationExcn(result)
        }
        freezeCosmos(eventMind.getCosmos)
      }

      val timeline = mind.getNarrative
      timeline.addEntry(new SmcTimelineEntry(
        interval, updatedCosmos, predicate, referenceMap),
        cosmosMutator)
    }
  }

  protected def wrapResponseText(text : String)
      : (SilSentence, String) =
  {
    (SilUnparsedSentence(text), text)
  }

  private def sentenceResponder(f : PartialSentenceResponder)
      : SentenceResponder = f.lift

  protected def rememberSentenceAnalysis(resultCollector : ResultCollectorType)
  {
    mind.rememberSentenceAnalysis(resultCollector.referenceMap)
  }

  private def processStateChange(
    resultCollector : ResultCollectorType,
    predicate : SilStatePredicate) : Try[(SilSentence, String)] =
  {
    debug("STATE CHANGE COMMAND")

    val result = predicateEvaluator.evaluatePredicate(
      predicate, resultCollector)

    result match {
      case Success(Trilean.True) => {
        debug("COUNTERFACTUAL")
        val (normalizedResponse, negateCollection) =
          responseRewriter.normalizeResponse(
            predicate, resultCollector, generalParams)
        assert(!negateCollection)
        val tamResponse = SilTam.indicative
        val responseSentence = SilPredicateSentence(
          normalizedResponse,
          tamResponse)
        Success(tupleN((responseSentence,
          sentencePrinter.sb.respondToCounterfactual(
            sentencePrinter.print(responseSentence)))))
      }
      case Success(_) => {
        assert(resultCollector.states.size == 1)
        val entities =
          resultCollector.lookup(predicate.subject).
            getOrElse(Set.empty).filterNot(entity => {
              resultCollector.entityMap.get(entity).
                getOrElse(Trilean.Unknown).assumeFalse
            })
        val invocation =
          SmcStateChangeInvocation(
            entities,
            resultCollector.states.head)
        debug(s"EXECUTE INVOCATION : $invocation")
        executor.executeInvocation(
          invocation, resultCollector.referenceMap) match
        {
          case Some(result) => {
            Success(wrapResponseText(result))
          }
          case _ => {
            Failure(new UnsupportedOperationException)
          }
        }
      }
      case Failure(e) => Failure(e)
    }
  }

  private def processPredicateQuery(
    resultCollector : ResultCollectorType) = sentenceResponder
  {
    case sentence @ SilPredicateQuery(
      predicate, question, originalAnswerInflection, tam, formality
    ) => {
      trace("PREDICATE QUERY")
      // FIXME deal with positive, modality

      val (rewrittenPredicate, answerInflection) = rewriteQuery(
        predicate, question,
        originalAnswerInflection, resultCollector)
      trace(s"REWRITTEN PREDICATE : $rewrittenPredicate")

      val result = evaluateTamPredicate(
        rewrittenPredicate, tam, resultCollector)
      result match {
        case Success(Trilean.Unknown) => {
          debug("ANSWER UNKNOWN")
          wrapResponseText(sentencePrinter.sb.respondDontKnow)
        }
        case Success(truth) => {
          debug(s"ANSWER : $truth")
          val truthBoolean = truth.assumeFalse
          val extremeLimit = question match {
            case QUESTION_WHICH | QUESTION_WHO |
                QUESTION_WHAT | QUESTION_WHERE => Int.MaxValue
            case QUESTION_HOW_MANY => 0
          }
          val (normalizedResponse, negateCollection) =
            responseRewriter.normalizeResponse(
              rewrittenPredicate, resultCollector,
              generalParams.copy(
                listLimit = extremeLimit),
              Some(question))
          trace(s"NORMALIZED RESPONSE : $normalizedResponse")
          val tamResponse = tam.withMood(MOOD_INDICATIVE).withPolarity(
            truthBoolean || negateCollection).
            withModality(tam.unemphaticModality)
          val responseSentence = SilPredicateSentence(
            normalizedResponse,
            tamResponse)
          val adjustedResponse = generalParams.verbosity match {
            // FIXME:  for RESPONSE_ELLIPSIS, include the verb as well
            // (or the adposition in the case of QUESTION_WHERE)
            case RESPONSE_TERSE | RESPONSE_ELLIPSIS => {
              val answer = (answerInflection, normalizedResponse) match {
                case (
                  INFLECT_ACCUSATIVE,
                  SilActionPredicate(_, _, Some(directObject), _)
                ) => {
                  sentencePrinter.print(
                    directObject,
                    INFLECT_ACCUSATIVE,
                    SilConjoining.NONE)
                }
                case (
                  INFLECT_COMPLEMENT,
                  SilStatePredicate(_, _, state, _)
                ) => {
                  sentencePrinter.print(
                    state, tamResponse, SilConjoining.NONE)
                }
                case (
                  INFLECT_ADPOSITIONED,
                  SilActionPredicate(_, _, _, modifiers)
                ) => {
                  val objRef = modifiers.flatMap(_ match {
                    case SilAdpositionalVerbModifier(
                      _, objRef
                    ) => {
                      Some(objRef)
                    }
                    case _ => None
                  }).head
                  sentencePrinter.print(
                    objRef, INFLECT_ADPOSITIONED, SilConjoining.NONE)
                }
                case (
                  INFLECT_ADPOSITIONED,
                  SilStatePredicate(_, _, SilAdpositionalState(_, objRef), _)
                ) => {
                  // FIXME need a way to automatically find the
                  // wildcard in either the state or the modifiers
                  sentencePrinter.print(
                    objRef, INFLECT_ADPOSITIONED, SilConjoining.NONE)
                }
                case _ => {
                  // FIXME lots of other cases need to be handled
                  sentencePrinter.print(
                    normalizedResponse.getSubject,
                    INFLECT_NOMINATIVE,
                    SilConjoining.NONE)
                }
              }
              sentencePrinter.sb.terminatedSentence(
                answer,
                tamResponse, sentence.formality)
            }
            case _ => {
              sentencePrinter.print(responseSentence)
            }
          }
          (responseSentence,
            sentencePrinter.sb.respondToQuery(adjustedResponse))
        }
        case Failure(e) => {
          debug("ERROR", e)
          wrapResponseText(e.getMessage)
        }
      }
    }
  }

  private def processPredicateSentence(
    resultCollector : ResultCollectorType) = sentenceResponder
  {
    case SilPredicateSentence(predicate, tam, formality) => {
      tam.mood match {
        // FIXME deal with positive, modality
        case MOOD_INTERROGATIVE => {
          trace("PREDICATE QUERY SENTENCE")
          val query = predicate
          val result = evaluateTamPredicate(query, tam, resultCollector)
          result match {
            case Success(Trilean.Unknown) => {
              debug("ANSWER UNKNOWN")
              wrapResponseText(sentencePrinter.sb.respondDontKnow)
            }
            case Success(truth) => {
              debug(s"ANSWER : $truth")
              val truthBoolean = truth.assumeFalse
              val params = query match {
                case rp : SilRelationshipPredicate => {
                  generalParams.copy(listLimit = 0)
                }
                case _ => generalParams
              }
              val (normalizedResponse, negateCollection) =
                responseRewriter.normalizeResponse(
                  query, resultCollector, params)
              trace(s"NORMALIZED RESPONSE : $normalizedResponse")
              val responseTruth = params.verbosity match {
                case RESPONSE_ELLIPSIS => {
                  query match {
                    case SilStatePredicate(_, _, SilExistenceState(_), _) => {
                      truthBoolean || negateCollection
                    }
                    case _ => {
                      truthBoolean
                    }
                  }
                }
                case _ => truthBoolean || negateCollection
              }
              val tamResponse = tam.withMood(MOOD_INDICATIVE).
                withPolarity(responseTruth).withModality(tam.unemphaticModality)
              val responseSentence = SilPredicateSentence(
                normalizedResponse,
                tamResponse)
              val printedSentence = {
                params.verbosity match {
                  case RESPONSE_TERSE => {
                    ""
                  }
                  case RESPONSE_ELLIPSIS => {
                    sentencePrinter.print(
                      responseSentence,
                      true)
                  }
                  case RESPONSE_COMPLETE => {
                    sentencePrinter.print(
                      responseSentence)
                  }
                }
              }
              (responseSentence,
                sentencePrinter.sb.respondToAssumption(
                  ASSUMED_TRUE, truthBoolean, printedSentence, false))
            }
            case Failure(e) => {
              debug("ERROR", e)
              wrapResponseText(e.getMessage)
            }
          }
        }
        case MOOD_INDICATIVE => {
          // FIXME deal with modality
          val positivity = tam.isPositive
          trace(s"POSITIVITY : $positivity")
          val predicateTruth = evaluateTamPredicate(
            predicate, tam, resultCollector)
          val tamResponse = {
            predicateTruth match {
              case Success(Trilean.False) => {
                tam.withMood(MOOD_INDICATIVE).negative
              }
              case _ => {
                // FIXME:  deal with uncertainty
                tam.withMood(MOOD_INDICATIVE)
              }
            }
          }.withModality(tam.unemphaticModality)
          predicateTruth match {
            case Success(Trilean.Unknown) => {
              debug("TRUTH UNKNOWN")
              wrapResponseText(sentencePrinter.sb.respondNoncommittal)
            }
            case Success(truth) => {
              debug(s"KNOWN TRUTH : $truth")
              if (truth.assumeFalse == positivity) {
                val (normalizedResponse, negateCollection) =
                  responseRewriter.normalizeResponse(
                    predicate, resultCollector, generalParams)
                assert(!negateCollection)
                val responseSentence = SilPredicateSentence(
                  normalizedResponse,
                  tamResponse)
                val printedSentence = {
                  generalParams.verbosity match {
                    case RESPONSE_TERSE => {
                      ""
                    }
                    case RESPONSE_ELLIPSIS => {
                      sentencePrinter.print(
                        responseSentence,
                        true)
                    }
                    case RESPONSE_COMPLETE => {
                      sentencePrinter.print(
                        responseSentence)
                    }
                  }
                }
                (responseSentence,
                  sentencePrinter.sb.respondToAssumption(
                    ASSUMED_TRUE, true, printedSentence, true))
              } else {
                // FIXME:  add details on inconsistency, and maybe try
                // to update state?
                wrapResponseText(sentencePrinter.sb.respondNoncommittal)
              }
            }
            case Failure(e) => {
              // FIXME:  try to update state?
              debug("ERROR", e)
              wrapResponseText(e.getMessage)
            }
          }
        }
        case MOOD_IMPERATIVE => {
          val stateChangeAttempt = predicate match {
            case actionPredicate : SilActionPredicate if (
              actionPredicate.directObject match {
                case Some(_ : SilQuotationReference) => false
                case Some(_) => true
                case _ => false
              }
            ) => {
              val actionModifiers = actionPredicate.modifiers
              val (actionWord, modifiers) = actionModifiers match {
                case Seq(SilBasicVerbModifier(word, _)) => {
                  tupleN((word, Seq.empty))
                }
                case _ => {
                  tupleN((actionPredicate.verb.decomposed.last,
                    actionModifiers))
                }
              }
              val pred = SilStatePredicate(
                actionPredicate.directObject.get,
                STATE_PREDEF_BE.toVerb,
                SilPropertyState(actionWord),
                modifiers
              )
              processStateChange(
                resultCollector,
                pred)
            }
            case _ => {
              Failure(new RuntimeException(
                sentencePrinter.sb.respondCannotUnderstand))
            }
          }
          stateChangeAttempt match {
            case Success(result) => result
            case Failure(e) => {
              executor.executeImperative(
                predicate, resultCollector.referenceMap) match
              {
                case Some(imperativeResult) => {
                  wrapResponseText(imperativeResult)
                }
                case _ => {
                  debug("ERROR", e)
                  wrapResponseText(e.getMessage)
                }
              }
            }
          }
        }
      }
    }
  }

  private def processUnsupportedSentence(
    resultCollector : ResultCollectorType) = sentenceResponder
  {
    case SilConjunctiveSentence(determiner, sentences, _) => {
      // FIXME
      trace("CONJUNCTIVE SENTENCE")
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
    }
    case SilAmbiguousSentence(alternatives, _) => {
      debug("AMBIGUOUS SENTENCE")
      // FIXME:  try each in turn and use first
      // that does not result in an error
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
    }
  }

  protected def evaluateTamPredicate(
    predicate : SilPredicate,
    tam : SilTam,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    assert(tam.unemphaticModality == MODAL_NEUTRAL)
    tam.tense match {
      case TENSE_PAST => {
        evaluatePastPredicate(predicate, resultCollector)
      }
      case TENSE_PRESENT => {
        predicateEvaluator.evaluatePredicate(predicate, resultCollector)
      }
      case TENSE_FUTURE => {
        // FIXME i18n
        fail("Future tense not supported yet.")
      }
    }
  }

  private def evaluatePastPredicate(
    predicate : SilPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    // FIXME i18n
    if (!mind.hasNarrative) {
      return fail("No narrative in progress.")
    }
    val isAction = predicate match {
      case _ : SilActionPredicate => true
      case _ => false
    }
    val timeframes = predicate.getModifiers.map(_ match {
      case SilAdpositionalVerbModifier(
        adp @ (SilAdposition.BEFORE | SilAdposition.AFTER),
        objRef
      ) => {
        Some((adp, objRef))
      }
      case _ => None
    })

    val iTimeframe = timeframes.indexWhere(t => !t.isEmpty)
    val (adp, boundPredicate, freePredicate, reducedModifiers) = {
      if (iTimeframe < 0) {
        if (!isAction) {
          return fail("A timeframe must be specified.")
        }
        (SilAdposition.AFTER, predicate, predicate, predicate.getModifiers)
      } else {
        val (adp, objRef) = timeframes(iTimeframe).get
        val reducedModifiers =
          predicate.getModifiers.patch(iTimeframe, Seq.empty, 1)
        val freePredicate =
          predicate.withNewModifiers(reducedModifiers)
        val boundPredicate =
          inputRewriter.bindPredicateWildcard(freePredicate, objRef)
        (adp, boundPredicate, freePredicate, reducedModifiers)
      }
    }

    val timeline = mind.getNarrative
    var matchSeen = (iTimeframe < 0)
    var success = false
    val iter = adp match {
      case SilAdposition.BEFORE => timeline.getEntries.reverseIterator
      case _ => timeline.getEntries.iterator
    }
    iter.foreach(entry => if (isAction) {
      val pastMatchTry = matchActions(
        entry.predicate, boundPredicate,
        entry.referenceMap, resultCollector, false)
      if (pastMatchTry.isFailure) {
        return pastMatchTry.map(_ => Trilean.Unknown)
      }
      val pastMatch = pastMatchTry.get
      if (matchSeen) {
        if (!pastMatch || (iTimeframe < 0)) {
          val bindMatchTry = matchActions(
            entry.predicate, freePredicate,
            entry.referenceMap, resultCollector, true)
          if (bindMatchTry.isFailure) {
            return bindMatchTry.map(_ => Trilean.Unknown)
          }
          if (bindMatchTry.get) {
            if (iTimeframe < 0) {
              success = true
            } else {
              return Success(Trilean.True)
            }
          }
        }
      } else {
        if (pastMatch) {
          matchSeen = true
        }
      }
    } else {
      val pastCollector = SmcResultCollector[EntityType]
      val pastMind = imagine(entry.updatedCosmos)
      val pastPredicateEvaluator = spawn(pastMind).predicateEvaluator
      val pastTruthTry = pastPredicateEvaluator.evaluatePredicate(
        boundPredicate, pastCollector)
      val pastTruth = pastTruthTry.getOrElse(return pastTruthTry).assumeFalse
      if (matchSeen) {
        if (!pastTruth) {
          // now re-evaluate the original predicate at that point in time
          return pastPredicateEvaluator.evaluatePredicate(
            freePredicate, resultCollector)
        }
      } else {
        if (pastTruth) {
          matchSeen = true
        }
      }
    })
    if (success) {
      Success(Trilean.True)
    } else {
      fail("No such timeframe and/or event in narrative.")
    }
  }

  protected def matchActions(
    eventActionPredicate : SilPredicate,
    queryActionPredicate : SilPredicate,
    eventReferenceMap : Map[SilReference, Set[EntityType]],
    resultCollector : ResultCollectorType,
    applyBindings : Boolean) : Try[Boolean] =
  Success({
    val queryReferenceMap = resultCollector.referenceMap
    (eventActionPredicate, queryActionPredicate) match {
      case (
        SilActionPredicate(eventSubject, eventAction,
          eventDirectObject, eventModifiers),
        SilActionPredicate(querySubject, queryAction,
          queryDirectObject, queryModifiers)
      ) if (mind.isEquivalentVerb(eventAction, queryAction)) => {
        def isVariable(phrase : SilPhrase) = {
          inputRewriter.containsWildcard(phrase)
        }
        val bindings = new mutable.ArrayBuffer[(SilReference, SilReference)]
        def bindVariable(queryRef : SilReference, eventRef : SilReference) {
          bindings += ((queryRef, eventRef))
        }
        def subjectMatch = {
          if (isVariable(querySubject)) {
            bindVariable(querySubject, eventSubject)
            true
          } else {
            queryReferenceMap(querySubject) == eventReferenceMap(eventSubject)
          }
        }
        def directObjectMatch = {
          queryDirectObject.forall(qdo => {
            if (isVariable(qdo)) {
              eventDirectObject.foreach(edo => bindVariable(qdo, edo))
              !eventDirectObject.isEmpty
            } else {
              eventDirectObject.map(edo => {
                queryReferenceMap(qdo) == eventReferenceMap(edo)
              }).getOrElse(false)
            }
          })
        }
        def modifiersMatch = {
          val (variableModifiers, reducedModifiers) =
            queryModifiers.partition(isVariable)
          // FIXME support other kinds of variable patterns
          val variableMatched = variableModifiers.forall(_ match {
            case SilAdpositionalVerbModifier(queryAdposition, queryObj) => {
              eventModifiers.exists(_ match {
                case SilAdpositionalVerbModifier(
                  eventAdposition, eventObj
                ) if (eventAdposition == queryAdposition) => {
                  bindVariable(queryObj, eventObj)
                  true
                }
                case _ => false
              })
            }
            case _ => false
          })
          val qmr = reducedModifiers.map(modifier =>
            replaceReferencesWithEntities(modifier, queryReferenceMap)).toSet
          val emr = eventModifiers.map(modifier =>
            replaceReferencesWithEntities(modifier, eventReferenceMap)).toSet
          variableMatched && qmr.subsetOf(emr)
        }
        if (subjectMatch && directObjectMatch && modifiersMatch) {
          if (applyBindings) {
            bindings.foreach({
              case (queryRef, eventRef) => {
                eventReferenceMap.get(eventRef).foreach(entities => {
                  queryReferenceMap.put(
                    queryRef,
                    queryReferenceMap.get(queryRef).
                      getOrElse(Set.empty) ++ entities)
                  entities.foreach(entity =>
                    resultCollector.entityMap.put(entity, Trilean.True))
                })
              }
            })
          }
          true
        } else {
          false
        }
      }
      case _ => false
    }
  })

  protected def imagine(alternateCosmos : CosmosType)
      : MindType =
  {
    throw new UnsupportedOperationException("I lack imagination")
  }

  protected def freezeCosmos(mutableCosmos : CosmosType) : CosmosType =
  {
    mutableCosmos
  }

  protected def spawn(subMind : MindType) =
  {
    new SmcResponder[EntityType, PropertyType, CosmosType, MindType](
      subMind, generalParams, executor, communicationContext)
  }

  protected def rewriteQuery(
    predicate : SilPredicate,
    question : SilQuestion,
    answerInflection : SilInflection,
    resultCollector : ResultCollectorType)
      : (SilPredicate, SilInflection) =
  {
    val queryRewriter = newQueryRewriter(question, answerInflection)
    val rewritten = queryRewriter.rewrite(
      queryRewriter.rewritePredicate, predicate)
    (rewritten, answerInflection)
  }

  protected def newQueryRewriter(
    question : SilQuestion,
    answerInflection : SilInflection) =
  {
    new SmcQueryRewriter(question, answerInflection)
  }

  private def replaceReferencesWithEntities(
    phrase : SilPhrase,
    referenceMap : Map[SilReference, Set[EntityType]]) : SilPhrase =
  {
    val rewriter = new SilPhraseRewriter
    def replaceReferences = rewriter.replacementMatcher(
      "replaceReferencesWithEntities", {
        case ref : SilReference => {
          referenceMap.get(ref) match {
            case Some(entities) => {
              SilConjunctiveReference(
                DETERMINER_ALL,
                entities.map(_.getUniqueIdentifier).toSeq.sorted.map(id =>
                  SilMappedReference(id, DETERMINER_UNSPECIFIED))
              )
            }
            case _ => ref
          }
        }
      }
    )
    rewriter.rewrite(
      replaceReferences, phrase, SilRewriteOptions(topDown = true))
  }
}
