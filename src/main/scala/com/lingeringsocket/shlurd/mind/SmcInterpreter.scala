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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import scala.util._

import spire.math._

import scala.collection._

case class SmcStateChangeInvocation[EntityType<:SilEntity](
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

case class SmcResponseParams(
  listLimit : Int = 3,
  thirdPersonPronouns : Boolean = true,
  verbosity : SmcResponseVerbosity = RESPONSE_COMPLETE
)
{
  def neverSummarize = (listLimit == Int.MaxValue)

  def alwaysSummarize = (listLimit == 0)
}

class SmcResultCollector[EntityType<:SilEntity](
  val referenceMap : mutable.Map[SilReference, Set[EntityType]])
{
  val entityMap = new mutable.LinkedHashMap[EntityType, Trilean]
  val states = new mutable.LinkedHashSet[SilWord]
  var isCategorization = false

  def spawn() = new SmcResultCollector[EntityType](referenceMap)
}

object SmcResultCollector
{
  def apply[EntityType<:SilEntity]() =
    new SmcResultCollector(
      new mutable.LinkedHashMap[SilReference, Set[EntityType]])
}

class SmcExecutor[EntityType<:SilEntity]
{
  def executeInvocation(
    invocation : SmcStateChangeInvocation[EntityType])
  {
    throw new UnsupportedOperationException
  }
}

class SmcInterpreter[
  EntityType<:SilEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  mind : MindType,
  generalParams : SmcResponseParams = SmcResponseParams(),
  executor : SmcExecutor[EntityType] = new SmcExecutor[EntityType])
    extends SmcDebuggable(SmcDebugger.maybe)
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  type SentenceInterpreter = PartialFunction[SilSentence, (SilSentence, String)]

  private def cosmos = mind.getCosmos

  private val inputRewriter = new SmcInputRewriter(mind)

  private val responseRewriter = new SmcResponseRewriter(mind)

  protected val sentencePrinter = new SilSentencePrinter

  lazy protected val predicateEvaluator = newPredicateEvaluator

  private def interpreterMatchers(
    resultCollector : ResultCollectorType
  ) = Seq(
    interpretStateChangeCommand(resultCollector),
    interpretPredicateQuery(resultCollector),
    interpretPredicateSentence(resultCollector),
    interpretUnsupportedSentence(resultCollector)
  ).reduceLeft(_ orElse _)

  def fail(msg : String) = cosmos.fail(msg)

  protected def newPredicateEvaluator() =
    new SmcPredicateEvaluator[EntityType, PropertyType, CosmosType, MindType](
      mind, sentencePrinter, debugger)

  def interpret(sentence : SilSentence, input : String = "") : String =
  {
    if (!input.isEmpty) {
      debug(s"INTERPRETER INPUT TEXT : $input")
    }
    debug(s"INTERPRETER INPUT SENTENCE : $sentence")
    mind.rememberSpeakerSentence(
      SmcConversation.SPEAKER_NAME_PERSON, sentence, input)
    SilPhraseValidator.validatePhrase(sentence)
    val resultCollector = SmcResultCollector[EntityType]
    val (responseSentence, responseText) =
      interpretImpl(sentence, resultCollector)
    debug(s"INTERPRETER RESPONSE TEXT : $responseText")
    debug(s"INTERPRETER RESPONSE SENTENCE : $responseSentence")
    if (mind.isConversing) {
      // perhaps we should synthesize referenceMap as we go instead
      // of attempting to reconstruct it here
      val responseResultCollector = SmcResultCollector[EntityType]
      val resolver = new SmcReferenceResolver(
        mind.getCosmos, new SilSentencePrinter, responseResultCollector,
        SmcResolutionOptions(
          failOnUnknown = false,
          resolveConjunctions = true,
          resolveUniqueDeterminers = true))
      resolver.resolve(responseSentence)
      mind.rememberSpeakerSentence(
        SmcConversation.SPEAKER_NAME_SHLURD,
        responseSentence, responseText, responseResultCollector.referenceMap)
    }
    responseText
  }

  protected def interpretImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    val normalizedInput = inputRewriter.normalizeInput(sentence)
    if (normalizedInput != sentence) {
      debug(s"REWRITTEN INPUT : $normalizedInput")
    }
    if (normalizedInput.isUninterpretable) {
      val unrecognized = responseRewriter.rewrite(
        responseRewriter.swapPronounsSpeakerListener, normalizedInput)
      val responder = new SmcUnrecognizedResponder(sentencePrinter)
      return wrapResponseText(responder.respond(unrecognized))
    }
    interpreterMatchers(resultCollector).applyOrElse(
      normalizedInput,
      { s : SilSentence =>
        debug("UNKNOWN SENTENCE")
        wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
      }
    )
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
        val eventInterpreter = spawn(eventMind)
        val result = eventInterpreter.interpret(sentence)
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

  private def sentenceInterpreter(f : SentenceInterpreter)
      : SentenceInterpreter = f

  private def interpretStateChangeCommand(
    resultCollector : ResultCollectorType) = sentenceInterpreter
  {
    case SilStateChangeCommand(predicate, _, formality) => {
      debug("STATE CHANGE COMMAND")

      val result = predicateEvaluator.evaluatePredicate(
        predicate, resultCollector)
      mind.rememberSentenceAnalysis(resultCollector.referenceMap)
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
          (responseSentence,
            sentencePrinter.sb.respondToCounterfactual(
              sentencePrinter.print(responseSentence)))
        }
        case Success(_) => {
          assert(resultCollector.states.size == 1)
          val invocation =
            SmcStateChangeInvocation(
              resultCollector.entityMap.filterNot(
                _._2.assumeFalse).keySet,
              resultCollector.states.head)
          debug(s"EXECUTE INVOCATION : $invocation")
          executor.executeInvocation(invocation)
          wrapResponseText(sentencePrinter.sb.respondCompliance)
        }
        case Failure(e) => {
          debug("ERROR", e)
          wrapResponseText(e.getMessage)
        }
      }
    }
  }

  private def interpretPredicateQuery(
    resultCollector : ResultCollectorType) = sentenceInterpreter
  {
    case sentence @ SilPredicateQuery(
      predicate, question, originalAnswerInflection, tam, formality
    ) => {
      debug("PREDICATE QUERY")
      // FIXME deal with positive, modality

      val (rewrittenPredicate, answerInflection) = rewriteQuery(
        predicate, question,
        originalAnswerInflection, resultCollector)
      debug(s"REWRITTEN PREDICATE : $rewrittenPredicate")

      val result = evaluateTamPredicate(
        rewrittenPredicate, tam, resultCollector)
      mind.rememberSentenceAnalysis(resultCollector.referenceMap)
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
          debug(s"NORMALIZED RESPONSE : $normalizedResponse")
          val tamResponse = tam.withMood(MOOD_INDICATIVE).withPolarity(
            truthBoolean || negateCollection)
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
                  SilStatePredicate(_, state, _)
                ) => {
                  sentencePrinter.print(
                    state, tamResponse, SilConjoining.NONE)
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

  private def interpretPredicateSentence(
    resultCollector : ResultCollectorType) = sentenceInterpreter
  {
    case SilPredicateSentence(predicate, tam, formality) => {
      tam.mood match {
        // FIXME deal with positive, modality
        case MOOD_INTERROGATIVE => {
          debug("PREDICATE QUERY SENTENCE")
          val query = predicate
          val result = evaluateTamPredicate(query, tam, resultCollector)
          mind.rememberSentenceAnalysis(resultCollector.referenceMap)
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
              debug(s"NORMALIZED RESPONSE : $normalizedResponse")
              val responseTruth = params.verbosity match {
                case RESPONSE_ELLIPSIS => truthBoolean
                case _ => truthBoolean || negateCollection
              }
              val responseSentence = SilPredicateSentence(
                normalizedResponse,
                tam.withMood(MOOD_INDICATIVE).withPolarity(responseTruth))
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
          debug(s"POSITIVITY : $positivity")
          val predicateTruth = evaluateTamPredicate(
            predicate, tam, resultCollector)
          mind.rememberSentenceAnalysis(resultCollector.referenceMap)
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
          }
          predicateTruth match {
            case Success(Trilean.Unknown) => {
              debug("TRUTH UNKNOWN")
              wrapResponseText("Oh, really?  Thanks for letting me know.")
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
                wrapResponseText("Oh, really?")
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
          debug(s"UNEXPECTED MOOD : $tam")
          wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
        }
      }
    }
  }

  private def interpretUnsupportedSentence(
    resultCollector : ResultCollectorType) = sentenceInterpreter
  {
    case SilConjunctiveSentence(determiner, sentences, _) => {
      // FIXME
      debug("CONJUNCTIVE SENTENCE")
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
    }
    case SilAmbiguousSentence(alternatives, _) => {
      debug("AMBIGUOUS SENTENCE")
      // FIXME:  try each in turn and use first
      // that does not result in an error
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand)
    }
  }

  private def evaluateTamPredicate(
    predicate : SilPredicate,
    tam : SilTam,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    assert(tam.modality == MODAL_NEUTRAL)
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
    if (iTimeframe < 0) {
      return fail("A timeframe must be specified.")
    }

    val reducedModifiers = predicate.getModifiers.patch(
      iTimeframe, Seq.empty, 1)
    val (adp, objRef) = timeframes(iTimeframe).get
    val timeline = mind.getNarrative
    var trueSeen = false
    val freePredicate =
      predicate.withNewModifiers(reducedModifiers)
    val boundPredicate =
      inputRewriter.bindPredicateWildcard(freePredicate, objRef)
    val iter = adp match {
      case SilAdposition.BEFORE => timeline.getEntries.reverseIterator
      case _ => timeline.getEntries.iterator
    }
    iter.foreach(entry => {
      val pastCollector = SmcResultCollector[EntityType]
      val pastMind = imagine(entry.updatedCosmos)
      val pastPredicateEvaluator = spawn(pastMind).predicateEvaluator
      val pastTruthTry = pastPredicateEvaluator.evaluatePredicate(
        boundPredicate, pastCollector)
      val pastTruth =
        pastTruthTry.getOrElse(return pastTruthTry).assumeFalse
      if (trueSeen) {
        if (!pastTruth) {
          // now re-evaluate the original predicate at that point in time
          return pastPredicateEvaluator.evaluatePredicate(
            freePredicate, resultCollector)
        }
      } else {
        if (pastTruth) {
          trueSeen = true
        }
      }
    })
    fail("No such timeframe in narrative.")
  }

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
    new SmcInterpreter[EntityType, PropertyType, CosmosType, MindType](
      subMind, generalParams, executor)
  }

  protected def rewriteQuery(
    predicate : SilPredicate,
    question : SilQuestion,
    answerInflection : SilInflection,
    resultCollector : ResultCollectorType)
      : (SilPredicate, SilInflection) =
  {
    val queryRewriter = new SmcQueryRewriter(question)
    val rewritten = queryRewriter.rewrite(
      queryRewriter.rewritePredicate, predicate)
    (rewritten, answerInflection)
  }
}
