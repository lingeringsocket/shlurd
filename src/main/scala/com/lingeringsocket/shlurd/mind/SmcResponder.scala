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
import com.lingeringsocket.shlurd.nlang._

import scala.util._

import spire.math._

import scala.collection._

import org.slf4j._

case class SmcStateChangeInvocation[EntityType<:SmcEntity](
  entities : Set[EntityType],
  state : SilWord)
{
}

sealed trait SmcResponseVerbosity
case object RESPONSE_TERSE extends SmcResponseVerbosity
case object RESPONSE_ELLIPSIS extends SmcResponseVerbosity
case object RESPONSE_COMPLETE extends SmcResponseVerbosity

sealed trait SmcExistenceAssumption
case object EXISTENCE_ASSUME_NOTHING extends SmcExistenceAssumption
case object EXISTENCE_ASSUME_UNKNOWN extends SmcExistenceAssumption

case class SmcResponseParams(
  listLimit : Int = 3,
  thirdPersonPronouns : Boolean = true,
  verbosity : SmcResponseVerbosity = RESPONSE_COMPLETE,
  existenceAssumption : SmcExistenceAssumption = EXISTENCE_ASSUME_NOTHING,
  reportExceptionCodes : Boolean = false,
  throwRejectedBeliefs : Boolean = false,
  rememberConversation : Boolean = true
)
{
  def neverSummarize = (listLimit == Int.MaxValue)

  def alwaysSummarize = (listLimit == 0)
}

class SmcExecutor[EntityType<:SmcEntity]
{
  def executeAction(
    predicate : SilActionPredicate,
    refMap : SmcRefMap[EntityType]) : Option[String] =
  {
    None
  }

  def executeImperative(
    predicate : SilPredicate,
    refMap : SmcRefMap[EntityType]) : Option[String] =
  {
    None
  }

  def executeInvocation(
    invocation : SmcStateChangeInvocation[EntityType],
    refMap : SmcRefMap[EntityType]) : Option[String] =
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
](
  tongue : SprTongue,
  responder : SmcResponder[EntityType, PropertyType, CosmosType, MindType])
    extends SilWordnetScorer(tongue)
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  protected def computeBoost(
    sentence : SilSentence,
    resultCollector : ResultCollectorType) : SilPhraseScore =
  {
    val refSet = SilUtils.collectReferences(sentence).toSet
    val score = 3*refSet.toSeq.flatMap(resultCollector.lookup).count(_.nonEmpty)
    SilPhraseScore.numeric(score)
  }

  override def computeGlobalScore(phrase : SilPhrase) : SilPhraseScore =
  {
    val boost = phrase match {
      case sentence : SilSentence => {
        if (sentence.isUninterpretable) {
          return SilPhraseScore.conBig
        }
        val annotator = responder.newAnnotator
        val analyzed = responder.getMind.analyzeSense(
          annotator, sentence)
        val resultCollector = responder.newResultCollector(annotator)
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
{
  def flip = SmcCommunicationContext(listenerEntity, speakerEntity)
}

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
  type ScopeType = SmcScope[EntityType, PropertyType, CosmosType, MindType]
  type MindScopeType =
    SmcMindScope[EntityType, PropertyType, CosmosType, MindType]
  type PartialSentenceResponder =
    PartialFunction[SilSentence, (SilSentence, String)]
  type SentenceResponder = (SilSentence) => Option[(SilSentence, String)]
  type AnnotatorType = SmcAnnotator[EntityType, SmcRefNote[EntityType]]

  private def cosmos = mind.getCosmos

  protected[mind] def newInputRewriter(
    annotator : AnnotatorType) = new SmcInputRewriter(mind, annotator)

  private def newResponseRewriter(annotator : AnnotatorType) =
    new SmcResponseRewriter(
      mind, communicationContext.flip, annotator)

  val sentencePrinter = mind.getTongue.newSentencePrinter(mind)

  val mindScope = new MindScopeType(mind, sentencePrinter)

  private implicit val tongue = mind.getTongue

  protected def responderMatchers(
    resultCollector : ResultCollectorType
  ) = Stream(
    processPredicateQuery(resultCollector),
    processPredicateSentence(resultCollector),
    processUnsupportedSentence(resultCollector)
  )

  def getMind = mind

  def newAnnotator() : AnnotatorType =
  {
    SmcAnnotator[EntityType]()
  }

  def newResultCollector(
    annotator : AnnotatorType
  ) : ResultCollectorType =
  {
    SmcResultCollector[EntityType](annotator)
  }

  protected def newPredicateEvaluator(
    annotator : AnnotatorType,
    scope : ScopeType = mindScope) =
    new SmcPredicateEvaluator[EntityType, PropertyType, CosmosType, MindType](
      annotator, scope, generalParams.existenceAssumption,
      communicationContext, debugger)

  def newParser(input : String) =
  {
    val context = SprContext(
      SnlUtils.defaultWordLabeler,
      scorer = new SmcContextualScorer(mind.getTongue, this),
      annotator = newAnnotator,
      genderAnalyzer = mind)
    SprParser(input, context)
  }

  def process(parseResult : SprParseResult, input : String = "") : String =
  {
    val inputAnnotator = newAnnotator
    val sentence = inputAnnotator.copy(
      parseResult.sentence, SilPhraseCopyOptions(preserveNotes = true))
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
    val analyzed = mind.analyzeSense(inputAnnotator, sentence)
    if (generalParams.rememberConversation) {
      mind.rememberSpeakerSentence(
        SmcConversation.SPEAKER_NAME_PERSON, analyzed, input)
    }
    val normalizedInput = newInputRewriter(inputAnnotator).
      normalizeInput(analyzed)
    if (normalizedInput != analyzed) {
      trace(s"REWRITTEN INPUT : $normalizedInput")
    }
    val inputResultCollector = newResultCollector(inputAnnotator)
    resolveReferencesImpl(normalizedInput, inputResultCollector)
    rememberSentenceAnalysis(inputResultCollector)
    val responseAnnotator = newAnnotator
    SilAnnotator.sanityCheck(inputAnnotator, normalizedInput)
    val copiedInput = responseAnnotator.copy(
      normalizedInput, SilPhraseCopyOptions(preserveNotes = true))
    val responseResultCollector = newResultCollector(responseAnnotator)
    val (responseSentence, responseText) =
      processResolved(copiedInput, responseResultCollector)
    debug(s"RESPONSE TEXT : $responseText")
    debug(s"RESPONSE SENTENCE : $responseSentence")
    SilAnnotator.sanityCheck(responseAnnotator, responseSentence)
    if (mind.isConversing) {
      // perhaps we should synthesize refMap as we go instead
      // of attempting to reconstruct it here
      val responseReparseCollector = newResultCollector(responseAnnotator)
      responseReparseCollector.swapSpeakerListener = true
      resolveReferences(
        responseSentence, responseReparseCollector)
      if (generalParams.rememberConversation) {
        mind.rememberSpeakerSentence(
          SmcConversation.SPEAKER_NAME_SHLURD,
          responseSentence, responseText, responseReparseCollector.refMap)
      }
    }
    responseText
  }

  def resolveReferences(
    phrase : SilPhrase,
    resultCollector : ResultCollectorType,
    throwFailures : Boolean = false,
    reify : Boolean = false,
    scope : ScopeType = mindScope
  ) : Try[Trilean] =
  {
    debugger.setContext(phrase.toString)
    resolveReferencesImpl(phrase, resultCollector, throwFailures, reify, scope)
  }

  protected def resolveReferencesImpl(
    phrase : SilPhrase,
    resultCollector : ResultCollectorType,
    throwFailures : Boolean = false,
    reify : Boolean = false,
    scope : ScopeType = mindScope) : Try[Trilean] =
  {
    newPredicateEvaluator(resultCollector.annotator, scope).resolveReferences(
      phrase, resultCollector, throwFailures, reify)
  }

  private def processResolved(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    if (sentence.isUninterpretable) {
      val responseRewriter = newResponseRewriter(resultCollector.annotator)
      val unrecognized = responseRewriter.rewrite(
        responseRewriter.swapSpeakerListener(
          resultCollector.refMap),
        sentence)
      val responder = new SmcUnrecognizedResponder(sentencePrinter)
      wrapResponseText(
        ShlurdExceptionCode.FailedParse,
        responder.respond(unrecognized))
    } else {
      processImpl(sentence, resultCollector)
    }
  }

  protected def processImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    responderMatchers(resultCollector).flatMap(_(sentence)).
      headOption.getOrElse {
        debug("UNKNOWN SENTENCE")
        wrapResponseText(
          ShlurdExceptionCode.FailedParse,
          sentencePrinter.sb.respondCannotUnderstand)
      }
  }

  protected def updateNarrative(
    interval : SmcTimeInterval,
    updatedCosmos : CosmosType,
    predicate : SilPredicate,
    resultCollector : ResultCollectorType)
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
        val result = eventResponder.process(
          SprParseResult(sentence, resultCollector.annotator))
        if (result != sentencePrinter.sb.respondCompliance) {
          throw ShlurdException(
            ShlurdExceptionCode.CausalityViolation, result)
        }
        freezeCosmos(eventMind.getCosmos)
      }

      val timeline = mind.getNarrative
      timeline.addEntry(new SmcTimelineEntry(
        interval, updatedCosmos, predicate, resultCollector.refMap),
        cosmosMutator)
    }
  }

  protected def wrapResponseText(ex : Throwable)
      : (SilSentence, String) =
  {
    debug("ERROR", ex)
    ex match {
      case ShlurdException(code, msg) => {
        wrapResponseText(code, msg)
      }
      case _ => {
        warn("NON-CODED THROWABLE:  " + ex)
        wrapResponseText(ex.getMessage)
      }
    }
  }

  protected def wrapResponseText(text : String)
      : (SilSentence, String) =
  {
    (SilUnparsedSentence(text), text)
  }

  protected def wrapResponseText(code : ShlurdExceptionCode, text : String)
      : (SilSentence, String) =
  {
    if (generalParams.reportExceptionCodes) {
      val embellished = s"$text\n\nFor more information see ${code.getUrl}"
      (SilUnparsedSentence(embellished), embellished)
    } else {
      wrapResponseText(text)
    }
  }

  private def sentenceResponder(f : PartialSentenceResponder)
      : SentenceResponder = f.lift

  protected def rememberSentenceAnalysis(resultCollector : ResultCollectorType)
  {
    if (generalParams.rememberConversation) {
      mind.rememberSentenceAnalysis(resultCollector.refMap)
    }
  }

  private def processStateChange(
    resultCollector : ResultCollectorType,
    predicate : SilStatePredicate) : Try[(SilSentence, String)] =
  {
    debug("STATE CHANGE COMMAND")

    val result = newPredicateEvaluator(
      resultCollector.annotator
    ).evaluatePredicate(
      predicate, resultCollector
    )

    result match {
      case Success(Trilean.True) => {
        debug("COUNTERFACTUAL")
        val responseRewriter = newResponseRewriter(resultCollector.annotator)
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
              resultCollector.neutralizedEntityMap.get(entity).
                getOrElse(Trilean.Unknown).assumeFalse
            })
        val invocation =
          SmcStateChangeInvocation(
            entities,
            resultCollector.states.head)
        debug(s"EXECUTE INVOCATION : $invocation")
        executor.executeInvocation(
          invocation, resultCollector.refMap) match
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
          val responseRewriter = newResponseRewriter(resultCollector.annotator)
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
          wrapResponseText(e)
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
              val responseRewriter = newResponseRewriter(
                resultCollector.annotator)
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
              wrapResponseText(e)
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
                val responseRewriter = newResponseRewriter(
                  resultCollector.annotator)
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
              wrapResponseText(e)
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
                case Seq(SilBasicVerbModifier(word)) => {
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
              cosmos.fail(
                ShlurdExceptionCode.FailedParse,
                sentencePrinter.sb.respondCannotUnderstand)
            }
          }
          stateChangeAttempt match {
            case Success(result) => result
            case Failure(e) => {
              e match {
                case ShlurdException(
                  ShlurdExceptionCode.UnknownForm
                    | ShlurdExceptionCode.NotUnique
                    | ShlurdExceptionCode.NonExistent
                    | ShlurdExceptionCode.MisqualifiedNoun
                    | ShlurdExceptionCode.UnresolvedPronoun
                    | ShlurdExceptionCode.AmbiguousPronoun,
                  _
                ) => {
                  wrapResponseText(e)
                }
                case _ => {
                  executor.executeImperative(
                    predicate, resultCollector.refMap) match
                  {
                    case Some(imperativeResult) => {
                      wrapResponseText(imperativeResult)
                    }
                    case _ => {
                      wrapResponseText(e)
                    }
                  }
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
      wrapResponseText(
        ShlurdExceptionCode.FailedParse,
        sentencePrinter.sb.respondCannotUnderstand)
    }
    case SilAmbiguousSentence(alternatives, _) => {
      debug("AMBIGUOUS SENTENCE")
      // FIXME:  try each in turn and use first
      // that does not result in an error
      wrapResponseText(
        ShlurdExceptionCode.FailedParse,
        sentencePrinter.sb.respondCannotUnderstand)
    }
  }

  protected def evaluateTamPredicate(
    predicate : SilPredicate,
    tam : SilTam,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    if (tam.unemphaticModality != MODAL_NEUTRAL) {
      cosmos.fail(
        ShlurdExceptionCode.NotYetImplemented,
        "Modals not supported yet.")
    } else {
      tam.tense match {
        case TENSE_PAST => {
          evaluatePastPredicate(predicate, resultCollector)
        }
        case TENSE_PRESENT => {
          newPredicateEvaluator(
            resultCollector.annotator
          ).evaluatePredicate(predicate, resultCollector)
        }
        case TENSE_FUTURE => {
          cosmos.fail(
            ShlurdExceptionCode.NotYetImplemented,
            "Future tense not supported yet.")
        }
      }
    }
  }

  private def evaluatePastPredicate(
    predicate : SilPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    if (!mind.hasNarrative) {
      return cosmos.fail(
        ShlurdExceptionCode.NotYetImplemented,
        "No narrative in progress.")
    }
    val isAction = predicate match {
      case _ : SilActionPredicate => true
      case _ => false
    }
    val timeframes = predicate.getModifiers.map(_ match {
      case SilAdpositionalVerbModifier(
        adp @ (SilMagicAdposition(MW_BEFORE | MW_AFTER)),
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
          return cosmos.fail(
            ShlurdExceptionCode.NotYetImplemented,
            "A timeframe must be specified.")
        }
        (SilAdposition(MW_AFTER), predicate, predicate, predicate.getModifiers)
      } else {
        val (adp, objRef) = timeframes(iTimeframe).get
        val reducedModifiers =
          predicate.getModifiers.patch(iTimeframe, Seq.empty, 1)
        val freePredicate =
          predicate.withNewModifiers(reducedModifiers)
        val bp =
          newInputRewriter(
            resultCollector.annotator
          ).bindPredicateWildcard(freePredicate, objRef)
        (adp, bp, freePredicate, reducedModifiers)
      }
    }

    val timeline = mind.getNarrative
    var matchSeen = (iTimeframe < 0)
    var success = false
    val iter = adp match {
      case SilMagicAdposition(MW_BEFORE) => timeline.getEntries.reverseIterator
      case _ => timeline.getEntries.iterator
    }

    iter.foreach(entry => if (isAction) {
      val entryPredicate = resultCollector.annotator.copy(
        entry.predicate, SilPhraseCopyOptions(preserveNotes = true))
      val entryRefMap = SmcResultCollector.snapshotRefMap(
        resultCollector.refMap)

      val pastMatchTry = matchActions(
        entryPredicate,
        boundPredicate,
        entryRefMap, resultCollector, false)
      if (pastMatchTry.isFailure) {
        return pastMatchTry.map(_ => Trilean.Unknown)
      }
      val pastMatch = pastMatchTry.get
      if (matchSeen) {
        if (!pastMatch || (iTimeframe < 0)) {
          val bindMatchTry = matchActions(
            entryPredicate, freePredicate,
            entryRefMap, resultCollector, true)
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
      val pastCollector = newResultCollector(resultCollector.annotator)
      val pastMind = imagine(entry.updatedCosmos)
      val pastPredicateEvaluator = spawn(pastMind).newPredicateEvaluator(
        resultCollector.annotator
      )
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
      cosmos.fail(
        ShlurdExceptionCode.NotYetImplemented,
        "No such timeframe and/or event in narrative.")
    }
  }

  protected def matchActions(
    eventActionPredicate : SilPredicate,
    queryActionPredicate : SilPredicate,
    eventRefMap : SmcRefMap[EntityType],
    resultCollector : ResultCollectorType,
    applyBindings : Boolean) : Try[Boolean] =
  Success({
    val queryRefMap = resultCollector.refMap
    (eventActionPredicate, queryActionPredicate) match {
      case (
        SilActionPredicate(eventSubject, eventAction,
          eventDirectObject, eventModifiers),
        SilActionPredicate(querySubject, queryAction,
          queryDirectObject, queryModifiers)
      ) if (mind.isEquivalentVerb(eventAction, queryAction)) => {
        def isVariable(phrase : SilPhrase) = {
          SmcPhraseQuerier.containsWildcard(phrase)
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
            queryRefMap(querySubject) == eventRefMap(eventSubject)
          }
        }
        def directObjectMatch = {
          queryDirectObject.forall(qdo => {
            if (isVariable(qdo)) {
              eventDirectObject.foreach(edo => bindVariable(qdo, edo))
              !eventDirectObject.isEmpty
            } else {
              eventDirectObject.map(edo => {
                queryRefMap(qdo) == eventRefMap(edo)
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
            replaceReferencesWithEntities(
              resultCollector.annotator, modifier, queryRefMap)).toSet
          val emr = eventModifiers.map(modifier =>
            replaceReferencesWithEntities(
              resultCollector.annotator, modifier, eventRefMap)).toSet
          variableMatched && qmr.subsetOf(emr)
        }
        if (subjectMatch && directObjectMatch && modifiersMatch) {
          if (applyBindings) {
            bindings.foreach({
              case (queryRef, eventRef) => {
                eventRefMap.get(eventRef).foreach(entities => {
                  queryRefMap.put(
                    queryRef,
                    queryRefMap.get(queryRef).
                      getOrElse(Set.empty) ++ entities)
                  entities.foreach(entity =>
                    resultCollector.saveEntityResult(entity, Trilean.True))
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
    val queryRewriter = newQueryRewriter(
      resultCollector.annotator, question, answerInflection)
    val rewritten = queryRewriter.rewrite(
      queryRewriter.rewritePredicate, predicate)
    (rewritten, answerInflection)
  }

  protected def newQueryRewriter(
    annotator : AnnotatorType,
    question : SilQuestion,
    answerInflection : SilInflection) =
  {
    new SmcQueryRewriter(tongue, annotator, question, answerInflection)
  }

  private def replaceReferencesWithEntities(
    annotator : AnnotatorType,
    phrase : SilPhrase,
    refMap : SmcRefMap[EntityType]) : SilPhrase =
  {
    val rewriter = new SilPhraseRewriter(annotator)
    def replaceReferences = rewriter.replacementMatcher(
      "replaceReferencesWithEntities", {
        case ref : SilReference => {
          refMap.get(ref) match {
            case Some(entities) => {
              annotator.conjunctiveRef(
                DETERMINER_ALL,
                entities.map(_.getUniqueIdentifier).toSeq.sorted.map(id =>
                  annotator.mappedRef(id, DETERMINER_ABSENT))
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
