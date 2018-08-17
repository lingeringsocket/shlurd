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

import org.slf4j._

import SprEnglishLemmas._

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
    throw new UnsupportedOperationException()
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
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  type PredicateEvaluator = (EntityType, SilReference) => Try[Trilean]

  type SentenceInterpreter = PartialFunction[SilSentence, (SilSentence, String)]

  private def cosmos = mind.getCosmos

  private val logger =
    LoggerFactory.getLogger(
      classOf[SmcInterpreter[
        EntityType, PropertyType, CosmosType, MindType]])

  private lazy val debugEnabled = logger.isDebugEnabled

  private var debugDepth = 0

  private val inputRewriter = new SmcInputRewriter(mind)

  private val responseRewriter = new SmcResponseRewriter(mind)

  protected val sentencePrinter = new SilSentencePrinter

  private val interpreterMatchers = Seq(
    interpretStateChangeCommand,
    interpretPredicateQuery,
    interpretPredicateSentence,
    interpretUnsupportedSentence
  ).reduceLeft(_ orElse _)

  def fail(msg : String) = cosmos.fail(msg)

  @inline protected final def debug(msg : => String)
  {
    if (debugEnabled) {
      val prefix = "*" * debugDepth
      logger.debug(prefix + msg)
    }
  }

  protected final def debug(msg : => String, t : Throwable)
  {
    if (debugEnabled) {
      val prefix = "*" * debugDepth
      logger.error(prefix + msg, t)
    }
  }

  def interpret(sentence : SilSentence, input : String = "") : String =
  {
    if (!input.isEmpty) {
      debug(s"INTERPRETER INPUT TEXT : $input")
    }
    debug(s"INTERPRETER INPUT SENTENCE : $sentence")
    mind.rememberSpeakerSentence(
      SmcConversation.SPEAKER_NAME_PERSON, sentence, input)
    SilPhraseValidator.validatePhrase(sentence)
    val (responseSentence, responseText) = interpretImpl(sentence)
    debug(s"INTERPRETER RESPONSE TEXT : $responseText")
    debug(s"INTERPRETER RESPONSE SENTENCE : $responseSentence")
    if (mind.isConversing) {
      // perhaps we should synthesize referenceMap as we go instead
      // of attempting to reconstruct it here
      val resultCollector = SmcResultCollector[EntityType]
      val rewriter = new SmcReferenceRewriter(
        mind.getCosmos, new SilSentencePrinter, resultCollector,
        SmcResolutionOptions(
          failOnUnknown = false,
          resolveConjunctions = true,
          resolveUniqueDeterminers = true))
      // discard the rewrite result; we just want the
      // resultCollector side effects
      rewriter.rewrite(rewriter.rewriteReferences, responseSentence)
      mind.rememberSpeakerSentence(
        SmcConversation.SPEAKER_NAME_SHLURD,
        responseSentence, responseText, resultCollector.referenceMap)
    }
    responseText
  }

  protected def interpretImpl(sentence : SilSentence)
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
    interpreterMatchers.applyOrElse(
      normalizedInput,
      { s : SilSentence =>
        debug("UNKNOWN SENTENCE")
        wrapResponseText(sentencePrinter.sb.respondCannotUnderstand())
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

  private def interpretStateChangeCommand = sentenceInterpreter {
    case SilStateChangeCommand(predicate, _, formality) => {
      debug("STATE CHANGE COMMAND")

      val resultCollector = SmcResultCollector[EntityType]
      val result = evaluatePredicate(predicate, resultCollector)
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
          wrapResponseText(sentencePrinter.sb.respondCompliance())
        }
        case Failure(e) => {
          debug("ERROR", e)
          wrapResponseText(e.getMessage)
        }
      }
    }
  }

  private def interpretPredicateQuery = sentenceInterpreter {
    case sentence @ SilPredicateQuery(
      predicate, question, answerInflection, tam, formality
    ) => {
      debug("PREDICATE QUERY")
      // FIXME deal with positive, modality

      val rewrittenPredicate = rewriteQuery(predicate, question)
      debug(s"REWRITTEN PREDICATE : $rewrittenPredicate")

      val resultCollector = SmcResultCollector[EntityType]
      val result = evaluateTamPredicate(
        rewrittenPredicate, tam, resultCollector)
      mind.rememberSentenceAnalysis(resultCollector.referenceMap)
      result match {
        case Success(Trilean.Unknown) => {
          debug("ANSWER UNKNOWN")
          wrapResponseText(sentencePrinter.sb.respondDontKnow())
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
                case _ => {
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

  private def interpretPredicateSentence = sentenceInterpreter {
    case SilPredicateSentence(predicate, tam, formality) => {
      val resultCollector = SmcResultCollector[EntityType]
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
              wrapResponseText(sentencePrinter.sb.respondDontKnow())
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
          wrapResponseText(sentencePrinter.sb.respondCannotUnderstand())
        }
      }
    }
  }

  private def interpretUnsupportedSentence = sentenceInterpreter {
    case SilConjunctiveSentence(determiner, sentences, _) => {
      // FIXME
      debug("CONJUNCTIVE SENTENCE")
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand())
    }
    case SilAmbiguousSentence(alternatives, _) => {
      debug("AMBIGUOUS SENTENCE")
      // FIXME:  try each in turn and use first
      // that does not result in an error
      wrapResponseText(sentencePrinter.sb.respondCannotUnderstand())
    }
  }

  private def evaluateDeterminer(
    tries : Iterable[Try[Trilean]], determiner : SilDeterminer)
      : Try[Trilean] =
  {
    debug(s"EVALUATE DETERMINER : $determiner OVER $tries")
    tries.find(_.isFailure) match {
      // FIXME:  combine failures
      case Some(failed) => failed
      case _ => {
        val results = tries.map(_.get)
        determiner match {
          case DETERMINER_NONE => {
            Success(!results.fold(Trilean.False)(_|_))
          }
          case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED => {
            val lowerBound = results.count(_.assumeFalse)
            if (lowerBound > 1) {
              Success(Trilean.False)
            } else {
              if (results.exists(_.isUnknown)) {
                Success(Trilean.Unknown)
              } else {
                Success(Trilean(lowerBound == 1))
              }
            }
          }
          case DETERMINER_ALL => {
            if (results.isEmpty) {
              // FIXME:  logic dictates otherwise
              Success(Trilean.False)
            } else {
              Success(results.fold(Trilean.True)(_&_))
            }
          }
          case DETERMINER_ANY | DETERMINER_SOME | DETERMINER_NONSPECIFIC => {
            Success(results.fold(Trilean.False)(_|_))
          }
          case _ => fail(sentencePrinter.sb.respondCannotUnderstand())
        }
      }
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
        evaluatePredicate(predicate, resultCollector)
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
      val pastInterpreter = spawn(pastMind)
      val pastTruthTry = pastInterpreter.evaluatePredicate(
        boundPredicate, pastCollector)
      val pastTruth =
        pastTruthTry.getOrElse(return pastTruthTry).assumeFalse
      if (trueSeen) {
        if (!pastTruth) {
          // now re-evaluate the original predicate at that point in time
          return pastInterpreter.evaluatePredicate(
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

  protected def evaluatePredicate(
    predicateOriginal : SilPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    debug(s"EVALUATE PREDICATE : $predicateOriginal")
    debugDepth += 1
    val predicate = {
      try {
        rewriteReferences(predicateOriginal, resultCollector)
      } catch {
        case ex : RuntimeException => {
          return Failure(ex)
        }
      }
    }
    if (predicate != predicateOriginal) {
      debug(s"REWRITTEN REFERENCES : $predicate")
    }
    // FIXME analyze verb modifiers
    val result = predicate match {
      case SilStatePredicate(subject, state, modifiers) => {
        state match {
          case SilConjunctiveState(determiner, states, _) => {
            // FIXME:  how to write to resultCollector.entityMap in this case?
            val tries = states.map(
              s => evaluatePredicate(
                SilStatePredicate(subject, s), resultCollector))
            evaluateDeterminer(tries, determiner)
          }
          case _ => evaluateNormalizedStatePredicate(
            subject, state, resultCollector)
        }
      }
      case SilRelationshipPredicate(
        subjectRef, complementRef, relationship, modifiers) =>
      {
        val subjectCollector = chooseSmcResultCollector(
          subjectRef, resultCollector)
        val complementCollector = chooseSmcResultCollector(
          complementRef, resultCollector)
        val categoryLabel = relationship match {
          case REL_IDENTITY => extractCategory(complementRef)
          case _ => ""
        }
        evaluatePredicateOverReference(
          subjectRef, REF_SUBJECT, subjectCollector)
        {
          (subjectEntity, entityRef) => {
            if (!categoryLabel.isEmpty) {
              resultCollector.isCategorization = true
              evaluateCategorization(subjectEntity, categoryLabel)
            } else {
              val context = relationship match {
                case REL_IDENTITY => REF_COMPLEMENT
                case REL_ASSOCIATION => REF_SUBJECT
              }
              if (relationship == REL_ASSOCIATION) {
                val roleQualifiers = extractRoleQualifiers(complementRef)
                if (roleQualifiers.size == 1) {
                  val roleName = roleQualifiers.head
                  cosmos.reifyRole(subjectEntity, roleName, true)
                }
              }
              evaluatePredicateOverReference(
                complementRef, context, complementCollector)
              {
                (complementEntity, entityRef) => evaluateRelationshipPredicate(
                  subjectEntity, complementRef, complementEntity, relationship
                )
              }
            }
          }
        }
      }
      case ap : SilActionPredicate => {
        // FIXME we should be calling updateNarrative() here too for
        // indicative statements
        evaluateActionPredicate(ap, resultCollector)
      }
      case _ => {
        debug("UNEXPECTED PREDICATE TYPE")
        fail(sentencePrinter.sb.respondCannotUnderstand())
      }
    }
    debugDepth -= 1
    debug(s"PREDICATE TRUTH : $result")
    result
  }

  protected def evaluateActionPredicate(
    ap : SilActionPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    debug("ACTION PREDICATES UNSUPPORTED")
    fail(sentencePrinter.sb.respondCannotUnderstand())
  }

  private def evaluateNormalizedStatePredicate(
    subjectRef : SilReference,
    originalState : SilState,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val context = originalState match {
      case _ : SilAdpositionalState => REF_ADPOSITION_SUBJ
      case _ => REF_SUBJECT
    }
    evaluatePredicateOverReference(subjectRef, context, resultCollector)
    {
      (entity, entityRef) => {
        val normalizedState = cosmos.normalizeState(entity, originalState)
        if (originalState != normalizedState) {
          debug(s"NORMALIZED STATE : $normalizedState")
        }
        normalizedState match {
          case SilExistenceState() => {
            Success(Trilean.True)
          }
          case SilPropertyState(word) => {
            evaluatePropertyStatePredicate(
              entity, entityRef, word, resultCollector)
          }
          case SilAdpositionalState(adposition, objRef) => {
            evaluateAdpositionStatePredicate(
              entity, adposition, objRef, resultCollector)
          }
          case _ => {
            debug(s"UNEXPECTED STATE : $normalizedState")
            fail(sentencePrinter.sb.respondCannotUnderstand())
          }
        }
      }
    }
  }

  private def extractCategory(reference : SilReference) : String =
  {
    // FIXME:  support qualifiers etc
    reference match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR) => noun.lemma
      case _ => ""
    }
  }

  private def evaluateRelationshipPredicate(
    subjectEntity : EntityType,
    complementRef : SilReference,
    complementEntity : EntityType,
    relationship : SilRelationship) : Try[Trilean] =
  {
    relationship match {
      case REL_IDENTITY => {
        val result = {
          if (subjectEntity.isTentative || complementEntity.isTentative) {
            Success(Trilean.Unknown)
          } else {
            Success(Trilean(subjectEntity == complementEntity))
          }
        }
        debug("RESULT FOR " +
          s"$subjectEntity == $complementEntity is $result")
        result
      }
      case REL_ASSOCIATION => {
        val roleQualifiers = extractRoleQualifiers(complementRef)
        val result = cosmos.evaluateEntityAdpositionPredicate(
          complementEntity, subjectEntity,
          SilAdposition.GENITIVE_OF, roleQualifiers)
        debug("RESULT FOR " +
          s"$complementEntity GENITIVE_OF " +
          s"$subjectEntity with $roleQualifiers is $result")
        result
      }
    }
  }

  private def extractRoleQualifiers(complementRef : SilReference)
      : Set[String] =
  {
    // FIXME:  do something less hacky
    complementRef match {
      case SilNounReference(noun, determiner, count) => {
        Set(noun.lemma)
      }
      case _ => Set.empty
    }
  }

  private def evaluateCategorization(
    entity : EntityType,
    categoryLabel : String) : Try[Trilean] =
  {
    val result = cosmos.evaluateEntityCategoryPredicate(entity, categoryLabel)
    debug("RESULT FOR " +
      s"$entity IN_CATEGORY " +
      s"$categoryLabel is $result")
    result match {
      case Failure(e) => {
        debug("ERROR", e)
        fail(sentencePrinter.sb.respondUnknown(SilWord(categoryLabel)))
      }
      case _ => result
    }
  }

  private def evaluatePredicateOverReference(
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState = SilNullState()
  )(evaluator : PredicateEvaluator)
      : Try[Trilean] =
  {
    debug("EVALUATE PREDICATE OVER REFERENCE : " +
      reference + " WITH CONTEXT " + context + " AND SPECIFIED STATE "
      + specifiedState)
    debugDepth += 1
    val result = evaluatePredicateOverReferenceImpl(
      reference, context, resultCollector,
      specifiedState, evaluator)
    debugDepth -= 1
    debug(s"PREDICATE TRUTH OVER REFERENCE : $result")
    result
  }

  private def evaluatePredicateOverEntities(
    unfilteredEntities : Iterable[EntityType],
    entityRef : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    determiner : SilDeterminer,
    count : SilCount,
    noun : SilWord,
    evaluator : PredicateEvaluator)
      : Try[Trilean] =
  {
    debug(s"CANDIDATE ENTITIES : $unfilteredEntities")
    // probably we should be pushing filters down into
    // resolveQualifiedNoun for efficiency
    val adpositionStates =
      SilReference.extractAdpositionSpecifiers(specifiedState)
    val entities = {
      if (adpositionStates.isEmpty) {
        unfilteredEntities
      } else {
        // should probably be doing some caching for
        // reference -> entity lookups
        unfilteredEntities.filter(subjectEntity =>
          adpositionStates.forall(adp => {
            val adposition = adp.adposition
            val qualifiers : Set[String] = {
              if (adposition == SilAdposition.GENITIVE_OF) {
                Set(noun.lemma)
              } else {
                Set.empty
              }
            }
            val evaluation = evaluatePredicateOverReference(
              adp.objRef, REF_ADPOSITION_OBJ,
                resultCollector.spawn)
            {
              (objEntity, entityRef) => {
                val result = cosmos.evaluateEntityAdpositionPredicate(
                  subjectEntity, objEntity, adposition, qualifiers)
                debug("RESULT FOR " +
                  s"$subjectEntity $adposition $objEntity " +
                  s"with $qualifiers is $result")
                result
              }
            }
            if (evaluation.isFailure) {
              return evaluation
            } else {
              evaluation.get.isTrue
            }
          })
        )
      }
    }
    if (!entities.isEmpty) {
      resultCollector.referenceMap.put(
        entityRef, SprUtils.orderedSet(entities))
    }
    determiner match {
      case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED => {
        if (entities.isEmpty && (context != REF_COMPLEMENT)) {
          fail(sentencePrinter.sb.respondNonexistent(noun))
        } else {
          count match {
            case COUNT_SINGULAR => {
              if (entities.isEmpty) {
                Success(Trilean.False)
              } else if (entities.size > 1) {
                if (determiner == DETERMINER_UNIQUE) {
                  fail(sentencePrinter.sb.respondAmbiguous(
                    noun))
                } else {
                  evaluateDeterminer(
                    entities.map(
                      invokeEvaluator(
                        _, entityRef, resultCollector, evaluator)),
                    DETERMINER_ANY)
                }
              } else {
                invokeEvaluator(
                  entities.head, entityRef, resultCollector, evaluator)
              }
            }
            case COUNT_PLURAL => {
              val newDeterminer = determiner match {
                case DETERMINER_UNIQUE => DETERMINER_ALL
                case _ => determiner
              }
              evaluateDeterminer(
                entities.map(
                  invokeEvaluator(_, entityRef, resultCollector, evaluator)),
                newDeterminer)
            }
          }
        }
      }
      case _ => {
        evaluateDeterminer(
          entities.map(invokeEvaluator(
            _, entityRef, resultCollector, evaluator)),
          determiner)
      }
    }
  }

  private def evaluatePredicateOverReferenceImpl(
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    evaluator : PredicateEvaluator)
      : Try[Trilean] =
  {
    val referenceMap = resultCollector.referenceMap
    // FIXME should maybe use normalizeState here, but it's a bit tricky
    reference match {
      case SilNounReference(noun, determiner, count) => {
        cosmos.resolveQualifiedNoun(
          noun.lemma, context,
          cosmos.qualifierSet(
            SilReference.extractQualifiers(specifiedState))) match
        {
          case Success(entities) => {
            evaluatePredicateOverEntities(
              entities,
              reference,
              context,
              resultCollector,
              specifiedState,
              determiner,
              count,
              noun,
              evaluator)
          }
          case Failure(e) => {
            debug("ERROR", e)
            fail(sentencePrinter.sb.respondUnknown(noun))
          }
        }
      }
      case pr : SilPronounReference => {
        mind.resolvePronoun(pr) match {
          case Success(entities) => {
            referenceMap.put(reference, entities)
            debug(s"CANDIDATE ENTITIES : $entities")
            evaluateDeterminer(
              entities.map(
                invokeEvaluator(_, reference, resultCollector, evaluator)),
              DETERMINER_ALL)
          }
          case Failure(e) => {
            debug("ERROR", e)
            fail(sentencePrinter.sb.respondUnknownPronoun(
              sentencePrinter.print(
                reference, INFLECT_NOMINATIVE, SilConjoining.NONE)))
          }
        }
      }
      case SilConjunctiveReference(determiner, references, separator) => {
        val results = references.map(
          evaluatePredicateOverReference(
            _, context, resultCollector, specifiedState)(evaluator))
        val combinedEntities = references.flatMap(sub => {
          referenceMap.get(sub) match {
            case Some(entities) => entities
            case _ => Seq.empty
          }
        })
        referenceMap.put(reference, combinedEntities.toSet)
        evaluateDeterminer(results, determiner)
      }
      case SilStateSpecifiedReference(sub, subState) => {
        val result = evaluatePredicateOverState(
          sub, subState, context, resultCollector, specifiedState, evaluator)
        referenceMap.get(sub) match {
          case Some(entities) => {
            referenceMap.put(reference, entities)
          }
          case _ =>
        }
        result
      }
      case SilGenitiveReference(possessor, possessee) => {
        val state = SilAdpositionalState(SilAdposition.GENITIVE_OF, possessor)
        evaluatePredicateOverState(
          possessee, state, context, resultCollector, specifiedState, evaluator)
      }
      case rr : SilResolvedReference[EntityType] => {
        evaluatePredicateOverEntities(
          rr.entities,
          rr,
          context,
          resultCollector,
          specifiedState,
          rr.determiner,
          SilReference.getCount(rr),
          rr.noun,
          evaluator)
      }
      case _ : SilUnknownReference => {
        debug("UNKNOWN REFERENCE")
        fail(sentencePrinter.sb.respondCannotUnderstand())
      }
    }
  }

  private def evaluatePredicateOverState(
    reference : SilReference,
    state : SilState,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    evaluator : PredicateEvaluator)
      : Try[Trilean] =
  {
    val combinedState = {
      if (specifiedState == SilNullState()) {
        state
      } else {
        SilConjunctiveState(
          DETERMINER_ALL,
          Seq(specifiedState, state),
          SEPARATOR_CONJOINED)
      }
    }
    evaluatePredicateOverReference(
      reference, context, resultCollector, combinedState)(evaluator)
  }

  private def invokeEvaluator(
    entity : EntityType,
    entityRef : SilReference,
    resultCollector : ResultCollectorType,
    evaluator : PredicateEvaluator) : Try[Trilean] =
  {
    val result = evaluator(
      entity, entityRef)
    result.foreach(resultCollector.entityMap.put(entity, _))
    result
  }

  private def evaluatePropertyStatePredicate(
    entity : EntityType,
    entityRef : SilReference,
    state : SilWord,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val result = cosmos.resolveProperty(entity, state.lemma) match {
      case Success((property, stateName)) => {
        resultCollector.states += SilWord(
          cosmos.getPropertyStateMap(property).get(stateName).
            getOrElse(stateName), stateName)
        cosmos.evaluateEntityPropertyPredicate(
          entity, property, stateName)
      }
      case Failure(e) => {
        debug("ERROR", e)
        val errorRef = entityRef match {
          case SilNounReference(noun, determiner, count) => {
            val rephrased = noun match {
              case SilWord(LEMMA_WHO, LEMMA_WHO) => SilWord(LEMMA_PERSON)
              case SilWord(LEMMA_WHOM, LEMMA_WHOM) => SilWord(LEMMA_PERSON)
              case SilWord(LEMMA_WHERE, LEMMA_WHERE) => SilWord(LEMMA_CONTAINER)
              case _ => noun
            }
            SilNounReference(rephrased, DETERMINER_NONSPECIFIC, COUNT_SINGULAR)
          }
          case _ => {
            cosmos.specificReference(entity, DETERMINER_NONSPECIFIC)
          }
        }
        fail(sentencePrinter.sb.respondUnknownState(
          sentencePrinter.print(
            errorRef,
            INFLECT_NOMINATIVE,
            SilConjoining.NONE),
          state))
      }
    }
    debug(s"RESULT FOR $entity is $result")
    result
  }

  private def evaluateAdpositionStatePredicate(
    subjectEntity : EntityType, adposition : SilAdposition,
    objRef : SilReference,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val objCollector = resultCollector.spawn
    evaluatePredicateOverReference(
      objRef, REF_ADPOSITION_OBJ, objCollector)
    {
      (objEntity, entityRef) => {
        val result = cosmos.evaluateEntityAdpositionPredicate(
          subjectEntity, objEntity, adposition)
        debug("RESULT FOR " +
          s"$subjectEntity $adposition $objEntity is $result")
        result
      }
    }
  }

  private def rewriteQuery(
    predicate : SilPredicate, question : SilQuestion) : SilPredicate =
  {
    val queryRewriter = new SmcQueryRewriter(question)
    queryRewriter.rewrite(
      queryRewriter.rewritePredicate, predicate)
  }

  private def rewriteReferences(
    predicate : SilPredicate,
    resultCollector : ResultCollectorType) : SilPredicate =
  {
    val referenceRewriter = new SmcReferenceRewriter(
      cosmos, sentencePrinter, resultCollector)
    referenceRewriter.rewrite(
      referenceRewriter.rewriteReferences, predicate)
  }

  private def chooseSmcResultCollector(
    phrase : SilPhrase,
    collector : ResultCollectorType) =
  {
    if (inputRewriter.containsWildcard(phrase)) {
      collector
    } else {
      collector.spawn
    }
  }
}
