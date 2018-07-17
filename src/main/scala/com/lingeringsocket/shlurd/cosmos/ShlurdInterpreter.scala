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
package com.lingeringsocket.shlurd.cosmos

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.print._

import scala.util._

import spire.math._

import scala.collection._

import org.slf4j._

import ShlurdEnglishLemmas._

case class ShlurdStateChangeInvocation[E<:ShlurdEntity](
  entities : Set[E],
  state : SilWord)
{
}

sealed trait ShlurdResponseVerbosity
case object RESPONSE_TERSE extends ShlurdResponseVerbosity
case object RESPONSE_ELLIPSIS extends ShlurdResponseVerbosity
case object RESPONSE_COMPLETE extends ShlurdResponseVerbosity

case class ShlurdResponseParams(
  listLimit : Int = 3,
  thirdPersonPronouns : Boolean = true,
  verbosity : ShlurdResponseVerbosity = RESPONSE_COMPLETE
)
{
  def neverSummarize = (listLimit == Int.MaxValue)

  def alwaysSummarize = (listLimit == 0)
}

class ResultCollector[E<:ShlurdEntity](
  val referenceMap : mutable.Map[SilReference, Set[E]])
{
  val entityMap = new mutable.LinkedHashMap[E, Trilean]
  val states = new mutable.LinkedHashSet[SilWord]
  var isCategorization = false

  def spawn() = new ResultCollector[E](referenceMap)
}

object ResultCollector
{
  def apply[E<:ShlurdEntity]() =
    new ResultCollector(new mutable.LinkedHashMap[SilReference, Set[E]])
}

class ShlurdInterpreter[E<:ShlurdEntity, P<:ShlurdProperty](
  mind : ShlurdMind[E,P],
  generalParams : ShlurdResponseParams = ShlurdResponseParams())
{
  type PredicateEvaluator = (E, SilReference) => Try[Trilean]

  type SentenceInterpreter = PartialFunction[SilSentence, String]

  private val cosmos = mind.getCosmos

  private val logger = LoggerFactory.getLogger(classOf[ShlurdInterpreter[E,P]])

  private lazy val debugEnabled = logger.isDebugEnabled

  private var debugDepth = 0

  private val responseRewriter = new ShlurdResponseRewriter(mind)

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

  def interpret(sentence : SilSentence) : String =
  {
    debug(s"INTERPRETER INPUT : $sentence")
    SilPhraseValidator.validatePhrase(sentence)
    val response = interpretImpl(sentence)
    debug(s"INTERPRETER RESPONSE : $response")
    response
  }

  protected def interpretImpl(sentence : SilSentence) : String =
  {
    if (sentence.isUninterpretable) {
      val unrecognized = responseRewriter.rewrite(
        responseRewriter.swapPronounsSpeakerListener, sentence)
      val responder = new ShlurdUnrecognizedResponder(sentencePrinter)
      return responder.respond(unrecognized)
    }
    interpreterMatchers.applyOrElse(
      sentence,
      { s : SilSentence =>
        debug("UNKNOWN SENTENCE")
        sentencePrinter.sb.respondCannotUnderstand()
      }
    )
  }

  private def sentenceInterpreter(f : SentenceInterpreter)
      : SentenceInterpreter = f

  private def interpretStateChangeCommand = sentenceInterpreter {
    case SilStateChangeCommand(predicate, _, formality) => {
      debug("STATE CHANGE COMMAND")

      val resultCollector = ResultCollector[E]
      evaluatePredicate(predicate, resultCollector) match {
        case Success(Trilean.True) => {
          debug("COUNTERFACTUAL")
          val (normalizedResponse, negateCollection) =
            responseRewriter.normalizeResponse(
              predicate, resultCollector, generalParams)
          assert(!negateCollection)
          val responseMood = MOOD_INDICATIVE_POSITIVE
          sentencePrinter.sb.respondToCounterfactual(
            sentencePrinter.print(
              SilPredicateSentence(
                normalizedResponse,
                responseMood)))
        }
        case Success(_) => {
          assert(resultCollector.states.size == 1)
          val invocation =
            ShlurdStateChangeInvocation(
              resultCollector.entityMap.filterNot(
                _._2.assumeFalse).keySet,
              resultCollector.states.head)
          debug(s"EXECUTE INVOCATION : $invocation")
          executeInvocation(invocation)
          sentencePrinter.sb.respondCompliance()
        }
        case Failure(e) => {
          debug("ERROR", e)
          e.getMessage
        }
      }
    }
  }

  private def interpretPredicateQuery = sentenceInterpreter {
    case sentence @ SilPredicateQuery(predicate, question, mood, formality) => {
      debug("PREDICATE QUERY")
      // FIXME deal with positive, modality

      val rewrittenPredicate = rewriteQuery(predicate, question)
      debug(s"REWRITTEN PREDICATE : $rewrittenPredicate")

      val resultCollector = ResultCollector[E]
      evaluatePredicate(rewrittenPredicate, resultCollector) match {
        case Success(Trilean.Unknown) => {
          debug("ANSWER UNKNOWN")
          sentencePrinter.sb.respondDontKnow()
        }
        case Success(truth) => {
          debug(s"ANSWER : $truth")
          val truthBoolean = truth.assumeFalse
          val extremeLimit = question match {
            case QUESTION_WHICH | QUESTION_WHO | QUESTION_WHERE => Int.MaxValue
            case QUESTION_HOW_MANY => 0
          }
          val (normalizedResponse, negateCollection) =
            responseRewriter.normalizeResponse(
              rewrittenPredicate, resultCollector,
              generalParams.copy(
                listLimit = extremeLimit),
              Some(question))
          debug(s"NORMALIZED RESPONSE : $normalizedResponse")
          val responseMood = SilIndicativeMood(
            truthBoolean || negateCollection)
          val adjustedResponse = generalParams.verbosity match {
            // FIXME:  for RESPONSE_ELLIPSIS, include the verb as well
            // (or the adposition in the case of QUESTION_WHERE)
            case RESPONSE_TERSE | RESPONSE_ELLIPSIS => {
              sentencePrinter.sb.terminatedSentence(
                sentencePrinter.print(
                  normalizedResponse.getSubject,
                  INFLECT_NOMINATIVE,
                  SilConjoining.NONE),
                responseMood, sentence.formality)
            }
            case _ => {
              sentencePrinter.print(
                SilPredicateSentence(
                  normalizedResponse,
                  responseMood))
            }
          }
          sentencePrinter.sb.respondToQuery(adjustedResponse)
        }
        case Failure(e) => {
          debug("ERROR", e)
          e.getMessage
        }
      }
    }
  }

  private def interpretPredicateSentence = sentenceInterpreter {
    case SilPredicateSentence(predicate, mood, formality) => {
      val resultCollector = ResultCollector[E]
      mood match {
        // FIXME deal with positive, modality
        case SilInterrogativeMood(positive, modality) => {
          debug("PREDICATE QUERY SENTENCE")
          val query = predicate
          evaluatePredicate(query, resultCollector) match {
            case Success(Trilean.Unknown) => {
              debug("ANSWER UNKNOWN")
              sentencePrinter.sb.respondDontKnow()
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
              val printedSentence = {
                params.verbosity match {
                  case RESPONSE_TERSE => {
                    ""
                  }
                  case RESPONSE_ELLIPSIS => {
                    sentencePrinter.print(
                      SilPredicateSentence(
                        normalizedResponse,
                        SilIndicativeMood(truthBoolean)),
                      true)
                  }
                  case RESPONSE_COMPLETE => {
                    sentencePrinter.print(
                      SilPredicateSentence(
                        normalizedResponse,
                        SilIndicativeMood(
                          truthBoolean || negateCollection)))
                  }
                }
              }
              sentencePrinter.sb.respondToAssumption(
                ASSUMED_TRUE, truthBoolean, printedSentence, false)
            }
            case Failure(e) => {
              debug("ERROR", e)
              e.getMessage
            }
          }
        }
        case _ : SilIndicativeMood => {
          // FIXME deal with mood.getModality
          val positivity = mood.isPositive
          debug(s"POSITIVITY : $positivity")
          val predicateTruth = evaluatePredicate(predicate, resultCollector)
          val responseMood = {
            predicateTruth match {
              case Success(Trilean.False) => {
                MOOD_INDICATIVE_NEGATIVE
              }
              case _ => {
                // FIXME:  deal with uncertainty
                MOOD_INDICATIVE_POSITIVE
              }
            }
          }
          predicateTruth match {
            case Success(Trilean.Unknown) => {
              debug("TRUTH UNKNOWN")
              // FIXME:  maybe try to update state?
              "Oh, really?  Thanks for letting me know."
            }
            case Success(truth) => {
              debug(s"KNOWN TRUTH : $truth")
              if (truth.assumeFalse == positivity) {
                val (normalizedResponse, negateCollection) =
                  responseRewriter.normalizeResponse(
                    predicate, resultCollector, generalParams)
                assert(!negateCollection)
                val printedSentence = {
                  generalParams.verbosity match {
                    case RESPONSE_TERSE => {
                      ""
                    }
                    case RESPONSE_ELLIPSIS => {
                      sentencePrinter.print(
                        SilPredicateSentence(
                          normalizedResponse,
                          responseMood),
                        true)
                    }
                    case RESPONSE_COMPLETE => {
                      sentencePrinter.print(
                        SilPredicateSentence(
                          normalizedResponse,
                          responseMood))
                    }
                  }
                }
                sentencePrinter.sb.respondToAssumption(
                  ASSUMED_TRUE, true, printedSentence, true)
              } else {
                // FIXME:  add details on inconsistency, and maybe try
                // to update state?
                "Oh, really?"
              }
            }
            case Failure(e) => {
              // FIXME:  try to update state?
              debug("ERROR", e)
              e.getMessage
            }
          }
        }
        case _ => {
          debug(s"UNEXPECTED MOOD : $mood")
          sentencePrinter.sb.respondCannotUnderstand()
        }
      }
    }
  }

  private def interpretUnsupportedSentence = sentenceInterpreter {
    case SilConjunctiveSentence(determiner, sentences, _) => {
      // FIXME
      debug("CONJUNCTIVE SENTENCE")
      sentencePrinter.sb.respondCannotUnderstand()
    }
    case SilAmbiguousSentence(alternatives, _) => {
      debug("AMBIGUOUS SENTENCE")
      // FIXME:  try each in turn and use first
      // that does not result in an error
      sentencePrinter.sb.respondCannotUnderstand()
    }
  }

  protected def executeInvocation(
    invocation : ShlurdStateChangeInvocation[E])
  {
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

  private def evaluatePredicate(
    predicateOriginal : SilPredicate,
    resultCollector : ResultCollector[E]) : Try[Trilean] =
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
    // FIXME implement action predicates, verb modifiers
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
        val subjectCollector = chooseResultCollector(
          subjectRef, resultCollector)
        val complementCollector = chooseResultCollector(
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
      case _ => {
        debug("UNEXPECTED PREDICATE TYPE")
        fail(sentencePrinter.sb.respondCannotUnderstand())
      }
    }
    debugDepth -= 1
    debug(s"PREDICATE TRUTH : $result")
    result
  }

  private def evaluateNormalizedStatePredicate(
    subjectRef : SilReference,
    originalState : SilState,
    resultCollector : ResultCollector[E])
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
    subjectEntity : E,
    complementRef : SilReference,
    complementEntity : E,
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
    entity : E,
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
    resultCollector : ResultCollector[E],
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
    unfilteredEntities : Iterable[E],
    entityRef : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollector[E],
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
        entityRef, ShlurdParseUtils.orderedSet(entities))
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
    resultCollector : ResultCollector[E],
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
      case SilPronounReference(person, gender, count) => {
        // FIXME for third-person, need conversational coreference resolution
        mind.resolvePronoun(person, gender, count) match {
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
      case rr : SilResolvedReference[E] => {
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
    resultCollector : ResultCollector[E],
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
    entity : E,
    entityRef : SilReference,
    resultCollector : ResultCollector[E],
    evaluator : PredicateEvaluator) : Try[Trilean] =
  {
    val result = evaluator(
      entity, entityRef)
    result.foreach(resultCollector.entityMap.put(entity, _))
    result
  }

  private def evaluatePropertyStatePredicate(
    entity : E,
    entityRef : SilReference,
    state : SilWord,
    resultCollector : ResultCollector[E])
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
    subjectEntity : E, adposition : SilAdposition,
    objRef : SilReference,
    resultCollector : ResultCollector[E])
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
    val queryRewriter = new ShlurdQueryRewriter(question)
    queryRewriter.rewrite(
      queryRewriter.rewritePredicate, predicate)
  }

  private def rewriteReferences(
    predicate : SilPredicate,
    resultCollector : ResultCollector[E]) : SilPredicate =
  {
    val referenceRewriter = new ShlurdReferenceRewriter(
      cosmos, sentencePrinter, resultCollector)
    referenceRewriter.rewrite(
      referenceRewriter.rewriteReferences, predicate)
  }

  private def chooseResultCollector(
    phrase : SilPhrase,
    collector : ResultCollector[E]) =
  {
    if (responseRewriter.containsWildcard(phrase)) {
      collector
    } else {
      collector.spawn
    }
  }
}
