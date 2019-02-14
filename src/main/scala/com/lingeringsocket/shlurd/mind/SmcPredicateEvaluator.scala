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

import SprEnglishLemmas._

class SmcPredicateEvaluator[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  mind : MindType,
  sentencePrinter : SilSentencePrinter,
  debugger : Option[SmcDebugger])
    extends SmcDebuggable(debugger)
{
  type ResultCollectorType = SmcResultCollector[EntityType]

  type EntityPredicateEvaluator = (EntityType, SilReference) => Try[Trilean]

  private val wildcardQuerier = new SmcPhraseRewriter

  private def cosmos = mind.getCosmos

  private def fail(msg : String) = cosmos.fail(msg)

  protected[mind] def evaluatePredicate(
    predicateOriginal : SilPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    trace(s"EVALUATE PREDICATE : $predicateOriginal")
    val predicate = normalizePredicate(
      predicateOriginal,
      resultCollector.referenceMap)
    if (predicate != predicateOriginal) {
      trace(s"NORMALIZED PREDICATE : $predicate")
    }
    debugPushLevel()
    // we are re-resolving with reification this time, so
    // already cached references might be stale
    resultCollector.referenceMap.clear
    val resolutionResult =
      resolveReferences(predicate, resultCollector, true, true)
    if (resolutionResult.isFailure) {
      return resolutionResult
    }
    // FIXME analyze verb modifiers
    val result = predicate match {
      case SilStatePredicate(
        subject, state, modifiers
      ) => {
        state match {
          case SilConjunctiveState(determiner, states, _) => {
            // FIXME:  how to write to resultCollector.entityMap in this case?
            val tries = states.map(
              s => evaluatePredicate(
                SilStatePredicate(subject, s), resultCollector))
            evaluateDeterminer(tries, determiner)
          }
          case _ => evaluateStatePredicate(
            subject, state, resultCollector)
        }
      }
      case SilRelationshipPredicate(
        subjectRef, complementRef, relationship, modifiers
      ) => {
        val subjectCollector = chooseResultCollector(
          subjectRef, resultCollector)
        val complementCollector = chooseResultCollector(
          complementRef, resultCollector)
        val (context, categoryLabel) =
          relationshipComplementContext(relationship, complementRef)
        val tryComplement = {
          if (!categoryLabel.isEmpty) {
            resultCollector.isCategorization = true
            Success(Trilean.Unknown)
          } else {
            // just in case we skip evaluating complementRef, force
            // it now for any side effects such as error detection
            resolveReference(complementRef, context, resultCollector).map(
              _ => Trilean.Unknown)
          }
        }
        if (tryComplement.isFailure) {
          tryComplement
        } else {
          evaluatePredicateOverReference(
            subjectRef, relationshipSubjectContext(relationship),
            subjectCollector)
          {
            (subjectEntity, entityRef) => {
              if (!categoryLabel.isEmpty) {
                evaluateCategorization(subjectEntity, categoryLabel)
              } else {
                if (relationship == REL_ASSOCIATION) {
                  val roleQualifiers = extractRoleQualifiers(complementRef)
                  if (roleQualifiers.size == 1) {
                    val roleName = roleQualifiers.head
                    cosmos.reifyRole(subjectEntity, roleName, true)
                    // invalidate any cached result for complementRef since
                    // we just reified a new entity
                    complementRef.descendantReferences.foreach(
                      resultCollector.referenceMap.remove
                    )
                  }
                }
                evaluatePredicateOverReference(
                  complementRef, context, complementCollector)
                {
                  (complementEntity, entityRef) => {
                    evaluateRelationshipPredicate(
                      subjectRef, subjectEntity,
                      complementRef, complementEntity,
                      relationship
                    )
                  }
                }
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
        fail(sentencePrinter.sb.respondCannotUnderstand)
      }
    }
    debugPopLevel()
    trace(s"PREDICATE TRUTH : $result")
    result
  }

  protected def evaluateActionPredicate(
    ap : SilActionPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    debug("ACTION PREDICATES UNSUPPORTED")
    fail(sentencePrinter.sb.respondCannotUnderstand)
  }

  private def resolveReference(
    ref : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType) =
  {
    // FIXME for a cache hit, we should assert that the result is
    // the same as what we would have gotten from re-evaluation;
    // however, this isn't currently true due to postprocessing
    resultCollector.referenceMap.get(ref) match {
      case Some(entities) => Success(entities)
      case _ => {
        evaluatePredicateOverReference(
          ref, context, resultCollector.spawn, SilNullState())
        {
          (entity, entityRef) => {
            Success(Trilean.Unknown)
          }
        }
      }
    }
  }

  private[mind] def resolveReferences(
    phrase : SilPhrase,
    resultCollector : ResultCollectorType,
    throwFailures : Boolean = false,
    reify : Boolean = false) : Try[Trilean] =
  {
    val phraseQuerier = new SilPhraseRewriter
    // FIXME the cases below need special handling at any
    // level (not just root of reference subtree)
    def resolveOne(
      ref : SilReference,
      context : SilReferenceContext)
    {
      val cached = resultCollector.referenceMap.contains(ref)
      ref match {
        case SilGenitiveReference(possessor, possessee) => {
          // regardless of resolution for possessee, make
          // sure possessor gets resolved
          resolveOne(possessor, context)
        }
        case _ =>
      }
      val result = resolveReference(ref, context, resultCollector)
      if (throwFailures) {
        // this will throw if result.isFailure
        result.get
      }
      if (reify && !cached) {
        result.foreach(_ => {
          ref match {
            case SilGenitiveReference(
              possessor,
              possessee @ SilNounReference(noun, _, _)
            ) => {
              val roleName = noun.lemma
              resultCollector.referenceMap.get(possessor).
                foreach(entities => {
                  entities.foreach(
                    entity => cosmos.reifyRole(entity, roleName, true))
                })
              // now clear cache and repeat to pick up the newly
              // reifed entities
              resultCollector.referenceMap.remove(ref)
              resultCollector.referenceMap.remove(possessee)
              resolveReference(ref, context, resultCollector)
            }
            case _ =>
          }
        })
      }
    }

    val rule = phraseQuerier.queryMatcher {
      case SilStatePredicate(subjectRef, state, modifiers) => {
        resolveOne(subjectRef, subjectStateContext(state))
      }
      case SilRelationshipPredicate(
        subjectRef, complementRef, relationship, modifiers
      ) => {
        resolveOne(subjectRef, relationshipSubjectContext(relationship))
        val (context, categoryLabel) =
          relationshipComplementContext(relationship, complementRef)
        if (categoryLabel.isEmpty) {
          resolveOne(complementRef, context)
        }
      }
      case SilActionPredicate(subject, action, directObject, modifiers) => {
        resolveOne(subject, REF_SUBJECT)
        directObject.foreach(resolveOne(_, REF_DIRECT_OBJECT))
      }
      case ap : SilAdpositionalPhrase => {
        resolveOne(ap.objRef, REF_ADPOSITION_OBJ)
      }
    }
    resultCollector.expandWildcards = false
    try {
      phraseQuerier.query(rule, phrase, SilRewriteOptions(topDown = true))
    } catch {
      case ex : Exception => {
        return Failure(ex)
      }
    }
    resultCollector.expandWildcards = true
    Success(Trilean.True)
  }

  private def subjectStateContext(state : SilState) : SilReferenceContext =
  {
    state match {
      // FIXME this is correct but causes headaches
      // case _ : SilAdpositionalState => REF_ADPOSITION_SUBJ
      case _ => REF_SUBJECT
    }
  }

  private def relationshipSubjectContext(
    rel : SilRelationship) : SilReferenceContext =
  {
    rel match {
      case REL_IDENTITY => REF_COMPLEMENT
      case REL_ASSOCIATION => REF_SUBJECT
    }
  }

  private def relationshipComplementContext(
    rel : SilRelationship,
    complementRef : SilReference) : (SilReferenceContext, String) =
  {
    rel match {
      case REL_IDENTITY => {
        tupleN((REF_COMPLEMENT, extractCategory(complementRef)))
      }
      case REL_ASSOCIATION => {
        tupleN((REF_SUBJECT, ""))
      }
    }
  }

  private def evaluatePropertyStateQuery(
    entity : EntityType,
    entityRef : SilReference,
    propertyName : String,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val result = cosmos.evaluateEntityProperty(entity, propertyName) match {
      case Success((Some(actualProperty), Some(stateName))) => {
        resultCollector.states += SilWord(
          cosmos.getPropertyStateMap(actualProperty).get(stateName).
            getOrElse(stateName), stateName)
        Success(Trilean.True)
      }
      case Success((_, _)) => {
        Success(Trilean.Unknown)
      }
      case Failure(e) => {
        debug("PROPERTY EVALUATION ERROR", e)
        Failure(e)
      }
    }
    trace(s"RESULT FOR $entity is $result")
    result
  }

  private def evaluateStatePredicate(
    subjectRef : SilReference,
    state : SilState,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val context = subjectStateContext(state)
    evaluatePredicateOverReference(subjectRef, context, resultCollector)
    {
      (entity, entityRef) => {
        state match {
          case SilExistenceState() => {
            Success(Trilean.True)
          }
          case SilPropertyState(word) => {
            evaluatePropertyStatePredicate(
              entity, entityRef, word, resultCollector)
          }
          case SilPropertyQueryState(propertyName) => {
            evaluatePropertyStateQuery(
              entity, entityRef, propertyName, resultCollector)
          }
          case SilAdpositionalState(adposition, objRef) => {
            evaluateAdpositionStatePredicate(
              entity, adposition, objRef, resultCollector)
          }
          case _ => {
            debug(s"UNEXPECTED STATE : $state")
            fail(sentencePrinter.sb.respondCannotUnderstand)
          }
        }
      }
    }
  }

  private def evaluateRelationshipPredicate(
    subjectRef : SilReference,
    subjectEntity : EntityType,
    complementRef : SilReference,
    complementEntity : EntityType,
    relationship : SilRelationship) : Try[Trilean] =
  {
    relationship match {
      case REL_IDENTITY => {
        val result = {
          if ((wildcardQuerier.containsWildcard(subjectRef) ||
            wildcardQuerier.containsWildcard(complementRef)) &&
            (subjectEntity.isTentative || complementEntity.isTentative))
          {
            Success(Trilean.Unknown)
          } else {
            Success(Trilean(subjectEntity == complementEntity))
          }
        }
        trace("RESULT FOR " +
          s"$subjectEntity == $complementEntity is $result")
        result
      }
      case REL_ASSOCIATION => {
        val roleQualifiers = extractRoleQualifiers(complementRef)
        val result = cosmos.evaluateEntityAdpositionPredicate(
          complementEntity, subjectEntity,
          SilAdposition.GENITIVE_OF, roleQualifiers)
        trace("RESULT FOR " +
          s"$complementEntity GENITIVE_OF " +
          s"$subjectEntity with $roleQualifiers is $result")
        result
      }
    }
  }

  private def evaluatePredicateOverReference(
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState = SilNullState()
  )(evaluator : EntityPredicateEvaluator)
      : Try[Trilean] =
  {
    trace("EVALUATE PREDICATE OVER REFERENCE : " +
      reference + " WITH CONTEXT " + context + " AND SPECIFIED STATE "
      + specifiedState)
    debugPushLevel()
    val result = evaluatePredicateOverReferenceImpl(
      reference, context, resultCollector,
      specifiedState, None, evaluator)
    debugPopLevel()
    trace(s"PREDICATE TRUTH OVER REFERENCE : $result")
    result
  }

  private def evaluatePredicateOverEntities(
    unfilteredEntities : Iterable[EntityType],
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    determiner : SilDeterminer,
    count : SilCount,
    noun : SilWord,
    evaluator : EntityPredicateEvaluator)
      : Try[Trilean] =
  {
    trace(s"CANDIDATE ENTITIES : $unfilteredEntities")
    // probably we should be pushing filters down into
    // resolveQualifiedNoun for efficiency
    val adpositionStates =
      SilReference.extractAdpositionSpecifiers(specifiedState)
    val entities = {
      if (adpositionStates.isEmpty) {
        unfilteredEntities
      } else {
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
                trace("RESULT FOR " +
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
    resultCollector.referenceMap.put(
      reference, SprUtils.orderedSet(entities))
    determiner match {
      case DETERMINER_UNIQUE | DETERMINER_UNSPECIFIED => {
        if (entities.isEmpty && (context == REF_SUBJECT)) {
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
                        _, reference, resultCollector, evaluator)),
                    DETERMINER_ANY)
                }
              } else {
                invokeEvaluator(
                  entities.head, reference, resultCollector, evaluator)
              }
            }
            case COUNT_PLURAL => {
              val newDeterminer = determiner match {
                case DETERMINER_UNIQUE => DETERMINER_ALL
                case _ => determiner
              }
              evaluateDeterminer(
                entities.map(
                  invokeEvaluator(_, reference, resultCollector, evaluator)),
                newDeterminer)
            }
          }
        }
      }
      case _ => {
        evaluateDeterminer(
          entities.map(invokeEvaluator(
            _, reference, resultCollector, evaluator)),
          determiner)
      }
    }
  }

  private def cacheReference(
    resultCollector : ResultCollectorType,
    ref : SilReference,
    evaluator : () => Try[Set[EntityType]]) =
  {
    resultCollector.referenceMap.get(ref) match {
      case Some(entities) => {
        Success(entities)
      }
      case _ => {
        evaluator()
      }
    }
  }

  private def evaluatePredicateOverReferenceImpl(
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    specifiedEntities : Option[Set[EntityType]],
    evaluator : EntityPredicateEvaluator)
      : Try[Trilean] =
  {
    val referenceMap = resultCollector.referenceMap
    reference match {
      case SilNounReference(noun, determiner, count) => {
        if (!resultCollector.expandWildcards) {
          val bail = determiner match {
            case DETERMINER_UNIQUE => false
            // FIXME this is silly
            case DETERMINER_UNSPECIFIED =>
              (noun.lemma == LEMMA_WHO) || (noun.lemma == LEMMA_WHAT) ||
              (noun.lemma == LEMMA_WHERE)
            case _ => true
          }
          if (bail) {
            return Success(Trilean.Unknown)
          }
        }
        val entitiesTry = {
          specifiedEntities match {
            case Some(entities) => {
              Success(entities)
            }
            case _ => {
              cacheReference(
                resultCollector,
                reference,
                () => mind.resolveQualifiedNoun(
                  noun, context,
                  cosmos.qualifierSet(
                    SilReference.extractQualifiers(specifiedState)))
              )
            }
          }
        }
        entitiesTry match {
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
      case prOriginal : SilPronounReference => {
        assert(specifiedState == SilNullState())
        val pr = {
          if (resultCollector.swapSpeakerListener) {
            val rewriter = new SmcResponseRewriter(mind)
            rewriter.rewrite(
              rewriter.swapPronounsSpeakerListener(referenceMap), prOriginal)
          } else {
            prOriginal
          }
        }
        val entitiesTry = cacheReference(
          resultCollector,
          reference,
          () => mind.resolvePronoun(pr).map(entities => {
            referenceMap.put(reference, entities)
            entities
          }))
        entitiesTry match {
          case Success(entities) => {
            trace(s"CANDIDATE ENTITIES : $entities")
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
        referenceMap.get(sub).foreach(
          entitySet => referenceMap.put(reference, entitySet))
        result
      }
      case SilGenitiveReference(possessor, possessee) => {
        referenceMap.get(reference) match {
          case Some(entities) => {
            evaluatePredicateOverReferenceImpl(
              possessee,
              context,
              resultCollector,
              specifiedState,
              Some(entities),
              evaluator
            )
          }
          case _ => {
            val state = SilAdpositionalState(
              SilAdposition.GENITIVE_OF, possessor)
            val result = evaluatePredicateOverState(
              possessee, state, REF_ADPOSITION_SUBJ, resultCollector,
              specifiedState, evaluator)
            referenceMap.get(possessee).foreach(
              entitySet => referenceMap.put(reference, entitySet))
            result
          }
        }
      }
      case _ : SilQuotationReference => {
        debug("QUOTATION REFERENCE")
        Success(Trilean.Unknown)
      }
      case _ : SilUnknownReference => {
        debug("UNKNOWN REFERENCE")
        fail(sentencePrinter.sb.respondCannotUnderstand)
      }
    }
  }

  private def evaluatePredicateOverState(
    reference : SilReference,
    state : SilState,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    evaluator : EntityPredicateEvaluator)
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

  private def evaluatePropertyStatePredicate(
    entity : EntityType,
    entityRef : SilReference,
    state : SilWord,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val result = cosmos.resolvePropertyState(entity, state.lemma) match {
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
              case SilWordLemma(LEMMA_WHO) =>
                SilWord(LEMMA_PERSON)
              case SilWordLemma(LEMMA_WHOM) =>
                SilWord(LEMMA_OBJECT)
              case SilWordLemma(LEMMA_WHAT) =>
                SilWord(LEMMA_THAT)
              case SilWordLemma(LEMMA_WHERE) =>
                SilWord(LEMMA_CONTAINER)
              case _ => noun
            }
            val rephrasedDeterminer = determiner match {
              case DETERMINER_ANY | DETERMINER_SOME => DETERMINER_NONSPECIFIC
              case _ => determiner
            }
            SilNounReference(rephrased, rephrasedDeterminer, count)
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
    trace(s"RESULT FOR $entity is $result")
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
        trace("RESULT FOR " +
          s"$subjectEntity $adposition $objEntity is $result")
        result
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
    trace("RESULT FOR " +
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

  private def invokeEvaluator(
    entity : EntityType,
    entityRef : SilReference,
    resultCollector : ResultCollectorType,
    evaluator : EntityPredicateEvaluator) : Try[Trilean] =
  {
    val result = evaluator(entity, entityRef)
    result.foreach(resultCollector.entityMap.put(entity, _))
    result
  }

  private def evaluateDeterminer(
    tries : Iterable[Try[Trilean]], determiner : SilDeterminer)
      : Try[Trilean] =
  {
    trace(s"EVALUATE DETERMINER : $determiner OVER $tries")
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
          case _ => fail(sentencePrinter.sb.respondCannotUnderstand)
        }
      }
    }
  }

  private def chooseResultCollector(
    phrase : SilPhrase,
    collector : ResultCollectorType) =
  {
    if (wildcardQuerier.containsWildcard(phrase, true)) {
      collector
    } else {
      collector.spawn
    }
  }

  protected def normalizePredicate(
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[EntityType]]) : SilPredicate =
  {
    predicate
  }
}
