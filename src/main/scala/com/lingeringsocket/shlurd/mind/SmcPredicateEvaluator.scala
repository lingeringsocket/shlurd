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

class SmcPredicateEvaluator[
  EntityType<:SmcEntity,
  PropertyType<:SmcProperty,
  CosmosType<:SmcCosmos[EntityType, PropertyType],
  MindType<:SmcMind[EntityType, PropertyType, CosmosType]
](
  annotator : SmcAnnotator[EntityType, SmcRefNote[EntityType]],
  scope : SmcScope[EntityType, PropertyType, CosmosType, MindType],
  existenceAssumption : SmcExistenceAssumption,
  communicationContext : SmcCommunicationContext[EntityType],
  debugger : SmcDebugger)
    extends SmcDebuggable(debugger)
{
  type ResultCollectorType = SmcResultCollector[EntityType]
  type EntityPredicateEvaluator = (EntityType, SilReference) => Try[Trilean]
  type AnnotatorType = SmcAnnotator[EntityType, SmcRefNote[EntityType]]

  private val mind = scope.getMind

  private val sentencePrinter = scope.getSentencePrinter

  private def cosmos = mind.getCosmos

  private implicit val tongue = mind.getTongue

  protected[mind] def evaluatePredicate(
    predicateOriginal : SilPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    trace(s"EVALUATE PREDICATE : $predicateOriginal")
    val predicateTry = normalizePredicateWithModifiers(
      resultCollector,
      predicateOriginal)
    predicateTry matchPartial {
      case Failure(e) => return Failure(e)
    }
    val predicate = predicateTry.get
    if (predicate != predicateOriginal) {
      trace(s"NORMALIZED PREDICATE : $predicate")
    }
    debugPushLevel()
    // we are re-resolving with reification this time, so
    // already cached references might be stale
    resultCollector.refMap.clear()
    val resolutionResult =
      resolveReferences(predicate, resultCollector, true, true)
    if (resolutionResult.isFailure) {
      debugPopLevel()
      return resolutionResult
    }
    val result = predicate match {
      case SilStatePredicate(
        subject, verb, state, modifiers
      ) => {
        val extraStates = modifiers.flatMap(_ match {
          case SilAdpositionalVerbModifier(adposition, objRef) => {
            Some(SilAdpositionalState(adposition, objRef))
          }
          case m => None
        })
        val combinedState = {
          if (extraStates.isEmpty) {
            state
          } else {
            state match {
              case SilConjunctiveState(determiner, states, separator) => {
                assert (determiner == DETERMINER_ALL)
                SilConjunctiveState(
                  determiner, states ++ extraStates, separator)
              }
              case _ => {
                SilConjunctiveState(DETERMINER_ALL, state +: extraStates)
              }
            }
          }
        }
        combinedState match {
          case SilConjunctiveState(determiner, states, _) => {
            // FIXME:  how to write to resultCollector.entityMap in this case?
            val tries = states.map(
              s => evaluatePredicate(
                SilStatePredicate(subject, verb, s), resultCollector))
            evaluateDeterminer(tries, determiner)
          }
          case _ => evaluateStatePredicate(
            subject, combinedState, resultCollector)
        }
      }
      case SilRelationshipPredicate(
        subjectRef, verb, complementRef, modifiers
      ) => {
        val subjectCollector = chooseResultCollector(
          subjectRef, resultCollector)
        val complementCollector = chooseResultCollector(
          complementRef, resultCollector)
        val (context, categoryLabel) =
          relationshipComplementContext(verb, complementRef)
        if (!categoryLabel.isEmpty) {
          resultCollector.isCategorization = true
        }
        if (SmcPhraseQuerier.containsWildcard(subjectRef, false, true) &&
          (SprRelationshipPredef(verb) ==
            REL_PREDEF_IDENTITY) &&
          categoryLabel.isEmpty
        ) {
          resultCollector.lookup(complementRef) match {
            case Some(entities) => {
              evaluatePredicateOverReferenceImpl(
                subjectRef,
                REF_SUBJECT,
                resultCollector,
                SilNullState(),
                Some(entities),
                {
                  (objEntity, entityRef) => {
                    if (objEntity.isTentative) {
                      Success(Trilean.Unknown)
                    } else {
                      entityRef match {
                        case SilOptionallyDeterminedReference(
                          SilNounReference(noun),
                          (_ : SilUnlimitedDeterminer) | DETERMINER_ABSENT
                        ) => {
                          noun match {
                            case SprPredefWord(PD_WHO | PD_WHAT) => {
                              Success(Trilean.True)
                            }
                            case _ => {
                              evaluateCategorization(objEntity, noun)
                            }
                          }
                        }
                        case _ => {
                          val message =
                            s"$objEntity UNEXPECTED REF : $entityRef"
                          debug(message)
                          cosmos.fail(
                            ShlurdExceptionCode.FailedParse,
                            message)
                        }
                      }
                    }
                  }
                }
              )
            }
            case _ => {
              Success(Trilean.Unknown)
            }
          }
        } else {
          evaluateRelationshipPredicateExpandWildcard(
            subjectRef,
            subjectCollector,
            categoryLabel,
            complementRef,
            complementCollector,
            context,
            resultCollector,
            verb
          )
        }
      }
      case ap : SilActionPredicate => {
        // FIXME we should be calling updateNarrative here too for
        // indicative statements
        evaluateActionPredicate(ap, resultCollector)
      }
      case _ => {
        debug("UNEXPECTED PREDICATE TYPE")
        cosmos.fail(
          ShlurdExceptionCode.FailedParse,
          sentencePrinter.sb.respondCannotUnderstand)
      }
    }
    if (SmcPhraseQuerier.containsVariable(predicate)) {
      val (keep, discard) = resultCollector.refMap.keys.partition(_ match {
          case SilDeterminedReference(_, DETERMINER_VARIABLE) => true
          case _ => false
      })
      resultCollector.neutralizedEntities ++=
        (discard.flatMap(ref => resultCollector.refMap(ref)).toSet --
          keep.flatMap(ref => resultCollector.refMap(ref)).toSet)
    }
    debugPopLevel()
    trace(s"PREDICATE TRUTH : $result")
    result
  }

  private def normalizeModifiers(
    modifiers : Seq[SilVerbModifier]) : Try[Seq[SilVerbModifier]] =
  {
    Try(modifiers.map(m =>
      normalizeModifier(m)
    ).map(_.get))
  }

  protected def normalizeModifier(
    modifier : SilVerbModifier
  ) : Try[SilVerbModifier] =
  {
    modifier match {
      case SilBasicVerbModifier(word) => {
        debug(s"UNEXPECTED MODIFIER : $word")
        cosmos.fail(
          ShlurdExceptionCode.UnknownModifier,
          sentencePrinter.sb.respondUnknownModifier(word))
      }
      case m => Success(m)
    }
  }

  protected def reifyRole(
    possessor : EntityType, roleName : SilWord, onlyIfProven : Boolean)
      : Set[EntityType] =
  {
    mind.reifyRole(possessor, roleName, onlyIfProven)
  }

  private def evaluateRelationshipPredicateExpandWildcard(
    subjectRef : SilReference,
    subjectCollector : ResultCollectorType,
    categoryLabel : Option[SilWord],
    complementRef : SilReference,
    complementCollector : ResultCollectorType,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    verb : SilWord
  ) : Try[Trilean] =
  {
    evaluatePredicateOverReference(
      subjectRef, relationshipSubjectContext(verb),
      subjectCollector)
    {
      (subjectEntity, entityRef) => {
        categoryLabel match {
          case Some(label) => {
            evaluateCategorization(subjectEntity, label)
          }
          case _ => {
            if (SprRelationshipPredef(verb) ==
              REL_PREDEF_ASSOC)
            {
              val roleQualifiers = extractRoleQualifiers(complementRef)
              if (roleQualifiers.size == 1) {
                val roleName = roleQualifiers.head
                reifyRole(subjectEntity, roleName, true)
                // invalidate any cached result for complementRef since
                // we just reified a new entity
                complementRef.descendantReferences.foreach(
                  resultCollector.refMap.remove
                )
                // and now stash away the new result
                val resolved = mind.resolveGenitive(subjectEntity, roleName)
                resolved match {
                  case Failure(_ : UnsupportedOperationException) => ;
                  case Failure(err) => return Failure(err)
                  case Success(entities) => {
                    resultCollector.refMap.put(complementRef, entities)
                  }
                }
              }
            }
            val unassumed = evaluatePredicateOverReference(
              complementRef, context, complementCollector)
            {
              (complementEntity, entityRef) => {
                evaluateRelationshipPredicate(
                  subjectRef, subjectEntity,
                  complementRef, complementEntity,
                  verb
                )
              }
            }
            assumeExistence(
              unassumed,
              SprRelationshipPredef(verb) == REL_PREDEF_ASSOC)
          }
        }
      }
    }
  }

  protected def evaluateActionPredicate(
    ap : SilActionPredicate,
    resultCollector : ResultCollectorType) : Try[Trilean] =
  {
    debug("ACTION PREDICATES UNSUPPORTED")
    Success(Trilean.Unknown)
  }

  private def resolveReference(
    ref : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    spawnCollector : Boolean = true) =
  {
    // FIXME for a cache hit, we should assert that the result is
    // the same as what we would have gotten from re-evaluation;
    // however, this isn't currently true due to postprocessing
    resultCollector.lookup(ref) match {
      case Some(entities) => Success(entities)
      case _ => {
        val chosenCollector = {
          if (spawnCollector) {
            resultCollector.spawn
          } else {
            resultCollector
          }
        }
        chosenCollector.resolvingReferences = true
        try {
          evaluatePredicateOverReference(
            ref, context, chosenCollector, SilNullState())
          {
            (entity, entityRef) => {
              Success(Trilean.Unknown)
            }
          }
        } finally {
          chosenCollector.resolvingReferences = false
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
    val phraseQuerier = new SilPhraseQuerier

    def resolveOne(
      ref : SilReference,
      context : SilReferenceContext) : Unit =
    {
      val cached = resultCollector.refMap.contains(ref)
      val result = resolveReference(ref, context, resultCollector)
      if (throwFailures) {
        // this will throw if result.isFailure
        result.get
      }
      if (reify && !cached) {
        result.foreach(_ => {
          ref matchPartial {
            case SilGenitiveReference(
              possessor,
              possessee @ SilOptionallyDeterminedReference(
                SilNounReference(noun), _
              )
            ) => {
              possessor match {
                case SilDeterminedReference(
                  _ : SilNounReference, _ : SilUnlimitedDeterminer
                ) => {
                  // force correlated evaluation after reference resolution
                  resultCollector.refMap.remove(ref)
                }
                case _ => {
                  resultCollector.lookup(possessor).
                    foreach(entities => {
                      entities.foreach(
                        entity => reifyRole(entity, noun, true))
                    })
                  // now clear cache and repeat to pick up the newly
                  // reifed entities
                  resultCollector.refMap.remove(ref)
                  resultCollector.refMap.remove(possessee)
                  resolveReference(ref, context, resultCollector)
                }
              }
            }
          }
        })
      }
    }

    val annotator = resultCollector.annotator
    var currentPredicate : Option[SilPredicate] = None
    def updateContext(
      ref : SilReference, context : SilReferenceContext) : Unit =
    {
      ref matchPartial {
        case ar : SilAnnotatedReference => {
          val note = annotator.getNote(ar)
          note.setContext(context)
          currentPredicate.foreach(pred => note.setPredicate(pred))
        }
      }
    }
    def getContext(ref : SilReference) =
    {
      ref match {
        case ar : SilAnnotatedReference => {
          annotator.getNote(ar).getContext
        }
        case _ => None
      }
    }

    val contextAnalyzer = phraseQuerier.queryMatcher {
      case sp @ SilStatePredicate(subjectRef, verb, state, modifiers) => {
        currentPredicate = Some(sp)
        updateContext(subjectRef, subjectStateContext(state))
      }
      case rp @ SilRelationshipPredicate(
        subjectRef, verb, complementRef, modifiers
      ) => {
        currentPredicate = Some(rp)
        updateContext(subjectRef, relationshipSubjectContext(verb))
        val (context, categoryLabel) =
          relationshipComplementContext(verb, complementRef)
        if (SprRelationshipPredef(verb) == REL_PREDEF_ASSOC) {
          if (extractRoleQualifiers(complementRef).size != 1) {
            updateContext(complementRef, context)
          }
        } else {
          if (categoryLabel.isEmpty) {
            updateContext(complementRef, context)
          }
        }
      }
      case ap @ SilActionPredicate(subject, verb, directObject, modifiers) => {
        currentPredicate = Some(ap)
        updateContext(subject, REF_SUBJECT)
        directObject.foreach(updateContext(_, REF_DIRECT_OBJECT))
      }
      case ap : SilAdpositionalPhrase => {
        updateContext(ap.objRef, REF_ADPOSITION_OBJ)
      }
      case SilGenitiveReference(possessor, possessee) => {
        updateContext(possessor, REF_GENITIVE_POSSESSOR)
      }
      case ssr @ SilStateSpecifiedReference(sub, _) => {
        if (resultCollector.analyzingAssertion) {
          ssr.state match {
            case _ : SilPropertyState => {
              updateContext(sub, REF_SPECIFIED)
            }
            case _ => {
              getContext(ssr).foreach(context => {
                updateContext(sub, context)
              })
              updateContext(ssr, REF_SPECIFIED)
            }
          }
        } else {
          updateContext(sub, REF_SPECIFIED)
        }
      }
      case SilDeterminedReference(_, _) => {
      }
      case ar @ SilAppositionalReference(primary, _) => {
        getContext(ar).foreach(context => {
          updateContext(primary, context)
        })
      }
      case ref : SilReference => {
        getContext(ref).foreach(context => {
          ref.childReferences.foreach(child => updateContext(child, context))
        })
      }
    }
    val referenceResolver = phraseQuerier.queryMatcher {
      case ref : SilReference => {
        getContext(ref).foreach(context => {
          if (context != REF_SPECIFIED) {
            resolveOne(ref, context)
          }
        })
      }
    }
    resultCollector.suppressWildcardExpansion += 1
    try {
      // first pass top-down to determine contexts
      phraseQuerier.query(
        contextAnalyzer, phrase, SilRewriteOptions(topDown = true))
      // then again bottom up to resolve references in contexts
      phraseQuerier.query(
        referenceResolver, phrase)
    } catch {
      case ex : Exception => {
        return Failure(ex)
      }
    } finally {
      resultCollector.suppressWildcardExpansion -= 1
    }
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
    verb : SilWord) : SilReferenceContext =
  {
    SprRelationshipPredef(verb) match {
      case REL_PREDEF_IDENTITY | REL_PREDEF_BECOME => REF_COMPLEMENT
      case REL_PREDEF_ASSOC => REF_SUBJECT
    }
  }

  private def relationshipComplementContext(
    verb : SilWord,
    complementRef : SilReference) : (SilReferenceContext, Option[SilWord]) =
  {
    SprRelationshipPredef(verb) match {
      case REL_PREDEF_IDENTITY | REL_PREDEF_BECOME => {
        tupleN((REF_COMPLEMENT, extractCategory(complementRef)))
      }
      case REL_PREDEF_ASSOC => {
        tupleN((REF_SUBJECT, None))
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
    val result = cosmos.evaluateEntityProperty(
      entity, propertyName
    ) match {
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
    val unassumed =
      evaluatePredicateOverReference(
        subjectRef, context, resultCollector
      ) {
        (entity, entityRef) => {
          state match {
            case SilExistenceState(_) => {
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
              cosmos.fail(
                ShlurdExceptionCode.FailedParse,
                sentencePrinter.sb.respondCannotUnderstand)
            }
          }
        }
      }
    assumeExistence(
      unassumed,
      state match {
        case SilExistenceState(_) | SilAdpositionalState(
          SprPredefAdposition(PD_GENITIVE_OF), _
        ) => true
        case _ => false
      }
    )
  }

  private def assumeExistence(
    unassumed : Try[Trilean],
    applicable : Boolean) =
  {
    if (applicable) {
      existenceAssumption match {
        case EXISTENCE_ASSUME_NOTHING => {
          unassumed
        }
        case EXISTENCE_ASSUME_UNKNOWN => {
          unassumed match {
            case Success(Trilean.False) => {
              Success(Trilean.Unknown)
            }
            case _ => unassumed
          }
        }
      }
    } else {
      unassumed
    }
  }

  private def evaluateRelationshipPredicate(
    subjectRef : SilReference,
    subjectEntity : EntityType,
    complementRef : SilReference,
    complementEntity : EntityType,
    verb : SilWord) : Try[Trilean] =
  {
    SprRelationshipPredef(verb) match {
      case REL_PREDEF_IDENTITY  => {
        val result = {
          if ((SmcPhraseQuerier.containsWildcard(subjectRef) ||
            SmcPhraseQuerier.containsWildcard(complementRef)) &&
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
      case REL_PREDEF_BECOME => {
        // maybe we could answer in the case of
        // "did Peter become a superhero?"
        Success(Trilean.Unknown)
      }
      case REL_PREDEF_ASSOC => {
        val roleQualifiers = extractRoleQualifiers(complementRef)
        val result = mind.evaluateEntityAdpositionPredicate(
          complementEntity, subjectEntity,
          SprPredefAdposition(PD_GENITIVE_OF), roleQualifiers)
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
    specifiedState : SilState = SilNullState(),
    enclosingDeterminer : SilDeterminer = DETERMINER_ABSENT
  )(evaluator : EntityPredicateEvaluator)
      : Try[Trilean] =
  {
    trace("EVALUATE PREDICATE OVER REFERENCE : " +
      reference + " WITH CONTEXT " + context + " AND SPECIFIED STATE "
      + specifiedState + " AND DETERMINER " + enclosingDeterminer)
    debugPushLevel()
    val result = evaluatePredicateOverReferenceImpl(
      reference, context, resultCollector,
      specifiedState, None, evaluator, enclosingDeterminer)
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
    val nUnfiltered = unfilteredEntities.size
    if (nUnfiltered > 100) {
      warn(s"OVERSIZE LOAD $nUnfiltered")
    }
    // probably we should be pushing filters down into
    // resolveQualifiedNoun for efficiency
    val adpositionStates =
      SilUtils.extractAdpositionSpecifiers(specifiedState)
    val crossResults =
      new mutable.LinkedHashMap[(EntityType, EntityType), Trilean]
    val entities = {
      if (adpositionStates.isEmpty) {
        unfilteredEntities
      } else {
        unfilteredEntities.filter(subjectEntity =>
          adpositionStates.forall(adp => {
            val adposition = adp.adposition
            val qualifiers : Set[SilWord] = {
              if (adposition == SprPredefAdposition(PD_GENITIVE_OF)) {
                Set(noun)
              } else {
                Set.empty
              }
            }
            val objRef = adp.objRef match {
              case SilConjunctiveReference(
                DETERMINER_ALL,
                references,
                separator
              ) if (resultCollector.resolvingReferences) => {
                // for the purpose of reference resolution,
                // interpret "the guides of Mason and Dixon" as
                // "Mason's guides and Dixon's guides",
                // i.e. "the guides of Mason and/or Dixon"
                annotator.conjunctiveRef(
                  DETERMINER_ANY,
                  references,
                  separator)
              }
              case other => other
            }
            val objCollector = resultCollector.spawn
            val evaluation = evaluatePredicateOverReference(
              objRef, REF_ADPOSITION_OBJ,
              objCollector)
            {
              (objEntity, entityRef) => {
                val result = mind.evaluateEntityAdpositionPredicate(
                  subjectEntity, objEntity, adposition, qualifiers)
                trace("RESULT FOR " +
                  s"$subjectEntity $adposition $objEntity " +
                  s"with $qualifiers is $result")
                result.foreach(trilean => {
                  crossResults.put((subjectEntity, objEntity), trilean)
                })
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
    resultCollector.refMap.put(
      reference, SprUtils.orderedSet(entities))
    val result = determiner match {
      case DETERMINER_DEFINITE | DETERMINER_ABSENT => {
        if (entities.isEmpty &&
          ((count == COUNT_SINGULAR) || (determiner == DETERMINER_DEFINITE)) &&
          ((context == REF_SUBJECT) || (determiner == DETERMINER_DEFINITE))
        ) {
          Failure(ShlurdException(
            ShlurdExceptionCode.NonExistent,
            sentencePrinter.sb.respondNonexistent(noun)))
        } else {
          // FIXME special handling for COUNT_ZERO_PLURAL, COUNT_MASS
          count match {
            case COUNT_PLURAL => {
              val newDeterminer = determiner match {
                case DETERMINER_DEFINITE => DETERMINER_ALL
                case _ => determiner
              }
              evaluateDeterminer(
                entities.map(
                  invokeEvaluator(_, reference, resultCollector, evaluator)),
                newDeterminer)
            }
            case _ => {
              if (entities.isEmpty) {
                Success(Trilean.False)
              } else if (entities.size > 1) {
                if ((determiner == DETERMINER_DEFINITE) ||
                  (noun.isProper && (determiner == DETERMINER_ABSENT)))
                {
                  cosmos.fail(
                    ShlurdExceptionCode.NotUnique,
                    sentencePrinter.sb.respondAmbiguous(noun))
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
    val combinedResults = crossResults.toSeq.map {
      case ((subjectEntity, objectEntity), crossTrilean) => {
        val subjectTrilean = resultCollector.fullEntityMap.get(subjectEntity).
          getOrElse(Trilean.Unknown)
        tupleN((objectEntity, crossTrilean && subjectTrilean))
      }
    }
    combinedResults.groupBy(_._1).foreach {
      case (objectEntity, results) => {
        resultCollector.saveEntityResult(
          objectEntity, results.map(_._2).reduceLeft(_ || _))
      }
    }
    result
  }

  private def cacheReference(
    resultCollector : ResultCollectorType,
    ref : SilReference,
    evaluator : () => Try[Set[EntityType]]) =
  {
    resultCollector.lookup(ref) match {
      case Some(entities) => {
        Success(entities)
      }
      case _ => {
        evaluator()
      }
    }
  }

  private def evaluatePredicateOverNounReference(
    reference : SilReference,
    noun : SilWord,
    determiner : SilDeterminer,
    count : SilCount,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    specifiedEntities : Option[Set[EntityType]],
    evaluator : EntityPredicateEvaluator
  ) : Try[Trilean] =
  {
    val qualifiers = cosmos.qualifierSet(
      SilUtils.extractQualifiers(specifiedState))
    if (resultCollector.suppressWildcardExpansion > 0) {
      val lemma = noun.toNounLemma
      val bail = determiner match {
        case DETERMINER_DEFINITE => false
        case _ : SilUnlimitedDeterminer => {
          context match {
            case REF_GENITIVE_POSSESSOR => false
            case _ => true
          }
        }
        // FIXME this is silly
        case DETERMINER_ABSENT => {
          ((count == COUNT_PLURAL) && (context == REF_COMPLEMENT)) ||
          ((context == REF_GENITIVE_POSSESSOR) && !noun.isProper)||
          (lemma == PD_WHO.toLemma) || (lemma == PD_WHAT.toLemma) ||
          (lemma == PD_WHERE.toLemma) ||
          (qualifiers.contains(PD_ANOTHER.toLemma))
        }
        case DETERMINER_ALL => false
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
            () => scope.resolveQualifiedNoun(
              noun, context,
              qualifiers).map(_.entities)
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
      case Failure(e : ShlurdException) => {
        trace("ERROR", e)
        Failure(e)
      }
      case Failure(e) => {
        trace("ERROR", e)
        cosmos.fail(
          ShlurdExceptionCode.UnknownForm,
          sentencePrinter.sb.respondUnknown(noun))
      }
    }
  }

  private def evaluatePredicateOverReferenceImpl(
    reference : SilReference,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    specifiedEntities : Option[Set[EntityType]],
    evaluator : EntityPredicateEvaluator,
    enclosingDeterminer : SilDeterminer = DETERMINER_ABSENT)
      : Try[Trilean] =
  {
    val refMap = resultCollector.refMap
    reference match {
      case SilCountedNounReference(noun, count) => {
        evaluatePredicateOverNounReference(
          reference,
          noun,
          enclosingDeterminer,
          count,
          context,
          resultCollector,
          specifiedState,
          specifiedEntities,
          evaluator)
      }
      case SilOptionallyDeterminedReference(
        SilCountedNounReference(noun, count), nounDeterminer
      ) => {
        val determiner = nounDeterminer match {
          case DETERMINER_ABSENT => enclosingDeterminer
          case _ => {
            assert(
              enclosingDeterminer == DETERMINER_ABSENT,
              tupleN((enclosingDeterminer, nounDeterminer)))
            nounDeterminer
          }
        }
        evaluatePredicateOverNounReference(
          reference,
          noun,
          determiner,
          count,
          context,
          resultCollector,
          specifiedState,
          specifiedEntities,
          evaluator)
      }
      case prOriginal : SilPronounReference => {
        // FIXME should support phrasing like
        // "hand it to her in the blue dress"
        if (specifiedState != SilNullState()) {
          return Success(Trilean.Unknown)
        }
        val pr = {
          if (resultCollector.swapSpeakerListener) {
            val rewriter = new SmcResponseRewriter(
              mind, communicationContext.flip, annotator)
            rewriter.rewrite(
              rewriter.swapSpeakerListener(refMap), prOriginal)
          } else {
            prOriginal
          }
        }
        val entitiesTry = cacheReference(
          resultCollector,
          reference,
          () => {
            // FIXME need some proper government+binding theory here
            val phraseScope = scope match {
              case _ : SmcPhraseScope[_, _, _, _] => scope
              case _ => {
                new SmcPhraseScope(
                  resultCollector.refMap, scope)
              }
            }
            phraseScope.resolvePronoun(
              annotator, communicationContext, pr
            ).map(out => {
              val entities = out.entities
              refMap.put(reference, entities)
              entities
            })
          }
        )
        entitiesTry.transform(entities => {
          trace(s"CANDIDATE ENTITIES : $entities")
          evaluateDeterminer(
            entities.map(
              invokeEvaluator(_, reference, resultCollector, evaluator)),
            DETERMINER_ALL)
        }, e => {
          trace("ERROR", e)
          Failure(e)
        })
      }
      case SilConjunctiveReference(determiner, references, separator) => {
        assert(enclosingDeterminer == DETERMINER_ABSENT)
        val results = references.map(
          evaluatePredicateOverReference(
            _, context, resultCollector, specifiedState)(evaluator))
        val combinedEntities = references.flatMap(sub => {
          refMap.get(sub) match {
            case Some(entities) => entities
            case _ => Seq.empty
          }
        })
        refMap.put(reference, combinedEntities.toSet)
        evaluateDeterminer(results, determiner)
      }
      case SilParenthesizedReference(sub, _) => {
        val result = evaluatePredicateOverReferenceImpl(
          sub, context, resultCollector, specifiedState,
          specifiedEntities, evaluator, enclosingDeterminer)
        refMap.get(sub).foreach(
          entitySet => refMap.put(reference, entitySet))
        result
      }
      case SilAppositionalReference(primary, _) => {
        val result = evaluatePredicateOverReferenceImpl(
          primary, context, resultCollector, specifiedState,
          specifiedEntities, evaluator, enclosingDeterminer)
        refMap.get(primary).foreach(
          entitySet => refMap.put(reference, entitySet))
        result
      }
      case SilStateSpecifiedReference(sub, subState) => {
        val result = evaluatePredicateOverState(
          sub, subState, context, resultCollector, specifiedState, evaluator,
          enclosingDeterminer)
        refMap.get(sub).foreach(
          entitySet => refMap.put(reference, entitySet))
        result
      }
      case SilDeterminedReference(sub, determiner) => {
        assert(enclosingDeterminer == DETERMINER_ABSENT)
        val actualDeterminer = determiner match {
          case DETERMINER_DEFINITE => {
            sub match {
              case SilStateSpecifiedReference(_, _ : SilAdpositionalState) => {
                if (resultCollector.analyzingAssertion) {
                  DETERMINER_NONSPECIFIC
                } else {
                  determiner
                }
              }
              case _ => determiner
            }
          }
          case _ => determiner
        }
        val result = evaluatePredicateOverReferenceImpl(
          sub, context, resultCollector, specifiedState, specifiedEntities,
          evaluator, actualDeterminer)
        refMap.get(sub).foreach(
          entitySet => refMap.put(reference, entitySet))
        result
      }
      case SilGenitiveReference(possessor, possessee) => {
        refMap.get(reference) match {
          case Some(entities) => {
            evaluatePredicateOverReferenceImpl(
              possessee,
              context,
              resultCollector,
              specifiedState,
              Some(entities),
              evaluator,
              DETERMINER_ABSENT
            )
          }
          case _ => {
            possessee matchPartial {
              case SilOptionallyDeterminedReference(
                SilMandatorySingular(noun),
                DETERMINER_ABSENT | (_ : SilUnlimitedDeterminer)
              ) => {
                resultCollector.lookup(possessor).
                  foreach(entities => {
                    if (entities.size >= 1) {
                      var matched = false
                      val rc = evaluatePredicateOverEntities(
                        entities,
                        possessor,
                        context,
                        resultCollector,
                        SilNullState(),
                        DETERMINER_ANY,
                        COUNT_SINGULAR,
                        noun,
                        {
                          (entity, entityRef) => {
                            cosmos.evaluateEntityProperty(
                              entity, noun.toLemma, true) match
                            {
                              case Success((Some(property), Some(value))) => {
                                matched = true
                                mind.resolvePropertyValueEntity(
                                  property, value) match
                                  {
                                    case Success(valueEntity) => {
                                      val resolved = Set(valueEntity)
                                      // FIXME need to do this additively
                                      // in case of multiple matches?
                                      resultCollector.refMap.put(
                                        reference, resolved)
                                      resultCollector.refMap.put(
                                        possessee, resolved)
                                      evaluator(valueEntity, possessee)
                                    }
                                    case Failure(e) => {
                                      debug("ERROR", e)
                                      Failure(e)
                                    }
                                  }
                              }
                              case Success((Some(property), None)) => {
                                matched = true
                                Success(Trilean.Unknown)
                              }
                              case _ => {
                                Success(Trilean.Unknown)
                              }
                            }
                          }
                        }
                      )
                      if (matched) {
                        return rc
                      }
                    }
                  })
              }
            }
            if (resultCollector.resolvingReferences) {
              possessee matchPartial {
                case SilOptionallyDeterminedReference(
                  SilNounReference(noun),
                  DETERMINER_ABSENT | (_ : SilUnlimitedDeterminer)
                ) => {
                  resultCollector.lookup(possessor).foreach(
                    possessorEntities => {
                      var supported = true
                      val possesseeEntities =
                        possessorEntities.flatMap(possessorEntity => {
                          mind.resolveGenitive(possessorEntity, noun) match {
                            case Success(entities) => entities
                            case Failure(_ : UnsupportedOperationException) => {
                              supported = false
                              Seq.empty
                            }
                            case Failure(error) => return Failure(error)
                          }
                        })
                      if (supported) {
                        val result = evaluatePredicateOverEntities(
                          possesseeEntities,
                          possessee,
                          context,
                          resultCollector,
                          specifiedState,
                          DETERMINER_NONSPECIFIC,
                          COUNT_PLURAL,
                          noun,
                          evaluator)
                        refMap.get(possessee).foreach(filtered => {
                          refMap.put(reference, filtered)
                        })
                        result
                      }
                    }
                  )
                }
              }
            }
            val state = SilAdpositionalState(
              SprPredefAdposition(PD_GENITIVE_OF), possessor)
            val result = evaluatePredicateOverState(
              possessee, state, REF_GENITIVE_POSSESSEE, resultCollector,
              specifiedState, evaluator)
            refMap.get(possessee).foreach(
              entitySet => refMap.put(reference, entitySet))
            result
          }
        }
      }
      case qr : SilQuotationReference => {
        trace("QUOTATION REFERENCE")
        mind.resolveQuotation(qr) match {
          case Success(entity) => {
            evaluator(entity, qr)
          }
          case _ => {
            Success(Trilean.Unknown)
          }
        }
      }
      case _ : SilUnknownReference => {
        debug("UNKNOWN REFERENCE")
        cosmos.fail(
          ShlurdExceptionCode.FailedParse,
          sentencePrinter.sb.respondCannotUnderstand)
      }
    }
  }

  private def evaluatePredicateOverState(
    reference : SilReference,
    state : SilState,
    context : SilReferenceContext,
    resultCollector : ResultCollectorType,
    specifiedState : SilState,
    evaluator : EntityPredicateEvaluator,
    enclosingDeterminer : SilDeterminer = DETERMINER_ABSENT)
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
      reference, context, resultCollector, combinedState,
      enclosingDeterminer)(evaluator)
  }

  protected def evaluatePropertyStatePredicate(
    entity : EntityType,
    entityRef : SilReference,
    state : SilWord,
    resultCollector : ResultCollectorType)
      : Try[Trilean] =
  {
    val result = cosmos.resolvePropertyState(
      entity, cosmos.encodeName(state)
    ) match {
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
          case SilOptionallyDeterminedReference(
            SilCountedNounReference(noun, count), determiner
          ) => {
            val rephrased = noun match {
              case SprPredefWord(PD_WHO) =>
                SilWord(SmcIdeals.FORM_SOMEONE)
              case SprPredefWord(PD_WHOM) =>
                SilWord(SmcIdeals.FORM_SOMEONE)
              case SprPredefWord(PD_WHAT) =>
                SprPredefWord(PD_THAT)
              case SprPredefWord(PD_WHERE) =>
                SilWord(SmcIdeals.ROLE_CONTAINER)
              case _ => noun
            }
            val rephrasedDeterminer = determiner match {
              case _ : SilUnlimitedDeterminer => DETERMINER_NONSPECIFIC
              case _ => determiner
            }
            annotator.determinedNounRef(rephrased, rephrasedDeterminer, count)
          }
          case _ => {
            mind.specificReference(annotator, entity, DETERMINER_NONSPECIFIC)
          }
        }
        cosmos.fail(
          ShlurdExceptionCode.UnknownState,
          sentencePrinter.sb.respondUnknownState(
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
        val result = mind.evaluateEntityAdpositionPredicate(
          subjectEntity, objEntity, adposition)
        trace("RESULT FOR " +
          s"$subjectEntity $adposition $objEntity is $result")
        result
      }
    }
  }

  private def extractCategory(reference : SilReference) : Option[SilWord] =
  {
    // FIXME:  support qualifiers etc
    reference match {
      case SilDeterminedReference(
        SilMandatorySingular(noun),
        DETERMINER_NONSPECIFIC
      ) => {
        Some(noun)
      }
      case _ => {
        None
      }
    }
  }

  private def extractRoleQualifiers(complementRef : SilReference)
      : Set[SilWord] =
  {
    // FIXME:  do something less hacky
    complementRef match {
      case SilOptionallyDeterminedReference(
        SilCountedNounReference(noun, count),
        determiner
      ) => {
        Set(noun)
      }
      case _ => {
        Set.empty
      }
    }
  }

  private def evaluateCategorization(
    entity : EntityType,
    categoryLabel : SilWord) : Try[Trilean] =
  {
    val result = mind.evaluateEntityCategoryPredicate(entity, categoryLabel)
    trace("RESULT FOR " +
      s"$entity IN_CATEGORY " +
      s"$categoryLabel is $result")
    result match {
      case Failure(e) => {
        debug("ERROR", e)
        cosmos.fail(
          ShlurdExceptionCode.UnknownForm,
          sentencePrinter.sb.respondUnknown(
            SilWord(categoryLabel.toNounLemma)))
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
    debugger.slowIncrement()
    val result = evaluator(entity, entityRef)
    result.foreach(resultCollector.saveEntityResult(entity, _))
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
          case DETERMINER_DEFINITE | DETERMINER_ABSENT => {
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
          case _ : SilIndefiniteDeterminer => {
            Success(results.fold(Trilean.False)(_|_))
          }
          case _ => cosmos.fail(
            ShlurdExceptionCode.FailedParse,
            sentencePrinter.sb.respondCannotUnderstand)
        }
      }
    }
  }

  private def chooseResultCollector(
    phrase : SilPhrase,
    collector : ResultCollectorType) =
  {
    if (SmcPhraseQuerier.containsWildcard(phrase, true)) {
      collector
    } else {
      collector.spawn
    }
  }

  private def normalizePredicateWithModifiers(
    resultCollector : ResultCollectorType,
    predicate : SilPredicate
  ) : Try[SilPredicate] =
  {
    val processedModifiers = normalizeModifiers(
      predicate.getModifiers)
    processedModifiers.flatMap(processedModifiers => {
      Success(normalizePredicate(
        resultCollector,
        predicate.withNewModifiers(processedModifiers)))
    })
  }

  protected def normalizePredicate(
    resultCollector : ResultCollectorType,
    predicate : SilPredicate
  ) : SilPredicate =
  {
    predicate
  }
}
