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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import scala.collection._
import scala.util._

import spire.math._

sealed trait SpcBeliefAcceptance
case object ACCEPT_NO_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_NEW_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_MODIFIED_BELIEFS extends SpcBeliefAcceptance

class SpcInterpreter(
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
  params : SmcResponseParams = SmcResponseParams(),
  executor : SmcExecutor[SpcEntity] = new SmcExecutor[SpcEntity]
) extends SmcInterpreter[
  SpcEntity, SpcProperty, SpcCosmos, SpcMind
](
  mind, params, executor
)
{
  private val already = new mutable.HashSet[SilPredicate]

  private var referenceMapOpt
      : Option[mutable.Map[SilReference, Set[SpcEntity]]] = None

  private val typeMemo = new mutable.LinkedHashMap[SilReference, SpcForm]

  override protected def spawn(subMind : SpcMind) =
  {
    new SpcInterpreter(subMind, beliefAcceptance, params)
  }

  override protected def newPredicateEvaluator() =
    new SmcPredicateEvaluator[SpcEntity, SpcProperty, SpcCosmos, SpcMind](
      mind, sentencePrinter, debugger)
  {
    override protected def evaluateActionPredicate(
      predicate : SilActionPredicate,
      resultCollector : ResultCollectorType) : Try[Trilean] =
    {
      if (checkCycle(predicate, already)) {
        return fail(sentencePrinter.sb.circularAction)
      }
      mind.getCosmos.getTriggers.filter(_.biconditional).foreach(trigger => {
        matchTrigger(mind.getCosmos, trigger, predicate,
          referenceMapOpt.get) match
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
      val triggers = mind.getCosmos.getTriggers.filter(_.biconditional)
      val replacements = triggers.flatMap(trigger => {
        matchTrigger(mind.getCosmos, trigger, stateNormalized, referenceMap)
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

  override protected def interpretImpl(
    sentence : SilSentence, resultCollector : ResultCollectorType)
      : (SilSentence, String) =
  {
    try {
      attemptInterpret(sentence, resultCollector)
    } finally {
      already.clear
      referenceMapOpt = None
      typeMemo.clear
    }
  }

  private def attemptInterpret(
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
      interpretBeliefOrAction(
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
    super.interpretImpl(sentence, resultCollector)
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
          SilNounReference(noun, DETERMINER_ANY, count),
          complement,
          REL_IDENTITY,
          modifiers
        ) => {
          val form = deriveType(complement)
          if (mind.getCosmos.formHasProperty(form, noun.lemma)) {
            val statePredicate = SilStatePredicate(
              complement,
              SilPropertyQueryState(noun.lemma),
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

  private def interpretBeliefOrAction(
    forkedCosmos : SpcCosmos,
    sentence : SilSentence,
    resultCollector : ResultCollectorType)
      : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    val beliefInterpreter =
      new SpcBeliefInterpreter(
        forkedCosmos,
        (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS),
        resultCollector)
    attemptAsBelief(beliefInterpreter, sentence).foreach(result => {
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
        val result = interpretTriggerablePredicate(forkedCosmos, predicate)
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

  private def attemptAsBelief(
    beliefInterpreter : SpcBeliefInterpreter,
    sentence : SilSentence) : Option[String] =
  {
    beliefInterpreter.recognizeBeliefs(sentence) match {
      case beliefs : Seq[SpcBelief] if (!beliefs.isEmpty) => {
        beliefs.foreach(belief => {
          debug(s"APPLYING NEW BELIEF : $belief")
          try {
            beliefInterpreter.applyBelief(belief)
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

  private def interpretTriggerablePredicate(
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

  private def applyTrigger(
    forkedCosmos : SpcCosmos,
    trigger : SilConditionalSentence,
    predicate : SilPredicate)
      : Option[String] =
  {
    val resultCollector = new SmcResultCollector[SpcEntity](referenceMapOpt.get)
    matchTrigger(
      forkedCosmos, trigger, predicate, referenceMapOpt.get) match
    {
      case Some(newPredicate) => {
        if (checkCycle(newPredicate, already)) {
          return Some(sentencePrinter.sb.circularAction)
        }
        val newSentence = SilPredicateSentence(newPredicate)
        spawn(imagine(forkedCosmos)).resolveReferences(
          newSentence, resultCollector, false, true)
        if (trigger.tamConsequent.modality == MODAL_MUST) {
          evaluateTamPredicate(
            newPredicate, SilTam.indicative, resultCollector) match
          {
            case Success(Trilean.True) => {
              None
            }
            // FIXME what about unknown or error?
            case _ => {
              // FIXME i18n
              Some("But " + sentencePrinter.printPredicateStatement(
                newPredicate, SilTam.indicative.negative) + ".")
            }
          }
        } else {
          val result = interpretBeliefOrAction(
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

  private def checkCycle(
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate]) : Boolean =
  {
    // FIXME make limit configurable and add test
    if (seen.contains(predicate) || (seen.size > 100)) {
      true
    } else {
      seen += predicate
      false
    }
  }

  private def matchTrigger(
    cosmos : SpcCosmos,
    trigger : SilConditionalSentence,
    predicate : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Option[SilPredicate] =
  {
    trace(s"ATTEMPT TRIGGER MATCH $trigger")
    val antecedent = trigger.antecedent
    val consequent = trigger.consequent
    val replacements = new mutable.LinkedHashMap[SilReference, SilReference]
    antecedent match {
      case SilStatePredicate(
        subject, state, modifiers
      ) => {
        val statePredicate = predicate match {
          case sp : SilStatePredicate => sp
          case _ => {
            trace(s"PREDICATE $predicate IS NOT A STATE")
            return None
          }
        }
        tupleN((state, statePredicate.state)) match {
          // FIXME allow other variable patterns
          case (
            SilAdpositionalState(adp1, obj1),
            SilAdpositionalState(adp2, obj2)
          ) if (adp1 == adp2) => {
            if (!prepareReplacement(
              cosmos, replacements, obj1,
              obj2, referenceMap))
            {
              return None
            }
          }
          case _ => {
            if (state != statePredicate.state) {
              trace(s"STATE $state " +
                s"DOES NOT MATCH ${statePredicate.state}")
              return None
            }
          }
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, statePredicate.subject, referenceMap))
        {
          return None
        }
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
        val relPredicate = predicate match {
          case rp : SilRelationshipPredicate => rp
          case _ => {
            trace(s"PREDICATE ${predicate} IS NOT A RELATIONSHIP")
            return None
          }
        }
        if (relationship != relPredicate.relationship) {
          trace(s"RELATIONSHIP ${relPredicate.relationship} DOES NOT MATCH")
          return None
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, relPredicate.subject, referenceMap))
        {
          return None
        }
        relationship match {
          case REL_IDENTITY => {
            if (!prepareReplacement(
              cosmos, replacements, complement,
              relPredicate.complement, referenceMap))
            {
              return None
            }
          }
          case REL_ASSOCIATION => {
            if (!prepareReplacement(
              cosmos, replacements, complement, relPredicate.complement,
              referenceMap))
            {
              return None
            }
          }
        }
      }
      case SilActionPredicate(
        subject, action, directObject, modifiers
      ) => {
        val actionPredicate = predicate match {
          case ap : SilActionPredicate => ap
          case _ => {
            trace(s"PREDICATE ${predicate} IS NOT AN ACTION")
            return None
          }
        }
        if (action.lemma != actionPredicate.action.lemma) {
          trace(s"ACTION ${actionPredicate.action.lemma} DOES NOT MATCH")
          return None
        }
        // FIXME detect colliding replacement nouns e.g.
        // "if an object hits an object"
        if (!prepareReplacement(
          cosmos, replacements, subject, actionPredicate.subject,
          referenceMap))
        {
          return None
        }
        directObject.foreach(obj => {
          actionPredicate.directObject match {
            case Some(actualObj) => {
              if (!prepareReplacement(
                cosmos, replacements, obj, actualObj, referenceMap))
              {
                return None
              }
            }
            case _ => {
              trace(s"DIRECT OBJECT MISSING")
              return None
            }
          }
        })
        // FIXME support multiple modifiers and other patterns
        val filteredModifiers = modifiers.filterNot(_ match {
          case bm : SilBasicVerbModifier => {
            if (actionPredicate.modifiers.contains(bm)) {
              // matched:  discard
              true
            } else {
              trace(s"BASIC VERB MODIFIER $bm MISSING")
              return None
            }
          }
          // keep
          case _ => false
        })
        filteredModifiers match {
          case Seq(SilAdpositionalVerbModifier(
            adposition, SilNounReference(
              objNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR))
          ) => {
            // FIXME some modifiers (e.g. "never") might
            // negate the match
            val actualRefs = actionPredicate.modifiers.flatMap(_ match {
              case SilAdpositionalVerbModifier(
                actualAdposition, actualRef
              ) if (adposition.words.toSet.subsetOf(
                // FIXME this is to allow "goes back to" to subsume "goes to"
                // but it's kinda dicey
                actualAdposition.words.toSet)
              ) => {
                // FIXME verify that actualRef matches objPattern
                Some(actualRef)
              }
              case _ => None
            })
            if (actualRefs.size != 1) {
              trace("VERB MODIFIER PATTERN DOES NOT MATCH")
              return None
            }
            val objPattern = SilNounReference(
              objNoun, DETERMINER_UNIQUE, COUNT_SINGULAR)
            replacements.put(objPattern, actualRefs.head)
          }
          case Seq() => {
          }
          case _ => {
            trace("VERB MODIFIER PATTERN UNSUPPORTED")
            return None
          }
        }
      }
      case _ => {
        trace("ANTECEDENT PATTERN UNSUPPORTED")
        return None
      }
    }
    val rewriter = new SilPhraseRewriter
    def replaceReferences = rewriter.replacementMatcher {
      case ref : SilReference => {
        replacements.get(ref).getOrElse(ref)
      }
    }
    val newPredicate = rewriter.rewrite(replaceReferences, consequent)
    Some(newPredicate)
  }


  private def prepareReplacement(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Boolean =
  {
    val patternMatched = prepareReplacementImpl(
      cosmos,
      replacements,
      ref,
      actualRef,
      referenceMap)
    if (patternMatched) {
      true
    } else {
      // FIXME we should prefer entity comparisons instead, but for that
      // we need two referenceMaps simultaneously
      if (ref != actualRef) {
        trace(s"PHRASE ${ref} DOES NOT MATCH ${actualRef}")
        false
      } else {
        true
      }
    }
  }

  private def prepareReplacementImpl(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    referenceMap : Map[SilReference, Set[SpcEntity]]) : Boolean =
  {
    // FIXME support other reference patterns
    ref match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        val resolvedForm = cosmos.resolveForm(noun.lemma)
        val patternRef = SilNounReference(
          noun, DETERMINER_UNIQUE, COUNT_SINGULAR)
        if (inputRewriter.containsWildcard(actualRef)) {
          replacements.put(patternRef, actualRef)
          return true
        }
        val (candidateRef, entities) = actualRef match {
          case pr : SilPronounReference => {
            mind.resolvePronoun(pr) match {
              case Success(entities) if (!entities.isEmpty) => {
                tupleN((
                  mind.getCosmos.specificReferences(entities),
                  entities))
              }
              case _ =>  {
                trace(s"PRONOUN $pr UNRESOLVED")
                return false
              }
            }
          }
          case SilNounReference(_, DETERMINER_NONSPECIFIC, _) => {
            trace(s"NONSPECIFIC REFERENCE $actualRef")
            return false
          }
          case _ => {
            referenceMap.get(actualRef) match {
              case Some(entities) => {
                tupleN((actualRef, entities))
              }
              case _ => {
                if (resolvedForm.isEmpty) {
                  tupleN((actualRef, Set.empty[SpcEntity]))
                } else {
                  trace(s"UNRESOLVED REFERENCE $actualRef")
                  return false
                }
              }
            }
          }
        }
        resolvedForm.foreach(form => {
          entities.foreach(entity => {
            if (!cosmos.getGraph.isHyponym(entity.form, form)) {
              trace(s"FORM ${entity.form} DOES NOT MATCH $form")
              return false
            }
          })
        })
        replacements.put(patternRef, candidateRef)
        true
      }
      case _ => {
        trace(s"REFERENCE PATTERN $ref UNSUPPORTED")
        false
      }
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
    mind.getCosmos.instantiateForm(SilWord(SpcMeta.ENTITY_METAFORM_NAME))
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
        case SilGenitiveReference(_, SilNounReference(noun, _, _)) => {
          // FIXME probably the possessor's type should be used for scoping
          // here?  Also need to handle properties.
          cosmos.resolveRole(noun.lemma) match {
            case Some(role) => {
              lcaType(cosmos.getGraph.getFormsForRole(role).toSet)
            }
            case _ => unknownType
          }
        }
        case SilNounReference(noun, _, _) => {
          // FIXME resolve roles as well?
          if (noun.isProper) {
            cosmos.getEntityBySynonym(noun.lemma).map(_.form).
              getOrElse(unknownType)
          } else {
            cosmos.resolveForm(noun.lemma).getOrElse(unknownType)
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
        eventReferenceMap, resultCollector, applyBindings)
      if (superMatch.isFailure) {
        return superMatch
      }
      if (superMatch.get) {
        return Success(true)
      } else {
        mind.getCosmos.getTriggers.foreach(trigger => {
          matchTrigger(mind.getCosmos, trigger,
            predicate, eventReferenceMap) match
          {
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
