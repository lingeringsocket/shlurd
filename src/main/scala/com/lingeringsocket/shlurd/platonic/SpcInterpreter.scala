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

import com.lingeringsocket.shlurd.parser._
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
  params : SmcResponseParams = SmcResponseParams()
) extends SmcInterpreter[
  SpcEntity, SpcProperty, SpcCosmos, SpcMind
](
  mind, params
)
{
  private val already = new mutable.HashSet[SilPredicate]

  private var referenceMap : Option[Map[SilReference, Set[SpcEntity]]] = None

  private val typeMap = new mutable.LinkedHashMap[SilReference, SpcForm]

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
      resultCollector : SmcResultCollector[SpcEntity]) : Try[Trilean] =
    {
      if (checkCycle(predicate)) {
        return fail(sentencePrinter.sb.circularAction)
      }
      mind.getCosmos.getTriggers.foreach(trigger => {
        // technically we should require iff for trigger instead of just if,
        // but let's not split hairs
        matchTrigger(mind.getCosmos, trigger, predicate) match {
          case Some(newPredicate) => {
            return super.evaluatePredicate(newPredicate, resultCollector)
          }
          case _ =>
        }
      })
      super.evaluateActionPredicate(predicate, resultCollector)
    }
  }

  override protected def imagine(
    alternateCosmos : SpcCosmos) =
  {
    new SpcMind(alternateCosmos.fork)
  }

  override protected def interpretImpl(sentence : SilSentence)
      : (SilSentence, String) =
  {
    try {
      attemptInterpret(sentence)
    } finally {
      already.clear
      referenceMap = None
      typeMap.clear
    }
  }

  private def attemptInterpret(sentence : SilSentence)
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
              (SmcTimeInterval.NEXT_INSTANT, predicate, mind.getCosmos, false)
            } else {
              val interval = Interval.point[SmcTimePoint](
                SmcRelativeTimePoint(
                  temporalRefs(iTemporal).get))
              val temporalCosmos = mind.getTemporalCosmos(interval)
              (interval,
                predicate.withNewModifiers(
                  predicate.getModifiers.patch(iTemporal, Seq.empty, 1)),
                temporalCosmos,
                true)
            }
          }
          (interval, Some(predicateOpt), baselineCosmos, temporal)
        }
        case _ => {
          (SmcTimeInterval.NEXT_INSTANT, None, mind.getCosmos, false)
        }
      }

      val beliefInterpreter =
        new SpcBeliefInterpreter(
          baselineCosmos.fork,
          (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS))
      val inputSentence =
        predicateOpt.map(
          SilPredicateSentence(_, sentence.tam)).getOrElse(sentence)
      interpretBeliefOrAction(
        beliefInterpreter, inputSentence
      ) match {
        case Some(result) => {
          if (result != sentencePrinter.sb.respondCompliance) {
            return wrapResponseText(result)
          }
          if (mind.hasNarrative) {
            predicateOpt.foreach(predicate => {
              val updatedCosmos = freezeCosmos(beliefInterpreter.cosmos)
              try {
                updateNarrative(
                  interval,
                  updatedCosmos,
                  predicate,
                  referenceMap.get)
              } catch {
                case CausalityViolationExcn(cause) => {
                  return wrapResponseText(cause)
                }
              }
            })
          }
          if (!temporal) {
            beliefInterpreter.cosmos.applyModifications
          }
          return wrapResponseText(result)
        }
        case _ =>
      }
    }
    already.clear
    // in case we haven't done this already, need to do it now
    // in case evaluateActionPredicate is called by super
    saveReferenceMap(sentence, mind.getCosmos)
    super.interpretImpl(sentence)
  }

  override protected def rewriteQuery(
    predicate : SilPredicate, question : SilQuestion,
    originalAnswerInflection : SilInflection)
      : (SilPredicate, SilInflection) =
  {
    val (rewritten, answerInflection) =
      super.rewriteQuery(predicate, question, originalAnswerInflection)
    if (question == QUESTION_WHICH) {
      rewritten match {
        case SilRelationshipPredicate(
          SilNounReference(noun, DETERMINER_ANY, count),
          complement,
          REL_IDENTITY,
          modifiers
        ) => {
          // FIXME resequence things so that rewriteReferences is already
          // done by super
          val form = deriveType(
            predicateEvaluator.rewriteReferences(
              complement, SmcResultCollector()))
          if (mind.getCosmos.formHasProperty(form, noun.lemma)) {
            val statePredicate = SilStatePredicate(
              complement,
              SilPropertyQueryState(noun.lemma),
              modifiers
            )
            return (statePredicate, INFLECT_COMPLEMENT)
          }
        }
        case _ =>
      }
    }
    (rewritten, answerInflection)
  }

  private def saveReferenceMap(
    sentence : SilSentence,
    cosmos : SpcCosmos)
  {
    // FIXME what if reentrant invocation needs the references???
    if (!referenceMap.isEmpty) {
      return
    }
    // FIXME this is madness...we make a half-hearted attempt at
    // collecting references here because SpcBeliefInterpreter does
    // not do anything in that regard.
    val resultCollector = SmcResultCollector[SpcEntity]
    val rewriter =
      new SmcReferenceRewriter(
        cosmos,
        new SilSentencePrinter,
        resultCollector,
        SmcResolutionOptions(
          failOnUnknown = false,
          resolveConjunctions = true,
          resolveUniqueDeterminers = true,
          reifyRoles = false))
    // discard the rewrite result; we just want the resultCollector
    // side effects
    rewriter.rewrite(rewriter.rewriteReferences, sentence)
    mind.rememberSentenceAnalysis(resultCollector.referenceMap)
    referenceMap = Some(resultCollector.referenceMap)
  }

  override protected def freezeCosmos(mutableCosmos : SpcCosmos) =
  {
    // FIXME use smart deltas instead of wholesale copy
    val frozenCosmos = new SpcCosmos
    frozenCosmos.copyFrom(mutableCosmos)
    frozenCosmos.asUnmodifiable
  }

  private def interpretBeliefOrAction(
    beliefInterpreter : SpcBeliefInterpreter,
    sentence : SilSentence)
      : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    attemptAsBelief(beliefInterpreter, sentence).foreach(result => {
      if (result != compliance) {
        return Some(result)
      } else {
        matched = true
      }
    })
    // defer until this point so that any newly created entities etc will
    // already have taken effect
    saveReferenceMap(sentence, beliefInterpreter.cosmos)
    sentence match {
      case SilPredicateSentence(predicate, _, _) => {
        val result = interpretTriggerablePredicate(beliefInterpreter, predicate)
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
              return Some(respondContradiction(ex))
            }
          }
          debug("NEW BELIEF ACCEPTED")
        })
        Some(sentencePrinter.sb.respondCompliance)
      }
      case _ => None
    }
  }

  private def interpretTriggerablePredicate(
    beliefInterpreter : SpcBeliefInterpreter,
    predicate : SilPredicate) : Option[String] =
  {
    var matched = false
    val compliance = sentencePrinter.sb.respondCompliance
    mind.getCosmos.getTriggers.foreach(trigger => {
      applyTrigger(
        beliefInterpreter, trigger, predicate
      ).foreach(result => {
        if (result != compliance) {
          return Some(result)
        } else {
          matched = true
        }
      })
    })
    if (matched) {
      Some(compliance)
    } else {
      None
    }
  }

  private def applyTrigger(
    beliefInterpreter : SpcBeliefInterpreter,
    trigger : SilConditionalSentence,
    predicate : SilPredicate)
      : Option[String] =
  {
    matchTrigger(beliefInterpreter.cosmos, trigger, predicate) match {
      case Some(newPredicate) => {
        if (checkCycle(newPredicate)) {
          return Some(sentencePrinter.sb.circularAction)
        }
        val newSentence = SilPredicateSentence(newPredicate)
        val result = interpretBeliefOrAction(
          beliefInterpreter, newSentence)
        if (result.isEmpty) {
          // FIXME i18n
          Some("Invalid consequent")
        } else {
          result
        }
      }
      case _ => None
    }
  }

  private def checkCycle(predicate : SilPredicate) : Boolean =
  {
    // FIXME make limit configurable and add test
    if (already.contains(predicate) || (already.size > 100)) {
      true
    } else {
      already += predicate
      false
    }
  }

  private def matchTrigger(
    cosmos : SpcCosmos,
    trigger : SilConditionalSentence,
    predicate : SilPredicate) : Option[SilPredicate] =
  {
    debug(s"ATTEMPT TRIGGER MATCH $trigger")
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
            debug(s"PREDICATE ${predicate} IS NOT A STATE")
            return None
          }
        }
        // FIXME allow this to be a variable
        if (state != statePredicate.state) {
          debug(s"STATE ${state} " +
            s"DOES NOT MATCH ${statePredicate.state}")
          return None
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, statePredicate.subject))
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
            debug(s"PREDICATE ${predicate} IS NOT A RELATIONSHIP")
            return None
          }
        }
        if (relationship != relPredicate.relationship) {
          debug(s"RELATIONSHIP ${relPredicate.relationship} DOES NOT MATCH")
          return None
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, relPredicate.subject))
        {
          return None
        }
        relationship match {
          case REL_IDENTITY => {
            // FIXME allow this to be a variable too?
            if (complement != relPredicate.complement) {
              debug(s"COMPLEMENT ${complement} " +
                s"DOES NOT MATCH ${relPredicate.complement}")
              return None
            }
          }
          case REL_ASSOCIATION => {
            if (!prepareReplacement(
              cosmos, replacements, complement, relPredicate.complement))
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
            debug(s"PREDICATE ${predicate} IS NOT AN ACTION")
            return None
          }
        }
        if (action.lemma != actionPredicate.action.lemma) {
          debug(s"ACTION ${actionPredicate.action.lemma} DOES NOT MATCH")
          return None
        }
        // FIXME detect colliding replacement nouns e.g.
        // "if an object hits an object"
        if (!prepareReplacement(
          cosmos, replacements, subject, actionPredicate.subject))
        {
          return None
        }
        directObject.foreach(obj => {
          actionPredicate.directObject match {
            case Some(actualObj) => {
              if (!prepareReplacement(cosmos, replacements, obj, actualObj)) {
                return None
              }
            }
            case _ => {
              debug(s"DIRECT OBJECT MISSING")
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
              debug("BASIC VERB MODIFIER MISSING")
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
              debug("VERB MODIFIER PATTERN DOES NOT MATCH")
              return None
            }
            val objPattern = SilNounReference(
              objNoun, DETERMINER_UNIQUE, COUNT_SINGULAR)
            replacements.put(objPattern, actualRefs.head)
          }
          case Seq() => {
          }
          case _ => {
            debug("VERB MODIFIER PATTERN UNSUPPORTED")
            return None
          }
        }
      }
      case _ => {
        debug("ANTECEDENT PATTERN UNSUPPORTED")
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
    actualRef : SilReference) : Boolean =
  {
    // FIXME support other reference patterns
    ref match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        val patternRef = SilNounReference(
          noun, DETERMINER_UNIQUE, COUNT_SINGULAR)
        val candidateRef = actualRef match {
          case pr : SilPronounReference => {
            mind.resolvePronoun(pr) match {
              case Success(set) if (!set.isEmpty) => {
                mind.getCosmos.specificReferences(set, DETERMINER_UNIQUE)
              }
              case _ =>  {
                debug("PRONOUN UNRESOLVED")
                return false
              }
            }
          }
          case SilNounReference(_, DETERMINER_NONSPECIFIC, _) => {
            debug("NONSPECIFIC REFERENCE")
            return false
          }
          case _ => actualRef
        }
        cosmos.resolveForm(noun.lemma).foreach(form => {
          referenceMap.flatMap(_.get(candidateRef)) match {
            case Some(entities) => {
              entities.foreach(_ match {
                case entity : SpcEntity => {
                  if (!cosmos.getGraph.isHyponym(entity.form, form)) {
                    debug("FORM DOES NOT MATCH")
                    return false
                  }
                }
                case _ => {
                  debug("UNEXPECTED ENTITY TYPE")
                  return false
                }
              })
            }
            case _ => {
              debug("UNRESOLVED REFERENCE")
              return false
            }
          }
        })
        replacements.put(patternRef, candidateRef)
        true
      }
      case _ => {
        debug("REFERENCE PATTERN UNSUPPORTED")
        false
      }
    }
  }

  // FIXME:  i18n
  private def respondContradiction(ex : RejectedBeliefExcn) : String =
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

  private[platonic] def deriveType(ref : SilReference) : SpcForm =
  {
    def cosmos = mind.getCosmos
    typeMap.getOrElseUpdate(ref, {
      ref match {
        case SilResolvedReference(entities, _, _) => {
          lcaType(entities.map(_.asInstanceOf[SpcEntity].form))
        }
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
          cosmos.resolveForm(noun.lemma).getOrElse(unknownType)
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
}
