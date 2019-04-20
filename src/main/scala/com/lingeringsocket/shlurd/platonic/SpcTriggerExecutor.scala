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

import SprEnglishLemmas._

import org.slf4j._

object SpcTriggerExecutor
{
  private val logger =
    LoggerFactory.getLogger(
      classOf[SpcTriggerExecutor])
}

class SpcTriggerExecutor(
  mind : SpcMind,
  communicationContext : SmcCommunicationContext[SpcEntity],
  inputRewriter : SmcInputRewriter[SpcEntity, SpcProperty, SpcCosmos])
    extends SmcDebuggable(SmcDebugger.maybe(SpcTriggerExecutor.logger))
{
  private[platonic] def matchTrigger(
    cosmos : SpcCosmos,
    trigger : SilConditionalSentence,
    predicate : SilPredicate,
    referenceMap : mutable.Map[SilReference, Set[SpcEntity]],
    strict : Boolean = true
  ) : Option[SilPredicate] =
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
        if (!mind.isEquivalentVerb(action, actionPredicate.action)) {
          def lemma = actionPredicate.action.toLemma
          trace(s"ACTION $lemma DOES NOT MATCH")
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
              if (strict) {
                trace(s"DIRECT OBJECT MISSING")
                return None
              }
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
              if (strict) {
                trace(s"BASIC VERB MODIFIER $bm MISSING")
                return None
              } else {
                false
              }
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
              ) if (adposition.word.decomposed.toSet.subsetOf(
                // FIXME this is to allow "goes back to" to subsume "goes to"
                // but it's kinda dicey
                actualAdposition.word.decomposed.toSet)
              ) => {
                // FIXME verify that actualRef matches objPattern
                Some(actualRef)
              }
              case _ => None
            })
            if (actualRefs.size != 1) {
              if (strict) {
                trace("VERB MODIFIER PATTERN DOES NOT MATCH")
                return None
              }
            } else {
              val objPattern = SilNounReference(
                objNoun, DETERMINER_UNIQUE, COUNT_SINGULAR)
              replacements.put(objPattern, actualRefs.head)
            }
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
    def replaceReferences = rewriter.replacementMatcher(
      "replaceReferences", {
        case ref : SilReference => {
          replacements.get(ref).getOrElse(ref)
        }
      }
    )
    val newPredicate = rewriter.rewrite(replaceReferences, consequent)
    if (isTraceEnabled) {
      debug(s"TRIGGER MATCH SUCCESSFUL")
    } else {
      debug(s"TRIGGER MATCH SUCESSFUL ${trigger}")
    }
    debug(s"TRIGGER ON ${predicate}\nPRODUCES ${newPredicate}")
    Some(newPredicate)
  }

  private def prepareReplacement(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    referenceMap : mutable.Map[SilReference, Set[SpcEntity]]) : Boolean =
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
    referenceMap : mutable.Map[SilReference, Set[SpcEntity]]) : Boolean =
  {
    // FIXME support other reference patterns
    ref match {
      case SilNounReference(
        noun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
      ) => {
        val resolvedForm = mind.resolveForm(noun)
        val patternRef = SilNounReference(
          noun, DETERMINER_UNIQUE, COUNT_SINGULAR)
        if (inputRewriter.containsWildcard(actualRef)) {
          val wildcardRef = actualRef match {
            // FIXME need the same treatment for other variables, but
            // with intersectionality
            case SilNounReference(
              SilWordLemma(LEMMA_WHAT),
              DETERMINER_ANY,
              count
            ) => {
              resolvedForm match {
                case Some(restrictedForm) => {
                  SilNounReference(
                    SilWord(restrictedForm.name),
                    DETERMINER_ANY,
                    count
                  )
                }
                case _ => actualRef
              }
            }
            case _ => actualRef
          }
          replacements.put(patternRef, wildcardRef)
          return true
        }
        val (candidateRef, entities) = actualRef match {
          case pr : SilPronounReference => {
            mind.resolvePronoun(communicationContext, pr) match {
              case Success(entities) if (!entities.isEmpty) => {
                tupleN((
                  mind.specificReferences(entities),
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
          case SilQuotationReference(_) => {
            tupleN((actualRef, Set.empty[SpcEntity]))
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
        val replacementRef = resolvedForm match {
          case Some(form) => {
            val filtered = entities.filter(entity => {
              cosmos.getGraph.isHyponym(entity.form, form)
            })
            if (filtered.isEmpty) {
              trace(s"NO FORM ${entities.map(_.form)} MATCHES $form")
              return false
            }
            val conjunction = {
              if (filtered.size == 1) {
                mind.specificReference(filtered.head, DETERMINER_UNIQUE)
              } else {
                SilConjunctiveReference(
                  DETERMINER_ALL,
                  filtered.toSeq.map(entity => {
                    val entityRef = mind.specificReference(
                      entity, DETERMINER_UNIQUE)
                    referenceMap.put(entityRef, Set(entity))
                    entityRef
                  })
                )
              }
            }
            referenceMap.put(conjunction, filtered)
            conjunction
          }
          case _ => candidateRef
        }
        replacements.put(patternRef, replacementRef)
        true
      }
      case _ => {
        trace(s"REFERENCE PATTERN $ref UNSUPPORTED")
        false
      }
    }
  }
}
