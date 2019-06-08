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

object SpcAssertionMapper
{
  private val logger =
    LoggerFactory.getLogger(classOf[SpcAssertionMapper])
}

class SpcAssertionMapper(
  mind : SpcMind,
  communicationContext : SmcCommunicationContext[SpcEntity],
  inputRewriter : SmcInputRewriter[SpcEntity, SpcProperty, SpcCosmos])
    extends SmcDebuggable(new SmcDebugger(SpcAssertionMapper.logger))
{
  private[platonic] def matchSubsumption(
    cosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    referenceMap : Map[SilReference, Set[SpcEntity]]
  ) : Boolean =
  {
    val operator = "SUBSUMES"
    trace(s"ATTEMPT MATCH $general $operator $specific")
    val matched = matchGeneralization(
      cosmos, general, specific,
      referenceMap,
      None,
      0
    )._1
    if (matched && !isTraceEnabled) {
      debug(s"MATCH SUCCESSFUL $general $operator $specific}")
    }
    matched
  }

  private[platonic] def matchImplication(
    operator : String,
    cosmos : SpcCosmos,
    implication : SilConditionalSentence,
    predicate : SilPredicate,
    referenceMapIn : Map[SilReference, Set[SpcEntity]],
    referenceMapOut : Option[mutable.Map[SilReference, Set[SpcEntity]]]
  ) : Option[SilPredicate] =
  {
    matchImplicationPlusAlternative(
      operator,
      cosmos, implication.antecedent, implication.consequent,
      predicate, Seq.empty, None,
      referenceMapIn, referenceMapOut, 0)._1
  }

  private[platonic] def matchImplicationPlusAlternative(
    operator : String,
    cosmos : SpcCosmos,
    antecedent : SilPredicate,
    consequent : SilPredicate,
    predicate : SilPredicate,
    additionalConsequents : Seq[SilPredicateSentence],
    alternative : Option[SilPredicateSentence],
    referenceMapIn : Map[SilReference, Set[SpcEntity]],
    referenceMapOut : Option[mutable.Map[SilReference, Set[SpcEntity]]],
    triggerDepth : Int
  ) : (
    Option[SilPredicate],
    Seq[SilPredicateSentence],
    Option[SilPredicateSentence]) =
  {
    trace(s"ATTEMPT MATCH $antecedent $operator $consequent")
    val (matched, replacements) = matchGeneralization(
      cosmos,
      antecedent,
      predicate,
      referenceMapIn,
      referenceMapOut,
      triggerDepth)
    if (matched) {
      if (!isTraceEnabled) {
        debug(s"MATCH SUCCESSFUL $antecedent $operator $consequent}")
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
      debug(s"MATCH FROM ${predicate}\nPRODUCES $newPredicate")
      val newAdditional = additionalConsequents.map(sentence => {
        rewriter.rewrite(replaceReferences, sentence)
      })
      newAdditional.foreach(sentence => {
        debug(s"WITH ADDITIONAL CONSEQUENT $sentence")
      })
      val newAlternative = alternative.map(sentence => {
        rewriter.rewrite(replaceReferences, sentence)
      })
      newAlternative.foreach(sentence => {
        debug(s"WITH ALTERNATIVE $sentence")
      })
      tupleN((Some(newPredicate), newAdditional, newAlternative))
    } else {
      tupleN((None, Seq.empty, None))
    }
  }

  private def isEquivalentVerb(
    verb1 : SilWord, verb2 : SilWord, triggerDepth : Int) : Boolean =
  {
    if (triggerDepth > 0) {
      verb1.toLemma == verb2.toLemma
    } else {
      mind.isEquivalentVerb(verb1, verb2)
    }
  }

  private def prepareReplacement(
    cosmos : SpcCosmos,
    replacements : mutable.Map[SilReference, SilReference],
    ref : SilReference,
    actualRef : SilReference,
    referenceMapIn : Map[SilReference, Set[SpcEntity]],
    referenceMapOut : Option[mutable.Map[SilReference, Set[SpcEntity]]]
  ) : Boolean =
  {
    val patternMatched = prepareReplacementImpl(
      cosmos,
      replacements,
      ref,
      actualRef,
      referenceMapIn,
      referenceMapOut)
    if (patternMatched) {
      true
    } else {
      // FIXME we should prefer entity comparisons instead, but for that
      // we need two referenceMaps simultaneously
      if (ref != actualRef) {
        trace(s"PHRASE $ref DOES NOT MATCH $actualRef")
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
    referenceMapIn : Map[SilReference, Set[SpcEntity]],
    referenceMapOut : Option[mutable.Map[SilReference, Set[SpcEntity]]]
  ) : Boolean =
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
            referenceMapIn.get(actualRef) match {
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
              cosmos.isHyponym(entity.form, form)
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
                    referenceMapOut.foreach(_.put(entityRef, Set(entity)))
                    entityRef
                  })
                )
              }
            }
            referenceMapOut.foreach(_.put(conjunction, filtered))
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

  private[platonic] def matchGeneralization(
    cosmos : SpcCosmos,
    general : SilPredicate,
    specific : SilPredicate,
    referenceMapIn : Map[SilReference, Set[SpcEntity]],
    referenceMapOut : Option[mutable.Map[SilReference, Set[SpcEntity]]],
    triggerDepth : Int
  ) : (Boolean, Map[SilReference, SilReference]) =
  {
    val replacements = new mutable.LinkedHashMap[SilReference, SilReference]
    def unmatched() = tupleN((false, replacements))
    general match {
      // FIXME match on verb; e.g. "is" implies "is" and "becomes" implies "is",
      // but "is" may not imply "becomes"
      case SilStatePredicate(
        subject, _, state, modifiers
      ) => {
        val statePredicate = specific match {
          case sp : SilStatePredicate => sp
          case _ => {
            trace(s"PREDICATE $specific IS NOT A STATE")
            return unmatched
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
              obj2, referenceMapIn, referenceMapOut))
            {
              return unmatched
            }
          }
          case _ => {
            if (state != statePredicate.state) {
              trace(s"STATE $state " +
                s"DOES NOT MATCH ${statePredicate.state}")
              return unmatched
            }
          }
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, statePredicate.subject,
          referenceMapIn, referenceMapOut))
        {
          return unmatched
        }
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        val relPredicate = specific match {
          case rp : SilRelationshipPredicate => rp
          case _ => {
            trace(s"PREDICATE $specific IS NOT A RELATIONSHIP")
            return unmatched
          }
        }
        if (verb != relPredicate.verb) {
          trace(s"RELATIONSHIP ${relPredicate.verb} DOES NOT MATCH")
          return unmatched
        }
        if (!prepareReplacement(
          cosmos, replacements, subject, relPredicate.subject,
          referenceMapIn, referenceMapOut))
        {
          return unmatched
        }
        if (!prepareReplacement(
          cosmos, replacements, complement,
          relPredicate.complement,
          referenceMapIn, referenceMapOut))
        {
          return unmatched
        }
      }
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        val actionPredicate = specific match {
          case ap : SilActionPredicate => ap
          case _ => {
            trace(s"PREDICATE $specific IS NOT AN ACTION")
            return unmatched
          }
        }
        if (!isEquivalentVerb(verb, actionPredicate.verb, triggerDepth)) {
          def lemma = actionPredicate.verb.toLemma
          trace(s"ACTION $lemma DOES NOT MATCH")
          return unmatched
        }
        // FIXME detect colliding replacement nouns e.g.
        // "if an object hits an object"
        if (!prepareReplacement(
          cosmos, replacements, subject, actionPredicate.subject,
          referenceMapIn, referenceMapOut))
        {
          return unmatched
        }
        directObject.foreach(obj => {
          actionPredicate.directObject match {
            case Some(actualObj) => {
              if (!prepareReplacement(
                cosmos, replacements, obj, actualObj,
                referenceMapIn, referenceMapOut))
              {
                return unmatched
              }
            }
            case _ => {
              trace(s"DIRECT OBJECT MISSING")
              return unmatched
            }
          }
        })
        // FIXME support multiple modifiers and other patterns
        val filteredModifiers = modifiers.filterNot(_ match {
          case staticModifier @ (
            _ : SilBasicVerbModifier |
              // FIXME should be using reference resolution for matching
              // instead
              SilAdpositionalVerbModifier(
                _, SilNounReference(_, DETERMINER_UNIQUE, _)
              )
          ) => {
            if (actionPredicate.modifiers.contains(staticModifier)) {
              // matched:  discard
              true
            } else {
              trace(s"VERB MODIFIER $staticModifier MISSING")
              return unmatched
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
                SilAdposition(adposition.word), actualRef
              ) => {
                // FIXME verify that actualRef matches objPattern
                Some(actualRef)
              }
              case _ => None
            })
            if (actualRefs.size != 1) {
              trace("VERB MODIFIER PATTERN DOES NOT MATCH")
              return unmatched
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
            return unmatched
          }
        }
      }
      case _ => {
        trace("ANTECEDENT PATTERN UNSUPPORTED")
        return unmatched
      }
    }
    if (isTraceEnabled) {
      debug("MATCH SUCCESSFUL")
    }
    tupleN((true, replacements))
  }
}