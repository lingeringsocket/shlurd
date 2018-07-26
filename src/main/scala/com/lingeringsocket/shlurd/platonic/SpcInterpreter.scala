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
import com.lingeringsocket.shlurd.cosmos._
import com.lingeringsocket.shlurd.print._

import scala.collection._
import scala.util._

sealed trait SpcBeliefAcceptance
case object ACCEPT_NO_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_NEW_BELIEFS extends SpcBeliefAcceptance
case object ACCEPT_MODIFIED_BELIEFS extends SpcBeliefAcceptance

class SpcInterpreter(
  mind : SpcMind,
  beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
  params : ShlurdResponseParams = ShlurdResponseParams())
    extends ShlurdInterpreter(mind, params)
{
  override protected def interpretImpl(sentence : SilSentence)
      : (SilSentence, String) =
  {
    // FIXME this is madness...we make a half-hearted attempt at
    // collecting references here because SpcBeliefInterpreter does
    // not do anything in that regard.
    if (mind.isConversing) {
      val resultCollector = ResultCollector[SpcEntity]
      val rewriter =
        new ShlurdReferenceRewriter[SpcEntity, SpcProperty](
          mind.getCosmos,
          new SilSentencePrinter,
          resultCollector,
          ShlurdResolutionOptions(
            failOnUnknown = false,
            resolveConjunctions = true,
            resolveUniqueDeterminers = true))
      // discard the rewrite result; we just want the
      // resultCollector side effects
      rewriter.rewrite(rewriter.rewriteReferences, sentence)
      mind.rememberSentenceAnalysis(resultCollector.referenceMap)
    }

    if ((beliefAcceptance != ACCEPT_NO_BELIEFS) &&
      sentence.mood.isIndicative)
    {
      val beliefInterpreter =
        new SpcBeliefInterpreter(
          mind.getCosmos.fork,
          (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS))
      interpretBeliefOrAction(beliefInterpreter, sentence) match {
        case Some(result) => {
          beliefInterpreter.cosmos.applyModifications
          return wrapResponseText(result)
        }
        case _ =>
      }
    }
    super.interpretImpl(sentence)
  }

  private def interpretBeliefOrAction(
    beliefInterpreter : SpcBeliefInterpreter,
    sentence : SilSentence) : Option[String] =
  {
    attemptAsBelief(beliefInterpreter, sentence).foreach(result => {
      return Some(result)
    })
    sentence match {
      case SilPredicateSentence(ap : SilActionPredicate, _, _) => {
        return Some(interpretAction(beliefInterpreter, ap))
      }
      case _ => None
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

  private def interpretAction(
    beliefInterpreter : SpcBeliefInterpreter,
    predicate : SilActionPredicate) : String =
  {
    val compliance = sentencePrinter.sb.respondCompliance
    mind.getCosmos.getTriggers.foreach(trigger => {
      applyTrigger(beliefInterpreter, trigger, predicate).foreach(result => {
        if (result != compliance) {
          return result
        }
      })
    })
    compliance
  }

  private def applyTrigger(
    beliefInterpreter : SpcBeliefInterpreter,
    trigger : SilConditionalSentence,
    predicate : SilActionPredicate) : Option[String] =
  {
    debug(s"APPLY TRIGGER $trigger")
    val antecedent = trigger.antecedent
    val consequent = trigger.consequent
    val replacements = new mutable.LinkedHashMap[SilReference, SilReference]
    antecedent match {
      case SilActionPredicate(
        subject, action, directObject, indirectObject, modifiers
      ) => {
        // FIXME process directObject, indirectObject
        if (action.lemma != predicate.action.lemma) {
          debug(s"ACTION ${predicate.action.lemma} DOES NOT MATCH")
          return None
        }
        // FIXME detect colliding replacement nouns e.g.
        // "if an object hits an object"
        if (!prepareReplacement(replacements, subject, predicate.subject)) {
          return None
        }
        directObject.foreach(obj => {
          predicate.directObject match {
            case Some(actualObj) => {
              if (!prepareReplacement(replacements, obj, actualObj)) {
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
            if (predicate.modifiers.contains(bm)) {
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
            // FIXME some predicate.modifiers (e.g. "never") might
            // negate the match
            val actualRefs = predicate.modifiers.flatMap(_ match {
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
    val newSentence = SilPredicateSentence(newPredicate)
    // FIXME detect loops and also limit recursion depth
    val result = interpretBeliefOrAction(beliefInterpreter, newSentence)
    if (result.isEmpty) {
      // FIXME i18n
      Some("Invalid consequent")
    } else {
      result
    }
  }

  private def prepareReplacement(
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
        // FIXME verify that actualRef matches refPattern
        val boundRef = actualRef match {
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
          case _ => actualRef
        }
        replacements.put(patternRef, boundRef)
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
        s"Previously I was told that ${originalBeliefString}." +
          s"  So I am unable to accept that ${beliefString}."
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
}
