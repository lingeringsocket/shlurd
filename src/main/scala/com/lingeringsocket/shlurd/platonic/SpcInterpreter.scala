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

import scala.collection._

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
  override protected def interpretImpl(sentence : SilSentence) : String =
  {
    if ((beliefAcceptance != ACCEPT_NO_BELIEFS) &&
      sentence.mood.isIndicative)
    {
      val beliefInterpreter =
        new SpcBeliefInterpreter(
          mind.getCosmos.fork,
          (beliefAcceptance == ACCEPT_MODIFIED_BELIEFS))
      attemptAsBelief(beliefInterpreter, sentence, true).foreach(result => {
        return result
      })
      sentence match {
        case SilPredicateSentence(ap : SilActionPredicate, _, _) => {
          return interpretAction(beliefInterpreter, ap)
        }
        case _ =>
      }
    }
    super.interpretImpl(sentence)
  }

  private def attemptAsBelief(
    beliefInterpreter : SpcBeliefInterpreter,
    sentence : SilSentence,
    commit : Boolean) : Option[String] =
  {
    beliefInterpreter.recognizeBelief(sentence) match {
      case Some(belief) => {
        debug(s"APPLYING NEW BELIEF : $belief")
        try {
          beliefInterpreter.applyBelief(belief)
        } catch {
          case ex : RejectedBeliefExcn => {
            debug("NEW BELIEF REJECTED", ex)
            return Some(respondContradiction(ex))
          }
        }
        if (commit) {
          beliefInterpreter.cosmos.applyModifications
          debug("NEW BELIEF ACCEPTED")
          Some(sentencePrinter.sb.respondCompliance)
        } else {
          None
        }
      }
      case _ => None
    }
  }

  private def interpretAction(
    beliefInterpreter : SpcBeliefInterpreter,
    predicate : SilActionPredicate) : String =
  {
    mind.getCosmos.getTriggers.foreach(trigger => {
      applyTrigger(beliefInterpreter, trigger, predicate).foreach(result => {
        return result
      })
    })
    beliefInterpreter.cosmos.applyModifications
    sentencePrinter.sb.respondCompliance
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
        // FIXME support other subject patterns
        subject match {
          case SilNounReference(
            subjectNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR
          ) => {
            val subjectPattern = SilNounReference(
              subjectNoun, DETERMINER_UNIQUE, COUNT_SINGULAR)
            // FIXME verify that predicate.subject matches subjectPattern
            replacements.put(subjectPattern, predicate.subject)
          }
          case _ => {
            debug("SUBJECT PATTERN UNSUPPORTED")
            return None
          }
        }
        // FIXME support multiple modifiers and other patterns
        modifiers match {
          case Seq(SilAdpositionalVerbModifier(
            adposition, SilNounReference(
              objNoun, DETERMINER_NONSPECIFIC, COUNT_SINGULAR))
          ) => {
            // FIXME some predicate.modifiers (e.g. "never") might
            // negate the match
            val actualRefs = predicate.modifiers.flatMap(_ match {
              case SilAdpositionalVerbModifier(adposition, actualRef) => {
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
    attemptAsBelief(beliefInterpreter, newSentence, false)
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
        s"Previously I was told that ${originalBeliefString}." +
          s"  So I am unclear how to interpret the belief that ${beliefString}."
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
