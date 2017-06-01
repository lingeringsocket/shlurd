// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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
package shlurd.print

import shlurd.parser._

class ShlurdSentencePrinter(parlance : ShlurdParlance = ShlurdDefaultParlance)
{
  val sb = SentenceParlanceBundle(parlance)

  def print(sentence : ShlurdSentence) : String =
  {
    sentence match {
      case ShlurdPredicateStatement(predicate) => {
        sb.statement(printPredicateStatement(predicate))
      }
      case ShlurdStateChangeCommand(predicate) => {
        sb.command(printPredicateCommand(predicate))
      }
      case ShlurdPredicateQuestion(predicate) => {
        sb.question(printPredicateQuestion(predicate))
      }
      case ShlurdUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(reference : ShlurdReference) : String =
  {
    reference match {
      case ShlurdEntityReference(entity, quantifier, count) => {
        sb.determinedNoun(
          sb.determiner(quantifier),
          sb.delemmatizeNoun(entity, count))
      }
      case ShlurdQualifiedReference(sub, qualifiers) => {
        val qualifierString = sb.composeQualifiers(qualifiers)
        sub match {
          case ShlurdEntityReference(entity, quantifier, count) => {
            sb.determinedNoun(
              sb.determiner(quantifier),
              sb.qualifiedNoun(
                qualifierString, sb.delemmatizeNoun(entity, count)))
          }
          case _ => {
            sb.qualifiedNoun(qualifierString, print(sub))
          }
        }
      }
      case ShlurdUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(state : ShlurdState) : String =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.delemmatizeState(state)
      }
      case ShlurdLocationState(locative, location) => {
        sb.locatedNoun(
          sb.position(locative),
          print(location))
      }
      case ShlurdUnknownState => {
        sb.unknownState
      }
    }
  }

  def printChangeStateVerb(state : ShlurdState) =
  {
    state match {
      case ShlurdPropertyState(state) => {
        sb.changeStateVerb(state)
      }
      case _ => print(state)
    }
  }

  def printPredicateStatement(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateStatement(
          print(subject), printCopula(subject, state), print(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateStatement
      }
    }
  }

  def printPredicateCommand(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateCommand(
          print(subject), printChangeStateVerb(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateCommand
      }
    }
  }

  def printPredicateQuestion(predicate : ShlurdPredicate) =
  {
    predicate match {
      case ShlurdStatePredicate(subject, state) => {
        sb.statePredicateQuestion(
          print(subject), printCopula(subject, state), print(state))
      }
      case ShlurdUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  def printCopula(subject : ShlurdReference, state : ShlurdState) : String =
  {
    subject match {
      case ShlurdEntityReference(entity, quantifier, count) => {
        sb.copula(count)
      }
      case ShlurdQualifiedReference(reference, qualifiers) => {
        printCopula(reference, state)
      }
      case ShlurdUnknownReference => {
        sb.unknownCopula
      }
    }
  }
}

object SentenceParlanceBundle
{
  def apply(parlance : ShlurdParlance) = new SentenceParlanceBundle
}

class SentenceParlanceBundle extends ShlurdParlanceBundle
{
  private def concat(s : String*) =
    s.mkString("")

  private def compose(s : String*) =
    s.mkString(" ")

  def command(s : String) =
    phrase(concat(s, "."))

  def statement(s : String) =
    phrase(concat(s, "."))

  def question(s : String) =
    phrase(concat(s, "?"))

  def statePredicateStatement(
    subject : String, copula : String, state : String) =
  {
    phrase(compose(subject, copula, state))
  }

  def statePredicateQuestion(
    subject : String, copula : String, state : String) =
  {
    phrase(compose(copula, subject, state))
  }

  def statePredicateCommand(subject : String, state : String) =
    phrase(compose(state, subject))

  def copula(count : ShlurdCount) =
  {
    count match {
      case COUNT_SINGULAR => {
        phrase("is")
      }
      case COUNT_PLURAL => {
        // FIXME
        phrase("are")
      }
    }
  }

  def determiner(quantifier : ShlurdQuantifier) =
  {
    quantifier match {
      case QUANT_NONE => phrase("no")
      case QUANT_SPECIFIC => phrase("the")
      case QUANT_ANY => phrase("a")
      case QUANT_ALL => phrase("all")
    }
  }

  def position(locative : ShlurdLocative) =
  {
    locative match {
      case LOC_INSIDE => phrase("in")
      case LOC_OUTSIDE => phrase("outside of")
      case LOC_AT => phrase("at")
      case LOC_NEAR => phrase("near")
      case LOC_ON => phrase("on")
      case LOC_ABOVE => phrase("above")
      case LOC_BELOW => phrase("below")
      case LOC_LEFT => phrase("to the left of")
      case LOC_RIGHT => phrase("to the right of")
      case LOC_FRONT => phrase("in front of")
      case LOC_BEHIND => phrase("behind")
    }
  }

  def changeStateVerb(state : ShlurdWord) =
    phrase(state.lemma)

  def delemmatizeNoun(entity : ShlurdWord, count : ShlurdCount) =
  {
    if (entity.inflected.isEmpty) {
      val lemma = entity.lemma
      count match {
        case COUNT_SINGULAR => {
          phrase(lemma)
        }
        case COUNT_PLURAL => {
          if (lemma.endsWith("s")) {
            phrase(concat(lemma, "es"))
          } else {
            phrase(concat(lemma, "s"))
          }
        }
      }
    } else {
      phrase(entity.inflected)
    }
  }

  def delemmatizeState(state : ShlurdWord) =
  {
    if (state.inflected.isEmpty) {
      val lemma = state.lemma
      if (lemma.endsWith("ed")) {
        phrase(lemma)
      } else if (lemma.endsWith("e")) {
        phrase(concat(lemma, "d"))
      } else {
        phrase(concat(lemma, "ed"))
      }
    } else {
      phrase(state.inflected)
    }
  }

  def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    if (qualifier.inflected.isEmpty) {
      phrase(qualifier.lemma)
    } else {
      phrase(qualifier.inflected)
    }
  }

  def composeQualifiers(qualifiers : Seq[ShlurdWord]) =
  {
    phrase(compose(qualifiers.map(delemmatizeQualifier(_)) :_*))
  }

  def qualifiedNoun(qualifiers : String, noun : String) =
  {
    phrase(compose(qualifiers, noun))
  }

  def determinedNoun(determiner : String, noun : String) =
  {
    phrase(compose(determiner, noun))
  }

  def locatedNoun(position : String, noun : String) =
  {
    phrase(compose(position, noun))
  }

  def unknownSentence() =
  {
    phrase("blah blah blah")
  }

  def unknownReference() =
  {
    phrase("something or other")
  }

  def unknownState() =
  {
    phrase("discombobulated")
  }

  def unknownCopula() =
  {
    phrase("be")
  }

  def unknownPredicateStatement() =
  {
    phrase("foo is bar")
  }

  def unknownPredicateCommand() =
  {
    phrase("make it so")
  }

  def unknownPredicateQuestion() =
  {
    phrase("is it what now")
  }
}
