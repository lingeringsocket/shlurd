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
package com.lingeringsocket.shlurd.ilang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

import SprEnglishLemmas._

object SilSentencePrinter
{
  private val ELLIPSIS_MARKER = "<...>"

  private val ELLIPSIS_REMOVAL = " " + ELLIPSIS_MARKER
}
import SilSentencePrinter._

class SilSentencePrinter(parlance : SilParlance = SilDefaultParlance)
{
  val sb = SilSentenceBundle(parlance)

  def print(
    sentence : SilSentence, ellipsis : Boolean = false) : String =
  {
    sb.terminatedSentence(
      printUnterminated(sentence, ellipsis),
      sentence.tam, sentence.formality)
  }

  def printUnterminated(
    sentence : SilSentence, ellipsis : Boolean = false) : String =
  {
    sentence match {
      case SilPredicateSentence(predicate, tam, _) => {
        tam.mood match {
          case MOOD_INDICATIVE =>  {
            printPredicateStatement(predicate, tam, ellipsis).
              replaceAllLiterally(ELLIPSIS_REMOVAL, "")
          }
          case MOOD_INTERROGATIVE => {
            printPredicateQuestion(predicate, tam)
          }
          case MOOD_IMPERATIVE => {
            printPredicateCommand(predicate, tam)
          }
        }
      }
      case SilPredicateQuery(
        predicate, question, answerInflection, tam, _
      ) => {
        printPredicateQuestion(
          predicate, tam, answerInflection, Some(question))
      }
      case SilConjunctiveSentence(determiner, sentences, separator) => {
        sb.conjoin(
          determiner, separator, INFLECT_NONE,
          sentences.map(s => printUnterminated(s, ellipsis)))
      }
      case SilAmbiguousSentence(alternatives, _) => {
        alternatives.map(s => printUnterminated(s, ellipsis)).mkString(" | ")
      }
      case SilConditionalSentence(
        antecedent, consequent, tamAntecedent, tamConsequent, biconditional, _
      ) => {
        assert(!ellipsis)
        sb.conditional(
          printPredicateStatement(
            antecedent, tamAntecedent, ellipsis),
          printPredicateStatement(
            consequent, tamConsequent, ellipsis),
          biconditional)
      }
      case SilUnparsedSentence(text) => text
      case _ : SilUnknownSentence => {
        sb.unknownSentence
      }
    }
  }

  def print(
    reference : SilReference,
    inflection : SilInflection,
    conjoining : SilConjoining) : String =
  {
    reference match {
      case SilNounReference(noun, determiner, count) => {
        sb.determinedNoun(
          determiner,
          sb.delemmatizeNoun(noun, count, inflection, conjoining))
      }
      case SilPronounReference(person, gender, count, distance) => {
        sb.pronoun(person, gender, count, distance, inflection, conjoining)
      }
      case SilConjunctiveReference(determiner, references, separator) => {
        sb.conjoin(
          determiner, separator, inflection,
          references.zipWithIndex.map {
            case (r, i) => print(
              r, inflection,
              SilConjoining(determiner, separator, i, references.size))
          }
        )
      }
      case SilStateSpecifiedReference(sub, state) => {
        state match {
          case adpositionalState : SilAdpositionalState => {
            val specified = print(sub, inflection, SilConjoining.NONE)
            val specifier = printAdpositionalPhrase(
              adpositionalState, conjoining)
            return sb.specifiedNoun(specifier, specified)
          }
          case _ => {

          }
        }
        val qualifierString = state match {
          case _ : SilUnknownState => sb.unknownState
          case _ =>  {
            sb.composeQualifiers(
              SilReference.extractQualifiers(state))
          }
        }
        sub match {
          case SilNounReference(noun, determiner, count) => {
            sb.determinedNoun(
              determiner,
              sb.qualifiedNoun(
                qualifierString,
                sb.delemmatizeNoun(noun, count, inflection, conjoining)))
          }
          case _ => {
            sb.qualifiedNoun(
              qualifierString, print(sub, inflection, conjoining))
          }
        }
      }
      case SilGenitiveReference(possessor, possessee) => {
        val qualifierString = possessor match {
          case SilPronounReference(person, gender, count, distance) => {
            sb.pronoun(
              person, gender, count, distance,
              INFLECT_GENITIVE, SilConjoining.NONE)
          }
          case _ => {
            print(possessor, INFLECT_GENITIVE, SilConjoining.NONE)
          }
        }
        sb.genitivePhrase(
          qualifierString, print(possessee, inflection, conjoining))
      }
      case SilQuotationReference(quotation) => {
        // FIXME delegate to sb
        DQUOTE + quotation + DQUOTE
      }
      case _ : SilUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(
    state : SilState, tam : SilTam, conjoining : SilConjoining)
      : String =
  {
    state match {
      case SilExistenceState() => {
        ""
      }
      case SilPropertyState(state) => {
        sb.delemmatizeState(state, tam, conjoining)
      }
      case SilPropertyQueryState(propertyName) => {
        propertyName
      }
      case adpositionalState : SilAdpositionalState => {
        printAdpositionalPhrase(adpositionalState, conjoining)
      }
      case SilConjunctiveState(determiner, states, separator) => {
        sb.conjoin(
          determiner, separator, INFLECT_NONE,
          states.zipWithIndex.map {
            case (s, i) => print(
              s, tam,
              SilConjoining(determiner, separator, i, states.size))
          }
        )
      }
      case SilNullState() | _ : SilUnknownState => {
        sb.unknownState
      }
    }
  }

  def printChangeStateVerb(
    state : SilState, changeVerb : Option[SilWord]) =
  {
    state match {
      case SilPropertyState(state) => {
        sb.changeStateVerb(state, changeVerb)
      }
      // FIXME:  conjoining, e.g. "close and lock the door"
      case _ => print(state, SilTam.imperative, SilConjoining.NONE)
    }
  }

  def printPredicateStatement(
    predicate : SilPredicate, tamOriginal : SilTam,
    ellipsis : Boolean = false) : String =
  {
    predicate match {
      case SilStatePredicate(subject, state, modifiers) => {
        val tam = tamOriginal
        val rhs = {
          if (ellipsis) {
            ELLIPSIS_MARKER
          } else {
            print(state, tam, SilConjoining.NONE)
          }
        }
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, state, tam, REL_IDENTITY,
            predicate.getInflectedCount, INFLECT_NONE),
          rhs,
          modifiers.map(printVerbModifier)
        )
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
        val complementInflection = relationship match {
          case REL_IDENTITY => INFLECT_NOMINATIVE
          case REL_ASSOCIATION => INFLECT_ACCUSATIVE
        }
        val tam = {
          if (ellipsis && (relationship == REL_ASSOCIATION)) {
            if (tamOriginal.isIndicative) {
              tamOriginal.withModality(MODAL_ELLIPTICAL)
            } else {
              tamOriginal
            }
          } else {
            tamOriginal
          }
        }
        val rhs = {
          if (ellipsis) {
            ELLIPSIS_MARKER
          } else {
            print(complement, complementInflection, SilConjoining.NONE)
          }
        }
        sb.relationshipPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, SilNullState(), tam, relationship,
            predicate.getInflectedCount, INFLECT_NONE),
          rhs,
          relationship,
          None,
          tam,
          modifiers.map(printVerbModifier))
      }
      case SilActionPredicate(
        subject, action, directObject, modifiers
      ) => {
        val count = SilReference.getCount(subject)
        sb.actionPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, action, tamOriginal,
            predicate.getInflectedCount, INFLECT_NONE),
          directObject.map(
            ref => print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE)),
          modifiers.map(printVerbModifier),
          tamOriginal)
      }
      case _ : SilUnknownPredicate => {
        sb.unknownPredicateStatement
      }
    }
  }

  def printPredicateCommand(
    predicate : SilPredicate, tam : SilTam) =
  {
    predicate match {
      case SilStatePredicate(subject, state, modifiers) => {
        val (subjectString, verbString) = subject match {
          case SilPronounReference(PERSON_SECOND, _, _, _) => {
            tupleN(("", printChangeStateVerb(state, Some(SilWord(LEMMA_BE)))))
          }
          case _ => {
            tupleN((print(subject, INFLECT_ACCUSATIVE, SilConjoining.NONE),
              printChangeStateVerb(state, None)))
          }
        }
        sb.statePredicateCommand(
          subjectString,
          verbString,
          modifiers.map(printVerbModifier))
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
        val action = relationship match {
          case REL_IDENTITY => SilWord.uninflected(LEMMA_BE)
          case REL_ASSOCIATION => SilWord.uninflected(LEMMA_HAVE)
        }
        sb.actionPredicate(
          "",
          sb.delemmatizeVerb(
            PERSON_SECOND, GENDER_N, COUNT_SINGULAR,
            tam, false, action, INFLECT_NONE),
          Some(print(complement, INFLECT_NONE, SilConjoining.NONE)),
          modifiers.map(printVerbModifier),
          tam)
      }
      case SilActionPredicate(_, action, directObject, modifiers) => {
        sb.actionPredicate(
          "",
          sb.delemmatizeVerb(
            PERSON_SECOND, GENDER_N, COUNT_SINGULAR,
            tam, false, action, INFLECT_NONE),
          directObject.map(
            ref => print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE)),
          modifiers.map(printVerbModifier),
          tam)
      }
      case _ => {
        sb.unknownPredicateCommand
      }
    }
  }

  def printPredicateQuestion(
    predicate : SilPredicate, tam : SilTam,
    answerInflection : SilInflection = INFLECT_NONE,
    question : Option[SilQuestion] = None) : String =
  {
    // FIXME when subject is a SilStateSpecifiedReference, the
    // state gets lost for questions such as QUESTION_WHAT
    val plainSubject = print(
      predicate.getSubject, INFLECT_NOMINATIVE, SilConjoining.NONE)
    val subjectString = answerInflection match {
      case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => plainSubject
      case _ => sb.query(plainSubject, question, answerInflection)
    }
    predicate match {
      case SilStatePredicate(subject, state, modifiers) => {
        val isExistential = state match {
          case SilExistenceState() => true
          case _ => false
        }
        sb.statePredicateQuestion(
          subjectString,
          getVerbSeq(
            subject, state, tam, REL_IDENTITY,
            predicate.getInflectedCount, answerInflection),
          print(state, tam, SilConjoining.NONE),
          isExistential,
          question,
          modifiers.map(printVerbModifier),
          answerInflection)
      }
      case SilActionPredicate(
        subject, action, directObject, modifiers
      ) => {
        val plainDirectObject = directObject.map(
          ref => print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE))
        val directObjectString = answerInflection match {
          case INFLECT_ACCUSATIVE => {
            plainDirectObject.map(sb.query(_, question, answerInflection))
          }
          case _ => plainDirectObject
        }
        val modifierStrings = {
          if (answerInflection == INFLECT_ADPOSITIONED) {
            val lastModifier = modifiers.last
            val lastRewritten = lastModifier match {
              case SilAdpositionalVerbModifier(adposition, objRef) => {
                val plainObj = print(
                  objRef, INFLECT_ADPOSITIONED, SilConjoining.NONE)
                sb.adpositionedNoun(
                  sb.adpositionString(adposition),
                  sb.query(plainObj, question, answerInflection),
                  SilConjoining.NONE)
              }
              case _ => throw new RuntimeException(
                s"unexpected modifier $lastModifier")
            }
            modifiers.dropRight(1).map(printVerbModifier) ++ Seq(lastRewritten)
          } else {
            modifiers.map(printVerbModifier)
          }
        }
        sb.actionPredicate(
          subjectString,
          getVerbSeq(subject, action, tam,
            predicate.getInflectedCount, answerInflection),
          directObjectString,
          modifierStrings,
          tam,
          answerInflection)
      }
      case SilRelationshipPredicate(
        subject, complement, relationship, modifiers
      ) => {
        sb.relationshipPredicate(
          subjectString,
          getVerbSeq(
            subject, SilNullState(), tam, relationship,
            predicate.getInflectedCount, answerInflection),
          print(complement, INFLECT_NOMINATIVE, SilConjoining.NONE),
          relationship,
          question,
          tam,
          modifiers.map(printVerbModifier))
      }
      case _ : SilUnknownPredicate => {
        sb.unknownPredicateQuestion
      }
    }
  }

  private def getVerbSeq(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, isExistential : Boolean,
    relationship : SilRelationship,
    answerInflection : SilInflection) : Seq[String] =
  {
    val verbLemma = relationship match {
      case REL_IDENTITY => {
        if (isExistential && (tam.modality == MODAL_EMPHATIC)) {
          LEMMA_EXIST
        } else {
          LEMMA_BE
        }
      }
      case REL_ASSOCIATION => LEMMA_HAVE
    }
    sb.delemmatizeVerb(
      person, gender, count, tam, isExistential, SilWord.uninflected(verbLemma),
      answerInflection)
  }

  private def getVerbSeq(
    subject : SilReference,
    action : SilWord,
    tam : SilTam,
    predicateCount : SilCount,
    answerInflection : SilInflection) : Seq[String] =
  {
    subject match {
      case _ : SilUnknownReference => {
        Seq(sb.unknownCopula)
      }
      case _ => {
        val (person, gender, count) = getSubjectAttributes(subject)
        sb.delemmatizeVerb(
          person, gender, combineCounts(count, predicateCount),
          tam, false, action, answerInflection)
      }
    }
  }

  private def getSubjectAttributes(
    subject : SilReference, isExistential : Boolean = false)
      : (SilPerson, SilGender, SilCount) =
  {
    subject match {
      case SilPronounReference(person, gender, count, _) => {
        tupleN((person, gender, count))
      }
      case SilNounReference(_, _, count) => {
        tupleN((PERSON_THIRD, GENDER_N, count))
      }
      case SilConjunctiveReference(determiner, references, _) => {
        val count = if (isExistential) {
          // FIXME:  this is probably English-specific
          SilReference.getCount(references.head)
        } else {
          determiner match {
            case DETERMINER_ALL => COUNT_PLURAL
            // DETERMINER_NONE is debatable
            case _ => COUNT_SINGULAR
          }
        }
        // FIXME:  also derive person and gender from underlying references,
        // since it makes a difference in languages such as Spanish
        tupleN((PERSON_THIRD, GENDER_N, count))
      }
      case SilStateSpecifiedReference(reference, _) => {
        getSubjectAttributes(reference, isExistential)
      }
      case SilGenitiveReference(possessor, possessee) => {
        getSubjectAttributes(possessee, isExistential)
      }
      case _ : SilQuotationReference => {
        tupleN((PERSON_THIRD, GENDER_N, COUNT_SINGULAR))
      }
      case _ : SilUnknownReference => {
        tupleN((PERSON_THIRD, GENDER_N, COUNT_SINGULAR))
      }
    }
  }

  private def combineCounts(count1 : SilCount, count2 : SilCount) : SilCount =
  {
    tupleN((count1, count2)) match {
      case (COUNT_SINGULAR, COUNT_SINGULAR) => COUNT_SINGULAR
      case _ => COUNT_PLURAL
    }
  }

  private def getVerbSeq(
    subject : SilReference, state : SilState, tam : SilTam,
    relationship : SilRelationship, predicateCount : SilCount,
    answerInflection : SilInflection)
      : Seq[String] =
  {
    subject match {
      case _ : SilUnknownReference => {
        Seq(sb.unknownCopula)
      }
      case _ => {
        val isExistential = state match {
          case SilExistenceState() => true
          case _ => false
        }
        val (person, gender, count) =
          getSubjectAttributes(subject, isExistential)
        getVerbSeq(
          person, gender, combineCounts(count, predicateCount),
          tam, isExistential, relationship, answerInflection)
      }
    }
  }

  def printVerbModifier(modifier : SilVerbModifier) : String =
  {
    modifier match {
      case SilBasicVerbModifier(words, _) => {
        sb.composeQualifiers(words)
      }
      case SilDanglingVerbModifier(SilAdposition(words)) => {
        sb.composeQualifiers(words)
      }
      case adpositionalPhrase : SilAdpositionalVerbModifier => {
        printAdpositionalPhrase(adpositionalPhrase, SilConjoining.NONE)
      }
      case _ : SilUnknownVerbModifier => {
        sb.unknownVerbModifier
      }
    }
  }

  def printAdpositionalPhrase(
    phrase : SilAdpositionalPhrase,
    conjoining : SilConjoining) : String =
  {
    phrase.adposition match {
      case SilAdposition.ADVERBIAL_TMP => {
        print(phrase.objRef, INFLECT_NONE, conjoining)
      }
      case _ => {
        sb.adpositionedNoun(
          sb.adpositionString(phrase.adposition),
          print(phrase.objRef, INFLECT_ACCUSATIVE, SilConjoining.NONE),
          conjoining)
      }
    }
  }
}

case class SilSentenceUnprintable() extends RuntimeException
{
}
