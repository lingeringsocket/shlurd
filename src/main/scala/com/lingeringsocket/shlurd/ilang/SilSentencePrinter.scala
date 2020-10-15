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

object SilSentencePrinter
{
  private val ELLIPSIS_MARKER = "<...>"

  private val ELLIPSIS_REMOVAL = " " + ELLIPSIS_MARKER
}
import SilSentencePrinter._

class SilSentencePrinter(
  tongue : SilTongue,
  genderAnalyzer : SilGenderAnalyzer)
{
  def getTongue = tongue

  val sb = tongue.newSentenceBundle()

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
          case MOOD_INDICATIVE | MOOD_SUBJUNCTIVE =>  {
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
        conjunction, antecedent, consequent,
        tamAntecedent, tamConsequent, biconditional, _
      ) => {
        assert(!ellipsis)
        sb.conditional(
          conjunction,
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
      case SilDeterminedReference(sub, determiner) => {
        val (person, gender, count) = getSubjectAttributes(sub)
        sb.determinedNoun(
          determiner,
          print(sub, inflection, conjoining),
          person,
          gender,
          count
        )
      }
      case SilCountedNounReference(noun, count) => {
        val gender = SilUtils.getGender(reference, genderAnalyzer)
        sb.delemmatizeNoun(noun, gender, count, inflection, conjoining)
      }
      case pr : SilPronounReference => {
        printPronoun(pr, inflection, conjoining)
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
      case SilParenthesizedReference(sub, bracket) => {
        val inside = inflection match {
          case INFLECT_GENITIVE => print(sub, INFLECT_NONE, SilConjoining.NONE)
          case _ => print(sub, inflection, SilConjoining.NONE)
        }
        sb.parenthetical(inside, inflection, conjoining, bracket)
      }
      case SilAppositionalReference(primary, secondary) => {
        sb.appositionedNoun(
          print(primary, inflection, SilConjoining.NONE),
          print(secondary, inflection, conjoining))
      }
      case SilStateSpecifiedReference(sub, state) => {
        state matchPartial {
          case adpositionalState : SilAdpositionalState => {
            val specified = print(sub, inflection, SilConjoining.NONE)
            val specifier = printAdpositionalPhrase(
              adpositionalState, conjoining)
            return sb.specifiedNoun(specifier, specified)
          }
        }
        val qualifierString = state match {
          case _ : SilUnknownState => sb.unknownState
          case _ =>  {
            sb.composeQualifiers(
              SilUtils.extractQualifiers(state))
          }
        }
        sb.separate(
          sb.qualifiedNoun(
            qualifierString,
            print(sub, inflection, SilConjoining.NONE)),
          conjoining)
      }
      case SilGenitiveReference(possessor, possessee) => {
        val (qualifierString, isPronoun) = possessor match {
          case pr : SilPronounReference => {
            tupleN((
              printPronoun(pr, INFLECT_GENITIVE, SilConjoining.NONE),
              true
            ))
          }
          case _ => {
            tupleN((
              print(possessor, INFLECT_GENITIVE, SilConjoining.NONE),
              false
            ))
          }
        }
        sb.genitivePhrase(
          qualifierString,
          print(possessee, inflection, conjoining),
          isPronoun)
      }
      case SilQuotationReference(quotation, bracket) => {
        sb.separate(
          sb.applyInflection(
            bracket.begin + quotation + bracket.end,
            COUNT_SINGULAR,
            inflection),
          conjoining)
      }
      case _ : SilUnknownReference => {
        sb.unknownReference
      }
    }
  }

  def print(
    state : SilState, tam : SilTam,
    subjectOpt : Option[SilReference], conjoining : SilConjoining)
      : String =
  {
    state match {
      case SilExistenceState(_) => {
        ""
      }
      case SilPropertyState(state) => {
        val (person, gender, count) = subjectOpt match {
          case Some(subject) => {
            getSubjectAttributes(subject)
          }
          case _ => {
            tupleN((PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR))
          }
        }
        sb.delemmatizeState(
          state,
          tam,
          person,
          gender,
          count,
          conjoining)
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
              s, tam, subjectOpt,
              SilConjoining(determiner, separator, i, states.size))
          }
        )
      }
      case SilNullState() | _ : SilUnknownState => {
        sb.unknownState
      }
    }
  }

  def printPronoun(
    ref : SilPronounReference,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    val word = ref.word.orElse {
      val usage = tongue.getPronounUsage(inflection, ref.proximity)
      val pronounKey = SilPronounKey(usage, ref.person)
      ref.pronounMap.get(pronounKey)
    }
    sb.pronoun(
      ref.person, ref.gender, ref.count, ref.proximity, ref.politeness,
      word, inflection, conjoining)
  }

  def printChangeStateVerb(
    state : SilState, changeVerb : Option[SilWord]) =
  {
    state match {
      case SilPropertyState(state) => {
        sb.changeStateVerb(state, changeVerb)
      }
      // FIXME:  conjoining, e.g. "close and lock the door"
      case _ => print(state, SilTam.imperative, None, SilConjoining.NONE)
    }
  }

  def printPredicateStatement(
    predicate : SilPredicate, tamOriginal : SilTam,
    ellipsis : Boolean = false) : String =
  {
    predicate match {
      case SilStatePredicate(subject, verb, state, modifiers) => {
        val tam = tamOriginal
        val rhs = {
          if (ellipsis && (!state.isInstanceOf[SilExistenceState])) {
            ELLIPSIS_MARKER
          } else {
            print(state, tam, Some(subject), SilConjoining.NONE)
          }
        }
        val existentialPronoun = getExistentialPronoun(state)
        sb.statePredicateStatement(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, state, tam, verb,
            predicate.getInflectedAttributes, INFLECT_NONE),
          rhs,
          existentialPronoun,
          modifiers.map(printVerbModifier)
        )
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        val complementInflection = {
          if (tongue.isBeingLemma(verb)) {
            INFLECT_NOMINATIVE
          } else {
            INFLECT_ACCUSATIVE
          }
        }
        val tam = {
          if (ellipsis && !tongue.isBeingLemma(verb)) {
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
            subject, SilNullState(), tam, verb,
            predicate.getInflectedAttributes, INFLECT_NONE),
          rhs,
          verb,
          None,
          tam,
          modifiers.map(printVerbModifier))
      }
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        sb.actionPredicate(
          print(subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          getVerbSeq(
            subject, verb, tamOriginal,
            predicate.getInflectedAttributes, INFLECT_NONE),
          directObject.map(
            ref => printDirectObject(ref)),
          modifiers.flatMap(printDative),
          modifiers.map(printVerbModifier),
          tamOriginal,
          objectPosition = sb.getObjectPosition(directObject))
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
      case SilStatePredicate(subject, verb, state, modifiers) => {
        val (subjectString, verbString) = subject match {
          case pr : SilPronounReference if (pr.person == PERSON_SECOND) => {
            tupleN(("", printChangeStateVerb(state, Some(verb))))
          }
          case _ => {
            tupleN((
              printDirectObject(subject),
              printChangeStateVerb(state, None)))
          }
        }
        sb.statePredicateCommand(
          subjectString,
          verbString,
          modifiers.map(printVerbModifier))
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        val uninflectedVerb = verb.toUninflected
        val (person, gender, count) = getCommandeeAttributes(subject)
        sb.actionPredicate(
          "",
          sb.delemmatizeVerb(
            person, gender, count,
            tam, None, uninflectedVerb, INFLECT_NONE),
          Some(print(complement, INFLECT_NONE, SilConjoining.NONE)),
          Seq.empty,
          modifiers.map(printVerbModifier),
          tam)
      }
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        val (person, gender, count) = getCommandeeAttributes(subject)
        sb.actionPredicate(
          "",
          sb.delemmatizeVerb(
            person, gender, count,
            tam, None, verb, INFLECT_NONE),
          directObject.map(
            ref => printDirectObject(ref)),
          modifiers.flatMap(printDative),
          modifiers.map(printVerbModifier),
          tam,
          objectPosition = sb.getObjectPosition(directObject))
      }
      case _ => {
        sb.unknownPredicateCommand
      }
    }
  }

  private def getCommandeeAttributes(subject : SilReference) =
  {
    subject match {
      case pr : SilPronounReference => {
        tupleN((tongue.getEffectivePerson(pr), pr.gender, pr.count))
      }
      case _ => {
        tupleN((PERSON_SECOND, GENDER_SOMEONE, COUNT_SINGULAR))
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
    val (plainSubject, subjectInflection) = predicate.getSubject match {
      case SilGenitiveReference(
        SilNounReference(noun),
        possessee
      ) if (tongue.isGenitiveVariableLemma(noun.toLemma)) => {
        tupleN((
          print(
            possessee, INFLECT_NOMINATIVE, SilConjoining.NONE),
          INFLECT_GENITIVE))
      }
      case subject => {
        tupleN((
          print(
            subject, INFLECT_NOMINATIVE, SilConjoining.NONE),
          answerInflection))
      }
    }
    val subjectString = subjectInflection match {
      case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => plainSubject
      case _ => sb.query(plainSubject, question, subjectInflection)
    }
    predicate match {
      case SilStatePredicate(subject, verb, state, modifiers) => {
        val existentialPronoun = getExistentialPronoun(state)
        val stateString = print(state, tam, Some(subject), SilConjoining.NONE)
        sb.statePredicateQuestion(
          subjectString,
          getVerbSeq(
            subject, state, tam, verb,
            predicate.getInflectedAttributes, answerInflection),
          stateString,
          existentialPronoun,
          question,
          modifiers.map(printVerbModifier),
          answerInflection)
      }
      case SilActionPredicate(
        subject, verb, directObject, modifiers
      ) => {
        val plainDirectObject = directObject.map(
          ref => printDirectObject(ref))
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
          getVerbSeq(subject, verb, tam,
            predicate.getInflectedAttributes, answerInflection),
          directObjectString,
          modifiers.flatMap(printDative),
          modifierStrings,
          tam,
          answerInflection,
          objectPosition = sb.getObjectPosition(directObject))
      }
      case SilRelationshipPredicate(
        subject, verb, complement, modifiers
      ) => {
        sb.relationshipPredicate(
          subjectString,
          getVerbSeq(
            subject, SilNullState(), tam, verb,
            predicate.getInflectedAttributes, answerInflection),
          print(complement, INFLECT_NOMINATIVE, SilConjoining.NONE),
          verb,
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
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord,
    answerInflection : SilInflection) : Seq[String] =
  {
    val uninflectedVerb = {
      if (existentialPronoun.nonEmpty && (tam.modality == MODAL_EMPHATIC)) {
        sb.existsVerb
      } else {
        verb.toUninflected
      }
    }
    sb.delemmatizeVerb(
      person, gender, count, tam, existentialPronoun, uninflectedVerb,
      answerInflection)
  }

  private def getVerbSeq(
    subject : SilReference,
    verb : SilWord,
    tam : SilTam,
    inflectedAttributes : SilVerbInflection,
    answerInflection : SilInflection) : Seq[String] =
  {
    val (person, gender, count) = getSubjectAttributes(subject)
    sb.delemmatizeVerb(
      combinePersons(person, inflectedAttributes.person),
      tongue.combineGenders(Seq(gender, inflectedAttributes.gender)),
      combineCounts(count, inflectedAttributes.count),
      tam, None, verb, answerInflection)
  }

  private def getSubjectAttributes(
    subject : SilReference, existentialPronoun : Option[SilWord] = None)
      : (SilPerson, SilGender, SilCount) =
  {
    subject match {
      case pr : SilPronounReference => {
        tupleN((tongue.getEffectivePerson(pr), pr.gender, pr.count))
      }
      case SilCountedNounReference(_, count) => {
        tupleN((
          PERSON_THIRD,
          SilUtils.getGender(subject, genderAnalyzer),
          count))
      }
      case SilConjunctiveReference(determiner, references, _) => {
        val count = if (existentialPronoun.nonEmpty) {
          // FIXME:  this is probably English-specific
          SilUtils.getCount(references.head)
        } else {
          determiner match {
            case DETERMINER_ALL => COUNT_PLURAL
            // DETERMINER_NONE is debatable
            case _ => COUNT_SINGULAR
          }
        }
        // FIXME:  also derive person from underlying references,
        // since it makes a difference in languages such as Spanish
        tupleN((
          PERSON_THIRD,
          SilUtils.getGender(subject, genderAnalyzer), count))
      }
      case SilParenthesizedReference(reference, _) => {
        getSubjectAttributes(reference, existentialPronoun)
      }
      case SilAppositionalReference(primary, _) => {
        getSubjectAttributes(primary, existentialPronoun)
      }
      case SilStateSpecifiedReference(reference, _) => {
        getSubjectAttributes(reference, existentialPronoun)
      }
      case SilDeterminedReference(reference, _) => {
        getSubjectAttributes(reference, existentialPronoun)
      }
      case SilGenitiveReference(possessor, possessee) => {
        getSubjectAttributes(possessee, existentialPronoun)
      }
      case _ : SilQuotationReference => {
        tupleN((PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR))
      }
      case _ : SilUnknownReference => {
        tupleN((PERSON_THIRD, GENDER_NEUTER, COUNT_SINGULAR))
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

  private def combinePersons(
    person1 : SilPerson, person2 : SilPerson) : SilPerson =
  {
    tupleN((person1, person2)) match {
      case (PERSON_FIRST, _) => PERSON_FIRST
      case (_, PERSON_FIRST) => PERSON_FIRST
      case (PERSON_SECOND, _) => PERSON_SECOND
      case (_, PERSON_SECOND) => PERSON_SECOND
      case _ => PERSON_THIRD
    }
  }

  private def getVerbSeq(
    subject : SilReference, state : SilState, tam : SilTam,
    verb : SilWord, inflectedAttributes : SilVerbInflection,
    answerInflection : SilInflection)
      : Seq[String] =
  {
    val existentialPronoun = getExistentialPronoun(state)
    val (person, gender, count) =
      getSubjectAttributes(subject, existentialPronoun)
    getVerbSeq(
      combinePersons(person, inflectedAttributes.person),
      tongue.combineGenders(Seq(gender, inflectedAttributes.gender)),
      combineCounts(count, inflectedAttributes.count),
      tam, existentialPronoun, verb, answerInflection)
  }

  private def getExistentialPronoun(state : SilState) =
  {
    state match {
      case SilExistenceState(existentialPronoun) => existentialPronoun
      case _ => None
    }
  }

  def printVerbModifier(modifier : SilVerbModifier) : String =
  {
    modifier match {
      case SilBasicVerbModifier(word) => {
        sb.composeQualifiers(Seq(word))
      }
      case SilDanglingVerbModifier(SilAdposition(word)) => {
        sb.composeQualifiers(Seq(word))
      }
      case adpositionalPhrase : SilAdpositionalVerbModifier => {
        printAdpositionalPhrase(adpositionalPhrase, SilConjoining.NONE)
      }
      case _ : SilUnknownVerbModifier => {
        sb.unknownVerbModifier
      }
    }
  }

  def printDative(modifier : SilVerbModifier) : Option[String] =
  {
    modifier match {
      case SilAdpositionalVerbModifier(
        SilAdposition(SilWordLemma(LEMMA_ADPOSITION_DATIVE)),
        objRef
      ) => {
        Some(print(objRef, INFLECT_DATIVE, SilConjoining.NONE))
      }
      case _ => {
        None
      }
    }
  }

  def printAdpositionalPhrase(
    phrase : SilAdpositionalPhrase,
    conjoining : SilConjoining) : String =
  {
    phrase.adposition.word.toLemma match {
      case LEMMA_ADVERBIAL_TMP => {
        print(phrase.objRef, INFLECT_NONE, conjoining)
      }
      case LEMMA_ADPOSITION_DATIVE => {
        ""
      }
      case _ => {
        sb.adpositionedNoun(
          sb.adpositionString(phrase.adposition),
          print(phrase.objRef, INFLECT_ADPOSITIONED, SilConjoining.NONE),
          conjoining)
      }
    }
  }

  private def printDirectObject(ref : SilReference) : String =
  {
    sb.directObject(
      print(ref, INFLECT_ACCUSATIVE, SilConjoining.NONE),
      genderAnalyzer.isPerson(ref, genderAnalyzer))
  }
}

case class SilSentenceUnprintable() extends RuntimeException
{
}
