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

import com.lingeringsocket.shlurd.parser._

// FIXME:  this is terrible
import SprEnglishLemmas._

class SilKoreanSentenceBundle extends SilSentenceBundle
{
  override def statePredicateStatement(
    subject : String, verbSeq : Seq[String], state : String,
    modifiers : Seq[String]) =
  {
    if (state.isEmpty) {
      compose((Seq(subject) ++ modifiers ++ verbSeq):_*)
    } else {
      compose((Seq(subject) ++ modifiers ++ Seq(state)):_*)
    }
  }

  override def actionPredicate(
    subject : String,
    verbSeq : Seq[String],
    directObject : Option[String],
    modifiers : Seq[String],
    tam : SilTam,
    answerInflection : SilInflection) =
  {
    // FIXME:  for interrogative mood, this only holds for "요" politeness
    compose((Seq(subject) ++ modifiers ++ directObject ++ verbSeq):_*)
  }

  override def relationshipPredicate(
    subject : String, verbSeq : Seq[String], complement : String,
    relationship : SilRelationship,
    question : Option[SilQuestion],
    tam : SilTam,
    modifiers : Seq[String]) =
  {
    // FIXME
    compose((Seq(subject) ++ modifiers ++ Seq(complement) ++ verbSeq):_*)
  }

  override def statePredicateQuestion(
    subject : String, verbSeq : Seq[String], state : String,
    isExistential : Boolean, question : Option[SilQuestion],
    modifiers : Seq[String],
    answerInflection : SilInflection) =
  {
    // FIXME:  only holds for "요" politeness
    statePredicateStatement(subject, verbSeq, state, modifiers)
  }

  override def statePredicateCommand(subject : String, state : String,
    modifiers : Seq[String]) =
  {
    compose((Seq(subject) ++ modifiers ++ Seq(state)):_*)
  }

  override def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, isExistential : Boolean,
    verb : SilWord, answerInflection : SilInflection) =
  {
    // FIXME arbitrary lemmas
    val verbLemma = verb.toLemma
    val exists = isExistential || (verbLemma == LEMMA_HAVE)
    // FIXME:  use tam.modality
    if (tam.isImperative) {
      Seq(conjugateImperative(verbLemma))
    } else {
      if (tam.isPositive) {
        if (exists) {
          Seq("있어요")
        } else {
          // FIXME:  use "예요" after vowel
          Seq("이에요")
        }
      } else {
        if (exists) {
          Seq("없어요")
        } else {
          Seq("아니에요")
        }
      }
    }
  }

  override def adpositionString(adposition : SilAdposition) =
  {
    // FIXME find significant word in phrase such as "to the right of"
    val pos = adposition.word.decomposed.head.lemma match {
      case LEMMA_IN | LEMMA_WITHIN | LEMMA_INSIDE => "안"
      case LEMMA_OUTSIDE => "밖"
      case LEMMA_AT => ""
      // FIXME:  distinguish "near" from "next to"
      case LEMMA_NEAR | LEMMA_NEARBY => "근처"
      case LEMMA_ON | LEMMA_ABOVE | LEMMA_OVER => "위"
      case LEMMA_BELOW | LEMMA_UNDER |
          LEMMA_UNDERNEATH | LEMMA_BENEATH => "밑"
      // FIXME:  need to attach 의 to previous word
      case LEMMA_LEFT => "왼쪽"
      case LEMMA_RIGHT => "오른쪽"
      case LEMMA_FRONT => "앞"
      case LEMMA_BACK | LEMMA_BEHIND => "뒤"
      // FIXME:  context-dependent
      case LEMMA_WITH => "하고"
      // FIXME:  OF etc
      case _ => compose(adposition.word.decomposed.map(_.lemma):_*)
    }
    // later need to distinguish 에 from 에서
    compose(concat(pos, "에"), "있어요")
  }

  override def actionVerb(
    action : SilWord) =
  {
    conjugateAction(action.toLemma)
  }

  override def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) =
  {
    // FIXME:  use changeVerb
    conjugateImperative(state.toLemma)
  }

  override def delemmatizeNoun(
    noun : SilWord,
    count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    inflectNoun(noun.toLemma, count, inflection, conjoining)
  }

  override def delemmatizeState(
    state : SilWord, tam : SilTam, conjoining : SilConjoining) =
  {
    // FIXME:  conjoining
    conjugateAdjective(state.toLemma, tam)
  }

  override def delemmatizeQualifier(qualifier : SilWord) =
  {
    qualifyAdjective(qualifier.toLemma)
  }

  override def conjoin(
    determiner : SilDeterminer,
    separator : SilSeparator,
    inflection : SilInflection,
    items : Seq[String]) =
  {
    // FIXME:  deal with other determiners such as DETERMINER_NONE
    compose(items:_*)
  }

  override def conditional(
    antecedent : String,
    consequent : String,
    biconditional : Boolean) =
  {
    // FIXME proper conjugation, biconditional
    compose(concat(antecedent, "면"), consequent)
  }

  override def composeQualifiers(qualifiers : Seq[SilWord]) =
  {
    compose(qualifiers.map(delemmatizeQualifier) :_*)
  }

  override def query(noun : String, question : Option[SilQuestion],
    answerInflection : SilInflection) =
  {
    question match {
      case Some(QUESTION_WHICH) => {
        compose("무슨", noun)
      }
      case Some(QUESTION_WHO) => {
        // FIXME inflection
        "누구"
      }
      case Some(QUESTION_WHAT) => {
        // FIXME inflection
        "뭐"
      }
      case Some(QUESTION_WHERE) => {
        // discriminate between being and action
        "어디에"
      }
      case _ => noun
    }
  }

  override def qualifiedNoun(qualifiers : String, noun : String) =
  {
    compose(qualifiers, noun)
  }

  override def specifiedNoun(specifier : String, noun : String) =
  {
    compose(specifier, noun)
  }

  override def determinedNoun(determiner : SilDeterminer, noun : String) =
  {
    val determinerString = determiner match {
      case DETERMINER_NONE => throw SilSentenceUnprintable()
      case DETERMINER_ALL => "모든"
      // FIXME:  sentence order is very much case-by-case
      case _ => ""
    }
    compose(determinerString, noun)
  }

  override def adpositionedNoun(
    position : String, noun : String, conjoining : SilConjoining) =
  {
    // FIXME:  need to overhaul caller
    separate(compose(noun, position), conjoining)
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    compose(genitive, head)
  }

  private def inflectPronoun(
    pn : String,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    inflectNoun(pn, COUNT_SINGULAR, inflection, conjoining)
  }

  override def pronoun(
    person : SilPerson, gender : SilGender, count : SilCount,
    distance : SilDistance, inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    person match {
      case PERSON_FIRST => count match {
        case COUNT_SINGULAR => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun("내", inflection, conjoining)
          case INFLECT_GENITIVE => "내"
          case _ => inflectPronoun("나", inflection, conjoining)
        }
        case COUNT_PLURAL => inflectPronoun("우리", inflection, conjoining)
      }
      case PERSON_SECOND => count match {
        case COUNT_SINGULAR => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun(
            "니", inflection, conjoining)
          case INFLECT_GENITIVE => "네"
          case _ => inflectPronoun("너", inflection, conjoining)
        }
        case COUNT_PLURAL => inflectPronoun("여러분", inflection, conjoining)
      }
      case PERSON_THIRD => count match {
        case COUNT_SINGULAR => gender match {
          case GENDER_M => inflectPronoun("그", inflection, conjoining)
          case GENDER_F => inflectPronoun("그녀", inflection, conjoining)
          case GENDER_N => {
            // FIXME discriminate "그" from "저"
            distance match {
              case DISTANCE_HERE =>
                inflectPronoun("이것", inflection, conjoining)
              case _ =>
                inflectPronoun("그것", inflection, conjoining)
            }
          }
        }
        case COUNT_PLURAL => {
          // FIXME discriminate "그" from "저"
          distance match {
            case DISTANCE_HERE =>
              inflectPronoun("이것들", inflection, conjoining)
            case DISTANCE_THERE =>
              inflectPronoun("그것들", inflection, conjoining)
            case DISTANCE_UNSPECIFIED =>
              inflectPronoun("그들", inflection, conjoining)
          }
        }
      }
    }
  }

  override def unknownSentence() =
  {
    "모모모"
  }

  override def unknownReference() =
  {
    "뭐뭐뭐"
  }

  override def unknownState() =
  {
    "뭐뭐뭐"
  }

  override def unknownVerbModifier() =
  {
    "뭐뭐뭐게"
  }

  override def unknownCopula() =
  {
    "뭐뭐뭐요"
  }

  override def unknownPredicateStatement() =
  {
    "모모모"
  }

  override def unknownPredicateCommand() =
  {
    "모모모"
  }

  override def unknownPredicateQuestion() =
  {
    "모모모"
  }

  private def isHangul(c : Character) =
  {
    Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.HANGUL
  }

  private def hasFinalConsonant(s : String) =
  {
    val last = s.last.toInt
    // http://gernot-katzers-spice-pages.com/var/korean_hangul_unicode.html
    val finalConsonant = (last - 44032) % 28
    (finalConsonant != 0)
  }

  def inflectNoun(
    lemma : String, count : SilCount,
    inflection : SilInflection, conjoining : SilConjoining) =
  {
    if (lemma.exists(c => isHangul(c))) {
      val numbered = count match {
        case COUNT_SINGULAR => lemma
        case COUNT_PLURAL => concat(lemma, "들")
      }
      val marker = {
        if (conjoining.isLast) {
          inflection match {
            case INFLECT_NONE | INFLECT_COMPLEMENT => ""
            case INFLECT_NOMINATIVE => {
              if (hasFinalConsonant(numbered)) {
                "이"
              } else {
                "가"
              }
            }
            case INFLECT_ACCUSATIVE => {
              if (hasFinalConsonant(numbered)) {
                "을"
              } else {
                "를"
              }
            }
            case INFLECT_ADPOSITIONED => {
              // FIXME this is only correct for SilAdposition.TO;
              // also, should take formality into account
              "에게"
            }
            case INFLECT_GENITIVE => "의"
          }
        } else {
          conjoining.determiner match {
            case DETERMINER_ANY | DETERMINER_UNIQUE => {
              if (hasFinalConsonant(numbered)) {
                "이나"
              } else {
                "나"
              }
            }
            // FIXME:  other variants such as comma, 도, 고, and 하고
            case _ => {
              if (hasFinalConsonant(numbered)) {
                "과"
              } else {
                "와"
              }
            }
          }
        }
      }
      concat(numbered, marker)
    } else {
      val marker = inflection match {
        case INFLECT_NONE => ""
        case INFLECT_NOMINATIVE => "(nominative)"
        case INFLECT_ACCUSATIVE => "(accusative)"
        case INFLECT_ADPOSITIONED => "(adpositioned)"
        case INFLECT_GENITIVE => "(genitive)"
        case INFLECT_COMPLEMENT => "(complement)"
      }
      compose(lemma, marker)
    }
  }

  def conjugateAction(lemma : String) =
  {
    compose(lemma, "(action)")
  }

  def conjugateImperative(lemma : String) =
  {
    compose(lemma, "(imperative)")
  }

  def conjugateAdjective(lemma : String, tam : SilTam) =
  {
    compose(lemma, "(subject complement)")
  }

  def qualifyAdjective(lemma : String) =
  {
    compose(lemma, "(qualifying adjective)")
  }

  override def respondToQuery(sentence : String) =
  {
    sentence
  }

  override def respondToCounterfactual(sentence : String) =
  {
    // FIXME should use 네요 sentence ending as well?
    compose("하지만 ", sentence)
  }

  override def respondAmbiguous(noun : SilWord) =
  {
    compose("무슨", noun.toLemma, "?")
  }

  override def respondUnknown(word : SilWord) =
  {
    "FIXME"
  }

  override def respondUnknownState(subject : String, word : SilWord) =
  {
    "FIXME"
  }

  override def respondUnknownPronoun(pronoun : String) =
  {
    "FIXME"
  }

  override def respondNonexistent(noun : SilWord) =
  {
    compose(noun.toLemma, "없어요")
  }

  override def respondCannotUnderstand() =
  {
    "이해 못 해요."
  }

  override def respondCompliance() =
  {
    "내, 알겠습니다."
  }

  override def circularAction() =
  {
    "Oopsie."
  }

  override def respondDontKnow() =
  {
    "몰라요."
  }

  override def respondNotUnderstood(
    tam : SilTam, predicate : String, errorPhrase : String) =
  {
    "FIXME"
  }

  override def predicateUnrecognizedSubject(
    tam : SilTam, complement : String, verbSeq : Seq[String],
    count : SilCount, changeVerb : Option[SilWord],
    question : Option[SilQuestion]) =
  {
    "FIXME"
  }

  override def predicateUnrecognizedComplement(
    tam : SilTam, subject : String,
    verbSeq : Seq[String],
    question : Option[SilQuestion],
    isRelationship : Boolean) =
  {
    "FIXME"
  }

  override def affirmAssumption(sentence : String, strength : Boolean) =
  {
    if (sentence.isEmpty) {
      "내."
    } else {
      val prefixed = {
        if (strength) {
          compose("맞아요,", sentence)
        } else {
          sentence
        }
      }
      compose("내,", prefixed)
    }
  }

  override def contradictAssumption(sentence : String, strength : Boolean) =
  {
    if (sentence.isEmpty) {
      "아니요."
    } else {
      val prefixed = {
        if (strength) {
          compose("실은", sentence)
        } else {
          sentence
        }
      }
      compose("아니요,", prefixed)
    }
  }
}
