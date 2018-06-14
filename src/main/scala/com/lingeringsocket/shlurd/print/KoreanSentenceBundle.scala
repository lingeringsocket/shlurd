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
package com.lingeringsocket.shlurd.print

import com.lingeringsocket.shlurd.parser._

// FIXME:  this is terrible
import ShlurdEnglishLemmas._

class KoreanSentenceBundle extends SilSentenceBundle
{
  override def statePredicateStatement(
    subject : String, copula : Seq[String], state : String) =
  {
    if (state.isEmpty) {
      compose((Seq(subject) ++ copula):_*)
    } else {
      compose(subject, state)
    }
  }

  override def relationshipPredicateStatement(
    subject : String, copula : Seq[String], complement : String) =
  {
    // FIXME
    compose((Seq(subject) ++ Seq(complement) ++ copula):_*)
  }

  override def statePredicateQuestion(
    subject : String, copula : Seq[String], state : String,
    question : Option[SilQuestion]) =
  {
    // only holds for "요" politeness
    statePredicateStatement(subject, copula, state)
  }

  override def relationshipPredicateQuestion(
    subject : String, copula : Seq[String], complement : String) =
  {
    // only holds for "요" politeness
    statePredicateStatement(subject, copula, complement)
  }

  override def statePredicateCommand(subject : String, state : String) =
    compose(subject, state)

  override def copula(
    person : SilPerson, gender : SilGender, count : SilCount,
    mood : SilMood, isExistential : Boolean,
    verbLemma : String) =
  {
    val exists = isExistential || (verbLemma == LEMMA_HAVE)
    mood match {
      case modalMood : SilModalMood => {
        // FIXME:  use modalMood.getModality
        if (modalMood.isPositive) {
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
      case _ => {
        throw SilSentenceUnprintable()
      }
    }
  }

  override def adpositionString(adposition : SilAdposition) =
  {
    val pos = adposition match {
      case ADP_INSIDE => "안"
      case ADP_OUTSIDE => "밖"
      case ADP_AT => ""
      case ADP_AS => "FIXME"
      // FIXME:  distinguish "near" from "next to"
      case ADP_NEAR => "근처"
      case ADP_ON => "위"
      case ADP_ABOVE => "위"
      case ADP_BELOW => "밑"
      // FIXME:  need to attach 의 to previous word
      case ADP_LEFT => "왼쪽"
      case ADP_RIGHT => "오른쪽"
      case ADP_FRONT => "앞"
      case ADP_BEHIND => "뒤"
      // FIXME:  context-dependent
      case ADP_WITH => "하고"
      case ADP_OF | ADP_GENITIVE_OF => "FIXME"
    }
    // later need to distinguish 에 from 에서
    compose(concat(pos, "에"), "있어요")
  }

  override def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) =
  {
    // FIXME:  use changeVerb
    conjugateImperative(state.lemma)
  }

  override def delemmatizeNoun(
    entity : SilWord,
    count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    inflectNoun(entity.lemma, count, inflection, conjoining)
  }

  override def delemmatizeState(
    state : SilWord, mood : SilMood, conjoining : SilConjoining) =
  {
    // FIXME:  conjoining
    conjugateAdjective(state.lemma, mood)
  }

  override def delemmatizeQualifier(qualifier : SilWord) =
  {
    qualifyAdjective(qualifier.lemma)
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

  override def composeQualifiers(qualifiers : Seq[SilWord]) =
  {
    compose(qualifiers.map(delemmatizeQualifier(_)) :_*)
  }

  override def query(noun : String, question : Option[SilQuestion]) =
  {
    question match {
      case Some(QUESTION_WHICH) => {
        compose("무슨", noun)
      }
      case Some(QUESTION_WHO) => {
        // FIXME inflection
        "누구"
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
    inflection : SilInflection, conjoining : SilConjoining) =
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
          case GENDER_N => inflectPronoun("그것", inflection, conjoining)
        }
        case COUNT_PLURAL => inflectPronoun("그들", inflection, conjoining)
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
            case INFLECT_NONE => ""
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
        case INFLECT_GENITIVE => "(genitive)"
      }
      compose(lemma, marker)
    }
  }

  def conjugateImperative(lemma : String) =
  {
    compose(lemma, "(imperative)")
  }

  def conjugateAdjective(lemma : String, mood : SilMood) =
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
    compose("무슨", noun.lemma, "?")
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
    compose(noun.lemma, "없어요")
  }

  override def respondCannotUnderstand() =
  {
    "이해 못 해요."
  }

  override def respondCompliance() =
  {
    "내, 알겠습니다."
  }

  override def respondDontKnow() =
  {
    "몰라요."
  }

  override def respondNotUnderstood(
    mood : SilMood, predicate : String, errorPhrase : String) =
  {
    "FIXME"
  }

  override def predicateUnrecognizedSubject(
    mood : SilMood, complement : String, copula : Seq[String],
    count : SilCount, changeVerb : Option[SilWord],
    question : Option[SilQuestion]) =
  {
    "FIXME"
  }

  override def predicateUnrecognizedComplement(
    mood : SilMood, subject : String,
    copula : Seq[String],
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
