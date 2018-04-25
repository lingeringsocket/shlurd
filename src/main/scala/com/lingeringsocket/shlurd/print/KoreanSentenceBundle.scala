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
package com.lingeringsocket.shlurd.print

import com.lingeringsocket.shlurd.parser._

class KoreanSentenceBundle extends ShlurdSentenceBundle
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

  override def identityPredicateStatement(
    subject : String, copula : Seq[String], complement : String) =
  {
    // FIXME
    compose((Seq(subject) ++ Seq(complement) ++ copula):_*)
  }

  override def statePredicateQuestion(
    subject : String, copula : Seq[String], state : String) =
  {
    // only holds for "요" politeness
    statePredicateStatement(subject, copula, state)
  }

  override def identityPredicateQuestion(
    subject : String, copula : Seq[String], complement : String) =
  {
    // only holds for "요" politeness
    statePredicateStatement(subject, copula, complement)
  }

  override def statePredicateCommand(subject : String, state : String) =
    compose(subject, state)

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood, isExistential : Boolean) =
  {
    mood match {
      case modalMood : ShlurdModalMood => {
        // FIXME:  use modalMood.getModality
        if (modalMood.isPositive) {
          if (isExistential) {
            Seq("있어요")
          } else {
            // FIXME:  use "예요" after vowel
            Seq("이에요")
          }
        } else {
          if (isExistential) {
            Seq("없어요")
          } else {
            Seq("아니에요")
          }
        }
      }
      case _ => {
        throw ShlurdSentenceUnprintable()
      }
    }
  }

  override def position(locative : ShlurdLocative) =
  {
    val pos = locative match {
      case LOC_INSIDE => "안"
      case LOC_OUTSIDE => "밖"
      case LOC_AT => ""
      // FIXME:  distinguish "near" from "next to"
      case LOC_NEAR => "근처"
      case LOC_ON => "위"
      case LOC_ABOVE => "위"
      case LOC_BELOW => "밑"
      // FIXME:  need to attach 의 to previous word
      case LOC_LEFT => "왼쪽"
      case LOC_RIGHT => "오른쪽"
      case LOC_FRONT => "앞"
      case LOC_BEHIND => "뒤"
      case LOC_GENITIVE_OF => "FIXME"
    }
    // later need to distinguish 에 from 에서
    compose(concat(pos, "에"), "있어요")
  }

  override def changeStateVerb(state : ShlurdWord) =
  {
    conjugateImperative(state.lemma)
  }

  override def delemmatizeNoun(
    entity : ShlurdWord,
    count : ShlurdCount,
    inflection : ShlurdInflection,
    conjoining : ShlurdConjoining) =
  {
    inflectNoun(entity.lemma, count, inflection, conjoining)
  }

  override def delemmatizeState(
    state : ShlurdWord, mood : ShlurdMood, conjoining : ShlurdConjoining) =
  {
    // FIXME:  conjoining
    conjugateAdjective(state.lemma, mood)
  }

  override def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    qualifyAdjective(qualifier.lemma)
  }

  override def conjoin(
    determiner : ShlurdDeterminer,
    separator : ShlurdSeparator,
    inflection : ShlurdInflection,
    items : Seq[String]) =
  {
    // FIXME:  deal with other determiners such as DETERMINER_NONE
    compose(items:_*)
  }

  override def composeQualifiers(qualifiers : Seq[ShlurdWord]) =
  {
    compose(qualifiers.map(delemmatizeQualifier(_)) :_*)
  }

  override def query(noun : String, question : Option[ShlurdQuestion]) =
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

  override def determinedNoun(determiner : ShlurdDeterminer, noun : String) =
  {
    val determinerString = determiner match {
      case DETERMINER_NONE => throw ShlurdSentenceUnprintable()
      case DETERMINER_ALL => "모든"
      // FIXME:  sentence order is very much case-by-case
      case _ => ""
    }
    compose(determinerString, noun)
  }

  override def locationalNoun(
    position : String, noun : String, conjoining : ShlurdConjoining) =
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
    inflection : ShlurdInflection,
    conjoining : ShlurdConjoining) =
  {
    inflectNoun(pn, COUNT_SINGULAR, inflection, conjoining)
  }

  override def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection, conjoining : ShlurdConjoining) =
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
    lemma : String, count : ShlurdCount,
    inflection : ShlurdInflection, conjoining : ShlurdConjoining) =
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

  def conjugateAdjective(lemma : String, mood : ShlurdMood) =
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

  override def respondAmbiguous(entity : String) =
  {
    compose("무슨", entity, "?")
  }

  override def respondUnique(entity : String) =
  {
    "FIXME"
  }

  override def respondUnknown(entity : String) =
  {
    "FIXME"
  }

  override def respondUnknownPronoun(entity : String) =
  {
    "FIXME"
  }

  override def respondNonexistent(entity : String) =
  {
    compose(entity, "없어요")
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

  override def affirmAssumption(sentence : String, strength : Boolean) =
  {
    val prefixed = {
      if (strength) {
        compose("맞아요,", sentence)
      } else {
        sentence
      }
    }
    compose("내,", prefixed)
  }

  override def contradictAssumption(sentence : String, strength : Boolean) =
  {
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
