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

class KoreanSentenceBundle extends ShlurdSentenceBundle
{
  override def command(s : String) =
    concat(s, ".")

  override def statement(s : String) =
    concat(s, ".")

  override def question(s : String) =
    concat(s, "?")

  override def statePredicateStatement(
    subject : String, copula : String, state : String) =
  {
    compose(subject, state)
  }

  override def statePredicateQuestion(
    subject : String, copula : String, state : String) =
  {
    compose(subject, state)
  }

  override def statePredicateCommand(subject : String, state : String) =
    compose(subject, state)

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood) =
  {
    mood match {
      case MOOD_INDICATIVE_POSITIVE | MOOD_INTERROGATIVE => {
        // FIXME:  use "예요" after vowel
        "이에요"
      }
      case MOOD_INDICATIVE_NEGATIVE => {
        "아니에요"
      }
      case _ => {
        throw ShlurdSentenceUnprintable()
      }
    }
  }

  override def determine(determiner : ShlurdDeterminer) =
  {
    determiner match {
      case DETERMINER_NONE => throw ShlurdSentenceUnprintable()
      case DETERMINER_ALL => "모든"
      case _ => ""
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
    }
    // later need to distinguish 에 from 에서
    compose(concat(pos, "에"), "있어요")
  }

  override def changeStateVerb(state : ShlurdWord) =
  {
    conjugateImperative(state.lemma)
  }

  override def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount, inflection : ShlurdInflection) =
  {
    inflectNoun(entity.lemma, count, inflection)
  }

  override def delemmatizeState(state : ShlurdWord, mood : ShlurdMood) =
  {
    conjugateAdjective(state.lemma, mood)
  }

  override def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    qualifyAdjective(qualifier.lemma)
  }

  override def composeQualifiers(qualifiers : Seq[ShlurdWord]) =
  {
    compose(qualifiers.map(delemmatizeQualifier(_)) :_*)
  }

  override def qualifiedNoun(qualifiers : String, noun : String) =
  {
    compose(qualifiers, noun)
  }

  override def determinedNoun(determiner : String, noun : String) =
  {
    compose(determiner, noun)
  }

  override def locationalNoun(position : String, noun : String) =
  {
    compose(noun, position)
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    compose(genitive, head)
  }

  private def inflectPronoun(pn : String, inflection : ShlurdInflection) =
  {
    inflectNoun(pn, COUNT_SINGULAR, inflection)
  }

  override def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection) =
  {
    person match {
      case PERSON_FIRST => count match {
        case COUNT_SINGULAR => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun("내", inflection)
          case INFLECT_GENITIVE => "내"
          case _ => inflectPronoun("나", inflection)
        }
        case COUNT_PLURAL => inflectPronoun("우리", inflection)
      }
      case PERSON_SECOND => count match {
        case COUNT_SINGULAR => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun("니", inflection)
          case INFLECT_GENITIVE => "네"
          case _ => inflectPronoun("너", inflection)
        }
        case COUNT_PLURAL => inflectPronoun("여러분", inflection)
      }
      case PERSON_THIRD => count match {
        case COUNT_SINGULAR => gender match {
          case GENDER_M => inflectPronoun("그", inflection)
          case GENDER_F => inflectPronoun("그녀", inflection)
          case GENDER_N => inflectPronoun("그것", inflection)
        }
        case COUNT_PLURAL => inflectPronoun("그들", inflection)
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

  def inflectNoun(lemma : String, count : ShlurdCount,
    inflection : ShlurdInflection) =
  {
    if (lemma.exists(c => isHangul(c))) {
      val numbered = count match {
        case COUNT_SINGULAR => lemma
        case COUNT_PLURAL => concat(lemma, "들")
      }
      val marker = {
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
