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
    phrase(concat(s, "."))

  override def statement(s : String) =
    phrase(concat(s, "."))

  override def question(s : String) =
    phrase(concat(s, "?"))

  override def statePredicateStatement(
    subject : String, copula : String, state : String) =
  {
    phrase(compose(subject, state))
  }

  override def statePredicateQuestion(
    subject : String, copula : String, state : String) =
  {
    phrase(compose(subject, state))
  }

  override def statePredicateCommand(subject : String, state : String) =
    phrase(compose(subject, state))

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount) =
  {
    phrase("이에요")
  }

  override def determiner(quantifier : ShlurdQuantifier) =
  {
    quantifier match {
      case QUANT_NONE => throw ShlurdSentenceUnprintable()
      case QUANT_ALL => phrase("모든")
      case _ => phrase("")
    }
  }

  override def position(locative : ShlurdLocative) =
  {
    val pos = locative match {
      case LOC_INSIDE => phrase("안")
      case LOC_OUTSIDE => phrase("밖")
      case LOC_AT => phrase("")
      // FIXME:  distinguish "near" from "next to"
      case LOC_NEAR => phrase("근처")
      case LOC_ON => phrase("위")
      case LOC_ABOVE => phrase("위")
      case LOC_BELOW => phrase("밑")
      // FIXME:  need to attach 의 to previous word
      case LOC_LEFT => phrase("왼쪽")
      case LOC_RIGHT => phrase("오른쪽")
      case LOC_FRONT => phrase("앞")
      case LOC_BEHIND => phrase("뒤")
    }
    // later need to distinguish 에 from 에서
    phrase(compose(concat(pos, "에"), "있어요"))
  }

  override def changeStateVerb(state : ShlurdWord) =
  {
    conjugateImperative(state.lemma)
  }

  override def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount, mark : ShlurdMark) =
  {
    markNoun(entity.lemma, count, mark)
  }

  override def delemmatizeState(state : ShlurdWord) =
  {
    conjugateAdjective(state.lemma)
  }

  override def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    qualifyAdjective(qualifier.lemma)
  }

  override def composeQualifiers(qualifiers : Seq[ShlurdWord]) =
  {
    phrase(compose(qualifiers.map(delemmatizeQualifier(_)) :_*))
  }

  override def qualifiedNoun(qualifiers : String, noun : String) =
  {
    phrase(compose(qualifiers, noun))
  }

  override def determinedNoun(determiner : String, noun : String) =
  {
    phrase(compose(determiner, noun))
  }

  override def locationalNoun(position : String, noun : String) =
  {
    phrase(compose(noun, position))
  }

  private def markPronoun(pn : String, mark : ShlurdMark) =
  {
    markNoun(pn, COUNT_SINGULAR, mark)
  }

  override def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mark : ShlurdMark) =
  {
    val s = person match {
      case PERSON_FIRST => count match {
        case COUNT_SINGULAR => mark match {
          case MARK_SUBJECT => markPronoun("내", mark)
          case _ => markPronoun("나", mark)
        }
        case COUNT_PLURAL => markPronoun("우리", mark)
      }
      case PERSON_SECOND => count match {
        case COUNT_SINGULAR => mark match {
          case MARK_SUBJECT => markPronoun("니", mark)
          case _ => markPronoun("너", mark)
        }
        case COUNT_PLURAL => markPronoun("여러분", mark)
      }
      case PERSON_THIRD => count match {
        case COUNT_SINGULAR => gender match {
          case GENDER_M => markPronoun("그", mark)
          case GENDER_F => markPronoun("그녀", mark)
          case GENDER_N => markPronoun("그것", mark)
        }
        case COUNT_PLURAL => markPronoun("그들", mark)
      }
    }
    phrase(s)
  }

  override def unknownSentence() =
  {
    phrase("모모모")
  }

  override def unknownReference() =
  {
    phrase("뭐뭐뭐")
  }

  override def unknownState() =
  {
    phrase("뭐뭐뭐")
  }

  override def unknownCopula() =
  {
    phrase("뭐뭐뭐요")
  }

  override def unknownPredicateStatement() =
  {
    phrase("모모모")
  }

  override def unknownPredicateCommand() =
  {
    phrase("모모모")
  }

  override def unknownPredicateQuestion() =
  {
    phrase("모모모")
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

  def markNoun(lemma : String, count : ShlurdCount, mark : ShlurdMark) =
  {
    if (lemma.exists(c => isHangul(c))) {
      val numbered = count match {
        case COUNT_SINGULAR => lemma
        case COUNT_PLURAL => concat(lemma, "들")
      }
      val marker = {
        mark match {
          case MARK_NONE => ""
          case MARK_SUBJECT => {
            if (hasFinalConsonant(numbered)) {
              "이"
            } else {
              "가"
            }
          }
          case MARK_DIRECT_OBJECT => {
            if (hasFinalConsonant(numbered)) {
              "을"
            } else {
              "를"
            }
          }
        }
      }
      phrase(concat(numbered, marker))
    } else {
      val marker = mark match {
        case MARK_NONE => ""
        case MARK_SUBJECT => "(subject)"
        case MARK_DIRECT_OBJECT => "(direct object)"
      }
      phrase(compose(lemma, marker))
    }
  }

  def conjugateImperative(lemma : String) =
  {
    phrase(compose(lemma, "(imperative)"))
  }

  def conjugateAdjective(lemma : String) =
  {
    phrase(compose(lemma, "(subject complement)"))
  }

  def qualifyAdjective(lemma : String) =
  {
    phrase(compose(lemma, "(qualifying adjective)"))
  }
}
