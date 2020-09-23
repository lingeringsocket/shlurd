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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

class SnlKoreanSentenceBundle extends SilSentenceBundle
{
  // FIXME this should be SnlKoreanTongue, which doesn't exist yet.
  private implicit val tongue = SnlUtils.defaultTongue

  override def getTongue = tongue

  override def statePredicateStatement(
    subject : String, verbSeq : Seq[String], state : String,
    existentialPronoun : Option[SilWord],
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
    verb : SilWord,
    question : Option[SilQuestion],
    tam : SilTam,
    modifiers : Seq[String]) =
  {
    // FIXME
    compose((Seq(subject) ++ modifiers ++ Seq(complement) ++ verbSeq):_*)
  }

  override def statePredicateQuestion(
    subject : String, verbSeq : Seq[String], state : String,
    existentialPronoun : Option[SilWord], question : Option[SilQuestion],
    modifiers : Seq[String],
    answerInflection : SilInflection) =
  {
    // FIXME:  only holds for "요" politeness
    statePredicateStatement(
      subject, verbSeq, state, existentialPronoun, modifiers)
  }

  override def statePredicateCommand(subject : String, state : String,
    modifiers : Seq[String]) =
  {
    compose((Seq(subject) ++ modifiers ++ Seq(state)):_*)
  }

  override def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord, answerInflection : SilInflection) =
  {
    // FIXME arbitrary lemmas
    val verbLemma = verb.toLemma
    val exists = existentialPronoun.nonEmpty ||
      tongue.isPossessionLemma(verbLemma)
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
    val pos = SprPredefWord.unapply(adposition.word).map(_ match {
      case PD_IN | PD_WITHIN | PD_INSIDE => "안"
      case PD_OUTSIDE => "밖"
      case PD_AT => ""
      // FIXME:  distinguish "near" from "next to"
      case PD_NEAR | PD_NEARBY => "근처"
      case PD_ON | PD_ABOVE | PD_OVER => "위"
      // case PD_BELOW | PD_BENEATH => "밑"
      // FIXME:  need to attach 의 to previous word
      case PD_LEFT => "왼쪽"
      case PD_RIGHT => "오른쪽"
      case PD_FRONT => "앞"
      case PD_BACK | PD_BEHIND => "뒤"
      // FIXME:  context-dependent
      case PD_WITH => "하고"
      // FIXME:  OF etc
      case _ => ""
    }).filterNot(_.isEmpty).getOrElse {
      compose(adposition.word.decomposed.map(_.lemma):_*)
    }
    // later need to distinguish 에 from 에서
    compose(concat(pos, "에"), "있어요")
  }

  override def actionVerb(
    verb : SilWord) =
  {
    conjugateAction(verb.toLemma)
  }

  override def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) =
  {
    // FIXME:  use changeVerb
    conjugateImperative(state.toLemma)
  }

  override def delemmatizeNoun(
    noun : SilWord,
    gender : SilGender,
    count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    inflectNoun(noun.toNounLemma, count, inflection, conjoining)
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
    conjunction : SilWord,
    antecedent : String,
    consequent : String,
    biconditional : Boolean) =
  {
    // FIXME proper conjugation, conjunction variation, biconditional
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

  override def specifiedNoun(specifier : String, noun : String) =
  {
    compose(specifier, noun)
  }

  override def determinedNoun(
    determiner : SilDeterminer,
    noun : String,
    person : SilPerson,
    gender : SilGender,
    count : SilCount) =
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

  override def applyInflection(
    base : String, count : SilCount, inflection : SilInflection) : String =
  {
    inflection match {
      case INFLECT_GENITIVE => {
        concat(base, "의")
      }
      case _ => base
    }
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
    proximity : SilProximity, word : Option[SilWord],
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    // FIXME handle reflexives and PROXIMITY_POSSESSEE, e.g. "mine" => "내것"
    // FIXME use word
    def standard = person match {
      case PERSON_FIRST => count match {
        case COUNT_PLURAL => inflectPronoun("우리", inflection, conjoining)
        case _ => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun("내", inflection, conjoining)
          case INFLECT_GENITIVE => "내"
          case _ => inflectPronoun("나", inflection, conjoining)
        }
      }
      case PERSON_SECOND => count match {
        case COUNT_PLURAL => inflectPronoun("여러분", inflection, conjoining)
        case _ => inflection match {
          case INFLECT_NOMINATIVE => inflectPronoun(
            "니", inflection, conjoining)
          case INFLECT_GENITIVE => "네"
          case _ => inflectPronoun("너", inflection, conjoining)
        }
      }
      case PERSON_THIRD => count match {
        case COUNT_PLURAL => {
          proximity match {
            case _ : SilHereProximity =>
              inflectPronoun("이것들", inflection, conjoining)
            case PROXIMITY_LISTENER_THERE =>
              inflectPronoun("그것들", inflection, conjoining)
            case PROXIMITY_OVER_THERE | PROXIMITY_WAY_OVER_THERE =>
              inflectPronoun("저것들", inflection, conjoining)
            case PROXIMITY_ENTITY | PROXIMITY_REFLEXIVE =>
              inflectPronoun("그들", inflection, conjoining)
            case PROXIMITY_POSSESSEE =>
              inflectPronoun("그들의것", inflection, conjoining)
            case PROXIMITY_ELIDED => ""
          }
        }
        case _ => gender.maybeBasic match {
          case Some(GENDER_MASCULINE | GENDER_SOMEONE) =>
            inflectPronoun("그", inflection, conjoining)
          case Some(GENDER_FEMININE) =>
            inflectPronoun("그녀", inflection, conjoining)
          case Some(GENDER_NEUTER) => {
            proximity match {
              case _ : SilHereProximity =>
                inflectPronoun("이것", inflection, conjoining)
              case PROXIMITY_LISTENER_THERE =>
                inflectPronoun("그것", inflection, conjoining)
              case PROXIMITY_ELIDED => ""
              case _ =>
                inflectPronoun("저것", inflection, conjoining)
            }
          }
          case Some(GENDER_SOMEWHERE) => {
            proximity match {
              case _ : SilHereProximity =>
                inflectPronoun("이곳", inflection, conjoining)
              case PROXIMITY_LISTENER_THERE =>
                inflectPronoun("그곳", inflection, conjoining)
              case PROXIMITY_ELIDED => ""
              case _ =>
                inflectPronoun("저곳", inflection, conjoining)
            }
          }
          case None => {
            throw new IllegalArgumentException("custom pronoun word required")
          }
        }
      }
    }
    word.map(w => w.recompose(w.decomposed.map(_.inflected))).
      getOrElse(standard)
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
        case COUNT_PLURAL => concat(lemma, "들")
        case _ => lemma
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
              // FIXME this is only correct for SilAdposition(PD_TO);
              // also, should take formality into account
              "에게"
            }
            case INFLECT_GENITIVE => "의"
          }
        } else {
          conjoining.determiner match {
            case (_ : SilUnlimitedDeterminer) | DETERMINER_DEFINITE => {
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
    compose("무슨", noun.toNounLemma, "?")
  }

  override def respondMisqualifiedNoun(
    noun : SilWord, qualifiers : Seq[String]) : String =
  {
    "FIXME"
  }

  override def respondUnknown(word : SilWord) =
  {
    "FIXME"
  }

  override def respondUnknownModifier(word : SilWord) =
  {
    "FIXME"
  }

  override def respondIrrelevant() =
  {
    "그래요?"
  }

  override def respondTriggerLimit() =
  {
    "대박!"
  }

  override def respondUnknownState(subject : String, word : SilWord) =
  {
    "FIXME"
  }

  override def respondAmbiguousPronoun(pronoun : String) =
  {
    "FIXME"
  }

  override def respondUnresolvedPronoun(pronoun : String) =
  {
    "FIXME"
  }

  override def respondNonexistent(noun : SilWord) =
  {
    compose(noun.toNounLemma, "없어요")
  }

  override def respondCannotUnderstand() =
  {
    "이해 못 해요."
  }

  override def respondCompliance() =
  {
    "내, 알겠습니다."
  }

  override def respondNoncommittal() =
  {
    "아, 그래요?"
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

  override def respondUnable(action : String) =
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

  override def existsVerb() : SilWord =
  {
    SilWord("있다")
  }
}
