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

import com.ibm.icu.text._

import java.util._

import SnlEnglishLemmas._
import ShlurdEnglishAffixes._

object SnlEnglishSentenceBundle
{
  private val numberFormat = new RuleBasedNumberFormat(
    Locale.ENGLISH, RuleBasedNumberFormat.SPELLOUT);
}

class SnlEnglishSentenceBundle(
  tongue : SprTongue
) extends SnlSentenceBundle(
  tongue,
  SnlEnglishSentenceBundle.numberFormat)
{
  private def delemmatizeModalVerb(
    tam : SilTam, verb : SilWord,
    person : SilPerson, gender : SilGender, count : SilCount)
      : Seq[String] =
  {
    val verbLemma = verb.toLemma
    val modality = {
      verbLemma match {
        case LEMMA_BE => tam.modality
        case _ => tam.modality match {
          case MODAL_NEUTRAL => MODAL_EMPHATIC
          case _ => tam.modality
        }
      }
    }
    val aux = {
      if (tam.isProgressive) {
        delemmatizeModelessVerb(person, gender, count, SilWord(LEMMA_BE), tam)
      } else {
        // FIXME conjugate result of auxVerbForModal?
        modality match {
          case MODAL_NEUTRAL => ""
          case MODAL_MUST => LEMMA_MUST
          case MODAL_MAY => LEMMA_MAY
          case MODAL_POSSIBLE => LEMMA_MIGHT
          case MODAL_CAPABLE => LEMMA_CAN
          case MODAL_PERMITTED => LEMMA_MAY
          case MODAL_SHOULD => LEMMA_SHOULD
          case MODAL_EMPHATIC | MODAL_ELLIPTICAL => {
            tam.tense match {
              case TENSE_PAST => "did"
              case TENSE_FUTURE => LEMMA_DO
              case TENSE_PRESENT => count match {
                case COUNT_PLURAL => LEMMA_DO
                case _ => {
                  person match {
                    case PERSON_THIRD => "does"
                    case _ => LEMMA_DO
                  }
                }
              }
              // not really meaningful here
              case TENSE_INFINITIVE => ""
            }
          }
        }
      }
    }
    val prefix = {
      if (tam.isNegative) {
        Seq(aux, LEMMA_NOT)
      } else {
        Seq(aux)
      }
    }
    if (tam.isProgressive) {
      prefix :+ delemmatizeProgressive(verb)
    } else {
      modality match {
        case MODAL_ELLIPTICAL => prefix
        case _ => prefix :+ verbLemma
      }
    }
  }

  private def delemmatizeModelessVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    word : SilWord, tam : SilTam
  ) : String =
  {
    val verb = word match {
      case sw : SilSimpleWord => {
        sw
      }
      case SilCompoundWord(components, style, _) => {
        return word.recompose(
          components.map(c =>
            delemmatizeModelessVerb(person, gender, count, c, tam)))
      }
    }
    val verbLemma = verb.toLemma
    if (tam.isImperative) {
      return verbLemma
    }
    verbLemma match {
      case LEMMA_BE => {
        tam.tense match {
          case TENSE_PAST => count match {
            case COUNT_PLURAL => "were"
            case _ => person match {
              case PERSON_SECOND => "were"
              case _ => "was"
            }
          }
          case TENSE_PRESENT => count match {
            case COUNT_PLURAL => "are"
            case _ => person match {
              case PERSON_FIRST => "am"
              case PERSON_SECOND => "are"
              case PERSON_THIRD => "is"
            }
          }
          case TENSE_FUTURE | TENSE_INFINITIVE => LEMMA_BE
        }
      }
      case LEMMA_HAVE => {
        tam.tense match {
          case TENSE_PAST => "had"
          case TENSE_FUTURE | TENSE_INFINITIVE => LEMMA_HAVE
          case TENSE_PRESENT => (person, count) match {
            case (PERSON_THIRD, COUNT_SINGULAR) => "has"
            case _ => LEMMA_HAVE
          }
        }
      }
      case _ => {
        if (verb.inflected.isEmpty) {
          // FIXME irregulars
          tam.tense match {
            case TENSE_PAST => {
              if (verbLemma.last == 'e') {
                concat(verbLemma, "d")
              } else {
                concat(verbLemma, "ed")
              }
            }
            case TENSE_FUTURE | TENSE_INFINITIVE => verbLemma
            case TENSE_PRESENT => (person, count) match {
              case (PERSON_THIRD, COUNT_SINGULAR) => concat(verbLemma, "s")
              case _ => verbLemma
            }
          }
        } else {
          verb.inflected
        }
      }
    }
  }

  override def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord,
    answerInflection : SilInflection
  )
      : Seq[String] =
  {
    val verbLemma = verb.toLemma
    if ((verbLemma != LEMMA_BE) && (verbLemma != LEMMA_EXIST) &&
      (tam.isNegative ||
        (tam.isInterrogative && (answerInflection != INFLECT_NOMINATIVE))))
    {
      return delemmatizeModalVerb(tam, verb, person, gender, count)
    }
    val seq = {
      if (tam.requiresAux) {
        delemmatizeModalVerb(tam, verb, person, gender, count)
      } else {
        val inflected = delemmatizeModelessVerb(
          person, gender, count, verb, tam)
        if (tam.isNegative) {
          Seq(inflected, LEMMA_NOT)
        } else {
          Seq(inflected)
        }
      }
    }
    existentialPronoun.map(_.toLemma).toSeq ++ seq
  }

  override def applyInflection(
    base : String, count : SilCount, inflection : SilInflection) : String =
  {
    inflection match {
      case INFLECT_GENITIVE => {
        count match {
          case COUNT_PLURAL => {
            concat(base, "'")
          }
          case _ => {
            if (base.endsWith("s")) {
              concat(base, "'")
            } else {
              concat(base, "'s")
            }
          }
        }
      }
      case _ => base
    }
  }

  private def delemmatizeProgressive(word : SilWord) : String =
  {
    val decomposed = word.decomposed
    val verb = decomposed.last
    val delemmatized = {
      if (verb.inflected.isEmpty) {
        // FIXME sometimes we need more morphing on the lemma first...
        val base = {
          if (verb.lemma == LEMMA_BE) {
            verb.lemma
          } else {
            verb.lemma.stripSuffix("e")
          }
        }
        concat(base, SUFFIX_ING)
      } else {
        verb.inflected
      }
    }
    word.recompose(decomposed.dropRight(1).map(_.inflected) :+ delemmatized)
  }

  override def delemmatizeState(
    word : SilWord, tam : SilTam, conjoining : SilConjoining) =
  {
    val decomposed = word.decomposed
    val state = decomposed.last
    val unseparated = {
      if (state.inflected.isEmpty) {
        val lemma = state.lemmaUnfolded
        if (lemma.endsWith("ed")) {
          lemma
        } else if (lemma.endsWith("e")) {
          concat(lemma, "d")
        } else {
          concat(lemma, "ed")
        }
      } else {
        state.inflected
      }
    }
    val seq = decomposed.dropRight(1).map(_.inflected) :+ unseparated
    separate(word.recompose(seq), conjoining)
  }

  override def genitivePhrase(
    genitive : String, head : String, isPronoun : Boolean) =
  {
    compose(genitive, head)
  }

  override def determinedNoun(
    determiner : SilDeterminer,
    noun : String,
    person : SilPerson,
    gender : SilGender,
    count : SilCount) =
  {
    val determinerString = determiner match {
      case DETERMINER_ABSENT | DETERMINER_VARIABLE => ""
      case DETERMINER_NONE => LEMMA_NO
      case DETERMINER_DEFINITE => LEMMA_THE
      case DETERMINER_NONSPECIFIC => {
        // FIXME:  in reality it can be a little more complicated...
        if ("aeiou".contains(noun.head) || noun.startsWith("spc-")) {
          "an"
        } else {
          LEMMA_A
        }
      }
      case DETERMINER_ANY => LEMMA_ANY
      case DETERMINER_SOME => LEMMA_SOME
      case DETERMINER_ALL => LEMMA_ALL
      case SilIntegerDeterminer(number : Int) => {
        cardinalNumber(number, GENDER_NEUTER, true)
      }
    }
    compose(determinerString, noun)
  }

  override def affirmation(strength : Boolean) : String =
  {
    if (strength) {
      "Right"
    } else {
      "Yes"
    }
  }

  override def negation(strength : Boolean) : String =
  {
    if (strength) {
      "No, actually"
    } else {
      "No"
    }
  }
}
