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

class EnglishSentenceBundle extends ShlurdSentenceBundle
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
    phrase(compose(subject, copula, state))
  }

  override def statePredicateQuestion(
    subject : String, copula : String, state : String) =
  {
    phrase(compose(copula, subject, state))
  }

  override def statePredicateCommand(subject : String, state : String) =
    phrase(compose(state, subject))

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount) =
  {
    val s = count match {
      case COUNT_SINGULAR => {
        person match {
          case PERSON_FIRST => "am"
          case PERSON_SECOND => "are"
          case PERSON_THIRD => "is"
        }
      }
      case COUNT_PLURAL => {
        "are"
      }
    }
    phrase(s)
  }

  override def determine(determiner : ShlurdDeterminer) =
  {
    determiner match {
      case DETERMINER_UNSPECIFIED => phrase("")
      case DETERMINER_NONE => phrase("no")
      case DETERMINER_UNIQUE => phrase("the")
      case DETERMINER_NONSPECIFIC => phrase("a")
      case DETERMINER_ANY => phrase("any")
      case DETERMINER_ALL => phrase("all")
    }
  }

  override def position(locative : ShlurdLocative) =
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

  override def changeStateVerb(state : ShlurdWord) =
    phrase(state.lemma)

  override def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount,
    inflection : ShlurdInflection) =
  {
    if (entity.inflected.isEmpty) {
      val lemma = entity.lemma
      val base = count match {
        case COUNT_SINGULAR => {
          lemma
        }
        case COUNT_PLURAL => {
          if (lemma.endsWith("s")) {
            concat(lemma, "es")
          } else {
            concat(lemma, "s")
          }
        }
      }
      inflection match {
        case INFLECT_GENITIVE => {
          count match {
            case COUNT_SINGULAR => {
              if (base.endsWith("s")) {
                phrase(concat(base, "'"))
              } else {
                phrase(concat(base, "'s"))
              }
            }
            case COUNT_PLURAL => {
              phrase(concat(base, "'"))
            }
          }
        }
        case _ => phrase(base)
      }
    } else {
      phrase(entity.inflected)
    }
  }

  override def delemmatizeState(state : ShlurdWord) =
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

  override def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    if (qualifier.inflected.isEmpty) {
      phrase(qualifier.lemma)
    } else {
      phrase(qualifier.inflected)
    }
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
    phrase(compose(position, noun))
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    phrase(compose(genitive, head))
  }

  override def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection) =
  {
    val s = person match {
      case PERSON_FIRST => count match {
        case COUNT_SINGULAR => inflection match {
          case INFLECT_ACCUSATIVE => "me"
          case INFLECT_GENITIVE => "my"
          case _ => "I"
        }
        case COUNT_PLURAL => inflection match {
          case INFLECT_ACCUSATIVE => "us"
          case INFLECT_GENITIVE => "our"
          case _ => "we"
        }
      }
      case PERSON_SECOND => inflection match {
        case INFLECT_GENITIVE => "your"
        case _ => "you"
      }
      case PERSON_THIRD => count match {
        case COUNT_SINGULAR => gender match {
          case GENDER_M => inflection match {
            case INFLECT_ACCUSATIVE => "him"
            case INFLECT_GENITIVE => "his"
            case _ => "he"
          }
          case GENDER_F => inflection match {
            case INFLECT_ACCUSATIVE | INFLECT_GENITIVE => "her"
            case _ => "she"
          }
          case GENDER_N => inflection match {
            case INFLECT_GENITIVE => "its"
            case _ => "it"
          }
        }
        case COUNT_PLURAL => inflection match {
          case INFLECT_ACCUSATIVE => "them"
          case INFLECT_GENITIVE => "their"
          case _ => "they"
        }
      }
    }
    phrase(s)
  }

  override def unknownSentence() =
  {
    phrase("blah blah blah")
  }

  override def unknownReference() =
  {
    phrase("something or other")
  }

  override def unknownState() =
  {
    phrase("discombobulated")
  }

  override def unknownCopula() =
  {
    phrase("be")
  }

  override def unknownPredicateStatement() =
  {
    phrase("foo is bar")
  }

  override def unknownPredicateCommand() =
  {
    phrase("make it so")
  }

  override def unknownPredicateQuestion() =
  {
    phrase("is it what now")
  }
}
