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

import ShlurdEnglishLemmas._

class EnglishSentenceBundle
    extends ShlurdSentenceBundle
{
  override def statePredicateStatement(
    subject : String, copula : Seq[String], state : String) =
  {
    if (state.isEmpty) {
      // existential
      compose((copula ++ Seq(subject)):_*)
    } else {
      composePredicateStatement(subject, copula, state)
    }
  }

  override def relationshipPredicateStatement(
    subject : String, verb : Seq[String], complement : String) =
  {
    composePredicateStatement(subject, verb, complement)
  }

  private def composePredicateStatement(
    subject : String, verb : Seq[String], complement : String) =
  {
    compose((Seq(subject) ++ verb ++ Seq(complement)):_*)
  }

  override def statePredicateQuestion(
    subject : String, copula : Seq[String], state : String,
    question : Option[ShlurdQuestion]) =
  {
    if (!question.isEmpty) {
      compose((Seq(subject) ++ copula.take(2).reverse ++
        copula.drop(2) ++ Seq(state)):_*)
    } else if (state.isEmpty) {
      compose((copula.take(2).reverse ++ copula.drop(2) ++ Seq(subject)):_*)
    } else {
      composePredicateQuestion(subject, copula, state)
    }
  }

  override def relationshipPredicateQuestion(
    subject : String, verb : Seq[String], complement : String) =
  {
    composePredicateQuestion(subject, verb, complement)
  }

  private def composePredicateQuestion(
    subject : String, verb : Seq[String], complement : String) =
  {
    val headSeq = Seq(verb.head)
    val tailSeq = verb.drop(1)
    verb.size match {
      // "is Larry clumsy?"
      case 1 =>
        compose((headSeq ++ Seq(subject, complement)):_*)
      // "is Larry not clumsy?" or "must Larry be clumsy?"
      case 2 =>
        compose((headSeq ++ Seq(subject) ++ tailSeq ++ Seq(complement)):_*)
      // "must Larry not be clumsy?"
      case _ =>
        compose((headSeq ++ Seq(subject) ++ tailSeq ++ Seq(complement)):_*)
    }
  }

  override def statePredicateCommand(subject : String, state : String) =
    compose(state, subject)

  private def modalCopula(
    mood : ShlurdMood, verbLemma : String,
    person : ShlurdPerson, count : ShlurdCount) =
  {
    val modality = {
      verbLemma match {
        case LEMMA_BE => mood.getModality
        case _ => mood.getModality match {
          case MODAL_NEUTRAL => MODAL_EMPHATIC
          case x => x
        }
      }
    }
    val aux = modality match {
      case MODAL_NEUTRAL => ""
      case MODAL_MUST => LEMMA_MUST
      case MODAL_MAY => LEMMA_MAY
      case MODAL_POSSIBLE => LEMMA_MIGHT
      case MODAL_CAPABLE => LEMMA_CAN
      case MODAL_PERMITTED => LEMMA_MAY
      case MODAL_SHOULD => LEMMA_SHOULD
      case MODAL_EMPHATIC => {
        count match {
          case COUNT_SINGULAR => {
            person match {
              case PERSON_THIRD => LEMMA_DOES
              case _ => LEMMA_DO
            }
          }
          case COUNT_PLURAL => LEMMA_DO
        }
      }
    }
    if (mood.isNegative) {
      Seq(aux, LEMMA_NOT, verbLemma)
    } else {
      Seq(aux, verbLemma)
    }
  }

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood, isExistential : Boolean,
    verbLemma : String) : Seq[String] =
  {
    if ((verbLemma != LEMMA_BE) && mood.isNegative) {
      return modalCopula(mood, verbLemma, person, count)
    }
    val seq = mood.getModality match {
      case MODAL_NEUTRAL => {
        val inflected = {
          count match {
            case COUNT_SINGULAR => {
              verbLemma match {
                case LEMMA_BE => {
                  person match {
                    case PERSON_FIRST => "am"
                    case PERSON_SECOND => "are"
                    case PERSON_THIRD => "is"
                  }
                }
                case LEMMA_HAVE => {
                  person match {
                    case PERSON_THIRD => "has"
                    case _ => LEMMA_HAVE
                  }
                }
                case LEMMA_EXIST => {
                  person match {
                    case PERSON_THIRD => "exists"
                    case _ => LEMMA_EXIST
                  }
                }
              }
            }
            case COUNT_PLURAL => {
              verbLemma match {
                case LEMMA_BE => "are"
                case x => x
              }
            }
          }
        }
        if (mood.isNegative) {
          Seq(inflected, LEMMA_NOT)
        } else {
          Seq(inflected)
        }
      }
      case _ => {
        modalCopula(mood, verbLemma, person, count)
      }
    }
    if (isExistential) {
      Seq(LEMMA_THERE) ++ seq
    } else {
      seq
    }
  }

  override def position(locative : ShlurdLocative) =
  {
    locative match {
      case LOC_INSIDE => LEMMA_IN
      case LOC_OUTSIDE => "outside of"
      case LOC_AT => LEMMA_AT
      case LOC_AS => LEMMA_AS
      case LOC_NEAR => LEMMA_NEAR
      case LOC_ON => LEMMA_ON
      case LOC_ABOVE => LEMMA_ABOVE
      case LOC_BELOW => LEMMA_BELOW
      case LOC_LEFT => "to the left of"
      case LOC_RIGHT => "to the right of"
      case LOC_FRONT => "in front of"
      case LOC_BEHIND => LEMMA_BEHIND
      case LOC_GENITIVE_OF => LEMMA_OF
    }
  }

  override def changeStateVerb(
    state : ShlurdWord, changeVerb : Option[ShlurdWord]) =
  {
    compose(changeVerb.map(_.lemma).getOrElse(""), state.lemma)
  }

  override def delemmatizeNoun(
    entity : ShlurdWord, count : ShlurdCount,
    inflection : ShlurdInflection,
    conjoining : ShlurdConjoining) =
  {
    val unseparated = {
      if (entity.inflected.isEmpty || (inflection == INFLECT_GENITIVE)) {
        val lemma = inflection match {
          case INFLECT_GENITIVE => entity.inflected
          case _ => entity.lemma
        }
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
                  concat(base, "'")
                } else {
                  concat(base, "'s")
                }
              }
              case COUNT_PLURAL => {
                concat(base, "'")
              }
            }
          }
          case _ => base
        }
      } else {
        entity.inflected
      }
    }
    separate(unseparated, conjoining)
  }

  override def delemmatizeState(
    state : ShlurdWord, mood : ShlurdMood, conjoining : ShlurdConjoining) =
  {
    val unseparated = {
      if (state.inflected.isEmpty) {
        val lemma = state.lemma
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
    separate(unseparated, conjoining)
  }

  override def delemmatizeQualifier(qualifier : ShlurdWord) =
  {
    if (qualifier.inflected.isEmpty) {
      qualifier.lemma
    } else {
      qualifier.inflected
    }
  }

  override def conjoin(
    determiner : ShlurdDeterminer,
    separator : ShlurdSeparator,
    inflection : ShlurdInflection,
    items : Seq[String]) =
  {
    val prefix = determiner match {
      case DETERMINER_NONE => LEMMA_NEITHER
      case DETERMINER_UNIQUE => LEMMA_EITHER
      case _ => ""
    }

    val infix = determiner match {
      case DETERMINER_NONE => LEMMA_NOR
      case DETERMINER_ANY | DETERMINER_UNIQUE => LEMMA_OR
      case _ => LEMMA_AND
    }

    val seq = items.dropRight(1).zipWithIndex.flatMap {
      case (n, i) => {
        separator match {
          case SEPARATOR_CONJOINED => Seq(n, infix)
          case _ => {
            if ((i + 2) < items.size) {
              Seq(n)
            } else {
              Seq(n, infix)
            }
          }
        }
      }
    }
    compose((Seq(prefix) ++ seq ++ Seq(items.last)):_*)
  }

  override def composeQualifiers(qualifiers : Seq[ShlurdWord]) =
  {
    compose(qualifiers.map(delemmatizeQualifier(_)) :_*)
  }

  override def query(noun : String, question : Option[ShlurdQuestion]) =
  {
    question match {
      case Some(QUESTION_WHICH) => {
        compose(LEMMA_WHICH, noun)
      }
      case Some(QUESTION_WHO) => {
        // FIXME inflection for whom, whose
        compose(LEMMA_WHO)
      }
      case Some(QUESTION_HOW_MANY) => {
        compose(LEMMA_HOW, LEMMA_MANY, noun)
      }
      case None => noun
    }
  }

  override def qualifiedNoun(qualifiers : String, noun : String) =
  {
    compose(qualifiers, noun)
  }

  override def specifiedNoun(specifier : String, noun : String) =
  {
    compose(noun, specifier)
  }

  override def determinedNoun(determiner : ShlurdDeterminer, noun : String) =
  {
    val determinerString = determiner match {
      case DETERMINER_UNSPECIFIED => ""
      case DETERMINER_NONE => LEMMA_NO
      case DETERMINER_UNIQUE => LEMMA_THE
      case DETERMINER_NONSPECIFIC => {
        // FIXME:  in reality it can be a little more complicated...
        if ("aeiou".contains(noun.head)) {
          "an"
        } else {
          LEMMA_A
        }
      }
      case DETERMINER_ANY => LEMMA_ANY
      case DETERMINER_SOME => LEMMA_SOME
      case DETERMINER_ALL => LEMMA_ALL
    }
    compose(determinerString, noun)
  }

  override def locationalNoun(
    position : String, noun : String, conjoining : ShlurdConjoining) =
  {
    separate(compose(position, noun), conjoining)
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    compose(genitive, head)
  }

  override def pronoun(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    inflection : ShlurdInflection, conjoining : ShlurdConjoining) =
  {
    val unseparated = {
      person match {
        case PERSON_FIRST => count match {
          case COUNT_SINGULAR => inflection match {
            case INFLECT_ACCUSATIVE => LEMMA_ME
            case INFLECT_GENITIVE => LEMMA_MY
            case _ => "I"
          }
          case COUNT_PLURAL => inflection match {
            case INFLECT_ACCUSATIVE => LEMMA_US
            case INFLECT_GENITIVE => LEMMA_OUR
            case _ => LEMMA_WE
          }
        }
        case PERSON_SECOND => inflection match {
          case INFLECT_GENITIVE => LEMMA_YOUR
          case _ => LEMMA_YOU
        }
        case PERSON_THIRD => count match {
          case COUNT_SINGULAR => gender match {
            case GENDER_M => inflection match {
              case INFLECT_ACCUSATIVE => LEMMA_HIM
              case INFLECT_GENITIVE => LEMMA_HIS
              case _ => LEMMA_HE
            }
            case GENDER_F => inflection match {
              case INFLECT_ACCUSATIVE | INFLECT_GENITIVE => LEMMA_HER
              case _ => LEMMA_SHE
            }
            case GENDER_N => inflection match {
              case INFLECT_GENITIVE => LEMMA_ITS
              case _ => LEMMA_IT
            }
          }
          case COUNT_PLURAL => inflection match {
            case INFLECT_ACCUSATIVE => LEMMA_THEM
            case INFLECT_GENITIVE => LEMMA_THEIR
            case _ => LEMMA_THEY
          }
        }
      }
    }
    separate(unseparated, conjoining)
  }

  override def unknownSentence() =
  {
    "blah blah blah"
  }

  override def unknownReference() =
  {
    "something or other"
  }

  override def unknownState() =
  {
    "discombobulated"
  }

  override def unknownCopula() =
  {
    LEMMA_BE
  }

  override def unknownPredicateStatement() =
  {
    "foo is bar"
  }

  override def unknownPredicateCommand() =
  {
    "make it so"
  }

  override def unknownPredicateQuestion() =
  {
    "is it what now"
  }

  override def respondToQuery(sentence : String) =
  {
    ShlurdParseUtils.capitalize(sentence)
  }

  override def respondToCounterfactual(sentence : String) =
  {
    compose("But", sentence.stripSuffix("."), "already.")
  }

  override def respondAmbiguous(entity : String) =
  {
    compose("Please be more specific about which", entity, "you mean.")
  }

  override def respondUnknown(entity : String) =
  {
    compose("Sorry, I don't know what you mean by", concat(entity, "."))
  }

  override def respondUnknownPronoun(entity : String) =
  {
    compose("Sorry, when you say",
      concat("'", entity, "'"), "I don't know who or what you mean.")
  }

  override def respondNonexistent(entity : String) =
  {
    compose("But I don't know about any such", concat(entity, "."))
  }

  override def respondCannotUnderstand() =
  {
    "Sorry, I cannot understand what you said."
  }

  override def respondDontKnow() =
  {
    "I don't know."
  }

  override def respondNotUnderstood(
    mood : ShlurdMood, predicate : String, errorPhrase : String) =
  {
    val prefix = mood match {
      case _ : ShlurdIndicativeMood => {
        "I think you are saying"
      }
      case _ : ShlurdInterrogativeMood => {
        "I think you are asking"
      }
      case MOOD_IMPERATIVE => {
        "I think you are telling me to"
      }
    }
    compose(
      prefix,
      concat(predicate, ","),
      "but I can't understand the phrase",
      concat("\"", errorPhrase, "\""))
  }

  override def predicateUnrecognizedSubject(
    mood : ShlurdMood, complement : String, copula : Seq[String],
    count : ShlurdCount, changeVerb : Option[ShlurdWord],
    question : Option[ShlurdQuestion]) =
  {
    val entity = count match {
      case COUNT_SINGULAR => {
        "entity"
      }
      case COUNT_PLURAL => {
        "entities"
      }
    }
    val something = {
      if (question.isEmpty) {
        compose("some", entity)
      } else {
        ""
      }
    }
    mood match {
      case _ : ShlurdIndicativeMood => {
        compose("that",
          composePredicateStatement(something, copula, complement))
      }
      case _ : ShlurdInterrogativeMood => {
        val whord = {
          if (question.isEmpty) {
            "whether"
          } else {
            query(entity, question)
          }
        }
        compose(whord,
          composePredicateStatement(something, copula, complement))
      }
      case MOOD_IMPERATIVE => {
        compose(changeVerb.map(_.lemma).getOrElse(""), complement, something)
      }
    }
  }

  override def predicateUnrecognizedComplement(
    mood : ShlurdMood, subject : String,
    copula : Seq[String],
    question : Option[ShlurdQuestion],
    isRelationship : Boolean) =
  {
    mood match {
      case MOOD_IMPERATIVE => {
        compose("do something with", subject)
      }
      case _ => {
        if (question.isEmpty) {
          compose("something about", subject)
        } else {
          val complement = {
            if (isRelationship) {
              compose("some", "entity")
            } else {
              compose("in", "some", "state")
            }
          }
          composePredicateStatement(
            query(subject, question), copula, complement)
        }
      }
    }
  }

  override def respondCompliance() =
  {
    "OK."
  }

  override def affirmAssumption(sentence : String, strength : Boolean) =
  {
    if (strength) {
      compose("Right,", sentence)
    } else {
      compose("Yes,", sentence)
    }
  }

  override def contradictAssumption(sentence : String, strength : Boolean) =
  {
    if (strength) {
      compose("No, actually", sentence)
    } else {
      compose("No,", sentence)
    }
  }
}
