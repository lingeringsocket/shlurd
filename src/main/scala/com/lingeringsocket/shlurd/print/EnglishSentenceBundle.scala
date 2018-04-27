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
    subject : String, copula : Seq[String], state : String) =
  {
    if (state.isEmpty) {
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
    mood : ShlurdMood, relationship : ShlurdRelationship,
    person : ShlurdPerson, count : ShlurdCount) =
  {
    val verb = relationship match {
      case REL_IDENTITY => "be"
      case REL_ASSOCIATION => "have"
    }
    val modality = {
      relationship match {
        case REL_ASSOCIATION => mood.getModality match {
          case MODAL_NEUTRAL => MODAL_EMPHATIC
          case x => x
        }
        case REL_IDENTITY => mood.getModality
      }
    }
    val aux = modality match {
      case MODAL_NEUTRAL => ""
      case MODAL_MUST => "must"
      case MODAL_MAY => "may"
      case MODAL_POSSIBLE => "might"
      case MODAL_CAPABLE => "can"
      case MODAL_PERMITTED => "may"
      case MODAL_SHOULD => "should"
      case MODAL_EMPHATIC => {
        count match {
          case COUNT_SINGULAR => {
            person match {
              case PERSON_THIRD => "does"
              case _ => "do"
            }
          }
          case COUNT_PLURAL => "do"
        }
      }
    }
    if (mood.isNegative) {
      Seq(aux, "not", verb)
    } else {
      Seq(aux, verb)
    }
  }

  override def copula(
    person : ShlurdPerson, gender : ShlurdGender, count : ShlurdCount,
    mood : ShlurdMood, isExistential : Boolean,
    relationship : ShlurdRelationship) : Seq[String] =
  {
    if ((relationship == REL_ASSOCIATION) && mood.isNegative) {
      return modalCopula(mood, relationship, person, count)
    }
    val seq = mood.getModality match {
      case MODAL_NEUTRAL => {
        val inflected = {
          count match {
            case COUNT_SINGULAR => {
              relationship match {
                case REL_IDENTITY => {
                  person match {
                    case PERSON_FIRST => "am"
                    case PERSON_SECOND => "are"
                    case PERSON_THIRD => "is"
                  }
                }
                case REL_ASSOCIATION => {
                  person match {
                    case PERSON_THIRD => "has"
                    case _ => "have"
                  }
                }
              }
            }
            case COUNT_PLURAL => {
              relationship match {
                case REL_IDENTITY => "are"
                case REL_ASSOCIATION => "have"
              }
            }
          }
        }
        if (mood.isNegative) {
          Seq(inflected, "not")
        } else {
          Seq(inflected)
        }
      }
      case _ => {
        modalCopula(mood, relationship, person, count)
      }
    }
    if (isExistential) {
      Seq("there") ++ seq
    } else {
      seq
    }
  }

  override def position(locative : ShlurdLocative) =
  {
    locative match {
      case LOC_INSIDE => "in"
      case LOC_OUTSIDE => "outside of"
      case LOC_AT => "at"
      case LOC_NEAR => "near"
      case LOC_ON => "on"
      case LOC_ABOVE => "above"
      case LOC_BELOW => "below"
      case LOC_LEFT => "to the left of"
      case LOC_RIGHT => "to the right of"
      case LOC_FRONT => "in front of"
      case LOC_BEHIND => "behind"
      case LOC_GENITIVE_OF => "of"
    }
  }

  override def changeStateVerb(state : ShlurdWord) =
    state.lemma

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
      case DETERMINER_NONE => "neither"
      case DETERMINER_UNIQUE => "either"
      case _ => ""
    }

    val infix = determiner match {
      case DETERMINER_NONE => "nor"
      case DETERMINER_ANY | DETERMINER_UNIQUE => "or"
      case _ => "and"
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
        compose("which", noun)
      }
      case Some(QUESTION_WHO) => {
        // FIXME inflection for whom, whose
        compose("who")
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
    compose(noun, specifier)
  }

  override def determinedNoun(determiner : ShlurdDeterminer, noun : String) =
  {
    val determinerString = determiner match {
      case DETERMINER_UNSPECIFIED => ""
      case DETERMINER_NONE => "no"
      case DETERMINER_UNIQUE => "the"
      case DETERMINER_NONSPECIFIC => {
        // FIXME:  in reality it can be a little more complicated...
        if ("aeiou".contains(noun.head)) {
          "an"
        } else {
          "a"
        }
      }
      case DETERMINER_ANY => "any"
      case DETERMINER_SOME => "some"
      case DETERMINER_ALL => "all"
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
    "be"
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

  override def respondUnique(entity : String) =
  {
    compose("But I only know about one", concat(entity, "."))
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
