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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

import org.atteo.evo.inflector.{English => EnglishPluralizer}

import com.ibm.icu.text._

import scala.util._
import java.util._

import SprEnglishLemmas._
import ShlurdEnglishAffixes._

object SilEnglishSentenceBundle
{
  private val enFormatter = new RuleBasedNumberFormat(
    Locale.ENGLISH, RuleBasedNumberFormat.SPELLOUT);
}

class SilEnglishSentenceBundle
    extends SilSentenceBundle
{
  import SilEnglishSentenceBundle._

  override def statePredicateStatement(
    subject : String, verbSeq : Seq[String], state : String,
    existentialPronoun : Option[SilWord],
    modifiers : Seq[String]) =
  {
    if (state.isEmpty) {
      if (existentialPronoun.nonEmpty) {
        // "there is ..."
        compose((verbSeq ++ Seq(subject) ++ modifiers):_*)
      } else {
        // "... exists"
        composePredicateStatement(subject, verbSeq, Seq.empty, modifiers)
      }
    } else {
      composePredicateStatement(subject, verbSeq, Seq(state), modifiers)
    }
  }

  override def actionPredicate(
    subject : String,
    verbSeq : Seq[String],
    directObject : Option[String],
    modifiersOriginal : Seq[String],
    tam : SilTam,
    answerInflection : SilInflection) =
  {
    val (adpositionPre, modifiers) = answerInflection match {
      case INFLECT_ADPOSITIONED =>
        tupleN((Some(modifiersOriginal.last), modifiersOriginal.dropRight(1)))
      case _ =>
        tupleN((None, modifiersOriginal))
    }
    val directObjectPost = answerInflection match {
      case INFLECT_ACCUSATIVE => None
      case _ => directObject
    }
    val complement = compose(directObjectPost.toSeq:_*)
    val primary = {
      val verbMaybeComma = {
        if (!tam.isImperative && complement.startsWith(DQUOTE)) {
          verbSeq.dropRight(1) :+ concat(verbSeq.last, ",")
        } else {
          verbSeq
        }
      }
      if (!tam.isInterrogative ||
        (answerInflection == INFLECT_NOMINATIVE))
      {
        composePredicateStatement(
          subject, verbMaybeComma, Seq(complement), modifiers)
      } else {
        composePredicateQuestion(
          subject, verbMaybeComma, Seq(complement), modifiers)
      }
    }
    answerInflection match {
      case INFLECT_ACCUSATIVE => {
        compose((directObject.toSeq ++ Seq(primary)):_*)
      }
      case _ => compose((adpositionPre.toSeq ++ Seq(primary)):_*)
    }
  }

  private def composePredicateStatement(
    subject : String, verbSeq : Seq[String], complement : Seq[String],
    modifiers : Seq[String] = Seq.empty) =
  {
    compose((Seq(subject) ++ verbSeq ++ complement ++ modifiers):_*)
  }

  override def statePredicateQuestion(
    subject : String, verbSeq : Seq[String], stateOriginal : String,
    existentialPronoun : Option[SilWord],
    question : Option[SilQuestion],
    modifiersOriginal : Seq[String],
    answerInflection : SilInflection) =
  {
    val (adpositionPre, modifiers, state) = answerInflection match {
      case INFLECT_ADPOSITIONED =>
        // FIXME this is a mess from "south of what is the cave?"
        if (modifiersOriginal.isEmpty) {
          tupleN((
            Some(stateOriginal),
            modifiersOriginal,
            ""
          ))
        } else {
          tupleN((
            Some(modifiersOriginal.last),
            modifiersOriginal.dropRight(1),
            stateOriginal))
        }
      case _ =>
        tupleN((None, modifiersOriginal, stateOriginal))
    }
    val primary = {
      if (!question.isEmpty) {
        if (existentialPronoun.nonEmpty) {
          compose((Seq(subject) ++ verbSeq.take(2).reverse ++
            verbSeq.drop(2) ++ Seq(state) ++ modifiers):_*)
        } else {
          if (answerInflection == INFLECT_NOMINATIVE) {
            composePredicateStatement(subject, verbSeq, Seq(state), modifiers)
          } else {
            composePredicateQuestion(subject, verbSeq, Seq(state), modifiers)
          }
        }
      } else if (state.isEmpty) {
        if (existentialPronoun.nonEmpty) {
          compose((verbSeq.take(2).reverse ++ verbSeq.drop(2) ++
            Seq(subject) ++ modifiers):_*)
        } else {
          actionPredicate(
            subject, verbSeq, None, modifiers,
            SilTam.interrogative,
            answerInflection)
        }
      } else {
        composePredicateQuestion(subject, verbSeq, Seq(state), modifiers)
      }
    }
    compose((adpositionPre.toSeq ++ Seq(primary)):_*)
  }

  override def relationshipPredicate(
    subject : String, verbSeq : Seq[String], complement : String,
    verb : SilWord,
    question : Option[SilQuestion],
    tam : SilTam,
    modifiers : Seq[String]) =
  {
    if (tam.isInterrogative && question.isEmpty) {
      if (isBeingLemma(verb)) {
        composePredicateQuestion(
          subject, verbSeq, Seq(complement), modifiers)
      } else {
        if (tam.isIndicative && !tam.requiresAux) {
          composePredicateStatement(
            subject, verbSeq, Seq(complement), modifiers)
        } else {
          composePredicateQuestion(
            subject, verbSeq, Seq(complement), modifiers)
        }
      }
    } else {
      composePredicateStatement(
        subject, verbSeq, Seq(complement), modifiers)
    }
  }

  private def composePredicateQuestion(
    subject : String, verbSeq : Seq[String], complement : Seq[String],
    modifiers : Seq[String]) =
  {
    if (complement.isEmpty) {
      compose((Seq(subject) ++ verbSeq ++ modifiers):_*)
    } else {
      val (headSeq, tailSeq) = verbSeq.splitAt(1)
      verbSeq.size match {
        // "is Larry clumsy?"
        case 1 =>
          compose((headSeq ++ Seq(subject) ++ complement ++ modifiers):_*)
        // "is Larry not clumsy?" or "must Larry be clumsy?"
        case 2 =>
          compose((headSeq ++ Seq(subject) ++ tailSeq ++
            complement ++ modifiers):_*)
        // "must Larry not be clumsy?"
        case _ =>
          compose((headSeq ++ Seq(subject) ++ tailSeq ++
            complement ++ modifiers):_*)
      }
    }
  }

  override def statePredicateCommand(subject : String, state : String,
    modifiers : Seq[String]) =
  {
    compose((Seq(state) ++ Seq(subject) ++ modifiers):_*)
  }

  private def delemmatizeModalVerb(
    tam : SilTam, verb : SilWord,
    person : SilPerson, gender : SilGender, count : SilCount) =
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
          case TENSE_FUTURE => LEMMA_BE
        }
      }
      case LEMMA_HAVE => {
        tam.tense match {
          case TENSE_PAST => "had"
          case TENSE_FUTURE => LEMMA_HAVE
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
            case TENSE_FUTURE => verbLemma
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

  override def adpositionString(adposition : SilAdposition) =
  {
    delemmatizeWord(adposition.word)
  }

  override def actionVerb(
    verb : SilWord) =
  {
    delemmatizeWord(verb)
  }

  override def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) =
  {
    compose(
      (changeVerb.toSeq :+ state).flatMap(_.decomposed).map(_.lemmaUnfolded):_*)
  }

  override def delemmatizeNoun(
    word : SilWord, count : SilCount,
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    val decomposed = word.decomposed
    val noun = decomposed.last
    val unseparated = {
      if (noun.inflected.isEmpty || (inflection == INFLECT_GENITIVE)) {
        val lemma = inflection match {
          case INFLECT_GENITIVE => noun.inflected
          case _ => noun.lemmaUnfolded
        }
        if (lemma.forall(Character.isDigit)) {
          cardinalNumber(lemma.toInt)
        } else {
          val base = count match {
            case COUNT_PLURAL => {
              EnglishPluralizer.plural(lemma)
            }
            case _ => {
              lemma
            }
          }
          applyInflection(base, count, inflection)
        }
      } else {
        noun.inflected
      }
    }
    val seq = decomposed.dropRight(1).map(_.inflected) :+ unseparated
    separate(word.recompose(seq), conjoining)
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

  private def delemmatizeWord(word : SilWord) =
  {
    word.recompose(word.decomposed.map(sw => {
      if (sw.inflected.isEmpty) {
        sw.lemmaUnfolded
      } else {
        sw.inflected
      }
    }))
  }

  override def delemmatizeQualifier(qualifier : SilWord) =
  {
    delemmatizeWord(qualifier)
  }

  override def conjoin(
    determiner : SilDeterminer,
    separator : SilSeparator,
    inflection : SilInflection,
    items : Seq[String]) =
  {
    val prefix = determiner match {
      case DETERMINER_NONE => LEMMA_NEITHER
      case DETERMINER_DEFINITE => LEMMA_EITHER
      case _ => ""
    }

    val infix = determiner match {
      case DETERMINER_NONE => LEMMA_NOR
      case (_ : SilUnlimitedDeterminer) | DETERMINER_DEFINITE => LEMMA_OR
      case DETERMINER_ABSENT => separator.punctuationMark
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
              if (determiner == DETERMINER_ABSENT) {
                Seq(concat(n, infix))
              } else {
                Seq(n, infix)
              }
            }
          }
        }
      }
    }
    compose((Seq(prefix) ++ seq ++ items.lastOption.toSeq):_*)
  }

  override def conditional(
    conjunction : SilWord,
    antecedent : String,
    consequent : String,
    biconditional : Boolean) =
  {
    val connective = {
      if (biconditional) {
        LEMMA_EQUIVALENTLY
      } else {
        LEMMA_THEN
      }
    }
    compose(
      delemmatizeWord(conjunction),
      concat(antecedent, ","), connective, consequent)
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
        compose(LEMMA_WHICH, noun)
      }
      case Some(QUESTION_WHO) => {
        answerInflection match {
          case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => {
            compose(LEMMA_WHOM)
          }
          case INFLECT_GENITIVE => {
            compose(LEMMA_WHOSE, noun)
          }
          case _ => {
            compose(LEMMA_WHO)
          }
        }
      }
      case Some(QUESTION_WHAT) => {
        compose(LEMMA_WHAT)
      }
      case Some(QUESTION_HOW_MANY) => {
        compose(LEMMA_HOW, LEMMA_MANY, noun)
      }
      case Some(QUESTION_WHERE) => {
        compose(LEMMA_WHERE)
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

  override def determinedNoun(determiner : SilDeterminer, noun : String) =
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
        cardinalNumber(number)
      }
    }
    compose(determinerString, noun)
  }

  override def cardinalNumber(num : Int) : String =
  {
    assert(num >= 0)
    enFormatter.format(num, "%spellout-cardinal")
  }

  override def cardinalValue(s : String) =
  {
    Try(enFormatter.parse(s).intValue)
  }

  override def ordinalNumber(num : Int) : String =
  {
    assert(num > 0)
    enFormatter.format(num, "%spellout-ordinal")
  }

  override def ordinalValue(s : String) =
  {
    Try(enFormatter.parse(s).intValue)
  }

  override def adpositionedNoun(
    position : String, noun : String, conjoining : SilConjoining) =
  {
    separate(compose(position, noun), conjoining)
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    compose(genitive, head)
  }

  override def pronoun(
    person : SilPerson, gender : SilGender, count : SilCount,
    distance : SilDistance, word : Option[SilWord], inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    def standard = {
      person match {
        case PERSON_FIRST => count match {
          case COUNT_PLURAL => inflection match {
            case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_US
            case INFLECT_GENITIVE => LEMMA_OUR
            case _ => distance match {
              case DISTANCE_REFLEXIVE => LEMMA_OURSELVES
              case _ => LEMMA_WE
            }
          }
          case _ => distance match {
            case DISTANCE_REFLEXIVE => LEMMA_MYSELF
            case _ => inflection match {
              case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_ME
              case INFLECT_GENITIVE => LEMMA_MY
              case _ => "I"
            }
          }
        }
        case PERSON_SECOND => inflection match {
          case INFLECT_GENITIVE => LEMMA_YOUR
          case _ => {
            distance match {
              case DISTANCE_REFLEXIVE => count match {
                case COUNT_PLURAL => LEMMA_YOURSELVES
                case _ => LEMMA_YOURSELF
              }
              case _ => LEMMA_YOU
            }
          }
        }
        case PERSON_THIRD => count match {
          case COUNT_PLURAL => distance match {
            case DISTANCE_HERE => LEMMA_THESE
            case DISTANCE_THERE => LEMMA_THOSE
            case DISTANCE_UNSPECIFIED => inflection match {
              case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_THEM
              case INFLECT_GENITIVE => LEMMA_THEIR
              case _ => LEMMA_THEY
            }
            case DISTANCE_REFLEXIVE => LEMMA_THEMSELVES
          }
          case _ => gender.maybeBasic match {
            case Some(GENDER_MASCULINE | GENDER_SOMEONE) => distance match {
              case DISTANCE_REFLEXIVE => LEMMA_HIMSELF
              case _ => inflection match {
                case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => LEMMA_HIM
                case INFLECT_GENITIVE => LEMMA_HIS
                case _ => LEMMA_HE
              }
            }
            case Some(GENDER_FEMININE) => distance match {
              case DISTANCE_REFLEXIVE => LEMMA_HERSELF
              case _ => inflection match {
                case INFLECT_ACCUSATIVE | INFLECT_GENITIVE |
                    INFLECT_ADPOSITIONED => LEMMA_HER
                case _ => LEMMA_SHE
              }
            }
            case Some(GENDER_NEUTER) => distance match {
              case DISTANCE_HERE => LEMMA_THIS
              case DISTANCE_THERE => LEMMA_THAT
              case DISTANCE_UNSPECIFIED => inflection match {
                case INFLECT_GENITIVE => LEMMA_ITS
                case _ => LEMMA_IT
              }
              case DISTANCE_REFLEXIVE => LEMMA_ITSELF
            }
            case Some(GENDER_SOMEWHERE) => distance match {
              case DISTANCE_HERE => LEMMA_HERE
              case _ => LEMMA_THERE
            }
            case _ => {
              throw new IllegalArgumentException("custom pronoun word required")
            }
          }
        }
      }
    }
    val inflected = word.map(w => w.recompose(w.decomposed.map(_.inflected))).
      getOrElse(standard)
    separate(inflected, conjoining)
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

  override def unknownVerbModifier() =
  {
    "mimsily"
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
    SprUtils.capitalize(sentence)
  }

  override def respondToCounterfactual(sentence : String) =
  {
    compose("But", sentence.stripSuffix("."), "already.")
  }

  override def respondAmbiguous(noun : SilWord) =
  {
    compose(
      "Please be more specific about which",
      noun.toUnfoldedLemma,
      "you mean.")
  }

  override def respondUnknown(word : SilWord) =
  {
    compose("Sorry, I don't know about any",
      concat("'", word.toUnfoldedLemma, "'."))
  }

  override def respondUnknownModifier(word : SilWord) =
  {
    compose("Sorry, I don't know what",
      concat("'", word.toUnfoldedLemma, "'"),
      "means in this context.")
  }

  override def respondUnknownState(subject : String, state : SilWord) =
  {
    compose("Sorry, I don't know what",
      concat("'", state.toUnfoldedLemma, "'"),
      "means for", concat(subject, "."))
  }

  override def respondUnresolvedPronoun(pronoun : String) =
  {
    compose("Sorry, when you say",
      concat("'", pronoun, "'"), "I don't know who or what you mean.")
  }

  override def respondAmbiguousPronoun(pronoun : String) =
  {
    compose("Sorry, when you say",
      concat("'", pronoun, "',"), "it's ambiguous.")
  }

  override def respondMisqualifiedNoun(
    noun : SilWord, qualifiers : Seq[String]) : String =
  {
    compose("Sorry, when you say",
      concat("'",
        compose((qualifiers :+ noun.toUnfoldedLemma):_*),
        "',"),
      "I don't know which you mean.")
  }

  override def respondNonexistent(noun : SilWord) =
  {
    compose("But I don't know about any such",
      concat(noun.toUnfoldedLemma, "."))
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
    tam : SilTam, predicate : String, errorPhrase : String) =
  {
    val prefix = tam.mood match {
      case MOOD_INDICATIVE => {
        "I think you are saying"
      }
      case MOOD_INTERROGATIVE => {
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
      concat(DQUOTE, errorPhrase, DQUOTE))
  }

  override def respondUnable(action : String) =
  {
    compose("One does not simply", concat(action, "."))
  }

  override def respondIrrelevant() =
  {
    "I'm not sure how to interpret that."
  }

  override def respondTriggerLimit() =
  {
    "Trigger limit exceeded."
  }

  override def predicateUnrecognizedSubject(
    tam : SilTam, complement : String, verbSeq : Seq[String],
    count : SilCount, changeVerb : Option[SilWord],
    question : Option[SilQuestion]) =
  {
    val entity = count match {
      case COUNT_PLURAL => {
        "entities"
      }
      case _ => {
        "entity"
      }
    }
    val something = {
      if (question.isEmpty) {
        compose("some", entity)
      } else {
        ""
      }
    }
    tam.mood match {
      case MOOD_INDICATIVE => {
        compose("that",
          composePredicateStatement(something, verbSeq, Seq(complement)))
      }
      case MOOD_INTERROGATIVE => {
        val whord = {
          if (question.isEmpty) {
            "whether"
          } else {
            query(entity, question)
          }
        }
        compose(whord,
          composePredicateStatement(something, verbSeq, Seq(complement)))
      }
      case MOOD_IMPERATIVE => {
        compose(
          changeVerb.map(_.toUnfoldedLemma).getOrElse(""),
          complement,
          something)
      }
    }
  }

  override def predicateUnrecognizedComplement(
    tam : SilTam, subject : String,
    verbSeq : Seq[String],
    question : Option[SilQuestion],
    isRelationship : Boolean) =
  {
    tam.mood match {
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
            query(subject, question), verbSeq, Seq(complement))
        }
      }
    }
  }

  override def respondCompliance() =
  {
    "OK."
  }

  override def respondNoncommittal() =
  {
    "Oh, really?"
  }

  override def affirmAssumption(sentence : String, strength : Boolean) =
  {
    val affirmation = {
      if (strength) {
        "Right"
      } else {
        "Yes"
      }
    }
    if (sentence.isEmpty) {
      concat(affirmation, ".")
    } else {
      compose(concat(affirmation, ","), sentence)
    }
  }

  override def contradictAssumption(sentence : String, strength : Boolean) =
  {
    if (sentence.isEmpty) {
      "No."
    } else {
      if (strength) {
        compose("No, actually", sentence)
      } else {
        compose("No,", sentence)
      }
    }
  }
}
