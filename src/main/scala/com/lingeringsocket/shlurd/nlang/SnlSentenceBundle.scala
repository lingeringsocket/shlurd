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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import com.ibm.icu.text._

import scala.util._

abstract class SnlSentenceBundle(
  tongueIn : SprTongue,
  numberFormatIn : RuleBasedNumberFormat)
    extends SilSentenceBundle
{
  protected implicit val tongue = tongueIn

  protected val numberFormat = numberFormatIn

  override def getTongue = tongue

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

  protected def composePredicateStatement(
    subject : String, verbSeq : Seq[String], complement : Seq[String],
    modifiers : Seq[String] = Seq.empty) =
  {
    compose((Seq(subject) ++ verbSeq ++ complement ++ modifiers):_*)
  }

  // FIXME need to factor out some English-specifics
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

  // FIXME need to factor out some English-specifics
  override def relationshipPredicate(
    subject : String, verbSeq : Seq[String], complement : String,
    verb : SilWord,
    question : Option[SilQuestion],
    tam : SilTam,
    modifiers : Seq[String]) =
  {
    if (tam.isInterrogative && question.isEmpty) {
      if (tongue.isBeingLemma(verb)) {
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

  // FIXME need to factor out some English-specifics
  protected def composePredicateQuestion(
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

  override def adpositionString(adposition : SilAdposition) =
  {
    delemmatizeWord(adposition.word)
  }

  override def actionVerb(
    verb : SilWord) =
  {
    delemmatizeWord(verb)
  }

  override def existsVerb() : SilWord =
  {
    SprPredefWord(PD_EXIST).toUninflected
  }

  override def changeStateVerb(
    state : SilWord, changeVerb : Option[SilWord]) =
  {
    compose(
      (changeVerb.toSeq :+ state).flatMap(_.decomposed).map(_.lemmaUnfolded):_*)
  }

  override def delemmatizeNoun(
    word : SilWord,
    gender : SilGender,
    count : SilCount,
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
          cardinalNumber(lemma.toInt, gender, false)
        } else {
          val base = count match {
            case COUNT_PLURAL => {
              tongue.pluralizeNoun(lemma)
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
      case DETERMINER_NONE => PD_NEITHER_DETERMINER.toLemma
      case DETERMINER_DEFINITE => PD_EITHER.toLemma
      case _ => ""
    }

    val infix = determiner match {
      case DETERMINER_NONE => PD_NOR.toLemma
      case (_ : SilUnlimitedDeterminer) | DETERMINER_DEFINITE => PD_OR.toLemma
      case DETERMINER_ABSENT => separator.punctuationMark
      case _ => PD_AND.toLemma
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
        PD_EQUIVALENTLY
      } else {
        PD_THEN
      }
    }
    compose(
      delemmatizeWord(conjunction),
      concat(antecedent, ","), connective.toLemma, consequent)
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
        compose(PD_WHICH.toLemma, noun)
      }
      case Some(QUESTION_WHO) => {
        answerInflection match {
          case INFLECT_ACCUSATIVE | INFLECT_ADPOSITIONED => {
            compose(PD_WHOM.toLemma)
          }
          case INFLECT_GENITIVE => {
            compose(PD_WHOSE.toLemma, noun)
          }
          case _ => {
            if (noun.isEmpty) {
              compose(PD_WHO.toLemma)
            } else {
              noun
            }
          }
        }
      }
      case Some(QUESTION_WHAT) => {
        compose(PD_WHAT.toLemma)
      }
      case Some(QUESTION_HOW_MANY) => {
        compose(PD_HOW_MANY.toLemma, noun)
      }
      case Some(QUESTION_WHERE) => {
        compose(PD_WHERE.toLemma)
      }
      case None => noun
    }
  }

  override def specifiedNoun(specifier : String, noun : String) =
  {
    compose(noun, specifier)
  }

  override def cardinalNumber(
    num : Int, gender : SilGender, isModifier : Boolean) : String =
  {
    assert(num >= 0)
    numberFormat.format(num, "%spellout-cardinal")
  }

  override def cardinalValue(s : String) =
  {
    Try(numberFormat.parse(s).intValue)
  }

  override def ordinalNumber(num : Int, gender : SilGender) : String =
  {
    assert(num > 0)
    numberFormat.format(num, "%spellout-ordinal")
  }

  override def ordinalValue(s : String) =
  {
    Try(numberFormat.parse(s).intValue)
  }

  override def adpositionedNoun(
    position : String, noun : String, conjoining : SilConjoining) =
  {
    separate(compose(position, noun), conjoining)
  }

  override def pronoun(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity, word : Option[SilWord],
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    if (proximity == PROXIMITY_ELIDED) {
      ""
    } else {
      def standard = tongue.pronounLemma(
        person, gender, count, proximity, inflection)
      val inflected = word.map(w => w.recompose(w.decomposed.map(_.inflected))).
        getOrElse(standard)
      separate(inflected, conjoining)
    }
  }

  // FIXME these need to be translated
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

  def affirmation(strength : Boolean) : String

  def negation(strength : Boolean) : String

  override def affirmAssumption(sentence : String, strength : Boolean) =
  {
    val interjection = affirmation(strength)
    if (sentence.isEmpty) {
      concat(interjection, ".")
    } else {
      compose(concat(interjection, ","), sentence)
    }
  }

  override def contradictAssumption(sentence : String, strength : Boolean) =
  {
    val interjection = negation(strength)
    if (sentence.isEmpty) {
      concat(interjection, ".")
    } else {
      compose(concat(interjection, ","), sentence)
    }
  }
}
