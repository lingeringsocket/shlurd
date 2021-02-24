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

import com.lingeringsocket.morphala.spanish._

import com.ibm.icu.text._

import java.util._

import scala.collection._

import SnlSpanishLemmas._

object SnlSpanishSentenceBundle
{
  private val numberFormat = new RuleBasedNumberFormat(
    new Locale("es"), RuleBasedNumberFormat.SPELLOUT);
}

class SnlSpanishSentenceBundle(
  tongue : SprTongue
) extends SnlSentenceBundle(tongue, SnlSpanishSentenceBundle.numberFormat)
{
  override protected def composePredicateStatement(
    subject : String, verbSeq : Seq[String], complement : Seq[String],
    modifiers : Seq[String] = Seq.empty,
    objectPosition : SilObjectPosition = OBJ_AFTER_VERB,
    dative : Seq[String] = Seq.empty) =
  {
    // For Spanish, we only use dative to represent indirect object
    // pronouns, and those always go before the verb (and before
    // the direct object if any)
    val (verbPre, verbMain) = {
      // FIXME generalize to any negative
      if (verbSeq.head == LEMMA_NO) {
        tupleN(verbSeq.take(1), verbSeq.drop(1))
      } else {
        tupleN(Seq.empty, verbSeq)
      }
    }
    val middle = objectPosition match {
      case OBJ_AFTER_VERB => verbMain ++ complement
      case OBJ_BEFORE_VERB => complement ++ verbMain
    }
    val dativeSe = {
      if ((objectPosition == OBJ_BEFORE_VERB) &&
        complement.head.startsWith("l") &&
        (dative.size == 1) && dative.head.startsWith("l"))
      {
        // horrors, we can't have alllliteration!
        Seq(LEMMA_SE)
      } else {
        dative
      }
    }
    compose((Seq(subject) ++ verbPre ++ dativeSe ++
      middle ++ modifiers).toSeq:_*)
  }

  override protected def composePredicateQuestion(
    subject : String, verbSeq : Seq[String], complement : Seq[String],
    modifiers : Seq[String],
    objectPosition : SilObjectPosition = OBJ_AFTER_VERB,
    dative : Seq[String] = Seq.empty) =
  {
    composePredicateStatement(
      subject, verbSeq, complement, modifiers,
      objectPosition, dative)
  }

  override def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord,
    answerInflection : SilInflection
  )
      : Seq[String] =
  {
    // FIXME:  SnlSpanishConjugation currently returns a single string;
    // when auxiliary verbs are included, we should break them
    // out
    val modalSeq = {
      if (tam.modality != MODAL_NEUTRAL) {
        val auxLemma = tongue.auxVerbForModal(tam.modality)
        val auxSeq = delemmatizeVerb(
          person, gender, count, tam.withModality(MODAL_NEUTRAL),
          existentialPronoun, SilWord.uninflected(auxLemma),
          answerInflection)
        val adpositionLemma = tongue.adpositionForAux(auxLemma)
        if (adpositionLemma.isEmpty) {
          auxSeq
        } else {
          auxSeq :+ adpositionLemma
        }
      } else {
        Seq.empty
      }
    }
    val (mainTam, mainVerb) = {
      if (modalSeq.isEmpty) {
        tupleN(tam, verb)
      } else {
        tupleN(
          tam.infinitive.withModality(MODAL_NEUTRAL).positive,
          verb.toUninflected)
      }
    }
    val mainSeq = mainVerb.decomposed.map(w => {
      if (w.inflected.isEmpty) {
        if ((w.lemma == LEMMA_HABER) && mainTam.isPresent) {
          // FIXME don't do this when used as an auxiliary.  Also,
          // technically all tenses are supposed to be conjugated
          // as singular, but supposedly native speakers tend
          // quite naturally to make the "there is/are" distinction?
          "hay"
        } else {
          SnlSpanishConjugation.conjugateVerb(
            w.lemma,
            SnlSpanishConjugationCoord(mainTam, person, count))
        }
      } else {
        w.inflected
      }
    })
    // FIXME handle MODAL_ELLIPTICAL
    val prefix = {
      if (mainTam.isNegative) {
        Seq(LEMMA_NO)
      } else {
        Seq.empty
      }
    }
    prefix ++ modalSeq ++ mainSeq
  }

  override def applyInflection(
    base : String, count : SilCount, inflection : SilInflection) : String =
  {
    // FIXME the real thing
    base
  }

  override def adpositionedNoun(
    position : String, noun : String, conjoining : SilConjoining) =
  {
    val isContractable = position match {
      case "a" | "A" | "de" | "De" => true
      case _ => false
    }
    if (isContractable && noun.startsWith("el ")) {
      separate(compose(position + "l", noun.stripPrefix("el ")), conjoining)
    } else {
      super.adpositionedNoun(position, noun, conjoining)
    }
  }

  override def delemmatizeState(
    word : SilWord, tam : SilTam,
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    conjoining : SilConjoining) : String =
  {
    val decomposed = word.decomposed
    val state = decomposed.last
    val unseparated = {
      if (state.inflected.isEmpty) {
        val uncorrected = {
          val lemma = state.lemmaUnfolded
          if (lemma.endsWith("r")) {
            SnlSpanishConjugation.conjugateParticiple(lemma)
          } else {
            lemma
          }
        }
        tongue.correctGenderCount(
          uncorrected, gender, count, true)
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
    if (isPronoun) {
      compose(genitive, head)
    } else {
      compose(head, LEMMA_DE, genitive)
    }
  }

  override def determinedNoun(
    determiner : SilDeterminer,
    noun : String,
    person : SilPerson,
    gender : SilGender,
    count : SilCount) =
  {
    val determinerLemma = determiner match {
      case DETERMINER_ABSENT | DETERMINER_VARIABLE => ""
      case DETERMINER_NONE => LEMMA_NINGUN
      case DETERMINER_DEFINITE => LEMMA_EL
      case DETERMINER_NONSPECIFIC => LEMMA_UN
      case DETERMINER_ANY => LEMMA_ALGUN
      case DETERMINER_SOME => LEMMA_ALGUN
      case DETERMINER_ALL => LEMMA_TODO
      case SilIntegerDeterminer(number : Int) => {
        cardinalNumber(number, gender, true)
      }
    }
    def startsWithAccentedA = {
      if (noun.startsWith("ha") || noun.startsWith("a") ||
        noun.startsWith("há") || noun.startsWith("á"))
      {
        val (vowelPos, accentedVowelPos, naturalPos) =
          SpanishUtils.analyzeStress(noun)
        val firstVowelPos = vowelPos.head
        (accentedVowelPos == firstVowelPos) || (naturalPos == firstVowelPos)
      } else {
        false
      }
    }
    val artificialGender = determinerLemma match {
      case LEMMA_EL | LEMMA_UN if (startsWithAccentedA) => GENDER_MASCULINE
      case _ => gender
    }
    val determinerInflected = tongue.correctGenderCount(
      determinerLemma, artificialGender, count, true)
    compose(determinerInflected, noun)
  }

  override def directObject(
    noun : String,
    isPerson : Boolean) : String =
  {
    if (isPerson) {
      // personal "a"
      compose(LEMMA_A, noun)
    } else {
      noun
    }
  }

  override def cardinalNumber(
    num : Int, gender : SilGender, isModifier : Boolean) : String =
  {
    assert(num >= 0)
    if ((num == 1) && (gender == GENDER_MASCULINE) && !isModifier) {
      "uno"
    } else {
      val spellout = gender match {
        case GENDER_FEMININE => "%spellout-cardinal-feminine"
        case _ => "%spellout-cardinal-masculine"
      }
      numberFormat.format(num, spellout)
    }
  }

  override def ordinalNumber(num : Int, gender : SilGender) : String =
  {
    // FIXME:  need to take number
    assert(num > 0)
    val spellout = gender match {
      case GENDER_FEMININE => "%spellout-ordinal-feminine"
      case _ => "%spellout-ordinal-masculine"
    }
    numberFormat.format(num, spellout)
  }

  override def affirmation(strength : Boolean) : String =
  {
    if (strength) {
      "Claro"
    } else {
      "Sí"
    }
  }

  override def negation(strength : Boolean) : String =
  {
    if (strength) {
      "Claro que no"
    } else {
      "No"
    }
  }

  override def getObjectPosition(
    refOpt : Option[SilReference]) : SilObjectPosition =
  {
    refOpt match {
      case Some(pr : SilPronounReference) => {
        OBJ_BEFORE_VERB
      }
      case _ => super.getObjectPosition(refOpt)
    }
  }

  override protected def introducerMark(
    tam : SilTam, formality : SilFormality) =
  {
    formality.force match {
      case FORCE_NEUTRAL => tam.mood match {
        case MOOD_INTERROGATIVE => "¿"
        case _ => ""
      }
      case FORCE_EXCLAMATION => "¡"
    }
  }

  override def respondDontKnow =
  {
    "No sé."
  }

  override def respondCannotUnderstand =
  {
    "¿Como?"
  }

  override def respondToCounterfactual(sentence : String) =
  {
    compose("Pero ya", sentence)
  }

  override def respondAmbiguous(noun : SilWord) =
  {
    compose(
      "Sea más específico sobre a qué",
      noun.toUnfoldedLemma,
      "se refiere.")
  }

  override def respondUnknown(word : SilWord) =
  {
    compose("Lo siento, no sé nada de",
      concat("'", word.toUnfoldedLemma, "'."))
  }

  override def respondUnknownModifier(word : SilWord) =
  {
    compose("Lo siento, no sé qué significa",
      concat("'", word.toUnfoldedLemma, "'"),
      "en este contexto.")
  }

  override def respondUnknownState(subject : String, state : SilWord) =
  {
    compose("Lo siento, no sé qué significa",
      concat("'", state.toUnfoldedLemma, "'"),
      "para", concat(subject, "."))
  }

  override def respondUnresolvedPronoun(pronoun : String) =
  {
    compose("Lo siento, cuando se dice",
      concat("'", pronoun, "'"), "no sé a quién ni a qué se refiere.")
  }

  override def respondAmbiguousPronoun(pronoun : String) =
  {
    compose("Lo siento, cuando se dice",
      concat("'", pronoun, "'"), "es ambiguo.")
  }

  override def respondMisqualifiedNoun(
    noun : SilWord, qualifiers : Seq[String]) : String =
  {
    compose("Lo siento, cuando se dice",
      concat("'",
        compose((qualifiers :+ noun.toUnfoldedLemma).toSeq:_*),
        "',"),
      "no sé a qué se refiere.")
  }

  override def respondNonexistent(noun : SilWord) =
  {
    // FIXME gender
    compose("Pero no conozco ningún", noun.toUnfoldedLemma, "así.")
  }

  override def respondNotUnderstood(
    tam : SilTam, predicate : String, errorPhrase : String) =
  {
    val prefix = tam.mood match {
      case MOOD_INDICATIVE => {
        "Creo que se dice"
      }
      case MOOD_SUBJUNCTIVE => {
        "Creo que se supone"
      }
      case MOOD_INTERROGATIVE => {
        "Creo que se pregunta"
      }
      case MOOD_IMPERATIVE => {
        "Creo que se ordena"
      }
    }
    compose(
      prefix,
      concat(predicate, ","),
      "pero no puedo entender la frase",
      concat(DQUOTE, errorPhrase, DQUOTE))
  }

  override def respondUnable(action : String) =
  {
    compose("Uno no simplemente", concat(action, "."))
  }

  override def respondIrrelevant =
  {
    "No estoy seguro de cómo interpretar eso."
  }

  override def respondNoncommittal =
  {
    "¿Ah, de verdad?"
  }
}
