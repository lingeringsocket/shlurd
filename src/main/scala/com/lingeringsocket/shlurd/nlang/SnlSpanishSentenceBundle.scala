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
        tupleN((verbSeq.take(1), verbSeq.drop(1)))
      } else {
        tupleN((Seq.empty, verbSeq))
      }
    }
    val middle = objectPosition match {
      case OBJ_AFTER_VERB => verbMain ++ complement
      case OBJ_BEFORE_VERB => complement ++ verbMain
    }
    compose((Seq(subject) ++ verbPre ++ dative ++ middle ++ modifiers).toSeq:_*)
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
        tupleN((tam, verb))
      } else {
        tupleN((
          tam.infinitive.withModality(MODAL_NEUTRAL).positive,
          verb.toUninflected))
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
    val determinerInflected = tongue.correctGenderCount(
      determinerLemma, gender, count, true)
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
}
