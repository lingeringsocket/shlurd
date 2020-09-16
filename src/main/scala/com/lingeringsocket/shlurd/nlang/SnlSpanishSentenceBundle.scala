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
          tam.infinitive.withModality(MODAL_NEUTRAL),
          verb.toUninflected))
      }
    }
    val mainSeq = mainVerb.decomposed.map(w => {
      if (w.inflected.isEmpty) {
        if ((w.lemma == LEMMA_HABER) && tam.isPresent) {
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
    modalSeq ++ mainSeq
  }

  override def applyInflection(
    base : String, count : SilCount, inflection : SilInflection) : String =
  {
    // FIXME the real thing
    base
  }

  override def delemmatizeState(
    word : SilWord, tam : SilTam, conjoining : SilConjoining) : String =
  {
    // FIXME the real thing
    val decomposed = word.decomposed.map(_.lemma)
    separate(word.recompose(decomposed), conjoining)
  }

  override def genitivePhrase(genitive : String, head : String) =
  {
    compose(head, LEMMA_DE, genitive)
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
        cardinalNumber(number)
      }
    }
    val determinerInflected = tongue.correctGenderCount(
      determinerLemma, gender, count, true)
    compose(determinerInflected, noun)
  }
}
