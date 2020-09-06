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

import com.lingeringsocket.shlurd.parser._

import com.ibm.icu.text._

import java.util._

import SprSpanishLemmas._

object SilSpanishSentenceBundle
{
  private val numberFormat = new RuleBasedNumberFormat(
    new Locale("es"), RuleBasedNumberFormat.SPELLOUT);
}

class SilSpanishSentenceBundle(
  tongue : SprTongue
) extends SilSvoSentenceBundle(tongue, SilSpanishSentenceBundle.numberFormat)
{
  override protected def delemmatizeModalVerb(
    tam : SilTam, verb : SilWord,
    person : SilPerson, gender : SilGender, count : SilCount)
      : Seq[String] =
  {
    verb.decomposed.map(w => {
      if (w.inflected.isEmpty) {
        SilSpanishConjugation.conjugateVerb(
          w.lemma,
          SilSpanishConjugationCoord(tam, person, count))
      } else {
        w.inflected
      }
    })
  }

  override protected def delemmatizeModelessVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    word : SilWord, tam : SilTam
  ) : String =
  {
    // FIXME conjugate
    word.recompose(delemmatizeModalVerb(tam, word, person, gender, count))
  }

  override def delemmatizeVerb(
    person : SilPerson, gender : SilGender, count : SilCount,
    tam : SilTam, existentialPronoun : Option[SilWord],
    verb : SilWord,
    answerInflection : SilInflection
  )
      : Seq[String] =
  {
    // FIXME conjugate
    delemmatizeModalVerb(tam, verb, person, gender, count)
  }

  override protected def pluralizeNoun(lemma : String) : String =
  {
    // FIXME the real thing
    lemma + "s"
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

  override def pronoun(
    person : SilPerson, gender : SilGender, count : SilCount,
    proximity : SilProximity, word : Option[SilWord],
    inflection : SilInflection,
    conjoining : SilConjoining) =
  {
    if (proximity == PROXIMITY_ELIDED) {
      ""
    } else {
      // FIXME
      val standard = "yo"
      val inflected = word.map(w => w.recompose(w.decomposed.map(_.inflected))).
        getOrElse(standard)
      separate(inflected, conjoining)
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
        cardinalNumber(number)
      }
    }
    val determinerInflected = tongue.correctGenderCount(
      determinerLemma, gender, count, true)
    compose(determinerInflected, noun)
  }
}
