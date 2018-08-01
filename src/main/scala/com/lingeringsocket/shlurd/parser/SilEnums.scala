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
package com.lingeringsocket.shlurd.parser

// FIXME
import ShlurdEnglishLemmas._

sealed trait SilPerson
case object PERSON_FIRST extends SilPerson
case object PERSON_SECOND extends SilPerson
case object PERSON_THIRD extends SilPerson

sealed trait SilGender
case object GENDER_M extends SilGender
case object GENDER_F extends SilGender
case object GENDER_N extends SilGender

sealed trait SilDeterminer
case object DETERMINER_NONE extends SilDeterminer
case object DETERMINER_UNIQUE extends SilDeterminer
case object DETERMINER_NONSPECIFIC extends SilDeterminer
case object DETERMINER_ANY extends SilDeterminer
case object DETERMINER_SOME extends SilDeterminer
case object DETERMINER_ALL extends SilDeterminer
case object DETERMINER_UNSPECIFIED extends SilDeterminer
case class SilIntegerDeterminer(number : Int) extends SilDeterminer

sealed trait SilDistance
case object DISTANCE_HERE extends SilDistance
case object DISTANCE_THERE extends SilDistance
case object DISTANCE_UNSPECIFIED extends SilDistance

case class SilAdposition(words : Seq[SilWord])
{
}

object SilAdposition
{
  private def adposition(lemma : String) =
  {
    SilAdposition(Seq(SilWord(lemma, lemma)))
  }

  val IN = adposition(LEMMA_IN)
  val INSIDE = adposition(LEMMA_INSIDE)
  val WITHIN = adposition(LEMMA_WITHIN)
  val OUTSIDE = adposition(LEMMA_OUTSIDE)
  val AT = adposition(LEMMA_AT)
  val WITH = adposition(LEMMA_WITH)
  val AS = adposition(LEMMA_AS)
  val NEAR = adposition(LEMMA_NEAR)
  val NEARBY = adposition(LEMMA_NEARBY)
  val ON = adposition(LEMMA_ON)
  val ABOVE = adposition(LEMMA_ABOVE)
  val OVER = adposition(LEMMA_OVER)
  val BELOW = adposition(LEMMA_BELOW)
  val UNDER = adposition(LEMMA_UNDER)
  val BENEATH = adposition(LEMMA_BENEATH)
  val UNDERNEATH = adposition(LEMMA_UNDERNEATH)
  val LEFT = adposition(LEMMA_LEFT)
  val RIGHT = adposition(LEMMA_RIGHT)
  val FRONT = adposition(LEMMA_FRONT)
  val BACK = adposition(LEMMA_BACK)
  val BEHIND = adposition(LEMMA_BEHIND)
  val TO = adposition(LEMMA_TO)
  val OF = adposition(LEMMA_OF)
  val GENITIVE_OF = adposition(LEMMA_GENITIVE_OF)
}

sealed trait SilCount
case object COUNT_SINGULAR extends SilCount
case object COUNT_PLURAL extends SilCount

sealed trait SilInflection
case object INFLECT_NONE extends SilInflection
case object INFLECT_NOMINATIVE extends SilInflection
case object INFLECT_ACCUSATIVE extends SilInflection
case object INFLECT_DATIVE extends SilInflection
case object INFLECT_GENITIVE extends SilInflection

sealed trait SilAssumption
case object ASSUMED_TRUE extends SilAssumption
case object ASSUMED_FALSE extends SilAssumption
case object ASSUMED_NOTHING extends SilAssumption

sealed trait SilForce
case object FORCE_NEUTRAL extends SilForce
case object FORCE_EXCLAMATION extends SilForce

sealed trait SilQuestion
case object QUESTION_WHO extends SilQuestion
case object QUESTION_WHAT extends SilQuestion
case object QUESTION_WHICH extends SilQuestion
case object QUESTION_HOW_MANY extends SilQuestion
case object QUESTION_WHERE extends SilQuestion

sealed trait SilMood
case object MOOD_INDICATIVE extends SilMood
case object MOOD_INTERROGATIVE extends SilMood
case object MOOD_IMPERATIVE extends SilMood

sealed trait SilModality
case object MODAL_NEUTRAL extends SilModality
case object MODAL_MUST extends SilModality
// "may" may be either possible or permitted
case object MODAL_MAY extends SilModality
case object MODAL_POSSIBLE extends SilModality
case object MODAL_CAPABLE extends SilModality
case object MODAL_PERMITTED extends SilModality
case object MODAL_SHOULD extends SilModality
case object MODAL_EMPHATIC extends SilModality
// FIXME combine aspect with modality
case object MODAL_PROGRESSIVE extends SilModality
case object MODAL_ELLIPTICAL extends SilModality

sealed trait SilRelationship
case object REL_IDENTITY extends SilRelationship
case object REL_ASSOCIATION extends SilRelationship

sealed trait SilSeparator
{
  def needComma(pos : Int, total : Int) : Boolean
}
case object SEPARATOR_CONJOINED extends SilSeparator
{
  override def needComma(pos : Int, total : Int) = false
}
case object SEPARATOR_COMMA extends SilSeparator
{
  override def needComma(pos : Int, total : Int) = ((pos + 2) < total)
}
case object SEPARATOR_OXFORD_COMMA extends SilSeparator
{
  override def needComma(pos : Int, total : Int) =
    (((pos + 1) < total) && (total > 2))
}

sealed case class SilFormality(
  force : SilForce
)

object SilFormality
{
  val DEFAULT = SilFormality(FORCE_NEUTRAL)
}

object SilTam
{
  def indicative =
    SilTamImmutable(MOOD_INDICATIVE, true, MODAL_NEUTRAL)

  def interrogative =
    SilTamImmutable(MOOD_INTERROGATIVE, true, MODAL_NEUTRAL)

  def imperative =
  {
    SilTamImmutable(MOOD_IMPERATIVE, true, MODAL_NEUTRAL)
  }
}

sealed trait SilTam
{
  def mood : SilMood
  def isIndicative : Boolean = (mood == MOOD_INDICATIVE)
  def isInterrogative : Boolean = (mood == MOOD_INTERROGATIVE)
  def isImperative : Boolean = (mood == MOOD_IMPERATIVE)
  def positivity : Boolean
  def isPositive : Boolean = positivity
  def isNegative : Boolean = !positivity
  def isProgressive : Boolean = false
  def modality : SilModality
  def positive : SilTam
  def negative : SilTam
  def withPositivity(positivity : Boolean) : SilTam
  def withModality(modality : SilModality) : SilTam
  def withMood(mood : SilMood) : SilTam
}

case class SilTamImmutable(
  mood : SilMood,
  positivity : Boolean,
  modality : SilModality
) extends SilTam
{
  override def isProgressive = (modality == MODAL_PROGRESSIVE)
  override def positive = copy(positivity = true)
  override def negative = copy(positivity = false)
  override def withPositivity(newPositivity : Boolean) =
    copy(positivity = newPositivity)
  override def withModality(newModality : SilModality) =
    copy(modality = newModality)
  override def withMood(newMood : SilMood) =
    copy(mood = newMood)
}
