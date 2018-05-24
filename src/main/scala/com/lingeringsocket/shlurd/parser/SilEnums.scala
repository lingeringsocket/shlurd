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

sealed trait SilLocative
case object LOC_INSIDE extends SilLocative
case object LOC_OUTSIDE extends SilLocative
case object LOC_AT extends SilLocative
case object LOC_AS extends SilLocative
case object LOC_NEAR extends SilLocative
case object LOC_ON extends SilLocative
case object LOC_ABOVE extends SilLocative
case object LOC_BELOW extends SilLocative
case object LOC_LEFT extends SilLocative
case object LOC_RIGHT extends SilLocative
case object LOC_FRONT extends SilLocative
case object LOC_BEHIND extends SilLocative
case object LOC_GENITIVE_OF extends SilLocative

sealed trait SilCount
case object COUNT_SINGULAR extends SilCount
case object COUNT_PLURAL extends SilCount

sealed trait SilInflection
case object INFLECT_NONE extends SilInflection
case object INFLECT_NOMINATIVE extends SilInflection
case object INFLECT_ACCUSATIVE extends SilInflection
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
case object QUESTION_WHICH extends SilQuestion
case object QUESTION_HOW_MANY extends SilQuestion

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

sealed trait SilMood
{
  def isPositive : Boolean = false
  def isNegative : Boolean = false
  def getModality : SilModality = MODAL_NEUTRAL
}
sealed trait SilModalMood extends SilMood
{
  def positive : Boolean
  def modality : SilModality
  override def isPositive = positive
  override def isNegative = !positive
  override def getModality = modality
}
sealed case class SilIndicativeMood(
  positive : Boolean,
  modality : SilModality = MODAL_NEUTRAL
) extends SilModalMood
{
}
sealed case class SilInterrogativeMood(
  positive : Boolean,
  modality : SilModality = MODAL_NEUTRAL
) extends SilModalMood
{
}
object MOOD_INDICATIVE_POSITIVE extends SilIndicativeMood(true)
object MOOD_INDICATIVE_NEGATIVE extends SilIndicativeMood(false)
object MOOD_INTERROGATIVE_POSITIVE extends SilInterrogativeMood(true)
object MOOD_INTERROGATIVE_NEGATIVE extends SilInterrogativeMood(false)
case object MOOD_IMPERATIVE extends SilMood