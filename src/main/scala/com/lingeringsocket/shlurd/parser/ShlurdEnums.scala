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

sealed trait ShlurdPerson
case object PERSON_FIRST extends ShlurdPerson
case object PERSON_SECOND extends ShlurdPerson
case object PERSON_THIRD extends ShlurdPerson

sealed trait ShlurdGender
case object GENDER_M extends ShlurdGender
case object GENDER_F extends ShlurdGender
case object GENDER_N extends ShlurdGender

sealed trait ShlurdDeterminer
case object DETERMINER_NONE extends ShlurdDeterminer
case object DETERMINER_UNIQUE extends ShlurdDeterminer
case object DETERMINER_NONSPECIFIC extends ShlurdDeterminer
case object DETERMINER_ANY extends ShlurdDeterminer
case object DETERMINER_SOME extends ShlurdDeterminer
case object DETERMINER_ALL extends ShlurdDeterminer
case object DETERMINER_UNSPECIFIED extends ShlurdDeterminer

sealed trait ShlurdLocative
case object LOC_INSIDE extends ShlurdLocative
case object LOC_OUTSIDE extends ShlurdLocative
case object LOC_AT extends ShlurdLocative
case object LOC_NEAR extends ShlurdLocative
case object LOC_ON extends ShlurdLocative
case object LOC_ABOVE extends ShlurdLocative
case object LOC_BELOW extends ShlurdLocative
case object LOC_LEFT extends ShlurdLocative
case object LOC_RIGHT extends ShlurdLocative
case object LOC_FRONT extends ShlurdLocative
case object LOC_BEHIND extends ShlurdLocative
case object LOC_GENITIVE_OF extends ShlurdLocative

sealed trait ShlurdCount
case object COUNT_SINGULAR extends ShlurdCount
case object COUNT_PLURAL extends ShlurdCount

sealed trait ShlurdInflection
case object INFLECT_NONE extends ShlurdInflection
case object INFLECT_NOMINATIVE extends ShlurdInflection
case object INFLECT_ACCUSATIVE extends ShlurdInflection
case object INFLECT_GENITIVE extends ShlurdInflection

sealed trait ShlurdAssumption
case object ASSUMED_TRUE extends ShlurdAssumption
case object ASSUMED_FALSE extends ShlurdAssumption
case object ASSUMED_NOTHING extends ShlurdAssumption

sealed trait ShlurdForce
case object FORCE_NEUTRAL extends ShlurdForce
case object FORCE_EXCLAMATION extends ShlurdForce

sealed trait ShlurdQuestion
case object QUESTION_WHO extends ShlurdQuestion
case object QUESTION_WHICH extends ShlurdQuestion
case object QUESTION_HOW_MANY extends ShlurdQuestion

sealed trait ShlurdModality
case object MODAL_NEUTRAL extends ShlurdModality
case object MODAL_MUST extends ShlurdModality
// "may" may be either possible or permitted
case object MODAL_MAY extends ShlurdModality
case object MODAL_POSSIBLE extends ShlurdModality
case object MODAL_CAPABLE extends ShlurdModality
case object MODAL_PERMITTED extends ShlurdModality
case object MODAL_SHOULD extends ShlurdModality
case object MODAL_EMPHATIC extends ShlurdModality

sealed trait ShlurdRelationship
case object REL_IDENTITY extends ShlurdRelationship
case object REL_ASSOCIATION extends ShlurdRelationship

sealed trait ShlurdSeparator
{
  def needComma(pos : Int, total : Int) : Boolean
}
case object SEPARATOR_CONJOINED extends ShlurdSeparator
{
  override def needComma(pos : Int, total : Int) = false
}
case object SEPARATOR_COMMA extends ShlurdSeparator
{
  override def needComma(pos : Int, total : Int) = ((pos + 2) < total)
}
case object SEPARATOR_OXFORD_COMMA extends ShlurdSeparator
{
  override def needComma(pos : Int, total : Int) =
    (((pos + 1) < total) && (total > 2))
}

sealed case class ShlurdFormality(
  force : ShlurdForce
)

object ShlurdFormality
{
  val DEFAULT = ShlurdFormality(FORCE_NEUTRAL)
}

sealed trait ShlurdMood
{
  def isPositive : Boolean = false
  def isNegative : Boolean = false
  def getModality : ShlurdModality = MODAL_NEUTRAL
}
sealed trait ShlurdModalMood extends ShlurdMood
{
  def positive : Boolean
  def modality : ShlurdModality
  override def isPositive = positive
  override def isNegative = !positive
  override def getModality = modality
}
sealed case class ShlurdIndicativeMood(
  positive : Boolean,
  modality : ShlurdModality = MODAL_NEUTRAL
) extends ShlurdModalMood
{
}
sealed case class ShlurdInterrogativeMood(
  positive : Boolean,
  modality : ShlurdModality = MODAL_NEUTRAL
) extends ShlurdModalMood
{
}
object MOOD_INDICATIVE_POSITIVE extends ShlurdIndicativeMood(true)
object MOOD_INDICATIVE_NEGATIVE extends ShlurdIndicativeMood(false)
object MOOD_INTERROGATIVE_POSITIVE extends ShlurdInterrogativeMood(true)
object MOOD_INTERROGATIVE_NEGATIVE extends ShlurdInterrogativeMood(false)
case object MOOD_IMPERATIVE extends ShlurdMood
