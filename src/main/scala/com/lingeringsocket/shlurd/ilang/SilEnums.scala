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

// FIXME
import SprEnglishLemmas._

sealed trait SilReferenceContext
case object REF_SUBJECT extends SilReferenceContext
case object REF_COMPLEMENT extends SilReferenceContext
case object REF_DIRECT_OBJECT extends SilReferenceContext
case object REF_ADPOSITION_OBJ extends SilReferenceContext
case object REF_ADPOSITION_SUBJ extends SilReferenceContext
case object REF_GENITIVE_POSSESSOR extends SilReferenceContext
case object REF_GENITIVE_POSSESSEE extends SilReferenceContext
case object REF_SPECIFIED extends SilReferenceContext

sealed trait SilPerson
case object PERSON_FIRST extends SilPerson
case object PERSON_SECOND extends SilPerson
case object PERSON_THIRD extends SilPerson

// SilGender is intentionally not sealed; we define some builtins here,
// and then leave it open for extension
trait SilGender
{
  def maybeBasic() : Option[SilBasicGender] = None
}
sealed trait SilBasicGender extends SilGender
{
  override def maybeBasic() = Some(this)
}
case object GENDER_MASCULINE extends SilBasicGender
case object GENDER_FEMININE extends SilBasicGender
case object GENDER_NEUTER extends SilBasicGender
case object GENDER_SOMEONE extends SilBasicGender

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
case object DISTANCE_REFLEXIVE extends SilDistance

sealed trait SilCompoundStyle
case object COMPOUND_OPEN extends SilCompoundStyle
case object COMPOUND_CLOSED extends SilCompoundStyle
case object COMPOUND_HYPHENATED extends SilCompoundStyle

case class SilAdposition(word : SilWord)
{
}

object SilAdposition
{
  def apply(words : Seq[SilSimpleWord]) : SilAdposition =
  {
    if (words.size == 1) {
      SilAdposition(words.head)
    } else {
      SilAdposition(SilCompoundWord(words))
    }
  }

  private def adposition(lemma : String) =
  {
    SilAdposition(SilWord(lemma))
  }

  val AMONG = adposition(LEMMA_AMONG)
  val EXCEPT = adposition(LEMMA_EXCEPT)
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
  val BEFORE = adposition(LEMMA_BEFORE)
  val AFTER = adposition(LEMMA_AFTER)
  val LEFT = adposition(LEMMA_LEFT)
  val RIGHT = adposition(LEMMA_RIGHT)
  val FRONT = adposition(LEMMA_FRONT)
  val BACK = adposition(LEMMA_BACK)
  val BEHIND = adposition(LEMMA_BEHIND)
  val TO = adposition(LEMMA_TO)
  val FROM = adposition(LEMMA_FROM)
  val OF = adposition(LEMMA_OF)
  val GENITIVE_OF = adposition(LEMMA_GENITIVE_OF)
  val ADVERBIAL_TMP = adposition(LEMMA_ADVERBIAL_TMP)
}

sealed trait SilCount
// "dog"
case object COUNT_SINGULAR extends SilCount
// "dogs"
case object COUNT_PLURAL extends SilCount
// "sheep"
case object COUNT_ZERO_PLURAL extends SilCount
// "rice"
case object COUNT_MASS extends SilCount

object SilMandatorySingular
{
  def unapply(ref : SilNounReference) =
  {
    ref.count match {
      case COUNT_SINGULAR => Some(ref.noun)
      case _ => None
    }
  }
}

object SilMandatoryPlural
{
  def unapply(ref : SilNounReference) =
  {
    ref.count match {
      case COUNT_PLURAL => Some(ref.noun)
      case _ => None
    }
  }
}

// FIXME:  this naming is nonstandard
sealed trait SilInflection
case object INFLECT_NONE extends SilInflection
case object INFLECT_NOMINATIVE extends SilInflection
case object INFLECT_ACCUSATIVE extends SilInflection
case object INFLECT_ADPOSITIONED extends SilInflection
case object INFLECT_GENITIVE extends SilInflection
case object INFLECT_COMPLEMENT extends SilInflection

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
case object MODAL_ELLIPTICAL extends SilModality

sealed trait SilPolarity
case object POLARITY_POSITIVE extends SilPolarity
case object POLARITY_NEGATIVE extends SilPolarity

sealed trait SilTense
case object TENSE_PAST extends SilTense
case object TENSE_PRESENT extends SilTense
case object TENSE_FUTURE extends SilTense

sealed trait SilAspect
case object ASPECT_SIMPLE extends SilAspect
case object ASPECT_PROGRESSIVE extends SilAspect

sealed trait SilRelationshipPredef
{
  def toLemma : String
  def toVerb = SilWord(toLemma)
}
case object REL_PREDEF_IDENTITY extends SilRelationshipPredef
{
  override def toLemma = LEMMA_BE
}
case object REL_PREDEF_BECOME extends SilRelationshipPredef
{
  override def toLemma = LEMMA_BECOME
}
case object REL_PREDEF_ASSOC extends SilRelationshipPredef
{
  override def toLemma = LEMMA_HAVE
}

object SilRelationshipPredef
{
  val enumeration = Seq(
    REL_PREDEF_IDENTITY, REL_PREDEF_BECOME, REL_PREDEF_ASSOC)

  val lemmaMap = Map(enumeration.map(rel => tupleN((rel.toLemma, rel))):_*)

  def apply(word : SilWord) =
  {
    lemmaMap.get(word.toLemma).getOrElse {
      throw new IllegalArgumentException(
        "Non-predef relationship verb " + word)
    }
  }
}

object SilRelationshipPredefVerb
{
  def unapply(w : SilSimpleWord) =
  {
    Some(SilRelationshipPredef(w))
  }
}

sealed trait SilStatePredef
{
  def toLemma : String
  def toVerb = SilWord(toLemma)
}

case object STATE_PREDEF_BE extends SilStatePredef
{
  override def toLemma = LEMMA_BE
}

case object STATE_PREDEF_BECOME extends SilStatePredef
{
  override def toLemma = LEMMA_BECOME
}

object SilStatePredef
{
  def apply(word : SilWord) =
  {
    word.toLemma match {
      case LEMMA_EXIST | LEMMA_BE => STATE_PREDEF_BE
      case LEMMA_BECOME => STATE_PREDEF_BECOME
      case _ => throw new IllegalArgumentException(
        "Non-predef state verb " + word)
    }
  }
}

object SilStatePredefVerb
{
  def unapply(w : SilSimpleWord) =
  {
    Some(SilStatePredef(w))
  }
}

sealed trait SilBracket
{
  def begin : String
  def end : String
}

case object BRACKET_NONE extends SilBracket
{
  override def begin = ""
  override def end = ""
}
case object BRACKET_PAREN extends SilBracket
{
  override def begin = "("
  override def end = ")"
}
case object BRACKET_CURLY extends SilBracket
{
  override def begin = "{"
  override def end = "}"
}
case object BRACKET_DQUOTE extends SilBracket
{
  override def begin = DQUOTE
  override def end = DQUOTE
}

sealed trait SilSeparator
{
  def needPunctuationAt(pos : Int, total : Int) : Boolean
  def punctuationMark : String
}
case object SEPARATOR_CONJOINED extends SilSeparator
{
  override def needPunctuationAt(pos : Int, total : Int) = false
  override def punctuationMark = ""
}
case object SEPARATOR_COMMA extends SilSeparator
{
  override def needPunctuationAt(pos : Int, total : Int) = ((pos + 2) < total)
  override def punctuationMark = ","
}
case object SEPARATOR_OXFORD_COMMA extends SilSeparator
{
  override def needPunctuationAt(pos : Int, total : Int) =
    (((pos + 1) < total) && (total > 2))
  override def punctuationMark = ","
}
case object SEPARATOR_SEMICOLON extends SilSeparator
{
  override def needPunctuationAt(pos : Int, total : Int) =
    (((pos + 1) < total) && (total > 2))
  override def punctuationMark = ";"
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
  private def apply(mood : SilMood) =
    SilTamImmutable(
      mood, POLARITY_POSITIVE, MODAL_NEUTRAL, ASPECT_SIMPLE, TENSE_PRESENT)

  def indicative = SilTam(MOOD_INDICATIVE)

  def interrogative = SilTam(MOOD_INTERROGATIVE)

  def imperative = SilTam(MOOD_IMPERATIVE)
}

sealed trait SilTam
{
  def mood : SilMood
  def polarity : SilPolarity
  def modality : SilModality
  def aspect : SilAspect
  def tense : SilTense
  def isIndicative : Boolean = (mood == MOOD_INDICATIVE)
  def isInterrogative : Boolean = (mood == MOOD_INTERROGATIVE)
  def isImperative : Boolean = (mood == MOOD_IMPERATIVE)
  def isPositive : Boolean = (polarity == POLARITY_POSITIVE)
  def isNegative : Boolean = (polarity == POLARITY_NEGATIVE)
  def isProgressive = (aspect == ASPECT_PROGRESSIVE)
  def isPast = (tense == TENSE_PAST)
  def isPresent = (tense == TENSE_PRESENT)
  def isFuture = (tense == TENSE_FUTURE)
  def positive : SilTam
  def negative : SilTam
  def progressive : SilTam
  def past : SilTam
  def present : SilTam
  def future : SilTam
  def withAspect(newAspect : SilAspect) : SilTam
  def withPolarity(newPolarity : SilPolarity) : SilTam
  def withModality(newModality : SilModality) : SilTam
  def withMood(newMood : SilMood) : SilTam
  def withTense(newTense : SilTense) : SilTam
  def isValid(withAssert : Boolean = false) : Boolean

  def withPolarity(newPolarity : Boolean) : SilTam =
    withPolarity(if (newPolarity) POLARITY_POSITIVE else POLARITY_NEGATIVE)

  def validate() : SilTam =
  {
    assert(isValid(true))
    this
  }

  // FIXME this is English-specific
  def requiresAux =
  {
    if (isProgressive) {
      true
    } else {
      modality match {
        case MODAL_NEUTRAL => false
        case _ => true
      }
    }
  }

  def unemphaticModality =
  {
    modality match {
      case MODAL_EMPHATIC => MODAL_NEUTRAL
      case _ => modality
    }
  }
}

case class SilTamImmutable(
  mood : SilMood,
  polarity : SilPolarity,
  modality : SilModality,
  aspect : SilAspect,
  tense : SilTense
) extends SilTam
{
  override def positive = withPolarity(POLARITY_POSITIVE)
  override def negative = withPolarity(POLARITY_NEGATIVE)
  override def progressive = withAspect(ASPECT_PROGRESSIVE)

  def past : SilTam = withTense(TENSE_PAST)
  def present : SilTam = withTense(TENSE_PRESENT)
  def future : SilTam = withTense(TENSE_FUTURE)

  override def withPolarity(newPolarity : SilPolarity) =
    copy(polarity = newPolarity).validate
  override def withAspect(newAspect : SilAspect) =
    copy(aspect = newAspect).validate
  override def withModality(newModality : SilModality) =
    copy(modality = newModality).validate
  override def withMood(newMood : SilMood) =
    copy(mood = newMood).validate
  override def withTense(newTense : SilTense) =
    copy(tense = newTense).validate

  override def isValid(withAssert : Boolean = false) : Boolean =
  {
    var valid = true
    def check(condition : Boolean) {
      assert(condition || !withAssert)
      if (!condition) {
        valid = false
      }
    }
    mood match {
      case MOOD_IMPERATIVE => {
        check(tense == TENSE_PRESENT)
        check(modality == MODAL_NEUTRAL)
        check(!isProgressive)
      }
      case _ => {
        if (isProgressive) {
          // this needs some refinement
          check(modality == MODAL_NEUTRAL)
        }
        if (isPast) {
          // this needs some refinement
          check(modality == MODAL_NEUTRAL || modality == MODAL_EMPHATIC)
        }
      }
    }
    valid
  }
}

case class SilPronounKey(
  usage : String,
  person : SilPerson
)
{
}

object SilPronounMap
{
  def apply() : SilPronounMap = Map.empty
}
