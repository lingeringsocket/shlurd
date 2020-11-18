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

import com.lingeringsocket.morphala.spanish._

import scala.collection._
import scala.collection.concurrent._

import SnlSpanishLemmas._

case class SnlSpanishConjugationCoord(
  person : SilPerson,
  count : SilCount,
  tense : SilTense,
  mood : SilMood,
  aspect : SilAspect
)
{
}

object SnlSpanishConjugationCoord
{
  def apply(
    tam : SilTam, person : SilPerson, count : SilCount
  ) : SnlSpanishConjugationCoord =
  {
    if (tam.isNegative && tam.isImperative) {
      SnlSpanishConjugationCoord(
        person,
        count,
        tam.tense,
        MOOD_SUBJUNCTIVE,
        tam.aspect)
    } else {
      SnlSpanishConjugationCoord(
        person,
        count,
        tam.tense,
        tam.mood,
        tam.aspect)
    }
  }
}

object SnlSpanishConjugation
{
  private val cache =
    new TrieMap[(String, String), SnlSpanishConjugationCoord]

  // Order of sequences below matters!  In case of collision, we want to pick
  // the first one.  For example, when indicative and imperative collide,
  // assume indicative.

  private val allPersons =
    Seq(PERSON_FIRST, PERSON_SECOND, PERSON_THIRD)
  private val allCounts =
    Seq(COUNT_SINGULAR, COUNT_PLURAL)
  private val allTenses =
    Seq(TENSE_PRESENT, TENSE_PAST, TENSE_FUTURE)
  private val someMoods =
    Seq(MOOD_INDICATIVE, MOOD_SUBJUNCTIVE)

  val fundamentalCoords = allPersons.flatMap(person => {
    allCounts.flatMap(count => {
      allTenses.map(tense => {
        SnlSpanishConjugationCoord(
          person, count, tense, MOOD_INDICATIVE, ASPECT_SIMPLE)
      })
    })
  }) ++ allCounts.map(count => {
    SnlSpanishConjugationCoord(
      PERSON_THIRD, count, TENSE_PRESENT, MOOD_IMPERATIVE, ASPECT_SIMPLE)
  }) ++ Seq(
    SnlSpanishConjugationCoord(
      PERSON_SECOND, COUNT_PLURAL, TENSE_PRESENT,
      MOOD_IMPERATIVE, ASPECT_SIMPLE
    )
  ) ++ allPersons.flatMap(person => {
    allCounts.flatMap(count => {
      allTenses.map(tense => {
        SnlSpanishConjugationCoord(
          person, count, tense, MOOD_SUBJUNCTIVE, ASPECT_SIMPLE)
      })
    })
  }) ++ allPersons.flatMap(person => {
    allCounts.flatMap(count => {
      someMoods.map(mood => {
        SnlSpanishConjugationCoord(
          person, count, TENSE_PAST, mood, ASPECT_IMPERFECT)
      })
    })
  })

  def isValidInfinitive(infinitive : String) : Boolean =
  {
    infinitive.endsWith("ar") || infinitive.endsWith("er") ||
      infinitive.endsWith("ir") || infinitive.endsWith("ír")
  }

  def conjugateGerund(
    infinitive : String) : String =
  {
    if (isValidInfinitive(infinitive)) {
      SpanishGerund.gerund(infinitive)
    } else {
      infinitive
    }
  }

  def conjugateParticiple(
    infinitive : String) : String =
  {
    if (isValidInfinitive(infinitive)) {
      SpanishParticiple.pastParticiple(infinitive)
    } else {
      infinitive
    }
  }

  def conjugateVerb(
    infinitive : String,
    coord : SnlSpanishConjugationCoord,
    fillCache : Boolean = false) : String =
  {
    if ((coord.tense == TENSE_INFINITIVE) || !isValidInfinitive(infinitive)) {
      return infinitive
    }
    // FIXME all the tams
    val spanishTense = {
      coord.mood match {
        case MOOD_IMPERATIVE => SpanishImperative
        case MOOD_SUBJUNCTIVE => {
          coord.tense match {
            case TENSE_PAST => SpanishImperfectSubjunctive
            case _ => SpanishPresentSubjunctive
          }
        }
        case _ => {
          coord.aspect match {
            case ASPECT_SIMPLE | ASPECT_PROGRESSIVE => {
              coord.tense match {
                case TENSE_PAST => SpanishPreterite
                case TENSE_FUTURE => SpanishFutureIndicative
                case _ => SpanishPresentIndicative
              }
            }
            case ASPECT_IMPERFECT => SpanishImperfectIndicative
          }
        }
      }
    }
    val iPerson = coord.person match {
      case PERSON_FIRST => 0
      case PERSON_SECOND => 1
      case PERSON_THIRD => 2
    }
    val (firstVerb, secondVerbOpt) = coord.aspect match {
      case ASPECT_PROGRESSIVE => {
        tupleN(LEMMA_ESTAR, Some(conjugateGerund(infinitive)))
      }
      case _ => {
        tupleN(infinitive, None)
      }
    }
    val conjugated =
      SpanishVerbConjugator.conjugate(
        firstVerb, spanishTense, iPerson, (coord.count != COUNT_SINGULAR)
      ) + (
        secondVerbOpt.map(v => s" $v").getOrElse("")
      )
    val key = tupleN(infinitive, conjugated)
    if (fillCache && !cache.contains(key)) {
      cache.put(key, coord)
    }
    conjugated
  }

  def getConjugationCoord(
    infinitive : String, conjugated : String
  ) : Option[SnlSpanishConjugationCoord] =
  {
    cache.get(tupleN(infinitive, conjugated)).orElse {
      fundamentalCoords.foreach(coord => {
        conjugateVerb(infinitive, coord, true)
      })
      cache.get(tupleN(infinitive, conjugated))
    }
  }
}
