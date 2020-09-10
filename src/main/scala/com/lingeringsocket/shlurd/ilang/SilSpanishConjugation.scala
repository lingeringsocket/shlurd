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

import jvendrow._

import scala.collection._
import scala.collection.concurrent._

case class SilSpanishConjugationCoord(
  person : SilPerson,
  count : SilCount,
  tense : SilTense,
  mood : SilMood,
  aspect : SilAspect
)
{
}

object SilSpanishConjugationCoord
{
  def apply(
    tam : SilTam, person : SilPerson, count : SilCount
  ) : SilSpanishConjugationCoord =
  {
    SilSpanishConjugationCoord(
      person,
      count,
      tam.tense,
      tam.mood,
      tam.aspect)
  }
}

object SilSpanishConjugation
{
  private val cache =
    new TrieMap[(String, String), SilSpanishConjugationCoord]

  private val allPersons =
    Seq(PERSON_FIRST, PERSON_SECOND, PERSON_THIRD)
  private val allCounts =
    Seq(COUNT_SINGULAR, COUNT_PLURAL)
  private val allTenses =
    Seq(TENSE_PRESENT, TENSE_PAST, TENSE_FUTURE)
  private val allAspects =
    Seq(ASPECT_SIMPLE, ASPECT_PROGRESSIVE)
  private val imperativePersons =
    Seq(PERSON_SECOND, PERSON_THIRD)

  private val validCoords = allPersons.flatMap(person => {
    allCounts.flatMap(count => {
      allTenses.flatMap(tense => {
        allAspects.map(aspect => {
          SilSpanishConjugationCoord(
            person, count, tense, MOOD_INDICATIVE, aspect)
        })
      })
    })
  }) ++ imperativePersons.map(person => {
    SilSpanishConjugationCoord(
      person, COUNT_SINGULAR, TENSE_PRESENT, MOOD_IMPERATIVE, ASPECT_SIMPLE)
  })

  def conjugateVerb(
    infinitive : String,
    coord : SilSpanishConjugationCoord) : String =
  {
    // FIXME all the tams
    val spanishTense = {
      coord.mood match {
        case MOOD_IMPERATIVE => {
          coord.person match {
            // informal
            case PERSON_SECOND => SpanishVerbConjugator.commandsPositive
            // formal
            case _ => SpanishVerbConjugator.presentSubjunctive
          }
        }
        case _ => {
          coord.aspect match {
            case ASPECT_SIMPLE => {
              coord.tense match {
                case TENSE_PAST => {
                  SpanishVerbConjugator.preterite
                }
                case TENSE_FUTURE => {
                  SpanishVerbConjugator.futureSimple
                }
                case _ => {
                  SpanishVerbConjugator.present
                }
              }
            }
            case ASPECT_PROGRESSIVE => {
              coord.tense match {
                case TENSE_PAST => {
                  SpanishVerbConjugator.pastProgressive
                }
                case TENSE_FUTURE => {
                  SpanishVerbConjugator.futureProgressive
                }
                case _ => {
                  SpanishVerbConjugator.presentProgressive
                }
              }
            }
          }
        }
      }
    }
    val iPerson = coord.person match {
      case PERSON_FIRST => 0
      case PERSON_SECOND => 1
      case PERSON_THIRD => 2
    }
    val conjugated =
      SpanishVerbConjugator.conjugate(
        infinitive, spanishTense, iPerson, (coord.count != COUNT_SINGULAR))
    cache.put(tupleN((infinitive, conjugated)), coord)
    conjugated
  }

  def getConjugationCoord(
    infinitive : String, conjugated : String
  ) : Option[SilSpanishConjugationCoord] =
  {
    cache.get(tupleN((infinitive, conjugated))).orElse {
      validCoords.foreach(coord => {
        conjugateVerb(infinitive, coord)
      })
      cache.get(tupleN((infinitive, conjugated)))
    }
  }
}
