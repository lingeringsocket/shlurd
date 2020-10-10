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

import SnlSpanishLemmas._

import org.specs2.mutable._

import java.io._
import scala.io._

class SnlSpanishConjugationSpec extends Specification
{
  private def parseLine(line : String) : Seq[String] =
  {
    line.split("\",\"").map(_.stripPrefix("\"").stripSuffix("\""))
  }

  "SpanishVerbConjugator" should
  {
    "conjugate one verb" in
    {
      SnlSpanishConjugation.conjugateVerb(
        LEMMA_SER,
        SnlSpanishConjugationCoord(
          PERSON_FIRST,
          COUNT_SINGULAR,
          TENSE_PRESENT,
          MOOD_INDICATIVE,
          ASPECT_SIMPLE)
      ) must be equalTo("soy")
    }

    "conjugate verb database" in
    {
      skipped("requires site specifics")

      // To run this, get the data from
      // https://github.com/ghidinelli/fred-jehle-spanish-verbs
      // convert dos2unix line endings, and
      // edit the path here
      val file = new File(
        "/home/jvs/open/fred-jehle-spanish-verbs/jehle_verb_database.csv")
      val source = Source.fromFile(file)
      val lines = source.getLines.toSeq
      val header = lines.head
      val headings = parseLine(header)
      headings must be equalTo Seq(
        "infinitive",
        "infinitive_english",
        "mood",
        "mood_english",
        "tense",
        "tense_english",
        "verb_english",
        "form_1s",
        "form_2s",
        "form_3s",
        "form_1p",
        "form_2p",
        "form_3p",
        "gerund",
        "gerund_english",
        "pastparticiple",
        "pastparticiple_english"
      )
      val data = lines.tail
      var count = 0
      data.foreach(line => {
        println(line)
        println
        val row = parseLine(line)
        row.size must be equalTo 17
        val infinitive = row(0).stripSuffix("(se)") match {
          case "vomit" => "vomitar"
          case x => x
        }
        val (mood, polarity) = row(2) match {
          case "Indicativo" =>
            tupleN((MOOD_INDICATIVE, POLARITY_POSITIVE))
          case "Imperativo Afirmativo" =>
            tupleN((MOOD_IMPERATIVE, POLARITY_POSITIVE))
          case "Imperativo Negativo" =>
            tupleN((MOOD_IMPERATIVE, POLARITY_NEGATIVE))
          // FIXME Subjuntivo
          case _ =>
            tupleN((MOOD_INTERROGATIVE, POLARITY_POSITIVE))
        }
        val tense = row(4) match {
          case "Futuro" => TENSE_FUTURE
          case "Pretérito" => TENSE_PAST
          case "Presente" => TENSE_PRESENT
            // FIXME handle Imperfecto, Condicional, Presente perfecto,
            // Futuro perfecto, Pluscuamperfecto, Pretérito anterior,
            // Condicional perfecto
          case _ => TENSE_INFINITIVE
        }
        val forms = row.slice(7, 13)
        // val gerund = row(13)
        // val participle = row(15)
        // FIXME: MOOD_IMPERATIVE
        if (
          (mood == MOOD_INDICATIVE) &&
            (tense != TENSE_INFINITIVE)
        ) {
          val coords = Seq(COUNT_SINGULAR, COUNT_PLURAL).flatMap(count => {
            Seq(PERSON_FIRST, PERSON_SECOND, PERSON_THIRD).map(person => {
              SnlSpanishConjugationCoord(
                person,
                count,
                tense,
                mood,
                ASPECT_SIMPLE)
            })
          })
          forms.zip(coords).foreach {
            case (form, coord) => {
              println("COORD = " + coord)
              println
              if ((coord.person == PERSON_FIRST) && (mood == MOOD_IMPERATIVE)) {
                form must beEmpty
              } else {
                val conjugated = SnlSpanishConjugation.conjugateVerb(
                  infinitive,
                  coord
                )
                val normalized = {
                  if (conjugated.startsWith("os ") && !form.startsWith("os ")) {
                    // ignore some inconsistencies in the verb database
                    conjugated.stripPrefix("os ")
                  } else {
                    conjugated
                  }
                }
                if (form.nonEmpty) {
                  normalized must be equalTo(form)
                }
              }
            }
          }
        }
        count += 1
      })
      count must be equalTo 11466
    }
  }
}
