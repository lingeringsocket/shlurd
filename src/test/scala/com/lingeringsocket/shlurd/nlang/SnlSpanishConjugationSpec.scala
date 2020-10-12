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

import net.sf.extjwnl.data._

import java.io._
import scala.io._

import scala.collection._
import scala.collection.JavaConverters._

class SnlSpanishConjugationSpec extends Specification
{
  private val wordnet = new SnlExternalWordnet("/extjwnl_data_spa.xml")

  private val LINE_COUNT = 11466

  private def parseLine(line : String) : Seq[String] =
  {
    line.split("\",\"").map(_.stripPrefix("\"").stripSuffix("\""))
  }

  private def readDatabaseLines() : Seq[String] =
  {
    // To run tests which use this, get the data from
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
    lines.tail
  }

  private def parseInfinitive(entry : String) : String =
  {
    entry.stripSuffix("(se)") match {
      case "vomit" => "vomitar"
      case x => x
    }
  }

  private def parseTam(row : Seq[String]) : SilTam =
  {
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
    SilTam.indicative.withMood(mood).withPolarity(polarity).withTense(tense)
  }

  private def acceptTam(tam : SilTam) : Boolean =
  {
    (tam.mood == MOOD_INDICATIVE) && (tam.tense != TENSE_INFINITIVE)
  }

  private def lastWord(s : String) : String =
  {
    s.split(" ").last
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

    "produce irregular form list" in
    {
      skipped("requires site specifics")

      val data = readDatabaseLines
      var count = 0
      var mismatches = 0
      data.foreach(line => {
        val row = parseLine(line)
        val infinitive = parseInfinitive(row(0)).stripSuffix("se")
        val tam = parseTam(row)
        if (acceptTam(tam)) {
          val forms = row.slice(7, 13)
          forms.foreach(form => {
            val conjugated = lastWord(form)
            if (conjugated.nonEmpty) {
              val bases = wordnet.getMorphology.lookupAllBaseForms(
                POS.VERB, conjugated).asScala.toSet
              if (!bases.contains(infinitive)) {
                println(s"v#$conjugated -addexc $infinitive")
                mismatches += 1
              }
            }
          })
        }
        count += 1
      })
      count must be equalTo LINE_COUNT
      mismatches must be equalTo 0
    }

    "conjugate verb database" in
    {
      skipped("requires site specifics")

      val data = readDatabaseLines
      var count = 0
      data.foreach(line => {
        println(line)
        println
        val row = parseLine(line)
        row.size must be equalTo 17
        val infinitive = parseInfinitive(row(0))
        val tam = parseTam(row)
        // val gerund = row(13)
        // val participle = row(15)
        // FIXME: MOOD_IMPERATIVE
        if (acceptTam(tam)) {
          val coords = Seq(COUNT_SINGULAR, COUNT_PLURAL).flatMap(count => {
            Seq(PERSON_FIRST, PERSON_SECOND, PERSON_THIRD).map(person => {
              SnlSpanishConjugationCoord(
                person,
                count,
                tam.tense,
                tam.mood,
                ASPECT_SIMPLE)
            })
          })
          val forms = row.slice(7, 13)
          forms.zip(coords).foreach {
            case (form, coord) => {
              println("COORD = " + coord)
              println
              if ((coord.person == PERSON_FIRST) &&
                (tam.mood == MOOD_IMPERATIVE))
              {
                form must beEmpty
              } else {
                val conjugated = SnlSpanishConjugation.conjugateVerb(
                  infinitive,
                  coord
                )
                val normalized = {
                  if (conjugated.startsWith("os ") && !form.startsWith("os ")) {
                    // ignore some inconsistencies in the verb database
                    lastWord(conjugated)
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
      count must be equalTo LINE_COUNT
    }
  }
}
