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

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._

import org.specs2.mutable._

import scala.util._

class SnlWordnetGlossPrepSpec extends Specification
{
  private val annotator = SilBasicAnnotator()

  private implicit val tongue = SnlUtils.defaultTongue

  private def parseAll(
    iter : Iterator[String], parser : String => Option[SilPhrase]) =
  {
    var total = 0
    var passed = 0
    var exception = 0
    var unrecognized = 0

    def reportStatus()
    {
      println
      println("TOTAL = " + total)
      println("UNRECOGNIZED = " + unrecognized)
      println("EXCEPTION = " + exception)
      println("PASSED = " + passed)
    }

    iter.foreach(definition => {
      total += 1
      if ((total % 10) == 0) {
        reportStatus
      }
      Try(parser(definition)) match {
        case Success(Some(predicate)) => {
          passed += 1
        }
        case Failure(_) => {
          exception += 1
        }
        case _ => {
          unrecognized += 1
        }
      }
    })
    reportStatus
    total
  }

  "SnlWordnetGlossPrep" should
  {
    "parse one noun gloss" in
    {
      SnlWordnetGlossPrep.parseNounGlosses("rivulet") must be equalTo Seq(
        Some(
          annotator.determinedRef(
            annotator.stateSpecifiedRef(
              annotator.nounRef(
                SilWord("stream")),
              SilPropertyState(SilWord("small"))),
            DETERMINER_NONSPECIFIC
          )
        )
      )
    }

    "parse one noun example" in
    {
      SnlPrincetonWordnet.getNounSenses("stair").flatMap(
        SnlPrincetonWordnet.getGlossExamples
      ).map(
        SnlWordnetGlossPrep.parseNounExample
      ) must be equalTo Seq(
        SilPredicateSentence(
          SilActionPredicate(
            annotator.basicPronounRef(
              PERSON_THIRD,
              GENDER_MASCULINE,
              COUNT_SINGULAR),
            SilWord("paused", "pause"),
            None,
            Seq(
              SilAdpositionalVerbModifier(
                SilAdposition(MW_ON),
                annotator.determinedRef(
                  annotator.stateSpecifiedRef(
                    annotator.nounRef(
                      SilWord("step")
                    ),
                    SilPropertyState(SilWord("bottom"))),
                  DETERMINER_DEFINITE)))),
          SilTam.indicative.past
        )
      )
    }

    "parse all noun examples" in
    {
      skipped("not ready for prime time")
      val examples = SnlPrincetonWordnet.allNounSenses.flatMap(
        SnlPrincetonWordnet.getGlossExamples)
      def parser(example : String) : Option[SilPhrase] =
      {
        val phrase = SnlWordnetGlossPrep.parseNounExample(example)
        if (phrase.hasUnknown) {
          None
        } else {
          Some(phrase)
        }
      }
      val total = parseAll(examples, parser)
      total must be equalTo 11529
    }

    "parse all noun glosses" in
    {
      skipped("not ready for prime time")
      val glosses = SnlPrincetonWordnet.allNounSenses.flatMap(sense =>
        SnlWordnetGlossPrep.getNounSenseDefinitions(
          sense))
      def parser(gloss : String) : Option[SilPhrase] =
      {
        SnlWordnetGlossPrep.parseNounDefinition(gloss)
      }
      val total = parseAll(glosses, parser)
      total must be equalTo 82192
    }
  }
}
