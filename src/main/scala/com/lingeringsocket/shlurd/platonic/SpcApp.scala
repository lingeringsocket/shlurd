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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd.cosmos._
import com.lingeringsocket.shlurd.parser._

import scala.io._

import ShlurdEnglishLemmas._

object SpcCosmosApp extends App
{
  private val cosmos = new SpcCosmos

  private val mind = new ShlurdMind(cosmos) {
    override def resolvePronoun(
      person : SilPerson,
      gender : SilGender,
      count : SilCount) =
    {
      if (count == COUNT_SINGULAR) {
        person match {
          case PERSON_FIRST => {
            cosmos.resolveQualifiedNoun(
              "interviewer", REF_SUBJECT, Set())
          }
          case PERSON_SECOND => {
            cosmos.resolveQualifiedNoun(
              LEMMA_PERSON, REF_SUBJECT, Set("shlurd"))
          }
          case _ => super.resolvePronoun(person, gender, count)
        }
      } else {
        super.resolvePronoun(person, gender, count)
      }
    }
  }

  private val interpreter = new SpcInterpreter(cosmos, mind, true)

  init()
  run()

  private def init()
  {
    val file = ShlurdParser.getResourceFile("/ontologies/console.txt")
    val source = Source.fromFile(file)
    cosmos.loadBeliefs(source)
  }

  private def run()
  {
    var exit = false
    while (!exit) {
      print("SHLURD> ")
      val input = StdIn.readLine
      if (input == null) {
        exit = true
      } else {
        val sentences = ShlurdParser(input).parseAll
        sentences.foreach(sentence => {
          val output = interpreter.interpret(sentence)
          println
          println(output)
          println
        })
      }
    }
    println
    println("Shutting down")
  }
}
