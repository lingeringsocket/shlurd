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

import scala.io._

/*
  sbt "runMain com.lingeringsocket.shlurd.parser.ShlurdParseTester" < \
    src/test/resources/expect/babl.txt
 */
class ShlurdParseTester
{
  def run(source : Source) =
  {
    var successes = 0
    var failures = 0
    var lineNumber = 1
    var lastCheckpoint = 0

    def reportStatus()
    {
      println
      println("SUCCESSES:  " + successes)
      println("FAILURES:  " + failures)
    }

    val iter = source.getLines
    while (iter.hasNext) {
      val input = iter.next
      val cleaned = input.dropWhile(_.isDigit).split("\t")
      val sentence = cleaned.head
      val answer = cleaned.drop(1).headOption.getOrElse("")
      val result = processOne(sentence, answer)
      if (result.isEmpty) {
        successes += 1
      } else {
        failures += 1
        println
        println(s"LINE $lineNumber FAILED:  $input")
        println(s"LINE $lineNumber $result")
      }
      lineNumber += 1
      if (lineNumber - lastCheckpoint > 20) {
        reportStatus
        lastCheckpoint = lineNumber
      }
    }
    reportStatus
    (successes, failures)
  }

  protected def processOne(
    input : String, answer : String) : String =
  {
    val sentence = ShlurdParser(input).parseOne
    if (sentence.hasUnknown) {
      s"INCOMPLETE PARSE:  $sentence"
    } else {
      ""
    }
  }
}

object ShlurdParseTester extends App
{
  val tester = new ShlurdParseTester
  tester.run(Source.stdin)
}
