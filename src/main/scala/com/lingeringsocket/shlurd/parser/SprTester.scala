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

import com.lingeringsocket.shlurd._

import scala.io._
import scala.util._

trait ConsoleOutput
{
  def println(s : String = "")
}

object DefaultConsoleOutput extends ConsoleOutput
{
  override def println(s : String = "")
  {
    Console.println(s)
  }
}

object NullConsoleOutput extends ConsoleOutput
{
  override def println(s : String = "")
  {
  }
}

/*
  sbt "runMain com.lingeringsocket.shlurd.parser.SprTester" < \
    src/test/resources/expect/babi-unit-script.txt
 */
class SprTester
{
  def run(source : Source, target : ConsoleOutput = DefaultConsoleOutput) =
  {
    var successes = 0
    var failures = 0
    var lineNumber = 1
    var lastCheckpoint = 0
    var lastSeqNo = 0

    def reportStatus()
    {
      target.println()
      target.println("SUCCESSES:  " + successes)
      target.println("FAILURES:  " + failures)
    }

    restartSequence

    val iter = source.getLines
    while (iter.hasNext) {
      val input = iter.next
      val prefix = input.takeWhile(_.isDigit)
      val seqNo = Try(prefix.toInt).getOrElse(0)
      if (seqNo < lastSeqNo) {
        restartSequence
      }
      lastSeqNo = seqNo
      val cleaned = input.stripPrefix(prefix).trim.split("\t")
      val sentence = cleaned.head
      val answer = cleaned.drop(1).headOption.getOrElse("")
      val blurbs = Set("Yesterday", "This morning",
        "This afternoon", "This evening")
      val munged = {
        blurbs.find(blurb => sentence.startsWith(blurb + " ")) match {
          case Some(blurb) => {
            blurb + "," + sentence.stripPrefix(blurb)
          }
          case None => sentence
        }
      }
      val result = Try(processOne(munged, answer)) match {
        case Success(r) => r
        case Failure(e) => {
          target.println(s"LINE $lineNumber ERROR:  $input")
          throw e
        }
      }
      if (result.isEmpty) {
        successes += 1
      } else {
        failures += 1
        target.println()
        target.println(s"LINE $lineNumber FAILED:  $input")
        target.println(s"LINE $lineNumber $result")
      }
      lineNumber += 1
      if (lineNumber - lastCheckpoint > 20) {
        reportStatus
        lastCheckpoint = lineNumber
      }
    }
    reportStatus
    tupleN((successes, failures))
  }

  protected def restartSequence()
  {
  }

  protected def processOne(
    input : String, answer : String) : String =
  {
    val sentence = SprParser(input).parseOne
    if (sentence.hasUnknown) {
      s"INCOMPLETE PARSE:  $sentence"
    } else {
      ""
    }
  }
}

object SprTester extends App
{
  val tester = new SprTester
  tester.run(Source.stdin)
}
