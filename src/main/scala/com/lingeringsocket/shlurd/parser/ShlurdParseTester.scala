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
    /home/jvs/Downloads/tasks_1-20_v1-2/en-valid/qa10_valid.txt
 */
object ShlurdParseTester extends App
{
  run()

  def run()
  {
    var successes = 0
    var failures = 0
    var done = false
    var lineNumber = 1
    var lastCheckpoint = 0

    def reportStatus()
    {
      println
      println("SUCCESSES:  " + successes)
      println("FAILURES:  " + failures)
    }

    while (!done) {
      val input = StdIn.readLine
      if (input == null) {
        done = true
      } else {
        val cleaned = input.dropWhile(_.isDigit)
        val sentence = ShlurdParser(cleaned).parseFirst
        if (sentence.hasUnknown) {
          failures += 1
          println(s"LINE $lineNumber FAILED:  $sentence")
        } else {
          successes += 1
        }
        lineNumber += 1
        if (lineNumber - lastCheckpoint > 20) {
          reportStatus
          lastCheckpoint = lineNumber
        }
      }
    }
    reportStatus
  }
}
