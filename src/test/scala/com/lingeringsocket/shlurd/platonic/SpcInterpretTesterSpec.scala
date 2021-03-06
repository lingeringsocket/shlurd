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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import scala.io._
import scala.util._

class SpcInterpretTesterSpec extends Specification
{
  "SpcInterpretTester" should
  {
    "interpret babi format" in
    {
      val beliefs = ResourceUtils.getResourceFile(
        "/expect/babi-unit-beliefs.txt")
      val script = ResourceUtils.getResourceFile(
        "/expect/babi-unit-script.txt")
      val tester = new SpcInterpretTester(beliefs.getAbsolutePath)
      val (successes, failures) = Using.resource(Source.fromFile(script)) {
        source => tester.run(
          source,
          NullConsoleOutput)
      }
      successes must be equalTo 14
      failures must be equalTo 0
    }

    "understand babi qa beliefs" in
    {
      val beliefs = ResourceUtils.getResourceFile(
        "/expect/babi-qa-beliefs.txt")
      val script = ResourceUtils.getResourceFile(
        "/expect/babi-qa-script.txt")
      val tester = new SpcInterpretTester(beliefs.getAbsolutePath)
      val (successes, failures) =  Using.resource(Source.fromFile(script)) {
        source => tester.run(
          source,
          NullConsoleOutput)
      }
      successes must be equalTo 39
      failures must be equalTo 0
    }

    "understand babi qa wordnet beliefs" in
    {
      val beliefs = ResourceUtils.getResourceFile(
        "/expect/babi-qa-beliefs.txt")
      val script = ResourceUtils.getResourceFile(
        "/expect/babi-qa-wordnet-script.txt")
      val tester = new SpcInterpretTester(beliefs.getAbsolutePath)
      val (successes, failures) =  Using.resource(Source.fromFile(script)) {
        source => tester.run(
          source,
          NullConsoleOutput)
      }
      successes must be equalTo 10
      failures must be equalTo 0
    }
  }
}
