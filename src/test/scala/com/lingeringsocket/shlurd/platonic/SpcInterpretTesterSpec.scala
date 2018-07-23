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

import com.lingeringsocket.shlurd.parser._

import org.specs2.mutable._

import scala.io._

class SpcInterpretTesterSpec extends Specification
{
  "SpcInterpretTester" should
  {
    "interpret babl format" in
    {
      val beliefs = ShlurdParser.getResourceFile(
        "/expect/babl-unit-beliefs.txt")
      val script = ShlurdParser.getResourceFile(
        "/expect/babl-unit-script.txt")
      Console.withOut(new java.io.ByteArrayOutputStream) {
        val tester = new SpcInterpretTester(beliefs.getAbsolutePath)
        val (successes, failures) = tester.run(Source.fromFile(script))
        successes must be equalTo 13
        failures must be equalTo 0
      }
    }
  }
}
