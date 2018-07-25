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
import com.lingeringsocket.shlurd.cosmos._

import scala.io._

/*
  sbt "runMain com.lingeringsocket.shlurd.platonic.SpcInterpretTester \
       src/test/resources/expect/babi-unit-beliefs.txt" \
    < src/test/resources/expect/babi-unit-script.txt
 */
class SpcInterpretTester(beliefsFile : String) extends ShlurdParseTester
{
  private val cosmos = new SpcCosmos
  SpcPrimordial.initCosmos(cosmos)
  cosmos.loadBeliefs(Source.fromFile(beliefsFile))

  private val interpreter =
    new SpcInterpreter(
      new SpcMind(cosmos), ACCEPT_MODIFIED_BELIEFS,
      ShlurdResponseParams().copy(verbosity = RESPONSE_TERSE))

  override protected def processOne(
    input : String, answer : String) =
  {
    val sentence = ShlurdParser(input).parseOne
    val response = interpreter.interpret(sentence)
    val expected = {
      if (answer.isEmpty) {
        "OK."
      } else {
        answer
      }
    }
    if (!response.toLowerCase.contains(expected.toLowerCase)) {
      s"INCORRECT RESPONSE:  $response"
    } else {
      ""
    }
  }
}

object SpcInterpretTester extends App
{
  val tester = new SpcInterpretTester(args.head)
  tester.run(Source.stdin)
}
