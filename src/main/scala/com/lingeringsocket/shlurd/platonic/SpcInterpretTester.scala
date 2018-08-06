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
class SpcInterpretTester(beliefsFile : String) extends SprTester
{
  private val seedCosmos = new SpcCosmos
  SpcPrimordial.initCosmos(seedCosmos)
  seedCosmos.loadBeliefs(Source.fromFile(beliefsFile))

  private var cosmos = new SpcCosmos
  private var mind = new SpcMind(cosmos)

  override protected def restartSequence()
  {
    cosmos = new SpcCosmos
    cosmos.copyFrom(seedCosmos)
    mind = new SpcMind(cosmos)
    mind.startConversation
    mind.startNarrative
  }

  override protected def processOne(
    input : String, answer : String) =
  {
    val interpreter =
      new SpcInterpreter(
        mind, ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams().copy(verbosity = RESPONSE_TERSE))

    val sentence = SprParser(input).parseOne
    val response = interpreter.interpret(sentence, input)
    val expected = {
      if (answer.isEmpty) {
        "OK."
      } else if (answer == "none") {
        "No "
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
