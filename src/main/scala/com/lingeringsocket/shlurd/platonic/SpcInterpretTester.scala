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
import com.lingeringsocket.shlurd.mind._

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
  private val seedMind = new SpcMind(seedCosmos)
  seedMind.loadBeliefs(Source.fromFile(beliefsFile))

  private var cosmos = new SpcCosmos
  private var mind = new SpcMind(cosmos)

  override protected def restartSequence()
  {
    cosmos = seedCosmos.newClone()
    mind = new SpcMind(cosmos)
    mind.startConversation
    mind.startNarrative
  }

  override protected def processOne(
    input : String, answer : String) =
  {
    val responder =
      new SpcResponder(
        mind,
        SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_TERSE))

    val sentence = responder.newParser(input).parseOne
    val response = responder.process(sentence, input)
    val expected = {
      if (answer.isEmpty) {
        Seq("OK.")
      } else if (answer == "none") {
        Seq("No ")
      } else {
        answer.split(",").toSeq
      }
    }
    if (!expected.forall(e => response.toLowerCase.contains(e.toLowerCase))) {
      s"INCORRECT RESPONSE:  $response"
    } else {
      if (false) {
        new java.io.PrintWriter("/tmp/graph.txt") { write(
          cosmos.getGraph.render(
            cosmos.getGraph.entityAssocs, "Entity assocs")); close
        }
      }
      ""
    }
  }
}

object SpcInterpretTester extends App
{
  val tester = new SpcInterpretTester(args.head)
  val (successes, failures) = tester.run(Source.stdin)
  if (failures != 0) {
    System.exit(1)
  }
}
