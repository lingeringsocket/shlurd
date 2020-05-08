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
import com.lingeringsocket.shlurd.mind._

class SpcResponseSpecification extends SpcProcessingSpecification
{
  abstract class ResponderContext(
    beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
    params : SmcResponseParams = SmcResponseParams(reportExceptionCodes = true)
  ) extends ProcessingContext
  {
    SpcPrimordial.initCosmos(cosmos)

    protected val responder =
      new SpcResponder(
        mind, SpcBeliefParams(beliefAcceptance), params)

    protected val responderWithoutPronouns =
      new SpcResponder(
        mind, SpcBeliefParams(beliefAcceptance), params.
          copy(thirdPersonPronouns = false))

    protected val responderTerse =
      new SpcResponder(
        mind, SpcBeliefParams(beliefAcceptance), params.
          copy(verbosity = RESPONSE_TERSE))

    protected val responderEllipsis =
      new SpcResponder(
        mind, SpcBeliefParams(beliefAcceptance), params.
          copy(verbosity = RESPONSE_ELLIPSIS))

    protected def process(input : String, expected : String) =
    {
      val parseResult = responder.newParser(input).parseOne
      s"pass:  $input" ==> (
        responder.process(parseResult, input) === expected)
    }

    protected def processExceptionExpected(
      input : String,
      message : String,
      code : ShlurdExceptionCode) =
    {
      val expected = s"$message\n\nFor more information see ${code.getUrl}"
      process(input, expected)
    }

    protected def processTerse(
      input : String,
      expected : String)
    {
      val parseResult = responderTerse.newParser(input).parseOne
      s"pass:  $input" ==> (
        responderTerse.process(parseResult, input) === expected)
    }

    protected def processBelief(input : String) =
    {
      process(input, "OK.")
    }

    protected def processWithResponder(
      specificResponder : SpcResponder,
      input : String) : String =
    {
      val parseResult = specificResponder.newParser(input).parseOne
      specificResponder.process(parseResult, input)
    }

    protected def processMatrix(
      input : String,
      expectedWithPronouns : String,
      expectedWithoutPronouns : String,
      expectedTerse : String,
      expectedEllipsis : String = "") =
    {
      processWithResponder(
        responder, input
      ) must be equalTo(expectedWithPronouns)
      processWithResponder(
        responderWithoutPronouns, input
      ) must be equalTo(expectedWithoutPronouns)
      processWithResponder(
        responderTerse, input
      ) must be equalTo(expectedTerse)
      if (!expectedEllipsis.isEmpty) {
        processWithResponder(
          responderEllipsis, input
        ) must be equalTo(expectedEllipsis)
      }
    }
  }
}
