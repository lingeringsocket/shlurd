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
package com.lingeringsocket.shlurd.mind

import com.lingeringsocket.shlurd.nlang._

class SmcSpanishResponderSpec extends SmcResponderSpecification
{
  private val cosmos = new ZooCosmos(true)

  abstract class SpanishResponderContext(
    responseParams : SmcResponseParams =
      SmcResponseParams(
        thirdPersonPronouns = false,
        reportExceptionCodes = true)
  ) extends ResponderContext(responseParams)
  {
    override protected def getCosmos = cosmos

    override protected def newMind = new ZooMind(cosmos, new SnlSpanishTongue(
      new SnlExternalWordnet("/spanish_net.xml")))
  }

  "SmcResponder with SnlSpanishTongue" should
  {
    "process questions" in new SpanishResponderContext
    {
      process("hay un tigre") must be equalTo(
        "Claro, hay un tigre.")
      process("hay un tigre?") must be equalTo(
        "Sí, hay un tigre.")
      // FIXME in English the response specifies a single tiger
      process("hay tigres?") must be equalTo(
        "Sí, hay tigres.")
      process("hay algún tigre?") must be equalTo(
        "Sí, hay un tigre.")
      process("hay algunos tigres?") must be equalTo(
        "Sí, hay un tigre.")
      process("hay un oso?") must be equalTo(
        "Sí, hay un oso.")
      process("hay un oso polar?") must be equalTo(
        "Sí, hay un oso polar.")
      process("hay osos?") must be equalTo(
        "Sí, hay osos.")
      process("hay un oso triste?") must be equalTo(
        "No, no hay un oso triste.")
      process("hay una salamandra?") must be equalTo(
        "No, no hay una salamandra.")
      process("hay una cabra niñera?") must be equalTo(
        "Sí, hay una cabra niñera.")
    }
  }
}
