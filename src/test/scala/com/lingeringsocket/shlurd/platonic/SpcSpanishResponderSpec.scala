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

import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.nlang._

class SpcSpanishResponderSpec extends SpcResponseSpecification
{
  abstract class SpanishResponderContext(
    beliefAcceptance : SpcBeliefAcceptance = ACCEPT_NO_BELIEFS,
    params : SmcResponseParams = SmcResponseParams(reportExceptionCodes = true)
  ) extends ResponderContext(beliefAcceptance, params)
  {
    override protected def getProcessingTongue = SnlUtils.spanishTongue
  }

  "SpcResponder with SnlSpanishTongue" should
  {
    "recognize beliefs" in new SpanishResponderContext(ACCEPT_NEW_BELIEFS)
    {
      processBelief("perros y gatos son tipos de animal")
      processBelief("hay un perro")
      processTerse("hay un perro?", "Sí.")
      processTerse("hay un gato?", "No.")
    }
  }
}
