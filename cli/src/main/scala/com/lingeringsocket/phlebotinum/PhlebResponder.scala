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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.collection._
import scala.util._

class PhlebResponder(
  terminal : Option[PhlebTerminal],
  mind : PhlebMind,
  beliefParams : SpcBeliefParams,
  params : SmcResponseParams,
  executor : SmcExecutor[SpcEntity],
  communicationContext : SmcCommunicationContext[SpcEntity])
    extends SpcResponder(mind, beliefParams, params,
      executor, communicationContext)
{
  import PhlebShell._

  override protected def spawn(subMind : SpcMind) =
  {
    new PhlebResponder(
      terminal,
      subMind.asInstanceOf[PhlebMind],
      beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
      params, executor, communicationContext)
  }

  override protected def checkCycle(
    annotator : SpcAnnotator,
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    refMap : SpcRefMap,
    isPrecondition : Boolean) : Try[Boolean] =
  {
    if (logger.isTraceEnabled || terminal.exists(_.isDebugging)) {
      val printed = sentencePrinter.printPredicateStatement(
        predicate, SilTam.indicative)
      val formatted = {
        if (isPrecondition) {
          s"VERIFY $printed"
        } else {
          s"TRIGGER $printed"
        }
      }
      logger.trace(formatted)
      terminal.foreach(_.emitDebug(formatted))
    }
    if (isPrecondition) {
      Success(false)
    } else {
      super.checkCycle(annotator, predicate, seen, refMap, isPrecondition)
    }
  }
}
