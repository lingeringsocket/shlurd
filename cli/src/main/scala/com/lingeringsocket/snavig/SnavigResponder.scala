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
package com.lingeringsocket.snavig

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.collection._
import scala.util._

class SnavigResponder(
  propagationShell : Option[SnavigShell],
  mind : SnavigMind,
  beliefParams : SpcBeliefParams,
  params : SmcResponseParams,
  executor : SmcExecutor[SpcEntity],
  communicationContext : SmcCommunicationContext[SpcEntity])
    extends SpcResponder(mind, beliefParams, params,
      executor, communicationContext)
{
  import SnavigShell._

  override protected def publishBelief(belief : SpcBelief)
  {
    lazy val printed = sentencePrinter.print(belief.sentence)
    if (logger.isTraceEnabled) {
      logger.trace(s"BELIEF $printed")
    }
    propagationShell.foreach(_.deferPhenomenon(printed))
  }

  override protected def spawn(subMind : SpcMind) =
  {
    new SnavigResponder(
      propagationShell, subMind.asInstanceOf[SnavigMind],
      beliefParams.copy(acceptance = ACCEPT_MODIFIED_BELIEFS),
      params, executor, communicationContext)
  }

  override protected def checkCycle(
    predicate : SilPredicate,
    seen : mutable.Set[SilPredicate],
    referenceMap : Map[SilReference, Set[SpcEntity]],
    isPrecondition : Boolean) : Try[Boolean] =
  {
    if (logger.isTraceEnabled) {
      val printed = sentencePrinter.printPredicateStatement(
        predicate, SilTam.indicative)
      if (isPrecondition) {
        logger.trace(s"VERIFY $printed")
      } else {
        logger.trace(s"TRIGGER $printed")
      }
    }
    if (isPrecondition) {
      Success(false)
    } else {
      super.checkCycle(predicate, seen, referenceMap, isPrecondition)
    }
  }
}
