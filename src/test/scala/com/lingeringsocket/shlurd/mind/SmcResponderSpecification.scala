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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.ilang._

import org.specs2.mutable._
import org.specs2.specification._

class SmcResponderSpecification extends Specification
{
  type CosmosType = ZooCosmos
  type MindType = ZooMind
  type StateChangeInvocation = SmcStateChangeInvocation[SmcEntity]

  class ZooResponder(
    mind : ZooMind,
    params : SmcResponseParams,
    executor : SmcExecutor[SmcEntity],
    communicationContext : SmcCommunicationContext[SmcEntity]
  ) extends SmcResponder[
    SmcEntity, SmcProperty, CosmosType, MindType](
    mind, params, executor, communicationContext)
  {
    private implicit val tongue = mind.getTongue

    override protected def newPredicateEvaluator(
      annotator : AnnotatorType,
      scope : ScopeType) =
    {
      new SmcPredicateEvaluator[SmcEntity, SmcProperty, CosmosType, MindType](
        annotator,
        scope, params.existenceAssumption,
        communicationContext, debugger)
      {
        private def normalizeState(
          state : SilState) : SilState =
        {
          state match {
            case SilAdpositionalState(
              SprMagicAdposition(MW_IN),
              SilMandatorySingular(
                SilWordInflected("dreamland")
              )) =>
              {
                SilPropertyState(SilWord("asleep"))
              }
            case _ => state
          }
        }

        override protected def normalizePredicate(
          resultCollector : ResultCollectorType,
          predicate : SilPredicate
        ) = {
          predicate match {
            case SilStatePredicate(subject, verb, state, modifiers) => {
              SilStatePredicate(subject, verb, normalizeState(state), modifiers)
            }
            case _ => predicate
          }
        }
      }
    }
  }

  abstract class ResponderContext(
    responseParams : SmcResponseParams
  ) extends Scope
  {
    protected def getCosmos : ZooCosmos

    protected val mind = newMind

    protected def newMind = new ZooMind(getCosmos)

    protected val communicationContext = SmcCommunicationContext[SmcEntity](
      Some(ZooVisitor),
      Some(ZooKeeper)
    )

    protected def process(
      input : String,
      params : SmcResponseParams = responseParams) =
    {
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation,
          refMap : SmcRefMap[SmcEntity]) =
        {
          throw new RuntimeException("unexpected invocation")
        }
      }
      val responder =
        new ZooResponder(mind, params, executor, communicationContext)

      val parseResult = responder.newParser(input).parseOne
      responder.process(parseResult, input)
    }

    protected def processExceptionExpected(
      input : String,
      message : String,
      code : ShlurdExceptionCode) =
    {
      process(input) must be equalTo(
        s"$message\n\nFor more information see ${code.getUrl}")
    }

    protected def processCommandExpected(
      input : String,
      invocation : StateChangeInvocation) =
    {
      val ok = "OK."
      var actualInvocation : Option[StateChangeInvocation] = None
      val executor = new SmcExecutor[SmcEntity] {
        override def executeInvocation(
          invocation : StateChangeInvocation,
          refMap : SmcRefMap[SmcEntity]) =
        {
          actualInvocation = Some(invocation)
          Some(ok)
        }
      }
      val responder =
        new ZooResponder(
          mind, responseParams, executor,
          communicationContext)
      val parseResult = responder.newParser(input).parseOne
      responder.process(parseResult, input) must be equalTo(ok)
      actualInvocation must be equalTo(Some(invocation))
    }
  }
}
