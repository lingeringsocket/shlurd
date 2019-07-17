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

import org.specs2.mutable._
import org.specs2.specification._

import scala.collection._
import scala.util._
import scala.io._

class SpcProcessingSpecification extends Specification
{
  trait ProcessingContext extends Scope
  {
    protected val cosmos = new SpcCosmos

    protected val mind = new SpcMind(cosmos)

    protected def process(
      input : String,
      beliefAcceptance : SpcBeliefAcceptance,
      params : SmcResponseParams =
        SmcResponseParams(verbosity = RESPONSE_TERSE),
      executor : SmcExecutor[SpcEntity] =
        new SmcExecutor[SpcEntity]) =
    {
      processBelief(
        input, SpcBeliefParams(beliefAcceptance), params, executor)
    }

    protected def processBelief(
      input : String,
      beliefParams : SpcBeliefParams,
      params : SmcResponseParams =
        SmcResponseParams(verbosity = RESPONSE_TERSE),
      executor : SmcExecutor[SpcEntity] =
        new SmcExecutor[SpcEntity]) =
    {
      val responder = new SpcResponder(
        mind,
        beliefParams,
        params,
        executor)
      val sentence = responder.newParser(input).parseOne
      responder.process(sentence)
    }

    protected def expectUnique(
      entities : Iterable[SpcEntity]) : SpcEntity =
    {
      entities.size must be equalTo(1)
      entities.head
    }

    protected def expectProperName(name : String) : SpcEntity =
    {
      expectUnique(
        cosmos.getEntities.filter(_.properName == name))
    }

    protected def expectNamedForm(name : String) =
    {
      val formOpt = cosmos.resolveForm(name)
      formOpt must beSome.which(_.name == name)
      formOpt.get
    }

    protected def expectNamedRole(form : SpcForm, name : String) =
    {
      val roleOpt = cosmos.resolveRole(form, name)
      roleOpt must beSome.which(_.name == name)
      roleOpt.get
    }

    protected def expectSingleProperty(form : SpcForm)
        : SpcProperty =
    {
      val properties = cosmos.getFormPropertyMap(form)
      properties.size must be equalTo 1
      properties.head._2
    }

    protected def expectFormSingleton(form : SpcForm) : SpcEntity =
    {
      expectUnique(
        cosmos.resolveQualifiedNoun(
          form.name, REF_SUBJECT, Set()))
    }

    protected def expectPerson(name : String) : SpcEntity =
    {
      expectNamedForm("person")
      expectUnique(cosmos.resolveQualifiedNoun(
        "person", REF_SUBJECT, Set(name)))
    }

    protected def expectUnique(
      entities : Try[Iterable[SpcEntity]]) : SpcEntity =
    {
      entities must beSuccessfulTry.which(_.size == 1)
      entities.get.head
    }

    protected def resolveForm(name : String) : SpcForm =
    {
      cosmos.resolveForm(name).get
    }

    protected def resolveRole(form : SpcForm, name : String) : SpcRole =
    {
      cosmos.resolveRole(form, name).get
    }

    protected def resolveGenitive(possessor : SpcEntity, roleName : String)
        : Set[SpcEntity] =
    {
      cosmos.resolveRole(possessor.form, roleName) match {
        case Some(role) => {
          cosmos.resolveGenitive(possessor, role)
        }
        case _ => Set.empty
      }
    }

    protected def loadBeliefs(resource : String)
    {
      val file = ResourceUtils.getResourceFile(resource)
      val source = Source.fromFile(file)
      mind.loadBeliefs(source)
    }
  }
}
