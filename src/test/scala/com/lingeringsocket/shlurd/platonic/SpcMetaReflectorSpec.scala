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

import org.specs2.mutable._
import org.specs2.specification._

class SpcMetaReflectorSpec extends Specification
{
  class MetaReflectorContext extends Scope
  {
    protected val cosmos = new SpcCosmos
    SpcPrimordial.initCosmos(cosmos)

    protected val mind = new SpcMind(cosmos)

    protected val responder =
      SpcResponder(
        mind, SpcBeliefParams(ACCEPT_MODIFIED_BELIEFS),
        SmcResponseParams(verbosity = RESPONSE_TERSE))

    protected def process(input : String, expected : String) =
    {
      val parseResult = responder.newParser(input).parseOne
      responder.process(parseResult, input).text must be equalTo(expected)
    }

    protected def processBelief(input : String) =
    {
      process(input, "OK.")
    }
  }

  "SpcMetaReflector" should
  {
    "get all meta" in new MetaReflectorContext
    {
      processBelief("a form is the same as an spc-form")
      processBelief("an entity is the same as an spc-entity")
      processBelief("a type is the same as an spc-type")
      processBelief("a form's realization is the same as an spc-realization")
      processBelief("a property is the same as an spc-property")
      processBelief("an spc-form's attribute is the same as an spc-attribute")
      processBelief("a value is the same as an spc-value")
      processBelief(
        "an spc-property's property-value is the same as an spc-property-value")
      processBelief("a pet's classification may be canine or feline")
      processBelief("Harry is a pet")
      processBelief("Harry's classification is canine")
      process("which properties are SPC-Form-pet's attributes",
        "SPC-Property-pet-classification.")
      process(
        "which values are SPC-Property-pet-classification's property-values",
        "SPC-Value-pet-classification-canine and " +
          "SPC-Value-pet-classification-feline.")
      process("which entities are SPC-Form-pet's realizations",
        "Harry.")
      process("which form is Harry's type",
        "SPC-Form-pet.")
    }
  }
}
