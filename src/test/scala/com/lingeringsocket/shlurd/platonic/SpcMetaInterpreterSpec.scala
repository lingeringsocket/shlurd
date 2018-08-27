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

import org.specs2.mutable._
import org.specs2.specification._

class SpcMetaInterpreterSpec extends Specification
{
  class MetaInterpreterContext extends Scope
  {
    protected val cosmos = new SpcCosmos
    SpcPrimordial.initCosmos(cosmos)

    protected val mind = new SpcMind(cosmos)

    protected val interpreter =
      new SpcInterpreter(
        mind, ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams(verbosity = RESPONSE_TERSE))

    protected def interpret(input : String, expected : String) =
    {
      val sentence = SprParser(input).parseOne
      interpreter.interpret(sentence, input) must be equalTo(expected)
    }

    protected def interpretBelief(input : String) =
    {
      interpret(input, "OK.")
    }
  }

  "SpcMetaInterpreter" should
  {
    "get all meta" in new MetaInterpreterContext
    {
      interpretBelief("a form is an spc-form")
      interpretBelief("an entity is an spc-entity")
      interpretBelief("a type is an spc-type")
      interpretBelief("a realization is an spc-realization")
      interpretBelief("a property is an spc-property")
      interpretBelief("an attribute is an spc-attribute")
      interpretBelief("a value is an spc-value")
      interpretBelief("a propvalue is an spc-propvalue")
      interpretBelief("a pet's classification may be canine or feline")
      interpretBelief("Fido is a pet")
      interpretBelief("Fido's classification is canine")
      interpret("which properties are SPC-Form-pet's attributes",
        "SPC-Property-pet-classification.")
      interpret("which values are SPC-Property-pet-classification's propvalues",
        "SPC-Value-pet-classification-canine and " +
          "SPC-Value-pet-classification-feline.")
      interpret("which entities are SPC-Form-pet's realizations",
        "Fido.")
      interpret("which form is Fido's type",
        "SPC-Form-pet.")
    }
  }
}
