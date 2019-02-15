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
package com.lingeringsocket.shlurd.cli

import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._

import java.io._

class ShlurdCliSpec extends Specification
{
  class ExposedCosmos extends SpcCosmos
  {
    def newSingletonEntity(form : SpcForm) =
    {
      instantiateEntity(form, Seq.empty)._1
    }
  }

  "ShlurdCliSerializer" should
  {
    "serialize and deserialize" in
    {
      val cosmos = new ExposedCosmos
      cosmos.getForms.size must be equalTo 0
      val form = cosmos.instantiateForm(SilWord("dog"))
      cosmos.getForms.size must be greaterThan 0
      val entity = cosmos.newSingletonEntity(form)
      val oldMind = new ShlurdCliMind(cosmos, entity, entity)
      val serializer = new ShlurdCliSerializer
      val file = File.createTempFile("testmind", ".kryo")
      try {
        serializer.saveMind(oldMind, file)
        val newMind = serializer.loadMind(file)
        newMind.getCosmos.getForms.size must be greaterThan 0
        newMind.getCosmos.getEntities.size must be greaterThan 0
      } finally {
        file.delete
      }
    }
  }
}
