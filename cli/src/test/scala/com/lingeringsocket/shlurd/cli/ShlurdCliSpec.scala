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

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._

import java.io._

class ShlurdCliSpec extends Specification
{
  "ShlurdCliSerializer" should
  {
    "serialize and deserialize" in
    {
      val cosmos = new SpcCosmos
      cosmos.getForms.size must be equalTo 0
      cosmos.instantiateForm(SilWord("dog"))
      cosmos.getForms.size must be equalTo 1
      val oldMind = new ShlurdCliMind(cosmos)
      val serializer = new ShlurdCliSerializer
      val file = File.createTempFile("testmind", ".kryo")
      try {
        serializer.save(oldMind, file)
        val newMind = serializer.load(file)
        newMind.getCosmos.getForms.size must be equalTo 1
      } finally {
        file.delete
      }
    }
  }
}
