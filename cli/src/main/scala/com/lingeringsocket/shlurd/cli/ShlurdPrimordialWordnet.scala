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

import com.lingeringsocket.shlurd.platonic._

import java.io._

object ShlurdPrimordialWordnet
{
  def loadCosmos() : SpcCosmos =
  {
    this.synchronized {
      val serializer = new ShlurdCliSerializer
      val file = new File("run/primordial-wordnet.zip")
      if (file.exists) {
        serializer.loadCosmos(file)
      } else {
        val cosmos = new SpcCosmos
        SpcPrimordial.initCosmos(cosmos)
        val wordnet = new SpcWordnet(cosmos)
        wordnet.loadAll
        serializer.saveCosmos(cosmos, file)
        cosmos
      }
    }
  }
}