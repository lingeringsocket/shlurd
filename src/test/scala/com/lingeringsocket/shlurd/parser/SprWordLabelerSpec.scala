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
package com.lingeringsocket.shlurd.parser

import SprPennTreebankLabels._

import org.specs2.mutable._

import scala.collection._

class SprWordLabelerSpec extends Specification
{
  private val labeler = new SprWordnetLabeler

  private def labelWord(token : String) : Set[String] =
  {
    labeler.labelWord(token, token, 1).map(_.label)
  }

  "SprWordnetLabeler" should
  {
    "label words correctly" in
    {
      labelWord("cow") must be equalTo Set(LABEL_NN, LABEL_VBP)
      labelWord("cows") must be equalTo Set(LABEL_NNS, LABEL_VBZ)
      labelWord("ox") must be equalTo Set(LABEL_NN)
      labelWord("oxen") must be equalTo Set(LABEL_NNS)
      labelWord("people") must be equalTo Set(LABEL_NNS, LABEL_VBP)
      labelWord("agenda") must be equalTo Set(LABEL_NN)
      labelWord("cattle") must be equalTo Set(LABEL_NNS)
      labelWord("mice") must be equalTo Set(LABEL_NNS)
    }
  }
}
