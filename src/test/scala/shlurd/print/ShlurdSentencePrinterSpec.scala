// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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
package shlurd.print

import org.specs2.mutable._

import shlurd.parser._

class ShlurdSentencePrinterSpec extends Specification
{
  private val printer = new ShlurdSentencePrinter

  private def normalize(s : String) =
    printer.print(ShlurdParser(s).parse)

  "ShlurdSentencePrinter" should
  {
    "normalize sentences" in
    {
      normalize("the door is closed") must be equalTo
        "the door is closed."
      normalize("is the door closed") must be equalTo
        "is the door closed?"
      normalize("close the door") must be equalTo
        "close the door."
      normalize("the chickens are fat") must be equalTo
        "the chickens are fat."
    }
  }
}
