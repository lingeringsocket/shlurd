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
package shlurd.parser

import org.specs2.mutable._

class ShlurdParserSpec extends Specification
{
  private val predDoorIsOpen =
    ShlurdStatePredicate(ShlurdFrontDoor,ShlurdDoorIsOpen)

  "ShlurdParser" should
  {
    "parse a statement" in
    {
      val input = "the door is open"
      ShlurdParser.parse(input) must be equalTo
        ShlurdPredicateStatement(predDoorIsOpen)
      ShlurdParser.parse(input + ".") must be equalTo
        ShlurdPredicateStatement(predDoorIsOpen)
      ShlurdParser.parse(input + "!") must be equalTo
        ShlurdPredicateStatement(predDoorIsOpen)
    }

    "parse a question" in
    {
      val input = "is the door open?"
      ShlurdParser.parse(input) must be equalTo
        ShlurdPredicateQuestion(predDoorIsOpen)
    }

    "parse a command" in
    {
      val input = "open the door"
      ShlurdParser.parse(input) must be equalTo
        ShlurdStateChangeCommand(predDoorIsOpen)
      ShlurdParser.parse(input + ".") must be equalTo
        ShlurdStateChangeCommand(predDoorIsOpen)
      ShlurdParser.parse(input + "!") must be equalTo
        ShlurdStateChangeCommand(predDoorIsOpen)
    }
  }
}
