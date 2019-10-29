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
package com.lingeringsocket.arclamp

import scala.collection._

object ArcLampAliases
{
  val map = Map(
    "d" -> "go downward",
    "down" -> "go downward",
    "e" -> "go east",
    "east" -> "go east",
    "g" -> "again",
    "i" -> "inventory",
    "inventory" -> "what am I holding",
    "l" -> "look",
    // FIXME this can instead be "no" in context
    "n" -> "go north",
    "north" -> "go north",
    "ne" -> "go northeast",
    "northeast" -> "go northeast",
    "nw" -> "go northwest",
    "northwest" -> "go northwest",
    "o" -> "oops",
    "q" -> "quit",
    "s" -> "go south",
    "south" -> "go south",
    "se" -> "go southeast",
    "southeast" -> "go southeast",
    "sw" -> "go southwest",
    "southwest" -> "go southwest",
    "u" -> "go upward",
    "up" -> "go upward",
    "w" -> "go west",
    "west" -> "go west",
    "x" -> "examine",
    "y" -> "yes",
    "z" -> "wait"
  )
}

