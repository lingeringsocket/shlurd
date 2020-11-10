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
package com.lingeringsocket.phlebotinum

import scala.collection._

object PhlebAliases
{
  val english = Map(
    "d" -> "go downward",
    "down" -> "go downward",
    "e" -> "go east",
    "east" -> "go east",
    "g" -> "again",
    "i" -> "inventory",
    "inventory" -> "what am I holding",
    "l" -> "look",
    "look" -> "reconnoiter",
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

  val spanish = Map(
    "d" -> "vaya al nadir",
    "abajo" -> "vaya al nadir",
    "baja" -> "vaya al nadir",
    "e" -> "este",
    "este" -> "vaya al este",
    "g" -> "otra vez",
    "i" -> "inventario",
    "inventario" -> "qué sujeto",
    "l" -> "mire",
    "m" -> "mire",
    "mire" -> "explore",
    "mira" -> "mire",
    // FIXME this can instead be "no" in context
    "n" -> "norte",
    "norte" -> "vaya al norte",
    "ne" -> "noreste",
    "noreste" -> "vaya al noreste",
    // FIXME "no" for "noroeste"?
    "nw" -> "noroeste",
    "noroeste" -> "vaya al noroeste",
    "o" -> "oeste",
    "q" -> "deje",
    "s" -> "sur",
    "sur" -> "vaya al sur",
    "se" -> "sudeste",
    "sudeste" -> "vaya al sudeste",
    "sw" -> "suroeste",
    "suroeste" -> "vaya al suroeste",
    "u" -> "vaya al cenit",
    "arriba" -> "vaya al cenit",
    "w" -> "oeste",
    "oeste" -> "vaya al oeste",
    "x" -> "estudie",
    "y" -> "sí",
    "z" -> "espere"
  )
}

