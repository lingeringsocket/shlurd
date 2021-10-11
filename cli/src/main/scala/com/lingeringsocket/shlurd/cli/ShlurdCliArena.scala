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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.platonic._

import scala.io._
import scala.util._

import java.io._

import sys.process._

object ShlurdCliArena
{
  val SIZE = 10

  val CALVIN = "Calvin"

  val HOBBES = "Hobbes"

  val ordinateRange = range(0 until SIZE)

  val cellRange = ordinateRange.flatMap(
    x => ordinateRange.map(y => tupleN(x, y)))

  def cellName(x : Int, y : Int) : String =
  {
    s"arena-${x}-${y}-cell"
  }

  def createArena(mind : SpcMind) : Unit =
  {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    cellRange.foreach {
      case (y, x) => {
        val cell = cellName(x, y)
        pw.println(s"${cell} is an arena-cell.")
        if (y > 0) {
          val north = cellName(x, y - 1)
          pw.println(s"${north} is above ${cell}.")
        }
        if (x > 0) {
          val west = cellName(x - 1, y)
          pw.println(s"${west} is before ${cell}.")
        }
      }
    }
    pw.flush
    val source = Source.fromString(sw.toString)
    mind.loadBeliefs(source)
  }

  def dumpArena(mind : SpcMind) : String =
  {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    val cosmos = mind.getCosmos
    pw.println()
    ordinateRange.foreach(y => {
      ordinateRange.foreach(x => {
        val cell = cellName(x, y)
        cosmos.getEntityBySynonym(cell) match {
          case Some(entity) => {
            val role = cosmos.resolveRole(
              entity.form, SmcIdeals.ROLE_CONTAINEE, true).get
            val content = cosmos.resolveGenitive(entity, role)
            pw.print(content.headOption.map(_.name.head).getOrElse('.'))
          }
          case _ => {
            pw.print(' ')
          }
        }
      })
      pw.println()
    })
    pw.flush
    sw.toString
  }
}

class ShlurdCliArenaTerminal extends ShlurdCliTerminal
{
  import ShlurdCliArena._

  var clientName = CALVIN

  override protected def loadBeliefs(bootMind : SpcMind) : Unit =
  {
    super.loadBeliefs(bootMind)
    Using.resource(
      ResourceUtils.getResourceSource("/console/arena-beliefs.txt"))
    {
      source => bootMind.loadBeliefs(source)
    }
  }

  override def initialize(bootMind : SpcMind) : Unit =
  {
    loadBeliefs(bootMind)
    createArena(bootMind)
    emitControl("Welcome to the Calvinball Arena!")
  }

  override def beforeCommand(mind : ShlurdCliMind) : Unit =
  {
    val dump = dumpArena(mind)
    Using.resource(new PrintWriter("/home/jvs/tmp/dump.txt")) {
      pw => pw.println(dump)
    }
  }

  override def afterCommand(mind : ShlurdCliMind) : Unit =
  {
    clientName match {
      case CALVIN => {
        clientName = HOBBES
      }
      case _ => {
        clientName = CALVIN
      }
    }
    "/home/jvs/bin/shlurdArenaToggle".!!
  }

  override def emitPrompt() : Unit =
  {
    print(s"$clientName> ")
  }

  override def emitResponse(msg : String) : Unit =
  {
    println(s"[Calvinball] $msg")
  }
}

object ShlurdCliArenaApp extends ShlurdCliBaseApp
{
  override def newTerminal = new ShlurdCliArenaTerminal
}
