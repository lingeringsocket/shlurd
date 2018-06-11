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

import scala.io._

import java.io._

object ShlurdCliApp extends App
{
  val serializer = new ShlurdCliSerializer
  val file = new File("shlurd-mind.zip")
  val mind = loadOrCreate(file)
  val app = new ShlurdCliApp(mind, file, serializer)
  app.run

  private def loadOrCreate(file : File) : ShlurdCliMind =
  {
    if (file.exists) {
      println("SHLURD> Awaking from kryo...")
      val oldMind = serializer.load(file)
      println("SHLURD> Hello again, human!")
      oldMind
    } else {
      println("SHLURD> Loading initial beliefs...")
      val cosmos = new SpcCosmos
      SpcPrimordial.initCosmos(cosmos)
      val beliefs = ShlurdParser.getResourceFile("/ontologies/console.txt")
      val source = Source.fromFile(beliefs)
      cosmos.loadBeliefs(source)
      println("SHLURD> Hello, human!")
      new ShlurdCliMind(cosmos)
    }
  }
}

class ShlurdCliApp(
  mind : ShlurdCliMind,
  file : File,
  serializer : ShlurdCliSerializer)
{
  private val interpreter = new SpcInterpreter(mind, true)

  private def run()
  {
    var exit = false
    println
    while (!exit) {
      print("> ")
      val input = StdIn.readLine
      if (input == null) {
        exit = true
      } else {
        val sentences = ShlurdParser(input).parseAll
        sentences.foreach(sentence => {
          val output = interpreter.interpret(sentence)
          println
          print("SHLURD> ")
          println(output)
          println
        })
      }
    }
    println
    println("SHLURD> Shutting down...")
    serializer.save(mind, file)
  }
}
