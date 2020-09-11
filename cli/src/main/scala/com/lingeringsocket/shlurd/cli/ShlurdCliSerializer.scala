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
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.platonic._

import com.twitter.chill.ScalaKryoInstantiator
import com.twitter.chill.TraversableSerializer
import com.esotericsoftware.kryo._
import com.esotericsoftware.kryo.io._

import scala.collection._

import java.io._
import java.util.zip._

object ShlurdCliSerializer
{
  val KRYO_ENTRY = "mind.kryo"

  val BELIEF_ENTRY = "beliefs.txt"

  val GML_ENTRY = "graphs.gml"

  private val graphMap = new mutable.LinkedHashMap[String, SpcGraph]

  private def saveGraph(name : String, graph : SpcGraph)
  {
    this.synchronized {
      graphMap.put(name, graph)
    }
  }

  private def accessGraph(name : String) : Option[SpcGraph] =
  {
    this.synchronized {
      graphMap.get(name)
    }
  }
}

class ShlurdCliSerializer
{
  import ShlurdCliSerializer._

  private val instantiator = new ScalaKryoInstantiator
  instantiator.setRegistrationRequired(false)

  protected val kryo = instantiator.newKryo

  private val graphSerializer = kryo.newDefaultSerializer(classOf[SpcGraph]).
    asInstanceOf[Serializer[SpcGraph]]

  // stackoverflow.com/questions/37869812/serialize-linked-hash-map-kryo
  kryo.register(
    classOf[mutable.LinkedHashMap[Any, Any]],
    new TraversableSerializer[
      (Any, Any),
      mutable.LinkedHashMap[Any, Any]](true))

  kryo.register(
    classOf[SpcGraph],
    new Serializer[SpcGraph]
      {
        def write(kser : Kryo, out : Output, graph : SpcGraph)
        {
          graph.name.foreach(graphName => {
            saveGraph(graphName, graph)
          })
          graphSerializer.write(kser, out, graph)
        }

        def read(kser : Kryo, in : Input, cls : Class[SpcGraph]) : SpcGraph =
        {
          val orig = graphSerializer.read(kser, in, cls)
          val graph = orig.baseName.map(baseName => {
            val base = accessGraph(baseName)
            base.map(orig.rebase).getOrElse(orig)
          }).getOrElse(orig)
          graph.name.foreach(graphName => {
            saveGraph(graphName, graph)
          })
          graph
        }
      }
  )

  def saveCosmos(cosmos : SpcCosmos, file : File)
  {
    file.getParentFile.mkdirs
    val zos = new ZipOutputStream(new FileOutputStream(file))
    try {
      saveEntry(zos, KRYO_ENTRY)(outputStream => {
        val output = new Output(outputStream)
        kryo.writeObject(output, cosmos)
        output.flush
      })
    } finally {
      zos.close
    }
  }

  def loadCosmos(file : File) : SpcCosmos =
  {
    val zis = new ZipInputStream(new FileInputStream(file))
    try {
      val nextEntry = zis.getNextEntry
      assert(nextEntry.getName == KRYO_ENTRY)
      val input = new Input(zis)
      kryo.readObject(input, classOf[SpcCosmos])
    } finally {
      zis.close
    }
  }

  def saveMind(mind : ShlurdCliMind, file : File)
  {
    file.getParentFile.mkdirs
    val zos = new ZipOutputStream(new FileOutputStream(file))
    try {
      saveEntry(zos, KRYO_ENTRY)(outputStream => {
        val output = new Output(outputStream)
        kryo.writeObject(output, mind)
        output.flush
      })
      saveEntry(zos, BELIEF_ENTRY)(outputStream => {
        val pw = new PrintWriter(outputStream)
        val creed = new SpcCreed(SpcAnnotator(), mind.getCosmos)
        val printer = new SilSentencePrinter(
          SnlUtils.defaultTongue, mind)
        creed.allBeliefs(printer).foreach(belief => {
          val beliefString = printer.print(belief)
          pw.println(SprUtils.capitalize(beliefString))
        })
        pw.flush
      })
      saveEntry(zos, GML_ENTRY)(outputStream => {
        val pw = new PrintWriter(outputStream)
        pw.println(mind.getCosmos.getGraph.render)
        pw.flush
      })
    } finally {
      zos.close
    }
  }

  protected def saveEntry(
    zos : ZipOutputStream,
    entry : String)(writeEntry : OutputStream => Unit)
  {
    zos.putNextEntry(new ZipEntry(entry))
    writeEntry(zos)
    zos.closeEntry
  }

  def loadMind(file : File) : ShlurdCliMind =
  {
    val zis = new ZipInputStream(new FileInputStream(file))
    try {
      val nextEntry = zis.getNextEntry
      assert(nextEntry.getName == KRYO_ENTRY)
      val input = new Input(zis)
      kryo.readObject(input, classOf[ShlurdCliMind])
    } finally {
      zis.close
    }
  }
}
