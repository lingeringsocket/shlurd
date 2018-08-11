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
import com.lingeringsocket.shlurd.platonic._

import com.twitter.chill.ScalaKryoInstantiator
import com.esotericsoftware.kryo.io._

import java.io._
import java.util.zip._

class ShlurdCliSerializer
{
  private val KRYO_ENTRY = "mind.kryo"

  private val BELIEF_ENTRY = "beliefs.txt"

  private val instantiator = new ScalaKryoInstantiator
  instantiator.setRegistrationRequired(false)
  private val kryo = instantiator.newKryo

  def save(mind : ShlurdCliMind, file : File)
  {
    val zos = new ZipOutputStream(new FileOutputStream(file))
    try {
      saveEntry(zos, KRYO_ENTRY)(outputStream => {
        val output = new Output(outputStream)
        kryo.writeObject(output, mind)
        output.flush
      })
      saveEntry(zos, BELIEF_ENTRY)(outputStream => {
        val pw = new PrintWriter(outputStream)
        val creed = new SpcCreed(mind.getCosmos)
        val printer = new SilSentencePrinter
        creed.allBeliefs.foreach(belief => {
          val beliefString = printer.print(belief)
          pw.println(SprUtils.capitalize(beliefString))
        })
        pw.flush
      })
    } finally {
      zos.close
    }
  }

  def saveEntry(
    zos : ZipOutputStream,
    entry : String)(writeEntry : OutputStream => Unit)
  {
    zos.putNextEntry(new ZipEntry(entry))
    writeEntry(zos)
    zos.closeEntry
  }

  def load(file : File) : ShlurdCliMind =
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
