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
package com.lingeringsocket.shlurd.nlang

import scala.util._
import scala.io._

import java.util.zip._
import java.io._

/*
 sbt "runMain com.lingeringsocket.shlurd.nlang.SnlSpanishPrep \
   /home/jvs/Downloads/es.freq"
 */
object SnlSpanishPrep extends App
{
  private val tongue = SnlUtils.spanishTongue

  private val wordnet = tongue.getWordnet

  run()

  def run() : Unit =
  {
    val freqFileIn = args.head
    val freqFileOut = "src/main/resources/spanish/freq.txt.gz"
    val dict = wordnet.getDictionary
    Using.resources(
      Source.fromFile(freqFileIn),
      new GZIPOutputStream(new FileOutputStream(freqFileOut))
    ) {
      (source, target) => {
        val pw = new PrintWriter(target)
        source.getLines().foreach(line => {
          val cols = line.trim.split(' ')
          val freq = cols(0)
          val lemma = cols(1)
          if (dict.lookupAllIndexWords(lemma).size > 0) {
            pw.println(s"$lemma $freq")
          }
        })
        pw.close
      }
    }
  }
}
