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
package com.lingeringsocket.shlurd

import net.sf.extjwnl.data._
import net.sf.extjwnl.dictionary._

import scala.collection.JavaConverters._

object ShlurdWordnet
{
  val dictionary = Dictionary.getDefaultResourceInstance

  val morphology = dictionary.getMorphologicalProcessor

  def isTransitiveVerb(lemma : String) : Boolean =
  {
    if (lemma == "go") {
      return false
    }
    Option(dictionary.getIndexWord(POS.VERB, lemma)) match {
      case Some(indexWord) => {
        // FIXME this is totally arbitrary; but to get this right, seems like
        // we need to parse through the actual glosses since the verb frames
        // don't distinguish adposition objects from direct objects
        val senses = indexWord.getSenses.asScala.take(4)
        senses.exists(sense => {
          sense.getVerbFrames.exists(frame =>
            frame.contains("----s some")
          )
        })
      }
      case _ => true
    }
  }
}
