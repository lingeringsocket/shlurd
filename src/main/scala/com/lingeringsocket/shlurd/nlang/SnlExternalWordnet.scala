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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

import net.sf.extjwnl.dictionary._
import net.sf.extjwnl.data._

import scala.jdk.CollectionConverters._

class SnlExternalWordnet(
  propertiesPath : String
) extends SprWordnet
{
  private val dictionary = Dictionary.getInstance(
    ResourceUtils.getResourceStream(propertiesPath))

  private val morphology = dictionary.getMorphologicalProcessor

  override def getDictionary = dictionary

  override def getMorphology = morphology
  override def getSortedSenses(indexWord : IndexWord) =
  {
    // needed in case sense numbers got reshuffled by ewn
    val senses = indexWord.getSenses.asScala
    val offsets = indexWord.getSynsetOffsets.toSeq
    senses.zip(offsets).sortBy(_._2).map(_._1)
  }
}
