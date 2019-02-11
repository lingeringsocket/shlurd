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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._

import net.sf.extjwnl.data._

import scala.collection.JavaConverters._

class SpcWordnet(cosmos : SpcCosmos)
{
  private val dictionary = ShlurdWordnet.dictionary

  private def debug(s : String)
  {
    // println(s)
  }

  def loadAll()
  {
    loadForms
    loadTaxonomy
  }

  def loadForms()
  {
    allSynsets.foreach(synset => {
      debug("SYNSET GLOSS = " + synset.getGloss)
      val words = synset.getWords.asScala.filter(
        word => isUsableFormName(word.getLemma))
      if (!words.isEmpty) {
        words.foreach(word => {
          debug("LEMMA = " + word.getLemma)
        })
        val form = cosmos.instantiateForm(SilWord(words.head.getLemma))
        words.tail.foreach(word => {
          cosmos.addIdealSynonym(word.getLemma, form.name)
        })
      } else {
        debug("UNUSABLE")
      }
      debug("")
    })
  }

  def loadTaxonomy()
  {
    allSynsets.foreach(hyponymSynset => {
      getSynsetForm(hyponymSynset) match {
        case Some(hyponymForm) => {
          val hypernyms = PointerUtils.getDirectHypernyms(hyponymSynset).asScala
          hypernyms.foreach(hypernym => {
            val hypernymSynset = hypernym.getSynset
            getSynsetForm(hypernymSynset) match {
              case Some(hypernymForm) => {
                cosmos.addIdealTaxonomy(hyponymForm, hypernymForm)
              }
              case _ =>
            }
          })
        }
        case _ =>
      }
    })
  }

  def getSynsetForm(synset : Synset) : Option[SpcForm] =
  {
    synset.getWords.asScala.toStream.map(_.getLemma).
      flatMap(cosmos.resolveForm).headOption
  }

  private def allSynsets() =
  {
    dictionary.getSynsetIterator(POS.NOUN).asScala
  }

  private def isUsableFormName(lemma : String) : Boolean =
  {
    // FIXME deal with multi-words, acronyms
    // FIXME deal with sense collisions
    ShlurdWordnet.isPlainWord(lemma) &&
      cosmos.resolveForm(lemma).isEmpty &&
      cosmos.resolveRole(lemma).isEmpty
  }
}
