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

  def loadAll()
  {
    loadAllForms
    loadAllTaxonomy
    loadAllAssociations
  }

  def loadAllForms()
  {
    ShlurdWordnet.allNounSenses.foreach(loadForm)
  }

  def loadForm(sense : Synset) : Option[SpcForm] =
  {
    getSynsetForm(sense).orElse {
      val words = sense.getWords.asScala.filter(
        word => isUsableFormName(word.getLemma))
      if (!words.isEmpty) {
        val form = cosmos.instantiateForm(SilWord(getIdealName(words.head)))
        words.tail.foreach(word => {
          cosmos.addIdealSynonym(getIdealName(word), form.name)
        })
        Some(form)
      } else {
        None
      }
    }
  }

  def loadAllTaxonomy()
  {
    ShlurdWordnet.allNounSenses.foreach(loadDirectHypernyms)
  }

  def loadAllAssociations()
  {
    ShlurdWordnet.allNounSenses.foreach(loadMeronyms)
  }

  def loadDirectHypernyms(hyponymSynset : Synset) : Seq[SpcForm] =
  {
    loadForm(hyponymSynset) match {
      case Some(hyponymForm) => {
        val hypernyms = PointerUtils.getDirectHypernyms(hyponymSynset).asScala
        hypernyms.flatMap(hypernym => {
          val hypernymSynset = hypernym.getSynset
          loadForm(hypernymSynset) match {
            case Some(hypernymForm) => {
              cosmos.addIdealTaxonomy(hyponymForm, hypernymForm)
              Some(hypernymForm)
            }
            case _ => None
          }
        })
      }
      case _ => Seq.empty
    }
  }

  def loadMeronyms(sense : Synset) : Seq[SpcRole] =
  {
    loadForm(sense) match {
      case Some(form) => {
        val meronyms = PointerUtils.getMeronyms(sense).asScala
        meronyms.flatMap(meronym => {
          val meronymSynset = meronym.getSynset
          loadForm(meronymSynset) match {
            case Some(meronymForm) => {
              val word = SilWord(
                s"wnr-${form.name}-${meronymForm.name}")
              val role = cosmos.instantiateRole(word)
              cosmos.addIdealTaxonomy(role, meronymForm)
              cosmos.addFormAssoc(form, role)
              Some(role)
            }
            case _ => None
          }
        })
      }
      case _ => Seq.empty
    }
  }

  def getSynsetForm(synset : Synset) : Option[SpcForm] =
  {
    synset.getWords.asScala.toStream.map(getIdealName).
      flatMap(cosmos.resolveForm).headOption
  }

  private def getIdealName(word : Word) : String =
  {
    s"wnf-${word.getLemma}-${word.getSenseNumber}"
  }

  private def isUsableFormName(lemma : String) : Boolean =
  {
    // FIXME deal with multi-words, acronyms
    ShlurdWordnet.isPlainWord(lemma)
  }
}
