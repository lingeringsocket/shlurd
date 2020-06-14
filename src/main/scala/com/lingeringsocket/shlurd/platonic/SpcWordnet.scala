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
import com.lingeringsocket.shlurd.mind._
import com.lingeringsocket.shlurd.ilang._

import net.sf.extjwnl.data._

import scala.collection._
import scala.collection.JavaConverters._

import java.util.regex._

object SpcWordnet
{
  private val usablePattern = Pattern.compile("[ \\p{javaLowerCase}]+")

  private val someoneCategories = Seq(
    "person"
  )

  private val objectCategories = Seq(
    "animal", "artifact", "body", "food", "group",
    "location", "object", "person", "plant",
    "phenomenon", "possession", "shape", "substance"
  )

  def getNoun(form : SpcForm) : String =
  {
    getNoun(form.name)
  }

  def getNoun(formName : String) : String =
  {
    if (formName.startsWith("wnf-")) {
      formName.split("wnf-").last.split('-').head
    } else {
      formName.stripPrefix("spc-")
    }
  }

  def getPossesseeNoun(role : SpcRole) : String =
  {
    if (role.name.startsWith("wnr-")) {
      role.name.stripPrefix("wnr-").split('-').head
    } else {
      role.name.stripPrefix("spc-")
    }
  }

  def getRoleName(
    possesseeForm : SpcForm) : String =
  {
    val stripped = possesseeForm.name.stripPrefix("wnf-")
    s"wnr-${stripped}"
  }

  private def isUsableFormName(lemma : String) : Boolean =
  {
    // FIXME deal with acronyms etc
    SpcWordnet.usablePattern.matcher(lemma).matches
  }
}

class SpcWordnet(cosmos : SpcCosmos)
{
  import SpcWordnet._

  def loadAll()
  {
    loadAllForms
    loadAllTaxonomy
    // not clear that these are terribly useful
    if (false) {
      loadAllAssociations
    }
    loadBasicGenders
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
        val form = cosmos.instantiateForm(SilWord(getFormName(words.head)))
        words.tail.foreach(word => {
          cosmos.addIdealSynonym(getFormName(word), form.name)
        })
        Some(form)
      } else {
        None
      }
    }
  }

  def loadAllTaxonomy()
  {
    ShlurdWordnet.allNounSenses.foreach(loadDirectHypernyms(_, true))
  }

  def loadAllAssociations()
  {
    ShlurdWordnet.allNounSenses.foreach(loadMeronyms)
  }

  def loadGender(biological : String, grammatical : String)
  {
    val grammaticalForm =
      ShlurdWordnet.getNounSenses(grammatical).flatMap(loadForm).head
    val grammaticalEntityName = SpcMeta.formMetaEntityName(grammaticalForm)
    cosmos.getEntityBySynonym(grammaticalEntityName).foreach(
      grammaticalEntity => {
        ShlurdWordnet.getNounSenses(biological).flatMap(loadForm).foreach(
          biologicalForm => {
            val biologicalEntityName =
              SpcMeta.formMetaEntityName(biologicalForm)
            cosmos.getEntityBySynonym(biologicalEntityName).foreach(
              biologicalEntity => {
                cosmos.getGenderRole(biologicalForm).foreach(genderRole => {
                  cosmos.addEntityAssocEdge(
                    biologicalEntity, grammaticalEntity, genderRole)
                })
              }
            )
          }
        )
      }
    )
  }

  def loadBasicGenders()
  {
    loadGender("female", "feminine")
    loadGender("male", "masculine")
  }

  def loadDirectHypernyms(
    hyponymSynset : Synset, includeImplicit : Boolean) : Seq[SpcForm] =
  {
    val (someoneForm, objectForm) = {
      if (includeImplicit) {
        tupleN((
          cosmos.resolveForm(SmcLemmas.LEMMA_SOMEONE),
          cosmos.resolveForm(SmcLemmas.LEMMA_OBJECT)))
      } else {
        tupleN((None, None))
      }
    }
    loadForm(hyponymSynset) match {
      case Some(hyponymForm) => {
        someoneForm.foreach(hypernymForm => {
          if (anyMatchingCategory(
            hyponymForm, hyponymSynset, someoneCategories)
          ) {
            cosmos.addIdealTaxonomy(hyponymForm, hypernymForm)
          }
        })
        objectForm.foreach(hypernymForm => {
          if (anyMatchingCategory(
            hyponymForm, hyponymSynset, objectCategories)
          ) {
            cosmos.addIdealTaxonomy(hyponymForm, hypernymForm)
          }
        })
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
      case Some(holonymForm) => {
        val meronyms = PointerUtils.getMeronyms(sense).asScala
        meronyms.flatMap(meronym => {
          val meronymSynset = meronym.getSynset
          loadForm(meronymSynset) match {
            case Some(meronymForm) => {
              val meronymRole = cosmos.instantiateRole(
                holonymForm,
                SilWord(getRoleName(meronymForm)))
              cosmos.addIdealTaxonomy(meronymRole, meronymForm)
              val edge = cosmos.addFormAssoc(holonymForm, meronymRole)
              val holonymRole = cosmos.instantiateRole(
                meronymForm,
                SilWord(getRoleName(holonymForm)))
              cosmos.addIdealTaxonomy(holonymRole, holonymForm)
              val inverseEdge = cosmos.addFormAssoc(meronymForm, holonymRole)
              val constraint = SpcCardinalityConstraint(0, 1)
              cosmos.annotateFormAssoc(inverseEdge, constraint)
              cosmos.connectInverseAssocEdges(edge, inverseEdge)
              Some(meronymRole)
            }
            case _ => None
          }
        })
      }
      case _ => Seq.empty
    }
  }

  def anyMatchingCategory(
    form : SpcForm, sense : Synset, categories : Seq[String]) : Boolean =
  {
    categories.contains(sense.getLexFileName.stripPrefix("noun.")) ||
      anyMatchingHypernym(
        form, categories.flatMap(
          category => ShlurdWordnet.getNounSenses(category).
            take(1).flatMap(loadForm)))
  }

  def anyMatchingHypernym(form : SpcForm, hypernyms : Seq[SpcForm]) : Boolean =
  {
    hypernyms.exists(
      hypernym => cosmos.isHyponym(form, hypernym))
  }

  def getSynsetForm(synset : Synset) : Option[SpcForm] =
  {
    synset.getWords.asScala.toStream.map(getFormName).
      flatMap(cosmos.resolveForm).headOption
  }

  def getFormName(word : Word) : String =
  {
    val encoded = cosmos.encodeName(word.getLemma)
    s"wnf-${encoded}-${word.getSenseNumber}"
  }
}
