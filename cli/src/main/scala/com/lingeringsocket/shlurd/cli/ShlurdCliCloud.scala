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

import net.sf.extjwnl.data._
import net.sf.extjwnl.data.mcr30.alignment._

import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.databind._

import java.net._

import scala.io._
import scala.collection._
import scala.util._

object ShlurdCliCloud
{
  private val wn30 = InterLingualIndex.getDictionary("wn30", "eng")

  private val babelnetKey = System.getenv("BABELNET_API_KEY")

  private val babelnetDefinitionUrl =
    "https://babelnet.org/synset?lang=EN"

  private val babelnetRestUrl =
    s"https://babelnet.io/v6/getSynset?key=${babelnetKey}"

  private def getSynset30(synset31 : Synset) : Synset =
  {
    InterLingualIndex.mapSynset(synset31, wn30)
  }

  private def getBabelnetId(synset31 : Synset) : String =
  {
    val synset30 = getSynset30(synset31)
    val offset30 = String.format("%08d", synset30.getOffset)
    s"wn:${offset30}n"
  }

  def getBabelnetDefinitionUrl(synset31 : Synset) : URL =
  {
    val id = getBabelnetId(synset31)
    new URL(s"${babelnetDefinitionUrl}&id=${id}")
  }

  def getBabelnetRestUrl(synset31 : Synset) : URL =
  {
    val id = getBabelnetId(synset31)
    new URL(s"${babelnetRestUrl}&id=${id}")
  }

  private def getBabelnetImages(
    synset31 : Synset, imageType : String) : List[URL] =
  {
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val restUrl = getBabelnetRestUrl(synset31)
    val json = Using.resource(Source.fromURL(restUrl)) {
      _.getLines().mkString("\n")
    }
    val map =
      mapper.readValue(json, classOf[Map[String, _]])
    val list = map.get("images").map(
      x => x.asInstanceOf[List[Map[String, _]]]).getOrElse(List.empty)
    list.filterNot {
      entry => {
        entry.get("badImage").map(_.asInstanceOf[Boolean]).getOrElse(false)
      }
    }.map(x => new URL(x(imageType).toString))
  }

  def getBabelnetThumbUrl(synset31 : Synset) : Option[URL] =
  {
    getBabelnetImages(synset31, "thumbUrl").headOption
  }

  def getBabelnetImageUrl(synset31 : Synset) : Option[URL] =
  {
    getBabelnetImages(synset31, "url").headOption
  }
}
