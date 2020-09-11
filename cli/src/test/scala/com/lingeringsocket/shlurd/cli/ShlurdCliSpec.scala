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

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.platonic._

import org.specs2.mutable._

import scala.io._

import java.io._

object ShlurdCliSpec
{
  def instantiateEntity(
    cosmos : SpcCosmos, form : SpcForm, name : String = "") =
  {
    cosmos.instantiateEntity(form, Seq.empty, name)._1
  }
}

class ShlurdCliSpec extends Specification
{
  import ShlurdCliSpec._

  "ShlurdCliSerializer" should
  {
    "serialize and deserialize minds" in
    {
      val cosmos = new SpcCosmos
      cosmos.getForms.size must be equalTo 0
      val form = cosmos.instantiateForm(SilWord("dog"))
      cosmos.getForms.size must be greaterThan 0
      instantiateEntity(cosmos, form)
      val oldMind = new ShlurdCliMind(cosmos)
      val serializer = new ShlurdCliSerializer
      val file = File.createTempFile("testmind", ".kryo")
      file.deleteOnExit
      serializer.saveMind(oldMind, file)
      val newMind = serializer.loadMind(file)
      newMind.getCosmos.getForms.size must be greaterThan 0
      newMind.getCosmos.getEntities.size must be greaterThan 0
    }

    "serialize and deserialize cosmos deltas" in
    {
      val cosmos = new SpcCosmos(SpcGraph("delta-test"))
      cosmos.getForms.size must be equalTo 0
      cosmos.getEntities.size must be equalTo 0
      val dogForm = cosmos.instantiateForm(SilWord("dog"))
      cosmos.getForms.size must be equalTo 1
      val humanForm = cosmos.instantiateForm(SilWord("human"))
      cosmos.getForms.size must be equalTo 2

      val ownerRole = cosmos.instantiateRole(dogForm, SilWord("owner"))
      cosmos.addIdealTaxonomy(ownerRole, humanForm)
      cosmos.getRoles.size must be equalTo 1

      cosmos.addFormAssoc(dogForm, ownerRole)

      val dog1 = instantiateEntity(cosmos, dogForm, "dog1")
      cosmos.getEntities.size must be equalTo 1

      val forkedCosmos = cosmos.fork(true)
      forkedCosmos.getForms.size must be equalTo 2
      forkedCosmos.getEntities.size must be equalTo 1
      forkedCosmos.getRoles.size must be equalTo 1
      val catForm = forkedCosmos.instantiateForm(SilWord("cat"))
      forkedCosmos.getForms.size must be equalTo 3
      instantiateEntity(forkedCosmos, dogForm, "dog2")
      instantiateEntity(forkedCosmos, catForm)
      forkedCosmos.getEntities.size must be equalTo 3

      val human = instantiateEntity(forkedCosmos, humanForm)
      forkedCosmos.getEntities.size must be equalTo 4
      forkedCosmos.addEntityAssoc(dog1, human, ownerRole)
      forkedCosmos.isEntityAssoc(dog1, human, ownerRole) must beTrue

      val serializer = new ShlurdCliSerializer
      val file = File.createTempFile("testcosmos", ".kryo")
      file.deleteOnExit
      val fileDelta = File.createTempFile("testdelta", ".kryo")
      fileDelta.deleteOnExit

      serializer.saveCosmos(cosmos, file)
      serializer.saveCosmos(forkedCosmos, fileDelta)

      val newCosmos = serializer.loadCosmos(file)
      newCosmos.getForms.size must be equalTo 2
      newCosmos.getEntities.size must be equalTo 1
      newCosmos.getRoles.size must be equalTo 1

      val newDelta = serializer.loadCosmos(fileDelta)
      newDelta.getForms.size must be equalTo 3
      newDelta.getEntities.size must be equalTo 4
      newDelta.getRoles.size must be equalTo 1

      newDelta.isHyponym(ownerRole, humanForm) must beTrue
      newDelta.isEntityAssoc(dog1, human, ownerRole) must beTrue
    }
  }

  "ShlurdCliApp" should
  {
    "interpret script" in
    {
      val script = Source.fromFile(
        ResourceUtils.getResourceFile("/expect/cli-script.txt")).getLines
      def nextScriptLine() : Option[String] = {
        if (!script.hasNext) {
          None
        } else {
          val s = script.next
          if (s.isEmpty) {
            nextScriptLine
          } else {
            Some(s)
          }
        }
      }
      val terminal = new ShlurdCliTerminal {
        override def emitPrompt()
        {
        }

        override def emitControl(msg : String)
        {
        }

        override def emitResponse(msg : String)
        {
          if (!msg.isEmpty) {
            nextScriptLine match {
              case Some(expected) => {
                msg must be equalTo expected
              }
              case _ => {
                msg must be equalTo "EOF"
              }
            }
          }
        }

        override def readCommand() : Option[String] =
        {
          nextScriptLine match {
            case Some(cmd) => {
              cmd must startWith("> ")
              Some(cmd.stripPrefix("> "))
            }
            case _ => None
          }
        }
      }
      val mind = ShlurdCliShell.newMind(terminal)

      // we no longer automatically load associations, so do it here
      // so we can keep the corresponding tests working
      new SpcWordnetOntology(
        SnlPrincetonWordnet, mind.getCosmos).loadAllAssociations

      val shell = new ShlurdCliShell(mind, terminal)
      shell.run
      nextScriptLine must beEmpty
    }
  }
}
