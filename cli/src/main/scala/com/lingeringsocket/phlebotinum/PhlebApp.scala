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
package com.lingeringsocket.phlebotinum

import com.lingeringsocket.shlurd._

import java.net._

object PhlebApp extends App
{
  private val GENERIC_SPANISH = "Generic-Spanish"

  private val SPANISH_FLAG = s"--tongue=$GENERIC_SPANISH"

  run()

  private def run() : Unit =
  {
    // preload
    PhlebBaseline.frozenCosmos

    val spanish = args.contains(SPANISH_FLAG)

    val reducedArgs = args.filterNot(_ == SPANISH_FLAG)

    val translatorOpt = {
      if (spanish) {
        Some(PhlebSpanishTranslator)
      } else {
        None
      }
    }

    if (reducedArgs.isEmpty) {
      PhlebShell.run("/example-phlebotinum/")
    } else {
      val url = new URL(reducedArgs.head)
      val terminal = new PhlebConsole
      {
        override def getInitSaveFile =
        {
          if (url.getProtocol == "file") {
            ""
          } else {
            url.getPath.stripPrefix("/") + super.getInitSaveFile
          }
        }

        override def getTranslatorOpt =
        {
          translatorOpt
        }

        override def getInitCommandOpt : Option[String] =
        {
          if (spanish) {
            Some(
              s"the player-character's expected-language is $GENERIC_SPANISH")
          } else {
            super.getInitCommandOpt
          }

        }
      }
      ResourceUtils.addUrl(url)
      if (verifyResource("base-axioms.txt") &&
        verifyResource("game-init.txt"))
      {
        PhlebShell.run("/", terminal)
      }
    }
  }

  private def verifyResource(resourceName : String) : Boolean =
  {
    val streamOpt = Option(ResourceUtils.getResourceStream(s"/${resourceName}"))
    if (streamOpt.isEmpty) {
      println(s"Invalid story location (missing ${resourceName})")
      false
    } else {
      streamOpt.foreach(stream => stream.close)
      true
    }
  }
}
