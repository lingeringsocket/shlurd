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
package com.lingeringsocket.shlurd.mind

import org.slf4j._

object SmcDebugger
{
  private val logger =
    LoggerFactory.getLogger(
      classOf[SmcDebugger])

  def maybe() : Option[SmcDebugger] =
  {
    if (logger.isDebugEnabled) {
      Some(new SmcDebugger)
    } else {
      None
    }
  }
}

class SmcDebugger
{
  import SmcDebugger._

  private var debugDepth = 0

  @inline final def debug(msg : => String)
  {
    val prefix = "*" * debugDepth
    logger.debug(prefix + msg)
  }

  final def debug(msg : => String, t : Throwable)
  {
    val prefix = "*" * debugDepth
    logger.error(prefix + msg, t)
  }

  @inline final def pushLevel()
  {
    debugDepth += 1
  }

  @inline final def popLevel()
  {
    debugDepth -= 1
  }
}

abstract class SmcDebuggable(protected val debugger : Option[SmcDebugger])
{
  @inline protected final def debug(msg : => String)
  {
    debugger.foreach(_.debug(msg))
  }

  protected final def debug(msg : => String, t : Throwable)
  {
    debugger.foreach(_.debug(msg, t))
  }

  @inline protected final def debugPushLevel()
  {
    debugger.foreach(_.pushLevel)
  }

  @inline protected final def debugPopLevel()
  {
    debugger.foreach(_.popLevel)
  }
}
