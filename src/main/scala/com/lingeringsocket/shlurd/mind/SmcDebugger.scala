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
  def maybe(logger : Logger) : Option[SmcDebugger] =
  {
    if (logger.isDebugEnabled) {
      Some(new SmcDebugger(logger))
    } else {
      None
    }
  }
}

class SmcDebugger(logger : Logger)
{
  private var debugDepth = 0

  @inline final def debug(msg : => String)
  {
    val prefix = "*" * debugDepth
    logger.debug(prefix + msg)
  }

  @inline final def trace(msg : => String)
  {
    val prefix = "*" * debugDepth
    logger.trace(prefix + msg)
  }

  final def trace(msg : => String, t : Throwable)
  {
    val prefix = "*" * debugDepth
    logger.trace(prefix + msg, t)
  }

  final def debug(msg : => String, t : Throwable)
  {
    val prefix = "*" * debugDepth
    logger.debug(prefix + msg, t)
  }

  @inline final def pushLevel()
  {
    debugDepth += 1
  }

  @inline final def popLevel()
  {
    debugDepth -= 1
  }

  @inline final def isTraceEnabled() : Boolean =
  {
    logger.isTraceEnabled
  }
}

abstract class SmcDebuggable(protected val debugger : Option[SmcDebugger])
{
  @inline protected final def debug(msg : => String)
  {
    debugger.foreach(_.debug(msg))
  }

  @inline protected final def trace(msg : => String)
  {
    debugger.foreach(_.trace(msg))
  }

  protected final def debug(msg : => String, t : Throwable)
  {
    debugger.foreach(_.debug(msg, t))
  }

  protected final def trace(msg : => String, t : Throwable)
  {
    debugger.foreach(_.trace(msg, t))
  }

  @inline protected final def debugPushLevel()
  {
    debugger.foreach(_.pushLevel)
  }

  @inline protected final def debugPopLevel()
  {
    debugger.foreach(_.popLevel)
  }

  @inline protected final def isTraceEnabled() : Boolean =
  {
    debugger.map(_.isTraceEnabled).getOrElse(false)
  }
}
