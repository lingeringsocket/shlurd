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

sealed trait ShlurdExceptionCode
{
  def getUrl = s"https://undefined.com/exceptionCodes#${getFragmentIdentifier}"

  def getFragmentIdentifier =
    getClass.getName.stripSuffix("$").split("\\$").last
}

object ShlurdExceptionCode
{
  case object FailedParse extends ShlurdExceptionCode
  case object UnknownForm extends ShlurdExceptionCode
  case object UnknownState extends ShlurdExceptionCode
  case object NotUnique extends ShlurdExceptionCode
  case object UnresolvedPronoun extends ShlurdExceptionCode
}

case class ShlurdException(
  code : ShlurdExceptionCode,
  msg : String
) extends RuntimeException(msg)
{
}

