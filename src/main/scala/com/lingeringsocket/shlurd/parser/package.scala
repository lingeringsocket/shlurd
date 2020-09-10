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

package object parser
{
  import scala.language.implicitConversions

  implicit def magicToString(
    keyword : SprMagicWord)(implicit tongue : SprTongue) : String =
  {
    tongue.keywordLemma(keyword)
  }

  private val NEED_CONVERSION = "Convert magic word toLemma before comparing"

  def == (s : String, w : SprMagicWord) =
  {
    throw new IllegalArgumentException(NEED_CONVERSION)
  }

  def == (w : SprMagicWord, s : String) =
  {
    throw new IllegalArgumentException(NEED_CONVERSION)
  }

  def != (s : String, w : SprMagicWord) =
  {
    throw new IllegalArgumentException(NEED_CONVERSION)
  }

  def != (w : SprMagicWord, s : String) =
  {
    throw new IllegalArgumentException(NEED_CONVERSION)
  }
}