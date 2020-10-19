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
package com.lingeringsocket

package object shlurd
{
  val DQUOTE = "\""

  val DQUOTE_CHAR = '"'

  // syntactic sugar to make it easier to spot where we are
  // constructing a tuple (and to make Emacs indentation happier)
  def tupleN[TupleN <: Product](t : TupleN) = t

  def range(r : Range) = r

  def shellCommand(pb : scala.sys.process.ProcessBuilder) = pb

  // avoid the need for a do-nothing catch-all on match statements
  // that don't produce a result
  implicit class MatchPartial[T](x : T)
  {
    def matchPartial(f : PartialFunction[T, Unit]) : Unit =
    {
      if (f.isDefinedAt(x)) {
        f(x)
      }
    }
  }

  // catch-all as None
  implicit class MatchMaybe[T, U](x : T)
  {
    def matchMaybe(f : PartialFunction[T, U]) : Unit =
    {
      f.lift(x)
    }

    def matchOption(f : PartialFunction[T, Option[U]]) : Unit =
    {
      f.lift(x).flatten
    }
  }
}
