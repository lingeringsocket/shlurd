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
package com.lingeringsocket.shlurd.parser

import com.lingeringsocket.shlurd.ilang._

import scala.collection._

object SprUtils
{
  def capitalize(s : String) = s"${s.head.toUpper}${s.tail}"

  def orderedSet[T](iterable : Iterable[T]) =
    (new mutable.LinkedHashSet[T] ++= iterable)

  def requireUnique(seq : Seq[SprSyntaxTree]) : SprSyntaxTree =
  {
    assert(seq.size == 1, seq)
    seq.head
  }

  def requireLeaf(seq : Seq[SprSyntaxTree]) : SprSyntaxLeaf =
  {
    requireUnique(seq) match {
      case leaf : SprSyntaxLeaf => leaf
      case nonLeaf => {
        throw new IllegalArgumentException("leaf expected but got " + nonLeaf)
      }
    }
  }

  def maybeSyntaxTree(phrase : SilPhrase) : Option[SprSyntaxTree] =
  {
    phrase.maybeSyntaxTree.map(_.asInstanceOf[SprSyntaxTree])
  }
}

sealed trait SprRelationshipPredef
{
  def toLemma(implicit tongue : SprTongue) : String =
    tongue.getRelPredefLemma(this)
  def toVerb(implicit tongue : SprTongue) = SilWord(toLemma)
}
case object REL_PREDEF_IDENTITY extends SprRelationshipPredef
case object REL_PREDEF_BECOME extends SprRelationshipPredef
case object REL_PREDEF_ASSOC extends SprRelationshipPredef

object SprRelationshipPredef
{
  val enumeration = Seq(
    REL_PREDEF_IDENTITY, REL_PREDEF_BECOME, REL_PREDEF_ASSOC)

  def apply(word : SilWord)(implicit tongue : SprTongue) =
  {
    tongue.relLemmaMap.get(word.toLemma).getOrElse {
      REL_PREDEF_IDENTITY
    }
  }
}

object SprRelationshipPredefVerb
{
  def unapply(w : SilSimpleWord)(implicit tongue : SprTongue) =
  {
    Some(SprRelationshipPredef(w))
  }
}

sealed trait SprStatePredef
{
  def toLemma(implicit tongue : SprTongue) : String =
    tongue.getStatePredefLemma(this)
  def toVerb(implicit tongue : SprTongue) = SilWord(toLemma)
}

case object STATE_PREDEF_BE extends SprStatePredef
case object STATE_PREDEF_BECOME extends SprStatePredef

object SprStatePredef
{
  def apply(word : SilWord)(implicit tongue : SprTongue) =
  {
    tongue.getStatePredefFromLemma(word.toLemma)
  }
}

object SprStatePredefVerb
{
  def unapply(w : SilSimpleWord)(implicit tongue : SprTongue) =
  {
    Some(SprStatePredef(w))
  }
}
