// shlurd:  a limited understanding of small worlds
// Copyright 2017-2017 John V. Sichi
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

import scala.collection._

trait ShlurdParseUtils
{
  def hasLabel(tree : ShlurdSyntaxTree, label : String) : Boolean =
  {
    tree.hasLabel(label)
  }

  def isVerb(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("VB")
  }

  def isModal(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "MD") || (isVerb(pt) && hasTerminalLemma(pt, "do"))
  }

  def isPossessive(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "POS")
  }

  def isParticle(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "PRT") || hasLabel(pt, "RP") ||
      (hasLabel(pt, "PP") && (pt.numChildren == 1))
  }

  def isParticipleOrGerund(verbal : ShlurdSyntaxTree) : Boolean =
  {
    verbal.label match {
      case "VBG" | "VBN" => {
        true
      }
      case _ => {
        false
      }
    }
  }

  def isExistential(np : ShlurdSyntaxTree) : Boolean =
  {
    isNounPhrase(np) && hasLabel(np.firstChild, "EX")
  }

  def isNounPhrase(np : ShlurdSyntaxTree) : Boolean =
  {
    np.hasLabel("NP")
  }

  def isVerbPhrase(vp : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(vp, "VP")
  }

  def isPrepositionalPhrase(pp : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pp, "PP")
  }

  def isCompoundPrepositionalPhrase(pp : ShlurdSyntaxTree) : Boolean =
  {
    isPrepositionalPhrase(pp) && (pp.numChildren > 1)
  }

  def isNoun(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("NN")
  }

  def isPronoun(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("PRP")
  }

  def isAdjective(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("JJ")
  }

  def isAdjectival(pt : ShlurdSyntaxTree) : Boolean =
  {
    isAdjective(pt) || isParticipleOrGerund(pt)
  }

  def isAdverb(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("RB")
  }

  def isPreposition(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.label.startsWith("IN")
  }

  def isSubQuestion(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "SQ")
  }

  def isQueryNoun(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "WHNP")
  }

  def unwrapPhrase(pt : ShlurdSyntaxTree) : ShlurdSyntaxTree =
  {
    if (pt.isPrePreTerminal && (pt.numChildren == 1)) {
      pt.firstChild
    } else {
      pt
    }
  }

  def isDeterminer(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, "DT")
  }

  def isComma(pt : ShlurdSyntaxTree) : Boolean =
  {
    hasLabel(pt, ",")
  }

  def isCoordinatingConjunction(pt : ShlurdSyntaxTree) : Boolean =
  {
    pt.isCoordinatingConjunction
  }

  def hasTerminalLemma(tree : ShlurdSyntaxTree, lemma : String) =
  {
    tree.isPreTerminal && (tree.firstChild.lemma == lemma)
  }

  def hasTerminalLabel(
    tree : ShlurdSyntaxTree,
    label : String, terminalLabel : String) : Boolean =
  {
    tree.isPreTerminal && hasLabel(tree, label) &&
      hasLabel(tree.firstChild, terminalLabel)
  }

  def isImperative(children : Seq[ShlurdSyntaxTree]) =
  {
    (children.size == 1) && hasLabel(children.head, "VP")
  }

  def isBeing(verbHead : ShlurdSyntaxTree) =
  {
    isVerb(verbHead) &&
      (hasTerminalLemma(verbHead, "be") || hasTerminalLemma(verbHead, "exist"))
  }

  def isPossession(verbHead : ShlurdSyntaxTree) =
  {
    isVerb(verbHead) &&
      (hasTerminalLemma(verbHead, "have"))
  }

  def isRelationship(verbHead : ShlurdSyntaxTree) =
  {
    isBeing(verbHead) || isPossession(verbHead)
  }

  def isExists(verbHead : ShlurdSyntaxTree) =
  {
    isVerb(verbHead) && hasTerminalLemma(verbHead, "exist")
  }

  def isSinglePhrase(seq : Seq[ShlurdSyntaxTree]) : Boolean =
  {
    (seq.size == 1) && !seq.head.isPreTerminal
  }
}

object ShlurdParseUtils
{
  val WHO_LEMMA = "who"

  val PERSON_LEMMA = "person"

  def capitalize(s : String) = s.head.toUpper + s.tail

  def orderedSet[T](iterable : Iterable[T]) =
    (new mutable.LinkedHashSet[T] ++= iterable)
}
