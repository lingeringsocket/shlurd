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

import edu.stanford.nlp.trees._

trait ShlurdParseUtils
{
  def getLabel(tree : Tree) : String =
  {
    tree.label.value.split("-").head
  }

  def hasLabel(tree : Tree, label : String) : Boolean =
  {
    getLabel(tree) == label
  }

  def isVerb(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("VB")
  }

  def isModal(pt : Tree) : Boolean =
  {
    hasLabel(pt, "MD")
  }

  def isParticle(pt : Tree) : Boolean =
  {
    hasLabel(pt, "PRT") || hasLabel(pt, "RP") ||
      (hasLabel(pt, "PP") && (pt.numChildren == 1))
  }

  def isParticipleOrGerund(verbal : Tree) : Boolean =
  {
    getLabel(verbal) match {
      case "VBG" | "VBN" => {
        true
      }
      case _ => {
        false
      }
    }
  }

  def isExistential(np : Tree) : Boolean =
  {
    isNounPhrase(np) && hasLabel(np.firstChild, "EX")
  }

  def isNounPhrase(np : Tree) : Boolean =
  {
    hasLabel(np, "NP")
  }

  def isVerbPhrase(vp : Tree) : Boolean =
  {
    hasLabel(vp, "VP")
  }

  def isPrepositionalPhrase(pp : Tree) : Boolean =
  {
    hasLabel(pp, "PP")
  }

  def isCompoundPrepositionalPhrase(pp : Tree) : Boolean =
  {
    isPrepositionalPhrase(pp) && (pp.numChildren > 1)
  }

  def isNoun(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("NN")
  }

  def isPronoun(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("PRP")
  }

  def isAdjective(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("JJ")
  }

  def isAdjectival(pt : Tree) : Boolean =
  {
    isAdjective(pt) || isParticipleOrGerund(pt)
  }

  def isAdverb(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("RB")
  }

  def isPreposition(pt : Tree) : Boolean =
  {
    getLabel(pt).startsWith("IN")
  }

  def isSubQuestion(pt : Tree) : Boolean =
  {
    hasLabel(pt, "SQ")
  }

  def isQueryNoun(pt : Tree) : Boolean =
  {
    hasLabel(pt, "WHNP")
  }

  def unwrapPhrase(pt : Tree) : Tree =
  {
    if (pt.isPrePreTerminal && (pt.numChildren == 1)) {
      pt.firstChild
    } else {
      pt
    }
  }

  def isDeterminer(pt : Tree) : Boolean =
  {
    hasLabel(pt, "DT")
  }

  def isComma(pt : Tree) : Boolean =
  {
    hasLabel(pt, ",")
  }

  def isCoordinatingConjunction(pt : Tree) : Boolean =
  {
    hasLabel(pt, "CC")
  }

  def hasTerminalLemma(tree : Tree, lemma : String) =
  {
    tree.isPreTerminal && (getLemma(tree.firstChild) == lemma)
  }

  def hasTerminalLabel(
    tree : Tree, label : String, terminalLabel : String) : Boolean =
  {
    tree.isPreTerminal && hasLabel(tree, label) &&
      hasLabel(tree.firstChild, terminalLabel)
  }

  def isImperative(children : Seq[Tree]) =
  {
    (children.size == 1) && hasLabel(children.head, "VP")
  }

  def isCopula(verbHead : Tree) =
  {
    isVerb(verbHead) &&
      (hasTerminalLemma(verbHead, "be") || hasTerminalLemma(verbHead, "exist"))
  }

  def isExists(verbHead : Tree) =
  {
    isVerb(verbHead) && hasTerminalLemma(verbHead, "exist")
  }

  def isSinglePhrase(seq : Seq[Tree]) : Boolean =
  {
    (seq.size == 1) && !seq.head.isPreTerminal
  }

  def getLemma(leaf : Tree) : String

  def getToken(leaf : Tree) : String
}

object ShlurdParseUtils
{
  def capitalize(s : String) = s.head.toUpper + s.tail
}
