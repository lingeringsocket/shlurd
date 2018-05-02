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

trait ShlurdAbstractSyntaxTree
{
  def label : String

  def lemma : String

  def token : String

  def children : Seq[ShlurdAbstractSyntaxTree]

  def numChildren = children.size

  def firstChild = children.head

  def lastChild = children.last

  def isLeaf = children.isEmpty

  def isPreTerminal = ((children.size == 1) && firstChild.isLeaf)

  def isPrePreTerminal = !isLeaf && children.forall(_.isPreTerminal)

  def hasLabel(s : String) = (label == s)

  def hasLemma(s : String) = (lemma == s)

  def hasTerminalLemma(lemma : String) =
    isPreTerminal && firstChild.hasLemma(lemma)

  def hasTerminalLabel(label : String, terminalLabel : String) =
    isPreTerminal && hasLabel(label) && firstChild.hasLabel(terminalLabel)

  def isRoot = hasLabel("ROOT")

  def isSentence = hasLabel("S")

  def isSBAR = hasLabel("SBAR")

  def isSBARQ = hasLabel("SBARQ")

  def isNounPhrase = hasLabel("NP")

  def isVerbPhrase = hasLabel("VP")

  def isPrepositionalPhrase = hasLabel("PP")

  def isSubQuestion = hasLabel("SQ")

  def isCompoundPrepositionalPhrase =
    isPrepositionalPhrase && (numChildren > 1)

  def isVerb = label.startsWith("VB")

  def isNoun = label.startsWith("NN")

  def isDeterminer = hasLabel("DT")

  def isQueryNoun = hasLabel("WHNP")

  def isQueryAdjective = hasLabel("WHADJP")

  def isQueryDeterminer = hasLabel("WDT")

  def isQueryPronoun = hasLabel("WP")

  def isPronoun = label.startsWith("PRP")

  def isAdjective = label.startsWith("JJ")

  def isAdverb = label.startsWith("RB")

  def isPreposition = label.startsWith("IN")

  def isAdjectival = isAdjective || isParticipleOrGerund

  def isPossessive = hasLabel("POS")

  def isCoordinatingConjunction = hasLabel("CC")

  def isModal =
    hasLabel("MD") || (isVerb && hasTerminalLemma("do"))

  def isParticle =
    hasLabel("PRT") || hasLabel("RP") ||
      (hasLabel("PP") && (numChildren == 1))

  def isParticipleOrGerund = hasLabel("VBG") || hasLabel("VBN")

  def isExistential =
    isNounPhrase && firstChild.hasLabel("EX")

  def isBeingVerb =
    isVerb && (hasTerminalLemma("be") || hasTerminalLemma("exist"))

  def isPossessionVerb =
    isVerb && hasTerminalLemma("have")

  def isRelationshipVerb = isBeingVerb || isPossessionVerb

  def isExistsVerb = isVerb && hasTerminalLemma("exist")

  def isComma = hasLabel(",")
}

sealed trait ShlurdSyntaxTree extends ShlurdAbstractSyntaxTree
{
  override def children : Seq[ShlurdSyntaxTree]

  override def firstChild : ShlurdSyntaxTree = children.head

  override def lastChild : ShlurdSyntaxTree = children.last

  def unwrapPhrase =
  {
    if (isPrePreTerminal && (numChildren == 1)) {
      firstChild
    } else {
      this
    }
  }
}

sealed trait ShlurdSyntaxNonLeaf extends ShlurdSyntaxTree
{
  override def token = ""

  override def lemma = ""

  override def toString =
  {
    val childrenString = children.mkString(" ")
    s"($label $childrenString)"
  }
}

case class ShlurdSyntaxNode(label : String, children : Seq[ShlurdSyntaxTree])
    extends ShlurdSyntaxNonLeaf
{
}

case class ShlurdSyntaxLeaf(label : String, lemma : String, token : String)
    extends ShlurdSyntaxTree
{
  override def children = Seq.empty

  override def toString = token
}

case class SptROOT(child : ShlurdSyntaxTree)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "ROOT"

  override def children = Seq(child)
}

case class SptS(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "S"
}

case class SptSBAR(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "SBAR"
}

case class SptSBARQ(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "SBARQ"
}

case class SptNP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "NP"
}

case class SptVP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "VP"
}

case class SptPP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "PP"
}

case class SptSQ(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "SQ"
}

case class SptWHNP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "WHNP"
}

case class SptWHADJP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "WHADJP"
}

case class SptWDT(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "WDT"
}

case class SptWP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "WP"
}

case class SptCC(child : ShlurdSyntaxTree)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "CC"

  override def children = Seq(child)
}

case class SptDT(child : ShlurdSyntaxTree)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "DT"

  override def children = Seq(child)
}

case class SptPOS(child : ShlurdSyntaxTree)
    extends ShlurdSyntaxNonLeaf
{
  override def label = "POS"

  override def children = Seq(child)
}
