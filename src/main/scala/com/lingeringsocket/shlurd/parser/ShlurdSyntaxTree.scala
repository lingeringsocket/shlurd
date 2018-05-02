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

object ShlurdPennTreebankLabels
{
  val LABEL_ROOT = "ROOT"
  val LABEL_S = "S"
  val LABEL_SBAR = "SBAR"
  val LABEL_SBARQ = "SBARQ"
  val LABEL_NP = "NP"
  val LABEL_VP = "VP"
  val LABEL_ADJP = "ADJP"
  val LABEL_ADVP = "ADVP"
  val LABEL_PP = "PP"
  val LABEL_PRT = "PRT"
  val LABEL_SQ = "SQ"
  val LABEL_WHNP = "WHNP"
  val LABEL_WHADJP = "WHADJP"
  val LABEL_WDT = "WDT"
  val LABEL_WP = "WP"
  val LABEL_NN = "NN"
  val LABEL_VB = "VB"
  val LABEL_VBG = "VBG"
  val LABEL_VBN = "VBN"
  val LABEL_EX = "EX"
  val LABEL_DT = "DT"
  val LABEL_CC = "CC"
  val LABEL_PRP = "PRP"
  val LABEL_JJ = "JJ"
  val LABEL_RB = "RB"
  val LABEL_IN = "IN"
  val LABEL_POS = "POS"
  val LABEL_MD = "MD"
  val LABEL_RP = "RP"

  val LABEL_COMMA = ","
  val LABEL_DOT = "."
  val LABEL_QUESTION_MARK = "?"
  val LABEL_EXCLAMATION_MARK = "!"
}

import ShlurdPennTreebankLabels._
import ShlurdEnglishLemmas._

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

  def isRoot = hasLabel(LABEL_ROOT)

  def isSentence = hasLabel(LABEL_S)

  def isSBAR = hasLabel(LABEL_SBAR)

  def isSBARQ = hasLabel(LABEL_SBARQ)

  def isNounPhrase = hasLabel(LABEL_NP)

  def isVerbPhrase = hasLabel(LABEL_VP)

  def isAdjectivePhrase = hasLabel(LABEL_ADJP)

  def isAdverbPhrase = hasLabel(LABEL_ADVP)

  def isPrepositionalPhrase = hasLabel(LABEL_PP)

  def isParticlePhrase = hasLabel(LABEL_PRT)

  def isSubQuestion = hasLabel(LABEL_SQ)

  def isCompoundPrepositionalPhrase =
    isPrepositionalPhrase && (numChildren > 1)

  def isVerb = label.startsWith(LABEL_VB)

  def isNoun = label.startsWith(LABEL_NN)

  def isDeterminer = hasLabel(LABEL_DT)

  def isQueryNoun = hasLabel(LABEL_WHNP)

  def isQueryAdjective = hasLabel(LABEL_WHADJP)

  def isQueryDeterminer = hasLabel(LABEL_WDT)

  def isQueryPronoun = hasLabel(LABEL_WP)

  def isPronoun = label.startsWith(LABEL_PRP)

  def isAdjective = label.startsWith(LABEL_JJ)

  def isAdverb = label.startsWith(LABEL_RB)

  def isPreposition = label.startsWith(LABEL_IN)

  def isAdjectival = isAdjective || isParticipleOrGerund

  def isPossessive = hasLabel(LABEL_POS)

  def isCoordinatingConjunction = hasLabel(LABEL_CC)

  def isModal =
    hasLabel(LABEL_MD) ||
      (isVerb && hasTerminalLemma(LEMMA_DO))

  def isParticle = hasLabel(LABEL_RP)

  def isParticleNode =
    isParticlePhrase || isParticle ||
      (isPrepositionalPhrase && (numChildren == 1))

  def isParticipleOrGerund = hasLabel(LABEL_VBG) || hasLabel(LABEL_VBN)

  def isExistential =
    isNounPhrase && firstChild.hasLabel(LABEL_EX)

  def isBeingVerb =
    isVerb && (hasTerminalLemma(LEMMA_BE) || hasTerminalLemma(LEMMA_EXIST))

  def isPossessionVerb =
    isVerb && hasTerminalLemma(LEMMA_HAVE)

  def isRelationshipVerb = isBeingVerb || isPossessionVerb

  def isExistsVerb = isVerb && hasTerminalLemma(LEMMA_EXIST)

  def isComma = hasLabel(LABEL_COMMA)
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

case class ShlurdSyntaxLeaf(label : String, lemma : String, token : String)
    extends ShlurdSyntaxTree
{
  override def children = Seq.empty

  override def toString = token
}


sealed trait ShlurdSyntaxUniqueChild extends ShlurdSyntaxNonLeaf
{
  def child : ShlurdSyntaxTree

  override def children = Seq(child)
}

sealed trait ShlurdSyntaxPreTerminal extends ShlurdSyntaxUniqueChild
{
  override def child : ShlurdSyntaxLeaf
}

sealed trait ShlurdSyntaxPhrase extends ShlurdSyntaxNonLeaf
{
}

case class ShlurdSyntaxNode(label : String, children : Seq[ShlurdSyntaxTree])
    extends ShlurdSyntaxNonLeaf
{
}

case class SptROOT(child : ShlurdSyntaxTree)
    extends ShlurdSyntaxUniqueChild
{
  override def label = LABEL_ROOT
}

case class SptS(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_S
}

case class SptSBAR(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_SBAR
}

case class SptSBARQ(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_SBARQ
}

case class SptNP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_NP
}

case class SptVP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_VP
}

case class SptADJP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_ADJP
}

case class SptADVP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_ADVP
}

case class SptPP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_PP
}

case class SptPRT(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_PRT
}

case class SptSQ(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_SQ
}

case class SptWHNP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_WHNP
}

case class SptWHADJP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_WHADJP
}

case class SptWDT(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_WDT
}

case class SptWP(children : ShlurdSyntaxTree*)
    extends ShlurdSyntaxPhrase
{
  override def label = LABEL_WP
}

case class SptCC(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPreTerminal
{
  override def label = LABEL_CC
}

case class SptDT(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPreTerminal
{
  override def label = LABEL_DT
}

case class SptPOS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPreTerminal
{
  override def label = LABEL_POS
}

case class SptRP(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPreTerminal
{
  override def label = LABEL_RP
}
