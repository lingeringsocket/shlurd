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

import scala.collection._

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
  val LABEL_NNS = "NNS"
  val LABEL_NNP = "NNP"
  val LABEL_NNPS = "NNPS"
  val LABEL_VB = "VB"
  val LABEL_VBZ = "VBZ"
  val LABEL_VBP = "VBP"
  val LABEL_VBD = "VBD"
  val LABEL_VBG = "VBG"
  val LABEL_VBN = "VBN"
  val LABEL_EX = "EX"
  val LABEL_DT = "DT"
  val LABEL_CC = "CC"
  val LABEL_PRP = "PRP"
  val LABEL_PRP_POS = "PRP$"
  val LABEL_JJ = "JJ"
  val LABEL_JJR = "JJR"
  val LABEL_JJS = "JJS"
  val LABEL_RB = "RB"
  val LABEL_RBR = "RBR"
  val LABEL_RBS = "RBS"
  val LABEL_IN = "IN"
  val LABEL_POS = "POS"
  val LABEL_MD = "MD"
  val LABEL_RP = "RP"
  val LABEL_CD = "CD"

  val LABEL_COMMA = ","
  val LABEL_DOT = "."
  val LABEL_QUESTION_MARK = "?"
  val LABEL_EXCLAMATION_MARK = "!"
}

import ShlurdPennTreebankLabels._
import ShlurdEnglishLemmas._
import ShlurdPrettyPrinter._

trait ShlurdAbstractSyntaxTree
    extends PrettyPrintable
{
  def label : String

  def lemma : String

  def token : String

  def incomingDep : String

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

  def isNounNode = isNoun || isNounPhrase

  def isNounPhrase = hasLabel(LABEL_NP)

  def isVerbPhrase = hasLabel(LABEL_VP)

  def isAdverbPhrase = hasLabel(LABEL_ADVP)

  def isAdverbNode = isAdverbPhrase || isAdverb

  def isAdpositionalPhrase = hasLabel(LABEL_PP)

  def isAdverbialPhrase = isAdpositionalPhrase || isAdverbNode

  def isParticlePhrase = hasLabel(LABEL_PRT)

  def isSubQuestion = hasLabel(LABEL_SQ)

  def isCompoundAdpositionalPhrase =
    isAdpositionalPhrase && (numChildren > 1)

  def isVerb = label.startsWith(LABEL_VB)

  def isNoun = label.startsWith(LABEL_NN)

  def isQueryNoun = hasLabel(LABEL_WHNP)

  def isPronoun = label.startsWith(LABEL_PRP)

  def isAdjective = label.startsWith(LABEL_JJ)

  def isAdverb = label.startsWith(LABEL_RB)

  def isAdposition = hasLabel(LABEL_IN)

  def isAdjectival = isAdjective || isParticipleOrGerund

  def isPossessive = hasLabel(LABEL_POS)

  def isCoordinatingConjunction = hasLabel(LABEL_CC)

  def isModal =
    hasLabel(LABEL_MD) ||
      (isVerb && hasTerminalLemma(LEMMA_DO))

  def isParticle = hasLabel(LABEL_RP)

  def isParticleNode =
    isParticlePhrase || isParticle ||
      (isAdpositionalPhrase && (numChildren == 1))

  def isParticipleOrGerund = hasLabel(LABEL_VBG) || hasLabel(LABEL_VBN)

  def isExistential =
    (isNounPhrase && firstChild.hasLabel(LABEL_EX)) || isExistsVerb

  def isBeingVerb =
    isVerb && (hasTerminalLemma(LEMMA_BE) || hasTerminalLemma(LEMMA_EXIST))

  def isPossessionVerb =
    isVerb && hasTerminalLemma(LEMMA_HAVE)

  def isRelationshipVerb = isBeingVerb || isPossessionVerb

  def isExistsVerb = isVerb && hasTerminalLemma(LEMMA_EXIST)

  def isComma = hasLabel(LABEL_COMMA)

  override def toString = ShlurdPrettyPrinter.prettyPrint(this)

  override def toDoc =
  {
    parens(
      string(label) <> nest(
        line <> vsep(children.map(_.toDoc).to[immutable.Seq], space)))
  }

  def containsIncomingDependency(dep : String) : Boolean =
  {
    (incomingDep == dep) ||
      children.exists(_.containsIncomingDependency(dep))
  }

  def foldedToken : String =
  {
    if (!lemma.isEmpty && lemma.head.isLower) {
      token.toLowerCase
    } else {
      token
    }
  }

  def toWordString : String =
  {
    if (children.isEmpty) {
      foldedToken
    } else {
      // FIXME uniform handling for all clitics
      children.map(_.toWordString).mkString(" ").
        replaceAllLiterally(" 's", "'s")
    }
  }
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

  def countLeaves : Int =
  {
    if (isLeaf) {
      1
    } else {
      children.map(_.countLeaves).sum
    }
  }
}

sealed trait ShlurdSyntaxNonLeaf extends ShlurdSyntaxTree
{
  override def token = ""

  override def lemma = ""

  override def incomingDep = ""
}

case class ShlurdSyntaxLeaf(
  label : String, lemma : String, token : String, incomingDep : String = "")
    extends ShlurdSyntaxTree
{
  override def children = Seq.empty

  override def toString = token

  override def toDoc : Doc =
  {
    value(this)
  }
}

sealed trait ShlurdSyntaxUniqueChild extends ShlurdSyntaxNonLeaf
{
  def child : ShlurdSyntaxTree

  override def children = Seq(child)
}

sealed trait ShlurdSyntaxPreTerminal extends ShlurdSyntaxUniqueChild
{
  override def child : ShlurdSyntaxLeaf

  override def toDoc : Doc =
  {
    parens(string(label) <+> child.toDoc)
  }
}

sealed trait ShlurdSyntaxPhrase extends ShlurdSyntaxNonLeaf
{
}

sealed trait ShlurdSyntaxNoun extends ShlurdSyntaxPreTerminal
{
  def isProper : Boolean = false
}

sealed trait ShlurdSyntaxVerb extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxPronoun extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxAdjective extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxAdverb extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxAdposition extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxConjunction extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxDeterminer extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxPossessive extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxParticle extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxModal extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxPunctuation extends ShlurdSyntaxPreTerminal
{
}

sealed trait ShlurdSyntaxNumber extends ShlurdSyntaxPreTerminal
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

case class SptNN(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxNoun
{
  override def label = LABEL_NN
}

case class SptNNS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxNoun
{
  override def label = LABEL_NNS
}

case class SptNNP(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxNoun
{
  override def label = LABEL_NNP

  override def isProper = true
}

case class SptNNPS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxNoun
{
  override def label = LABEL_NNPS

  override def isProper = true
}

case class SptPRP(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPronoun
{
  override def label = LABEL_PRP
}

case class SptPRP_POS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPronoun
{
  override def label = LABEL_PRP_POS
}

case class SptVB(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VB
}

case class SptVBZ(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VBZ
}

case class SptVBP(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VBP
}

case class SptVBD(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VBD
}

case class SptVBN(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VBN
}

case class SptVBG(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxVerb
{
  override def label = LABEL_VBG
}

case class SptCC(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxConjunction
{
  override def label = LABEL_CC
}

case class SptDT(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxDeterminer
{
  override def label = LABEL_DT
}

case class SptPOS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPossessive
{
  override def label = LABEL_POS
}

case class SptRP(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxParticle
{
  override def label = LABEL_RP
}

case class SptMD(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxModal
{
  override def label = LABEL_MD
}

case class SptJJ(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdjective
{
  override def label = LABEL_JJ
}

case class SptJJR(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdjective
{
  override def label = LABEL_JJR
}

case class SptJJS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdjective
{
  override def label = LABEL_JJS
}

case class SptRB(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdverb
{
  override def label = LABEL_RB
}

case class SptRBR(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdverb
{
  override def label = LABEL_RBR
}

case class SptRBS(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdverb
{
  override def label = LABEL_RBS
}

case class SptIN(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxAdposition
{
  override def label = LABEL_IN
}

case class SptCD(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxNumber
{
  override def label = LABEL_CD
}

case class SptDOT(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPunctuation
{
  override def label = LABEL_DOT
}

case class SptCOMMA(child : ShlurdSyntaxLeaf)
    extends ShlurdSyntaxPunctuation
{
  override def label = LABEL_COMMA
}
