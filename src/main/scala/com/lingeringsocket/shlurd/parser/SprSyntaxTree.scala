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

object SprPennTreebankLabels
{
  val LABEL_ROOT = "ROOT"
  val LABEL_S = "S"
  val LABEL_SINV = "SINV"
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
  val LABEL_WHPP = "WHPP"
  val LABEL_WHADJP = "WHADJP"
  val LABEL_WHADVP = "WHADVP"
  val LABEL_WDT = "WDT"
  val LABEL_WP = "WP"
  val LABEL_WRB = "WRB"
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
  val LABEL_TO = "TO"
  val LABEL_POS = "POS"
  val LABEL_MD = "MD"
  val LABEL_RP = "RP"
  val LABEL_CD = "CD"
  val LABEL_TMOD = "TMOD"

  val LABEL_COMMA = ","
  val LABEL_DOT = "."
  val LABEL_QUESTION_MARK = "?"
  val LABEL_EXCLAMATION_MARK = "!"
}

import SprPennTreebankLabels._
import SprEnglishLemmas._
import SprEnglishAffixes._
import SprPrettyPrinter._

trait SprAbstractSyntaxTree
    extends PrettyPrintable
{
  def label : String

  def lemma : String

  def token : String

  def incomingDep : String

  def tags : Set[String] = Set.empty

  def children : Seq[SprAbstractSyntaxTree]

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

  def isVerbNode = isVerb || isVerbPhrase

  def isAdverbPhrase = hasLabel(LABEL_ADVP)

  def isAdverbNode = isAdverbPhrase || isAdverb

  def isAdpositionalPhrase = hasLabel(LABEL_PP)

  def isAdverbialPhrase =
    isAdpositionalPhrase || isAdverbNode || hasLabel(LABEL_TMOD)

  def isParticlePhrase = hasLabel(LABEL_PRT)

  def isSubQuestion = hasLabel(LABEL_SQ)

  def isCompoundAdpositionalPhrase =
    isAdpositionalPhrase && (numChildren > 1)

  def isVerb = label.startsWith(LABEL_VB)

  def isVerbPastTense = label.equals(LABEL_VBD) || label.equals(LABEL_VBN)

  def isNoun = label.startsWith(LABEL_NN)

  def isQueryPhrase =
    isQueryNounPhrase || isQueryAdverbPhrase || isQueryAdpositionPhrase

  def isQueryNounPhrase = hasLabel(LABEL_WHNP)

  def isQueryAdpositionPhrase = hasLabel(LABEL_WHPP)

  def isQueryAdverbPhrase = hasLabel(LABEL_WHADVP)

  def isDeterminer = hasLabel(LABEL_DT)

  def isPronoun = label.startsWith(LABEL_PRP)

  def isPronounOrDemonstrative = isPronoun || isDemonstrative

  def isAdjective = label.startsWith(LABEL_JJ)

  def isAdverb = label.startsWith(LABEL_RB)

  def isAdposition = hasLabel(LABEL_IN) || hasLabel(LABEL_TO)

  def isAdjectival = isAdjective || isParticipleOrGerund

  def isPossessive = hasLabel(LABEL_POS)

  def isCoordinatingConjunction = hasLabel(LABEL_CC)

  def isModal : Boolean =
    hasLabel(LABEL_MD) ||
      (isVerb && hasTerminalLemma(LEMMA_DO)) ||
      (isVerbPhrase && (numChildren == 1) && firstChild.isModal)

  def isParticle = hasLabel(LABEL_RP)

  def isParticleNode =
    isParticlePhrase || isParticle ||
      (isAdpositionalPhrase && (numChildren == 1))

  def isParticipleOrGerund = isParticiple || isGerund

  def isParticiple = hasLabel(LABEL_VBN)

  def isGerund = hasLabel(LABEL_VBG)

  // FIXME this is English-specific
  def isProgressiveVerb =
    isGerund && firstChild.foldedToken.endsWith(SUFFIX_ING)

  def isExistential =
    (isNounPhrase && firstChild.hasLabel(LABEL_EX)) || isExistsVerb

  def isBeingVerb =
    isVerb && (hasTerminalLemma(LEMMA_BE) || hasTerminalLemma(LEMMA_EXIST))

  def isPossessionVerb =
    isVerb && hasTerminalLemma(LEMMA_HAVE)

  def isRelationshipVerb = isBeingVerb || isPossessionVerb

  def isExistsVerb = isVerb && hasTerminalLemma(LEMMA_EXIST)

  def isDemonstrative = isPreTerminal &&
    Set(LEMMA_THIS, LEMMA_THAT, LEMMA_THESE, LEMMA_THOSE).contains(
      firstChild.lemma)

  def isComma = hasLabel(LABEL_COMMA)

  def isPause = isComma || hasLabel(LABEL_DOT)

  override def toString = SprPrettyPrinter.prettyPrint(this)

  override def toDoc =
  {
    parens(
      string(label) <> nest(
        line <> vsep(children.map(_.toDoc).to[immutable.Seq], space)))
  }

  def containsIncomingDependency(dep : String) : Boolean =
  {
    (incomingDep.contains(dep)) ||
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

  def countLeaves : Int =
  {
    if (isLeaf) {
      1
    } else {
      children.map(_.countLeaves).sum
    }
  }

  def toWordString : String =
  {
    if (children.isEmpty) {
      foldedToken
    } else {
      // FIXME uniform handling for all clitics and punctuation
      children.map(_.toWordString).mkString(" ").
        replaceAllLiterally(" 's", "'s").
        replaceAllLiterally(" ,", ",")
    }
  }
}

sealed trait SprSyntaxTree extends SprAbstractSyntaxTree
{
  override def children : Seq[SprSyntaxTree]

  override def firstChild : SprSyntaxTree = children.head

  override def lastChild : SprSyntaxTree = children.last

  def unwrapPhrase =
  {
    if (isPrePreTerminal && (numChildren == 1)) {
      firstChild
    } else {
      this
    }
  }

  def isThen = unwrapPhrase.hasTerminalLemma(LEMMA_THEN)
}

sealed trait SprSyntaxNonLeaf extends SprSyntaxTree
{
  override def token = ""

  override def lemma = ""

  override def incomingDep = ""
}

case class SprSyntaxLeaf(
  label : String, lemma : String, token : String, incomingDep : String = "")
    extends SprSyntaxTree
{
  override def children = Seq.empty

  override def toString = token

  override def toDoc : Doc =
  {
    value(this)
  }
}

sealed trait SprSyntaxUniqueChild extends SprSyntaxNonLeaf
{
  def child : SprSyntaxTree

  override def children = Seq(child)
}

sealed trait SprSyntaxPreTerminal extends SprSyntaxUniqueChild
{
  override def child : SprSyntaxLeaf

  override def toDoc : Doc =
  {
    parens(string(label) <+> child.toDoc)
  }
}

sealed trait SprSyntaxPhrase extends SprSyntaxNonLeaf
{
}

sealed trait SprSyntaxNoun extends SprSyntaxPreTerminal
{
  def isProper : Boolean = false
}

sealed trait SprSyntaxVerb extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxPronoun extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxAdjective extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxAdverb extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxAdposition extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxConjunction extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxDeterminer extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxPossessive extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxParticle extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxModal extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxPunctuation extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxNumber extends SprSyntaxPreTerminal
{
}

case class SprSyntaxNode(label : String, children : Seq[SprSyntaxTree])
    extends SprSyntaxNonLeaf
{
}

case class SptROOT(child : SprSyntaxTree)
    extends SprSyntaxUniqueChild
{
  override def label = LABEL_ROOT
}

case class SptS(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_S
}

case class SptSINV(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_SINV
}

case class SptSBAR(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_SBAR
}

case class SptSBARQ(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_SBARQ
}

case class SptNP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_NP
}

case class SptVP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_VP
}

case class SptADJP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_ADJP
}

case class SptADVP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_ADVP
}

case class SptPP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_PP
}

case class SptPRT(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_PRT
}

case class SptSQ(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_SQ
}

case class SptWHNP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WHNP
}

case class SptWHPP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WHPP
}

case class SptWHADJP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WHADJP
}

case class SptWHADVP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WHADVP
}

case class SptWDT(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WDT
}

case class SptWRB(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WRB
}

case class SptWP(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_WP
}

case class SptNN(child : SprSyntaxLeaf)
    extends SprSyntaxNoun
{
  override def label = LABEL_NN
}

case class SptNNS(child : SprSyntaxLeaf)
    extends SprSyntaxNoun
{
  override def label = LABEL_NNS
}

case class SptNNP(child : SprSyntaxLeaf)
    extends SprSyntaxNoun
{
  override def label = LABEL_NNP

  override def isProper = true
}

case class SptNNPS(child : SprSyntaxLeaf)
    extends SprSyntaxNoun
{
  override def label = LABEL_NNPS

  override def isProper = true
}

case class SptPRP(child : SprSyntaxLeaf)
    extends SprSyntaxPronoun
{
  override def label = LABEL_PRP
}

case class SptPRP_POS(child : SprSyntaxLeaf)
    extends SprSyntaxPronoun
{
  override def label = LABEL_PRP_POS
}

case class SptVB(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VB
}

case class SptVBZ(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBZ
}

case class SptVBP(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBP
}

case class SptVBD(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBD
}

case class SptVBN(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBN
}

case class SptVBG(child : SprSyntaxLeaf)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBG
}

case class SptCC(child : SprSyntaxLeaf)
    extends SprSyntaxConjunction
{
  override def label = LABEL_CC
}

case class SptDT(child : SprSyntaxLeaf)
    extends SprSyntaxDeterminer
{
  override def label = LABEL_DT
}

case class SptPOS(child : SprSyntaxLeaf)
    extends SprSyntaxPossessive
{
  override def label = LABEL_POS
}

case class SptRP(child : SprSyntaxLeaf)
    extends SprSyntaxParticle
{
  override def label = LABEL_RP
}

case class SptMD(child : SprSyntaxLeaf)
    extends SprSyntaxModal
{
  override def label = LABEL_MD
}

case class SptJJ(child : SprSyntaxLeaf)
    extends SprSyntaxAdjective
{
  override def label = LABEL_JJ
}

case class SptJJR(child : SprSyntaxLeaf)
    extends SprSyntaxAdjective
{
  override def label = LABEL_JJR
}

case class SptJJS(child : SprSyntaxLeaf)
    extends SprSyntaxAdjective
{
  override def label = LABEL_JJS
}

case class SptRB(child : SprSyntaxLeaf)
    extends SprSyntaxAdverb
{
  override def label = LABEL_RB
}

case class SptRBR(child : SprSyntaxLeaf)
    extends SprSyntaxAdverb
{
  override def label = LABEL_RBR
}

case class SptRBS(child : SprSyntaxLeaf)
    extends SprSyntaxAdverb
{
  override def label = LABEL_RBS
}

case class SptIN(child : SprSyntaxLeaf)
    extends SprSyntaxAdposition
{
  override def label = LABEL_IN
}

case class SptTO(child : SprSyntaxLeaf)
    extends SprSyntaxAdposition
{
  override def label = LABEL_TO
}

case class SptCD(child : SprSyntaxLeaf)
    extends SprSyntaxNumber
{
  override def label = LABEL_CD
}

case class SptDOT(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_DOT
}

case class SptCOMMA(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_COMMA
}

// this is a non-standard one we cons up to wrap tmod NP's as adverbial
case class SptTMOD(child : SprSyntaxTree)
    extends SprSyntaxUniqueChild
{
  override def label = LABEL_TMOD
}
