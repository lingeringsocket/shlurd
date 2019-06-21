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
  val LABEL_AMBIGUOUS = "AMBIGUOUS"
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
  val LABEL_WP_POS = "WP$"
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
  val LABEL_DPP = "DPP"
  val LABEL_TMOD = "TMOD"
  val LABEL_NNQ = "NNQ"
  val LABEL_NNC = "NNC"
  val LABEL_RBC = "RBC"
  val LABEL_VBC = "VBC"

  val LABEL_COMMA = ","
  val LABEL_SEMICOLON = ";"
  val LABEL_DOT = "."
  val LABEL_QUESTION_MARK = "?"
  val LABEL_EXCLAMATION_MARK = "!"
  val LABEL_LQUOTE = "``"
  val LABEL_RQUOTE = "''"
  val LABEL_LPAREN = "-LRB-"
  val LABEL_RPAREN = "-RRB-"
}

import SprPennTreebankLabels._
import SprEnglishLemmas._
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

  def isNounNode = isNoun || isNounPhrase || isPronounOrDemonstrative

  def isNounPhrase = hasLabel(LABEL_NP)

  def isAdjectivePhrase = hasLabel(LABEL_ADJP)

  def isVerbPhrase = hasLabel(LABEL_VP)

  def isVerbNode = isVerb || isVerbPhrase

  def isAdverbPhrase = hasLabel(LABEL_ADVP)

  def isAdverbNode = isAdverbPhrase || isAdverb

  def isAdpositionalPhrase = hasLabel(LABEL_PP) || hasLabel(LABEL_DPP)

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

  def isPossessiveClitic = hasLabel(LABEL_POS)

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

  def isProgressiveVerb = isGerund

  def isExistential =
    (isNounPhrase && firstChild.hasLabel(LABEL_EX)) || isExistsVerb

  def isBeingVerb =
  {
    if (isVerb && isPreTerminal) {
      isBeingLemma(firstChild.lemma)
    } else {
      false
    }
  }

  def isPossessionVerb =
    isVerb && hasTerminalLemma(LEMMA_HAVE)

  def isRelationshipVerb = isBeingVerb || isPossessionVerb

  def isExistsVerb = isVerb && hasTerminalLemma(LEMMA_EXIST)

  def isDemonstrative = isPreTerminal &&
    Set(LEMMA_THIS, LEMMA_THAT, LEMMA_THESE, LEMMA_THOSE).contains(
      firstChild.lemma)

  def isComma = hasLabel(LABEL_COMMA)

  def isSemicolon = hasLabel(LABEL_SEMICOLON)

  def isPause = isComma || isSemicolon || hasLabel(LABEL_DOT)

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

  def isEquivalently = unwrapPhrase.hasTerminalLemma(LEMMA_EQUIVALENTLY)
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

sealed trait SprSyntaxNoun extends SprSyntaxNonLeaf
{
  def isProper : Boolean = false
}

sealed trait SprSyntaxSimpleNoun extends SprSyntaxPreTerminal
    with SprSyntaxNoun
{
}

sealed trait SprSyntaxVerb extends SprSyntaxNonLeaf
{
}

sealed trait SprSyntaxSimpleVerb extends SprSyntaxPreTerminal
    with SprSyntaxVerb
{
}

sealed trait SprSyntaxPronoun extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxAdjective extends SprSyntaxPreTerminal
{
}

sealed trait SprSyntaxAdverb extends SprSyntaxNonLeaf
{
}

sealed trait SprSyntaxSimpleAdverb extends SprSyntaxPreTerminal
    with SprSyntaxAdverb
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

// this is a non-standard one we cons up to wrap ambiguous alternatives
case class SptAMBIGUOUS(children : SprSyntaxTree*)
    extends SprSyntaxPhrase
{
  override def label = LABEL_AMBIGUOUS
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
  override def label =
  {
    if (children.size == 1) {
      LABEL_DPP
    } else {
      LABEL_PP
    }
  }
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

case class SptWDT(child : SprSyntaxLeaf)
    extends SprSyntaxPreTerminal
{
  override def label = LABEL_WDT
}

case class SptWRB(child : SprSyntaxLeaf)
    extends SprSyntaxPreTerminal
{
  override def label = LABEL_WRB
}

case class SptWP(child : SprSyntaxLeaf)
    extends SprSyntaxPreTerminal
{
  override def label = LABEL_WP
}

case class SptWP_POS(child : SprSyntaxLeaf)
    extends SprSyntaxPreTerminal
{
  override def label = LABEL_WP_POS
}

case class SptNN(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleNoun
{
  override def label = LABEL_NN
}

case class SptNNS(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleNoun
{
  override def label = LABEL_NNS
}

case class SptNNP(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleNoun
{
  override def label = LABEL_NNP

  override def isProper = true
}

case class SptNNPS(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleNoun
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
    extends SprSyntaxSimpleVerb
{
  override def label = LABEL_VB
}

case class SptVBZ(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleVerb
{
  override def label = LABEL_VBZ
}

case class SptVBP(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleVerb
{
  override def label = LABEL_VBP
}

case class SptVBD(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleVerb
{
  override def label = LABEL_VBD
}

case class SptVBN(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleVerb
{
  override def label = LABEL_VBN
}

case class SptVBG(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleVerb
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

case class SptEX(child : SprSyntaxLeaf)
    extends SprSyntaxPreTerminal
{
  override def label = LABEL_EX
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
    extends SprSyntaxSimpleAdverb
{
  override def label = LABEL_RB
}

case class SptRBR(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleAdverb
{
  override def label = LABEL_RBR
}

case class SptRBS(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleAdverb
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

case class SptSEMICOLON(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_SEMICOLON
}

case class SptLRB(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_LPAREN
}

case class SptRRB(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_RPAREN
}

// this is a non-standard one we cons up to wrap tmod NP's as adverbial
case class SptTMOD(child : SprSyntaxTree)
    extends SprSyntaxUniqueChild
{
  override def label = LABEL_TMOD
}

// another non-standard one we cons up to wrap quotations
case class SptNNQ(child : SprSyntaxLeaf)
    extends SprSyntaxSimpleNoun
{
  override def label = LABEL_NNQ
}

// another non-standard one we cons up for compound nouns
case class SptNNC(children : SprSyntaxTree*)
    extends SprSyntaxNoun
{
  override def label = LABEL_NNC

  override def isProper = children.exists(_ match {
    case noun : SprSyntaxNoun => noun.isProper
    case _ => false
  })
}

// another non-standard one we cons up for compound adverbs
case class SptRBC(children : SprSyntaxTree*)
    extends SprSyntaxAdverb
{
  override def label = LABEL_RBC
}

// another non-standard one we cons up for compound verbs
case class SptVBC(children : SprSyntaxTree*)
    extends SprSyntaxVerb
{
  override def label = LABEL_VBC

  override def isVerbPastTense = children.exists(_.isVerbPastTense)
}
