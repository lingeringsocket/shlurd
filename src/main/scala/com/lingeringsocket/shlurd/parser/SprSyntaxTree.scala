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
  private val all = new mutable.LinkedHashSet[String]

  private def label(s : String) : String =
  {
    all += s
    s
  }

  def getAll : Set[String] = all

  val LABEL_ROOT = label("ROOT")
  val LABEL_AMBIGUOUS = label("AMBIGUOUS")
  val LABEL_S = label("S")
  val LABEL_SINV = label("SINV")
  val LABEL_SBAR = label("SBAR")
  val LABEL_SBARQ = label("SBARQ")
  val LABEL_NP = label("NP")
  val LABEL_VP = label("VP")
  val LABEL_ADJP = label("ADJP")
  val LABEL_ADVP = label("ADVP")
  val LABEL_PP = label("PP")
  val LABEL_PRT = label("PRT")
  val LABEL_SQ = label("SQ")
  val LABEL_WHNP = label("WHNP")
  val LABEL_WHPP = label("WHPP")
  val LABEL_WHADJP = label("WHADJP")
  val LABEL_WHADVP = label("WHADVP")
  val LABEL_WDT = label("WDT")
  val LABEL_WP = label("WP")
  val LABEL_WP_POS = label("WP$")
  val LABEL_WRB = label("WRB")
  val LABEL_NN = label("NN")
  val LABEL_NNS = label("NNS")
  val LABEL_NNP = label("NNP")
  val LABEL_NNPS = label("NNPS")
  val LABEL_VB = label("VB")
  val LABEL_VBZ = label("VBZ")
  val LABEL_VBP = label("VBP")
  val LABEL_VBD = label("VBD")
  val LABEL_VBG = label("VBG")
  val LABEL_VBN = label("VBN")
  val LABEL_EX = label("EX")
  val LABEL_DT = label("DT")
  val LABEL_CC = label("CC")
  val LABEL_PRP = label("PRP")
  // ephemeral labels used for pronoun special cases
  val LABEL_PRP_OBJ = "PRP_OBJ"
  val LABEL_PRP_REFLEXIVE = "PRP_REFLEXIVE"
  val LABEL_PRP_POS = label("PRP$")
  val LABEL_JJ = label("JJ")
  val LABEL_JJR = label("JJR")
  val LABEL_JJS = label("JJS")
  val LABEL_RB = label("RB")
  val LABEL_RBR = label("RBR")
  val LABEL_RBS = label("RBS")
  val LABEL_IN = label("IN")
  val LABEL_TO = label("TO")
  val LABEL_POS = label("POS")
  val LABEL_MD = label("MD")
  val LABEL_RP = label("RP")
  val LABEL_CD = label("CD")
  val LABEL_DPP = label("DPP")
  val LABEL_TMOD = label("TMOD")
  val LABEL_NNE = label("NNE")
  val LABEL_NNQ = label("NNQ")
  val LABEL_NNC = label("NNC")
  val LABEL_RBC = label("RBC")
  val LABEL_VBC = label("VBC")

  val LABEL_COMMA = label(",")
  val LABEL_SEMICOLON = label(";")
  val LABEL_DOT = label(".")
  val LABEL_QUESTION_MARK = label("?")
  val LABEL_EXCLAMATION_MARK = label("!")
  val LABEL_LQUOTE = label("``")
  val LABEL_RQUOTE = label("''")
  val LABEL_LPAREN = label("-LRB-")
  val LABEL_RPAREN = label("-RRB-")
  val LABEL_LCURLY = label("-LCB-")
  val LABEL_RCURLY = label("-RCB-")
}

import SprPennTreebankLabels._
import SprEnglishLemmas._
import SprPrettyPrinter._

trait SprAbstractSyntaxTree
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

  def isNounOrPronoun = isNoun || isNounPhrase || isPronounOrDemonstrative

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

  def isPossessivePronoun = hasLabel(LABEL_PRP_POS)

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

  def isExistential(implicit tongue : SprTongue) =
    (isNounPhrase && firstChild.hasLabel(LABEL_EX)) || isExistsVerb

  def isBeingVerb(implicit tongue : SprTongue) =
  {
    if (isVerb && isPreTerminal) {
      isBeingLemma(firstChild.lemma)
    } else {
      false
    }
  }

  def isPossessionVerb(implicit tongue : SprTongue) =
    isVerb && isPreTerminal && tongue.isPossessionLemma(firstChild.lemma)

  def isRelationshipVerb(implicit tongue : SprTongue) =
    isBeingVerb || isPossessionVerb

  def isExistsVerb(implicit tongue : SprTongue) =
    isVerb && isPreTerminal && tongue.isExistsLemma(firstChild.lemma)

  def isDemonstrative = isPreTerminal &&
    Set(LEMMA_THIS, LEMMA_THAT, LEMMA_THESE, LEMMA_THOSE).contains(
      firstChild.lemma)

  def isComma = hasLabel(LABEL_COMMA)

  def isSemicolon = hasLabel(LABEL_SEMICOLON)

  def isPause = isComma || isSemicolon || hasLabel(LABEL_DOT)

  override def toString = SprPrettyPrinter.prettyPrint(this)

  def toDoc : Doc =
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

case class SptLCB(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_LCURLY
}

case class SptRCB(child : SprSyntaxLeaf)
    extends SprSyntaxPunctuation
{
  override def label = LABEL_RCURLY
}

// this is a non-standard one we cons up to wrap tmod NP's as adverbial
case class SptTMOD(child : SprSyntaxTree)
    extends SprSyntaxUniqueChild
{
  override def label = LABEL_TMOD
}

// another non-standard one we cons up to represent elided subjects
case class SptNNE()
    extends SprSyntaxNoun
{
  override def label = LABEL_NNE

  override def children = Seq.empty
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
