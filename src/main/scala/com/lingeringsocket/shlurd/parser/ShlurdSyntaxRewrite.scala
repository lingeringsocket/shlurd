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

import org.kiama.rewriting._

import ShlurdParseUtils._
import ShlurdPennTreebankLabels._

object ShlurdSyntaxRewrite
{
  private val phraseConstructors = Map(
    LABEL_S -> SptS.apply _,
    LABEL_SBAR -> SptSBAR.apply _,
    LABEL_SBARQ -> SptSBARQ.apply _,
    LABEL_NP -> SptNP.apply _,
    LABEL_VP -> SptVP.apply _,
    LABEL_ADJP -> SptADJP.apply _,
    LABEL_ADVP -> SptADVP.apply _,
    LABEL_PP -> SptPP.apply _,
    LABEL_PRT -> SptPRT.apply _,
    LABEL_SQ -> SptSQ.apply _,
    LABEL_WHNP -> SptWHNP.apply _,
    LABEL_WHADJP -> SptWHADJP.apply _,
    LABEL_WDT -> SptWDT.apply _,
    LABEL_WP -> SptWP.apply _
  )

  private val uniqueChildConstructors = Map(
    LABEL_ROOT -> SptROOT
  )

  private val preTerminalConstructors = Map(
    LABEL_NN -> SptNN,
    LABEL_NNS -> SptNNS,
    LABEL_NNP -> SptNNP,
    LABEL_NNPS -> SptNNPS,
    LABEL_PRP -> SptPRP,
    LABEL_PRPP -> SptPRPP,
    LABEL_VB -> SptVB,
    LABEL_VBZ -> SptVBZ,
    LABEL_VBP -> SptVBP,
    LABEL_VBD -> SptVBD,
    LABEL_VBN -> SptVBN,
    LABEL_VBG -> SptVBG,
    LABEL_JJ -> SptJJ,
    LABEL_JJR -> SptJJR,
    LABEL_JJS -> SptJJS,
    LABEL_RB -> SptRB,
    LABEL_RBR -> SptRBR,
    LABEL_RBS -> SptRBS,
    LABEL_CC -> SptCC,
    LABEL_DT -> SptDT,
    LABEL_IN -> SptIN,
    LABEL_RP -> SptRP,
    LABEL_MD -> SptMD,
    LABEL_POS -> SptPOS,
    LABEL_DOT -> SptDOT,
    LABEL_COMMA -> SptCOMMA
  )

  def recompose(
    tree : ShlurdAbstractSyntaxTree,
    children : Seq[ShlurdSyntaxTree]) : ShlurdSyntaxTree =
  {
    val label = tree.label
    if ((children.size == 1) && tree.isInstanceOf[ShlurdSyntaxNonLeaf]
      && (children.head.label == label))
    {
      return children.head
    }
    preTerminalConstructors.get(label).foreach(
      constructor => return constructor(expectLeaf(children))
    )
    phraseConstructors.get(label).foreach(
      constructor => return constructor(children)
    )
    uniqueChildConstructors.get(label).foreach(
      constructor => return constructor(expectUnique(children))
    )
    ShlurdSyntaxNode(label, children)
  }

  def rewriteAbstract(tree : ShlurdAbstractSyntaxTree) : ShlurdSyntaxTree =
  {
    if (tree.isLeaf) {
      ShlurdSyntaxLeaf(tree.label, tree.lemma, tree.token)
    } else {
      val children = tree.children.map(rewriteAbstract)
      recompose(tree, children)
    }
  }

  def rewriteEither = rewrite {
    case SptNP(
      SptNP(SptCC(dt), n1),
      SptCC(cc),
      n2
    ) if (dt.hasLemma("either")) => {
      SptNP(
        SptCC(dt),
        n1,
        SptCC(cc),
        n2)
    }
  }

  def rewrite(
    rule : PartialFunction[ShlurdSyntaxTree, ShlurdSyntaxTree])
      : (ShlurdSyntaxTree) => ShlurdSyntaxTree =
  {
    val strategy = Rewriter.rule[ShlurdSyntaxTree](rule)
    Rewriter.rewrite(
        Rewriter.everywherebu("rewriteEverywhere", strategy))
  }
}
