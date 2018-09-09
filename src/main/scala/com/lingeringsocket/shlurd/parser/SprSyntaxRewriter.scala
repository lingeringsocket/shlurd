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

import org.kiama.rewriting._

import SprUtils._
import SprPennTreebankLabels._
import SprEnglishLemmas._

object SprSyntaxRewriter
{
  private val phraseConstructors = Map(
    LABEL_S -> SptS.apply _,
    LABEL_SINV -> SptSINV.apply _,
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
    LABEL_WHPP -> SptWHPP.apply _,
    LABEL_WHADJP -> SptWHADJP.apply _,
    LABEL_WHADVP -> SptWHADVP.apply _,
    LABEL_WDT -> SptWDT.apply _,
    LABEL_WRB -> SptWRB.apply _,
    LABEL_WP -> SptWP.apply _
  )

  private val uniqueChildConstructors = Map(
    LABEL_ROOT -> SptROOT,
    LABEL_TMOD -> SptTMOD
  )

  private val preTerminalConstructors = Map(
    LABEL_NN -> SptNN,
    LABEL_NNS -> SptNNS,
    LABEL_NNP -> SptNNP,
    LABEL_NNPS -> SptNNPS,
    LABEL_PRP -> SptPRP,
    LABEL_PRP_POS -> SptPRP_POS,
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
    LABEL_CD -> SptCD,
    LABEL_IN -> SptIN,
    LABEL_TO -> SptTO,
    LABEL_RP -> SptRP,
    LABEL_MD -> SptMD,
    LABEL_POS -> SptPOS,
    LABEL_DOT -> SptDOT,
    LABEL_COMMA -> SptCOMMA
  )

  def recompose(
    tree : SprAbstractSyntaxTree,
    children : Seq[SprSyntaxTree]) : SprSyntaxTree =
  {
    val label = tree.label
    if ((children.size == 1) && tree.isInstanceOf[SprSyntaxNonLeaf]
      && (children.head.label == label))
    {
      return children.head
    }
    recompose(label, children)
  }

  def recompose(
    label : String,
    children : Seq[SprSyntaxTree]) : SprSyntaxTree =
  {
    preTerminalConstructors.get(label).foreach(
      constructor => return constructor(requireLeaf(children))
    )
    phraseConstructors.get(label).foreach(
      constructor => return constructor(children)
    )
    uniqueChildConstructors.get(label).foreach(
      constructor => return constructor(requireUnique(children))
    )
    SprSyntaxNode(label, children)
  }

  def rewriteAbstract(
    tree : SprAbstractSyntaxTree,
    stripDependencies : Boolean = false) : SprSyntaxTree =
  {
    if (tree.isLeaf) {
      val incomingDep = {
        if (stripDependencies) {
          ""
        } else {
          tree.incomingDep
        }
      }
      SprSyntaxLeaf(tree.label, tree.lemma, tree.token, incomingDep)
    } else {
      val children = tree.children.map(
        c => rewriteAbstract(c, stripDependencies))
      recompose(tree, children)
    }
  }

  def rewriteWarts = rewrite {
    case np @ SptNP(children @ _*) if (children.count(_.isDeterminer) == 2)=> {
      val iFirst = children.indexWhere(_.isDeterminer)
      assert(iFirst >= 0)
      val iSecond = children.indexWhere(_.isDeterminer, iFirst + 1)
      assert(iSecond >= 0)
      if (iSecond == iFirst + 1) {
        np
      } else {
        SptNP(
          SptNP(children.take(iSecond):_*),
          SptNP(children.drop(iSecond):_*)
        )
      }
    }
    case SptVP(
      vbz @ SptVBZ(vb),
      SptNP(
        np : SptNP,
        pp @ SptPP(
          _ : SptTO,
          _
        )
      )
    ) if (vb.lemma == LEMMA_BE) => {
      SptVP(vbz, np, pp)
    }
    case vp @ SptVP(children @ _*) => {
      def pullUpNP(child : SprSyntaxTree) = {
        child match {
          case SptNP(grand @ _*) if (grand.forall(_.isNounPhrase)) => {
            grand
          }
          case _ => Seq(child)
        }
      }
      SptVP(children.flatMap(pullUpNP):_*)
    }
    case SptNP(
      SptNP(SptCC(dt), n1),
      SptCC(cc),
      n2
    ) if (dt.hasLemma(LEMMA_EITHER)) => {
      SptNP(
        SptCC(dt),
        n1,
        SptCC(cc),
        n2)
    }
    case np : SptNP if (np.containsIncomingDependency("tmod")) => {
      SptTMOD(np)
    }
    case SptNP(
      SptNP(nn : SptNN),
      pp : SptPP
    ) => {
      SptADVP(
        SptNP(nn),
        pp
      )
    }
    case SptS(
      SptVP(
        SptSQ(
          vb : SprSyntaxVerb,
          SptS(children @ _*)
        )
      ),
      remainder @ _*
    ) => {
      SptSQ((Seq(vb) ++ children ++ remainder):_*)
    }
    case SptS(
      SptVP(
        SptVBG(vbg),
        SptSBAR(
          dem : SptIN,
          SptS(children @ _*))
      ),
      remainder @ _*
    ) if (dem.isDemonstrative) => {
      SptS(
        (Seq(SptPP(
          SptIN(vbg),
          SptDT(dem.child)
        )) ++ children ++ remainder):_*
      )
    }
  }

  def rewrite(
    rule : PartialFunction[SprSyntaxTree, SprSyntaxTree])
      : (SprSyntaxTree) => SprSyntaxTree =
  {
    val strategy = Rewriter.rule[SprSyntaxTree](rule)
    Rewriter.rewrite(
        Rewriter.everywherebu("rewriteEverywhere", strategy))
  }
}
