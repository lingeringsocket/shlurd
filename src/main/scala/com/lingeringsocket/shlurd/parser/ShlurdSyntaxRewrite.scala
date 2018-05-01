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

object ShlurdSyntaxRewrite extends ShlurdParseUtils
{
  def rewriteAbstract(tree : ShlurdAbstractSyntaxTree) : ShlurdSyntaxTree =
  {
    if (tree.isLeaf) {
      ShlurdSyntaxLeaf(tree.label, tree.lemma, tree.token)
    } else {
      val children = tree.children.map(rewriteAbstract)
      if (tree.isNounPhrase) {
        SptNP(children:_*)
      } else if (tree.isCoordinatingConjunction) {
        SptCC(expectUnique(children))
      } else {
        ShlurdSyntaxNode(tree.label, children)
      }
    }
  }

  private def expectUnique(seq : Seq[ShlurdSyntaxTree]) =
  {
    assert(seq.size == 1)
    seq.head
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
