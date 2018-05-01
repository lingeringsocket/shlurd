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

object ShlurdSyntaxRewrites
{
  def copyTree = rewrite {
    case t : ShlurdSyntaxTree => {
      if (t.isLeaf) {
        ShlurdPennLeaf(t.label, t.lemma, t.token)
      } else {
        ShlurdPennNode(t.label, t.children)
      }
    }
  }

  def rewrite(rule : PartialFunction[ShlurdSyntaxTree, ShlurdSyntaxTree])
      : (ShlurdSyntaxTree) => ShlurdSyntaxTree =
  {
    val strategy = Rewriter.rule[ShlurdSyntaxTree](rule)
    Rewriter.rewrite(
      Rewriter.everywherebu("rewriteDefault", strategy)
    )
  }
}
