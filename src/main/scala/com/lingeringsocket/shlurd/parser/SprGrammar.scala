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

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

import scala.io._

object SprGrammar extends StandardTokenParsers
{
  case class PhraseRule(pattern : Seq[String], label : String)

  def arrow = "->"

  def grammar = rule . +

  def rule = pattern ~ arrow ~ label ^^ { case p ~ _ ~ l => PhraseRule(p, l) }

  def pattern = label . +

  def label = ident

  override val lexical = new StdLexical {
    delimiters += arrow
  }

  def buildTrie(source : Source, trie : SprPhrasePatternTrie)
  {
    val input = source.getLines.mkString("\n")
    val rules = phrase(grammar)(new lexical.Scanner(input)).get
    rules.foreach(rule => {
      trie.addPattern(rule.pattern, rule.label)
    })
  }
}
