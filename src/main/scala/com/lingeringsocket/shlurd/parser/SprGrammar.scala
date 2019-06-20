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
  case class PhraseRule(label : String, alternatives : Seq[Seq[String]])

  case class SymbolRule(symbol : String, alternatives : Seq[Seq[String]])

  def arrow = "->"

  def semicolon = ";"

  def assignment = ":="

  def bar = "|"

  def lparen = "("

  def rparen = ")"

  def grammar = rep1(rule)

  def rule = phraseRule | alternativeRule

  def phraseRule = label ~ arrow ~ alternatives ~ semicolon ^^
    { case l ~ _ ~ a ~ _ => PhraseRule(l, a) }

  def alternativeRule = symbol ~ assignment ~ alternatives ~ semicolon ^^
    { case s ~ _ ~ a ~ _ => SymbolRule(s, a) }

  def pattern = rep1(component) ^^ { case s => s.flatten }

  def alternatives = rep1sep(pattern, bar)

  def component = label ^^ { case l => Seq(l) } |
    (lparen ~ label ~ rparen ^^ { case _ ~ l ~ _ => Seq(l, "?") })

  def label = ident

  def symbol = ident

  override val lexical = new StdLexical {
    delimiters ++= Seq(arrow, semicolon, assignment, bar, lparen, rparen)
  }

  def buildTrie(source : Source, trie : SprPhrasePatternTrie)
  {
    val input = source.getLines.mkString("\n")
    val result = phrase(grammar)(new lexical.Scanner(input))
    result match {
      case Success(rules, _) => {
        rules.foreach {
          case PhraseRule(label, alternatives) => {
            alternatives.foreach(alternative => {
              trie.addPattern(alternative, label)
            })
          }
          case SymbolRule(symbol, patterns) => {
            trie.addSymbol(symbol, patterns)
          }
        }
      }
      case ns : NoSuccess => {
        throw new RuntimeException(ns.msg)
      }
    }
  }
}
