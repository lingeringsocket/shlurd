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

import java.io._

import SprPennTreebankLabels._

class SprPhrasePatternTrie
{
  private val children = new mutable.LinkedHashMap[String, SprPhrasePatternTrie]

  private val labels = new mutable.LinkedHashSet[String]

  private var maxPatternLength : Int = 1

  private val symbols = new mutable.LinkedHashMap[String, Seq[Seq[String]]]

  def foldLabel(label : String) : String =
  {
    (label match {
      case LABEL_NNS | LABEL_NNP | LABEL_NNPS | LABEL_NNQ |
          LABEL_NNC => LABEL_NN
      case LABEL_VBP | LABEL_VBD | LABEL_VBZ | LABEL_VBC => LABEL_VB
      case LABEL_RP | LABEL_RBC => LABEL_RB
      case "," => "COMMA"
      case ";" => "SEMICOLON"
      case "PRP_POS" => LABEL_PRP_POS
      case "WP_POS" => LABEL_WP_POS
      case "LPAREN" | "(" => LABEL_LPAREN
      case "RPAREN" | ")" => LABEL_RPAREN
      case _ => label
    }).intern
  }

  def getMaxPatternLength() : Int =
  {
    maxPatternLength
  }

  def matchPatterns(
    seq : Seq[Set[SprSyntaxTree]], start : Int, minLength : Int = 1)
      : Map[Int, Set[SprSyntaxTree]] =
  {
    if (maxPatternLength >= minLength) {
      val map = new mutable.HashMap[Int, mutable.Set[SprSyntaxTree]]
      matchPatternsSub(seq, start, map, Seq.empty, minLength)
      map
    } else {
      Map.empty
    }
  }

  private def matchPatternsSub(
    seq : Seq[Set[SprSyntaxTree]],
    start : Int,
    map : mutable.Map[Int, mutable.Set[SprSyntaxTree]],
    prefix : Seq[SprSyntaxTree],
    minLength : Int
  )
  {
    labels.foreach(label => {
      if (prefix.size >= minLength) {
        val newTree = SprSyntaxRewriter.recompose(label, prefix)
        map.getOrElseUpdate(
          prefix.size,
          new mutable.HashSet[SprSyntaxTree]
        ) += newTree
      }
    })
    if ((start < seq.size) && children.nonEmpty) {
      seq(start).foreach(syntaxTree => {
        val label = foldLabel(syntaxTree.label)
        children.get(label).foreach(child => {
          child.matchPatternsSub(
            seq, start + 1, map, prefix :+ syntaxTree, minLength)
        })
      })
    }
  }

  def addPattern(syntaxTree : SprSyntaxTree)
  {
    val children = syntaxTree.children.map(child => foldLabel(child.label))
    val pattern = {
      if (syntaxTree.children.last.hasLabel(LABEL_DOT)) {
        children.dropRight(1)
      } else {
        children
      }
    }
    addPatternImpl(
      pattern,
      foldLabel(syntaxTree.label))
  }

  def addPattern(pattern : Seq[String], label : String)
  {
    addPatternImpl(pattern.map(foldLabel), foldLabel(label))
  }

  private[parser] def addPatternImpl(pattern : Seq[String], label : String)
  {
    if (pattern.isEmpty) {
      labels += label
    } else {
      val symbol = pattern.head
      val alternatives = symbols.get(symbol).getOrElse(Seq(Seq(symbol)))
      alternatives.foreach(alternative => {
        if (symbols.contains(alternative.head)) {
          addPatternImpl(alternative ++ pattern.tail, label)
        } else {
          val child = children.getOrElseUpdate(alternative.head, {
            new SprPhrasePatternTrie
          })
          child.addPatternImpl(alternative.tail ++ pattern.tail, label)
        }
      })
    }
    maxPatternLength = {
      if (children.isEmpty) {
        1
      } else {
        1 + children.values.map(_.getMaxPatternLength).max
      }
    }
  }

  def addSymbol(symbol : String, patterns : Seq[Seq[String]])
  {
    symbols.put(symbol, patterns.map(_.map(foldLabel)))
  }

  private def dump(pw : PrintWriter, level : Int)
  {
    val prefix = "  " * level
    pw.print(prefix)
    pw.println(s"LABELS:  $labels")
    children.foreach({
      case (label, child) => {
        pw.print(prefix)
        pw.println(s"CHILD:  $label")
        child.dump(pw, level + 1)
      }
    })
  }

  def exportText(pw : PrintWriter, prefix : String = "")
  {
    labels.foreach(label => {
      pw.println(s"$label -> $prefix")
    })
    children.foreach {
      case (label, child) => {
        child.exportText(pw, s"$prefix $label")
      }
    }
  }

  override def toString =
  {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    exportText(pw)
    pw.close
    sw.toString
  }
}
