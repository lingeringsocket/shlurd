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

import com.lingeringsocket.shlurd._

import scala.collection._

import java.io._

import SprPennTreebankLabels._

object SprPhrasePatternTrie
{
  val CYCLE_INFINITY = 100000

  case class CycleLinker(
    trie : SprPhrasePatternTrie,
    firstTrie : Option[SprPhrasePatternTrie]
  )
  {
  }
}

class SprPhrasePatternTrie(
  symbols : mutable.Map[String, Seq[Seq[String]]] =
    new mutable.LinkedHashMap[String, Seq[Seq[String]]]
)
{
  import SprPhrasePatternTrie._

  private val children = new mutable.LinkedHashMap[String, SprPhrasePatternTrie]

  private val labels = new mutable.LinkedHashSet[String]

  private var cycleStart : Boolean = false

  private val cycleLinks = new mutable.HashSet[SprPhrasePatternTrie]

  private var maxPatternLength : Int = 1

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
    if (prefix.size >= minLength) {
      labels.foreach(label => {
        val newTree = SprSyntaxRewriter.recompose(label, prefix)
        map.getOrElseUpdate(
          prefix.size,
          new mutable.HashSet[SprSyntaxTree]
        ) += newTree
      })
    }
    if ((start < seq.size) && children.nonEmpty) {
      seq(start).foreach(syntaxTree => {
        val label = foldLabel(syntaxTree.label)
        children.get(label).foreach(child => {
          if ((prefix.size + 1 + child.maxPatternLength) >= minLength) {
            child.matchPatternsSub(
              seq, start + 1, map, prefix :+ syntaxTree, minLength)
          }
        })
        children.get("+").foreach(child => {
          child.matchPatternsSub(
            seq, start, map, prefix, minLength)
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
    addFoldedPattern(
      pattern,
      foldLabel(syntaxTree.label),
      None)
  }

  def addPattern(pattern : Seq[String], label : String)
  {
    addFoldedPattern(pattern.map(foldLabel), foldLabel(label), None)
  }

  private[parser] def addFoldedPattern(
    pattern : Seq[String],
    label : String,
    cycleLinker : Option[CycleLinker])
  {
    val iOptional = pattern.indexOf("?")
    if (iOptional == -1) {
      addUnrolledPattern(pattern, label, cycleLinker)
    } else {
      addFoldedPattern(
        pattern.take(iOptional) ++ pattern.drop(iOptional + 1),
        label,
        cycleLinker)
      addFoldedPattern(
        pattern.take(iOptional - 1) ++ pattern.drop(iOptional + 1),
        label,
        cycleLinker)
    }
}

  private[parser] def addUnrolledPattern(
    pattern : Seq[String],
    label : String,
    cycleLinker : Option[CycleLinker])
  {
    if (pattern.isEmpty) {
      labels += label
    } else {
      val symbol = pattern.head
      val patternTail = pattern.tail
      if (symbol == "-") {
        assert(cycleLinker.nonEmpty)
        val cycleTrie = cycleLinker.get.trie
        assert(!children.contains("+"))
        cycleTrie.cycleLinks += this
        children.put("+", cycleTrie)
        addUnrolledPattern(patternTail, label, None)
      } else {
        val (remainder, newLinker, insert) = patternTail.headOption match {
          case Some("+") => {
            assert(cycleLinker.isEmpty)
            val cycleTrie = new SprPhrasePatternTrie(symbols)
            tupleN((patternTail.tail,
              Some(CycleLinker(cycleTrie, Some(cycleTrie))), Seq("-")))
          }
          case _ => {
            tupleN((patternTail, cycleLinker, Seq.empty))
          }
        }
        val alternatives = symbols.get(symbol).getOrElse(Seq(Seq(symbol)))
        alternatives.foreach(alternative => {
          if (symbols.contains(alternative.head) || alternative.contains("+")) {
            addFoldedPattern(
              alternative ++ insert ++ remainder, label, newLinker)
          } else {
            val child = children.getOrElseUpdate(alternative.head, {
              new SprPhrasePatternTrie(symbols)
            })
            val childLinker = newLinker match {
              case Some(CycleLinker(trie, Some(firstTrie))) => {
                child.cycleStart = true
                firstTrie.children.put(alternative.head, child)
                Some(CycleLinker(trie, None))
              }
              case _ => newLinker
            }
            child.addFoldedPattern(
              alternative.tail ++ insert ++ remainder, label, childLinker)
          }
        })
      }
    }
    maxPatternLength = {
      if (cycleLinker.nonEmpty) {
        CYCLE_INFINITY
      } else {
        if (children.isEmpty) {
          1
        } else {
          val childMax = children.values.map(_.getMaxPatternLength).max
          if (childMax == CYCLE_INFINITY) {
            CYCLE_INFINITY
          } else {
            1 + childMax
          }
        }
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
    if (cycleLinks.isEmpty) {
      children.foreach({
        case (label, child) => {
          pw.print(prefix)
          if (child.cycleLinks.contains(this)) {
            pw.println(s"CYCLE:  " + child.children.keys)
          } else {
            pw.println(s"CHILD:  $label")
            child.dump(pw, level + 1)
          }
        }
      })
    }
  }

  def exportText(pw : PrintWriter, prefix : String = "")
  {
    labels.foreach(label => {
      pw.println(s"$label -> $prefix")
    })
    val anyCycle = children.keySet.contains("+")
    children.foreach {
      case (label, child) => {
        if (!child.cycleLinks.contains(this)) {
          if (child.cycleStart) {
            child.exportText(pw, s"$prefix ($label")
          } else if (anyCycle) {
            child.exportText(pw, s"$prefix)+ $label")
          } else {
            child.exportText(pw, s"$prefix $label")
          }
        }
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
