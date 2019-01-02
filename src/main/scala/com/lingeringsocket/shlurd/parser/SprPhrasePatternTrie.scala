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

import scala.io._
import java.io._

import SprPennTreebankLabels._

class SprPhrasePatternTrie
{
  private val children = new mutable.LinkedHashMap[String, SprPhrasePatternTrie]

  private val labels = new mutable.LinkedHashSet[String]

  def foldLabel(label : String) : String =
  {
    (label match {
      case LABEL_NNS | LABEL_NNP | LABEL_NNPS | LABEL_NNQ => LABEL_NN
      case LABEL_VBP | LABEL_VBD | LABEL_VBZ => LABEL_VB
      case LABEL_RP => LABEL_RB
      case "," => "COMMA"
      case _ => label
    }).intern
  }

  def generateSuccessors() : Map[String, Set[String]] =
  {
    val map = new mutable.HashMap[String, mutable.Set[String]]
    generateSuccessorsOne(map)
    children.values.foreach(_.generateSuccessorsOne(map))
    map
  }

  def generateSuccessorsOne(map : mutable.Map[String, mutable.Set[String]])
  {
    val newMap = children.mapValues(_.children.keySet)
    newMap.foreach {
      case (label, set) => {
        if (!map.contains(label)) {
          val newSet = new mutable.LinkedHashSet[String]
          newSet ++= set
          map.put(label, newSet)
        }
      }
    }
    map.foreach {
      case (label, set) => {
        if (!set.isEmpty && newMap.contains(label)) {
          val newSet = newMap(label)
          if (newSet.isEmpty) {
            set.clear
          } else {
            set ++= newSet
          }
        }
      }
    }
  }

  def generateExpansions() : Map[String, Set[String]] =
  {
    var prev = children.map {
      case (label, child) => {
        tupleN((label, child.getAllReductions))
      }
    }
    while (true) {
      val next = new mutable.LinkedHashMap[String, Set[String]]
      prev.foreach {
        case (label, set) => {
          val expansion = set.flatMap(prev.get(_).getOrElse(Set.empty))
          next.put(label, set ++ expansion)
        }
      }
      if (next == prev) {
        return next
      }
      prev = next
    }
    return prev
  }

  def getAllReductions() : Set[String] =
  {
    labels ++ children.values.flatMap(_.getAllReductions)
  }

  def matchPatterns(seq : Seq[Set[SprSyntaxTree]], start : Int)
      : Map[Int, Set[SprSyntaxTree]] =
  {
    val map = new mutable.HashMap[Int, mutable.Set[SprSyntaxTree]]
    matchPatternsSub(seq, start, map, Seq.empty)
    map
  }

  private def matchPatternsSub(
    seq : Seq[Set[SprSyntaxTree]],
    start : Int,
    map : mutable.Map[Int, mutable.Set[SprSyntaxTree]],
    prefix : Seq[SprSyntaxTree]
  )
  {
    labels.foreach(label => {
      val newTree = SprSyntaxRewriter.recompose(label, prefix)
      map.getOrElseUpdate(
        prefix.size,
        new mutable.HashSet[SprSyntaxTree]
      ) += newTree
    })
    if ((start < seq.size) && children.nonEmpty) {
      seq(start).foreach(syntaxTree => {
        val label = foldLabel(syntaxTree.label)
        children.get(label).foreach(child => {
          child.matchPatternsSub(
            seq, start + 1, map, prefix :+ syntaxTree)
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
    addPattern(
      pattern,
      foldLabel(syntaxTree.label))
  }

  private def addPattern(pattern : Seq[String], label : String)
  {
    if (pattern.isEmpty) {
      labels += label
    } else {
      val child = children.getOrElseUpdate(pattern.head, {
        new SprPhrasePatternTrie
      })
      child.addPattern(pattern.tail, label)
    }
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
      pw.println(s"$prefix -> $label")
    })
    children.foreach {
      case (label, child) => {
        child.exportText(pw, s"$prefix $label")
      }
    }
  }

  def importText(source : Source) : SprPhrasePatternTrie =
  {
    source.getLines.foreach(line => {
      val components = line.trim.split(" ")
      val iArrow = components.indexOf("->")
      if ((iArrow < 0) || (iArrow != (components.size - 2))) {
        throw new RuntimeException("invalid trie source")
      }
      val pattern = components.take(iArrow)
      val label = components.last
      addPattern(pattern, label)
    })
    this
  }

  override def toString =
  {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    dump(pw, 0)
    pw.close
    sw.toString
  }
}
