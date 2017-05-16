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
package shlurd.parser

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._

import scala.collection.JavaConverters._

object ShlurdParser
{
  private def dump(sentence : Sentence)
  {
    val tree = sentence.parse
    println("TREE = " + tree)
    Range(0, sentence.length).foreach(i => {
      println("DEP = " + sentence.incomingDependencyLabel(i))
    })
  }

  private def getLabel(tree : Tree) : String =
  {
    tree.label.value
  }

  private def hasLabel(tree : Tree, label : String) : Boolean =
  {
    getLabel(tree) == label
  }

  private def hasTerminalLabel(
    tree : Tree, label : String, terminalLabel : String) : Boolean =
  {
    tree.isPreTerminal && hasLabel(tree, label) &&
      hasLabel(tree.firstChild, terminalLabel)
  }

  private def expectRoot(tree : Tree, implicitQuestion : Boolean) =
  {
    if (hasLabel(tree, "ROOT")) {
      assert(tree.numChildren == 1)
      expectSentence(tree.firstChild, implicitQuestion)
    } else {
      ShlurdUnknownSentence
    }
  }

  private def truncatePunctuation(
    tree : Tree, punctuationMarks : Iterable[String]) : Array[Tree] =
  {
    val children = tree.children
    if (punctuationMarks.exists(punctuation =>
      hasTerminalLabel(children.last, ".", punctuation)))
    {
      children.dropRight(1)
    } else {
      children
    }
  }

  private def isImperative(children : Array[Tree]) =
  {
    (children.size == 1) && hasLabel(children.head, "VP")
  }

  private def expectSentence(tree : Tree, implicitQuestion : Boolean) =
  {
    if (hasLabel(tree, "S")) {
      val isQuestion =
        hasTerminalLabel(tree.children.last, ".", "?") && !implicitQuestion
      val children = truncatePunctuation(tree, Seq(".", "!", "?"))
      if (isImperative(children)) {
        expectCommand(children.head)
      } else if (children.size == 2) {
        val np = children.head
        val vp = children.last
        if ((hasLabel(np, "NP")) && (hasLabel(vp, "VP"))) {
          expectStatement(np, vp, isQuestion)
        } else {
          ShlurdUnknownSentence
        }
      } else {
        ShlurdUnknownSentence
      }
    } else if (hasLabel(tree, "SQ")) {
      val children = truncatePunctuation(tree, Seq("?"))
      if (isImperative(children)) {
        expectCommand(children.head)
      } else  if (children.size == 3) {
        val vp = children(0)
        val np = children(1)
        val ap = children(2)
        if (!hasTerminalLabel(vp, "VBZ", "is")) {
          ShlurdUnknownSentence
        } else {
          ShlurdPredicateQuestion(
            expectPredicate(np, ap))
        }
      } else {
        ShlurdUnknownSentence
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectStatement(np : Tree, vp : Tree, isQuestion : Boolean) =
  {
    if (hasTerminalLabel(vp.firstChild, "VBZ", "is")) {
      val predicate = expectPredicate(np, vp.lastChild)
      if (isQuestion) {
        ShlurdPredicateQuestion(predicate)
      } else {
        ShlurdPredicateStatement(predicate)
      }
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectCommand(vp : Tree) =
  {
    if (vp.numChildren == 2) {
      val state = expectCommandState(vp.firstChild)
      val subject = expectSubject(vp.lastChild)
      ShlurdStateChangeCommand(
        ShlurdStatePredicate(subject, state))
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectSubject(np : Tree) =
  {
    if (np.numChildren == 2) {
      val determiner = np.firstChild
      val nounHead = np.lastChild
      if (hasLabel(determiner, "DT")) {
        if (hasLabel(nounHead, "NN")) {
          val noun = nounHead.firstChild
          if (hasLabel(noun, "door")) {
            ShlurdFrontDoor
          } else {
            ShlurdUnknownSubject
          }
        } else {
          ShlurdUnknownSubject
        }
      } else {
        ShlurdUnknownSubject
      }
    } else {
      ShlurdUnknownSubject
    }
  }

  private def expectCommandState(verb : Tree) =
  {
    if (verb.isPreTerminal && hasLabel(verb, "VB")) {
      getLabel(verb.firstChild) match {
        case "open" => ShlurdDoorIsOpen
        case "close" => ShlurdDoorIsClosed
        case _ => ShlurdUnknownState
      }
    } else {
      ShlurdUnknownState
    }
  }

  private def expectState(ap : Tree) =
  {
    if (hasLabel(ap, "JJ") && ap.isPreTerminal) {
      if (hasLabel(ap.firstChild, "open")) {
        ShlurdDoorIsOpen
      } else if (hasLabel(ap.firstChild, "closed")) {
        ShlurdDoorIsClosed
      } else {
        ShlurdUnknownState
      }
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPredicate(np : Tree, complement : Tree) =
  {
    if (hasLabel(complement, "ADJP")) {
      val subject = expectSubject(np)
      val state = expectState(complement.firstChild)
      ShlurdStatePredicate(subject,state)
    } else {
      ShlurdUnknownPredicate
    }
  }

  private def parseIfPunctuated(input : String, implicitQuestion : Boolean)
      : Option[ShlurdSentence] =
  {
    val doc = new Document(input)
    val sentences = doc.sentences.asScala
    assert(sentences.size == 1)
    val sentence = sentences.head
    val tree = sentence.parse
    if (tree.preTerminalYield.asScala.last.value ==  "."){
      Some(expectRoot(tree, implicitQuestion))
    } else {
      None
    }
  }

  def parse(input : String) : ShlurdSentence =
  {
    parseIfPunctuated(input, false).getOrElse(
      parseIfPunctuated(input + "?", true).getOrElse(ShlurdUnknownSentence))
  }
}
