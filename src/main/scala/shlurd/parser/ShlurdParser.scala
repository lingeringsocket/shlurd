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
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.collection.JavaConverters._

import ShlurdQuantifier._

class ShlurdParser(
  tree : Tree, lemmas : Seq[String], implicitQuestion : Boolean)
{
  private def getLabel(tree : Tree) : String =
  {
    tree.label.value
  }

  private def hasLabel(tree : Tree, label : String) : Boolean =
  {
    getLabel(tree) == label
  }

  private def isVerb(verbHead : Tree) : Boolean =
  {
    getLabel(verbHead).startsWith("VB")
  }

  private def isNoun(nounHead : Tree) : Boolean =
  {
    getLabel(nounHead).startsWith("NN")
  }

  private def isAdjective(adjHead : Tree) : Boolean =
  {
    getLabel(adjHead).startsWith("JJ")
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
        val verbHead = children(0)
        val np = children(1)
        val ap = children(2)
        if (isVerb(verbHead) && hasTerminalLemma(verbHead, "be")) {
          ShlurdPredicateQuestion(
            expectPredicate(np, ap))
        } else {
          ShlurdUnknownSentence
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
    val verbHead = vp.firstChild
    if (isVerb(verbHead) && hasTerminalLemma(verbHead, "be")) {
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
      val state = expectVerbState(vp.firstChild)
      val subject = expectReference(vp.lastChild)
      ShlurdStateChangeCommand(
        ShlurdStatePredicate(subject, state))
    } else {
      ShlurdUnknownSentence
    }
  }

  private def expectReference(np : Tree) =
  {
    val intro = np.firstChild
    val (quantifier, components) = {
      if (hasLabel(intro, "DT")) {
        (expectQuantifier(intro.firstChild), np.children.drop(1))
      } else {
        (QUANT_ANY, np.children)
      }
    }
    if (components.forall(c => isNoun(c) || isAdjective(c))) {
      ShlurdConcreteReference(
        components.map(c => getLemma(c.firstChild)).mkString(" "),
        quantifier)
    } else {
      ShlurdUnknownReference
    }
  }

  private def expectQuantifier(leaf : Tree) =
  {
    getLemma(leaf) match {
      case "no" => QUANT_NONE
      case "all" => QUANT_ALL
      case "the" => QUANT_ONE
      case _ => QUANT_ANY
    }
  }

  private def expectNounReference(
    nounHead : Tree, quantifier : ShlurdQuantifier) =
  {
    if (isNoun(nounHead)) {
      val noun = nounHead.firstChild
      ShlurdConcreteReference(getLemma(noun), quantifier)
    } else {
      ShlurdUnknownReference
    }
  }

  private def hasTerminalLemma(tree : Tree, lemma : String) =
  {
    tree.isPreTerminal && (getLemma(tree.firstChild) == lemma)
  }

  private def getLemma(leaf : Tree) : String =
  {
    lemmas(leaf.label.asInstanceOf[HasIndex].index)
  }

  private def expectVerbState(verbHead : Tree) =
  {
    if (verbHead.isPreTerminal && isVerb(verbHead)) {
      ShlurdPhysicalState(getLemma(verbHead.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectAdjectiveState(ap : Tree) =
  {
    if (isAdjective(ap) && ap.isPreTerminal) {
      ShlurdPhysicalState(getLemma(ap.firstChild))
    } else {
      ShlurdUnknownState
    }
  }

  private def expectPredicate(np : Tree, complement : Tree) =
  {
    val subject = expectReference(np)
    if (hasLabel(complement, "ADJP")) {
      val state = expectAdjectiveState(complement.firstChild)
      ShlurdStatePredicate(subject, state)
    } else if (hasLabel(complement, "VP")) {
      // TODO:  ambiguity for action (passive construction) vs
      // state (participial adjective)
      if (complement.isPrePreTerminal) {
        ShlurdStatePredicate(
          subject, expectVerbState(complement.firstChild))
      } else {
        ShlurdUnknownPredicate
      }
    } else {
      ShlurdUnknownPredicate
    }
  }

  def parse() : ShlurdSentence =
  {
    expectRoot(tree, implicitQuestion)
  }
}

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

  private def tokenize(input : String) : Sentence =
  {
    val doc = new Document(input)
    val sentences = doc.sentences.asScala
    assert(sentences.size == 1)
    sentences.head
  }

  private def newParser(
    sentence : Sentence, tree : Tree, implicitQuestion : Boolean)
      : ShlurdParser =
  {
    tree.indexLeaves(0, true)
    val lemmas = sentence.lemmas.asScala
    new ShlurdParser(tree, lemmas, implicitQuestion)
  }

  def apply(input : String) : ShlurdParser =
  {
    val sentence = tokenize(input)
    val tree = sentence.parse
    if (tree.preTerminalYield.asScala.last.value ==  ".") {
      newParser(sentence, tree, false)
    } else {
      val question = tokenize(input + "?")
      newParser(question, question.parse, true)
    }
  }
}
