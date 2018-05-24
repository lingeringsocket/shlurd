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

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.io._
import scala.collection.JavaConverters._

import java.io._
import java.util.Properties

import ShlurdPennTreebankLabels._
import ShlurdParseUtils._

trait ShlurdParser
{
  def parseOne() : SilSentence

  def parseFirst() : SilSentence

  def parseAll() : Seq[SilSentence]
}

class ShlurdFallbackParser(
  parsers : Seq[() => ShlurdParser])
    extends ShlurdParser
{
  override def parseOne() : SilSentence =
  {
    var best : Option[SilSentence] = None
    var bestCount = Int.MaxValue
    parsers.foreach(parserSupplier => {
      val parser = parserSupplier()
      val sentence = parser.parseOne
      if (!sentence.hasUnknown) {
        return sentence
      }
      // if not even one parser produces a complete parse, choose
      // the one with the minimum number of unparsed leaves
      val count = sentence.countUnknownSyntaxLeaves
      if (count < bestCount) {
        best = Some(sentence)
      }
    })
    best.get
  }

  override def parseFirst() = parseOne

  override def parseAll() = Seq(parseOne)
}

class ShlurdSingleParser(
  tree : ShlurdSyntaxTree, tokens : Seq[String], lemmas : Seq[String],
  guessedQuestion : Boolean)
    extends ShlurdParser
{
  private def parseRoot(tree : ShlurdSyntaxTree) =
  {
    tree match {
      case SptROOT(sentenceSyntaxTree) => {
        val parsingRewriter = new ShlurdParsingRewriter(
          new ShlurdSyntaxAnalyzer(guessedQuestion))
        parsingRewriter.parseSentence(sentenceSyntaxTree)
      }
      case _ => SilUnrecognizedSentence(tree)
    }
  }

  override def parseOne() = parseRoot(tree)

  override def parseFirst() = parseOne

  override def parseAll() = Seq(parseOne)
}

class ShlurdMultipleParser(singles : Seq[ShlurdParser])
    extends ShlurdParser
{
  override def parseOne() : SilSentence =
  {
    assert(singles.size == 1)
    parseFirst
  }

  override def parseFirst() = singles.head.parseOne

  override def parseAll() = singles.map(_.parseOne)
}

class CorenlpTreeWrapper(
  corenlp : Tree, tokens : Seq[String], lemmas : Seq[String])
    extends ShlurdAbstractSyntaxTree
{
  private val wrappedChildren =
    corenlp.children.map(new CorenlpTreeWrapper(_, tokens, lemmas))

  override def label =
    corenlp.label.value.split("-").head

  override def lemma =
    lemmas(corenlp.label.asInstanceOf[HasIndex].index)

  override def token = tokens(corenlp.label.asInstanceOf[HasIndex].index)

  override def children = wrappedChildren
}

object ShlurdParser
{
  def getEmptyDocument() = new Document("")

  def debug(s : String)
  {
    tokenize(s).foreach(sentence => {
      val parser = prepareOne(sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }

  private def tokenize(input : String) : Seq[Sentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala
  }

  private def prepareOne(
    sentence : Sentence, dump : Boolean = false) : ShlurdParser =
  {
    val tokens = sentence.originalTexts.asScala
    val sentenceString = sentence.text
    val punctuation = Set(
      LABEL_DOT, LABEL_QUESTION_MARK, LABEL_EXCLAMATION_MARK)
    if (punctuation.contains(tokens.last)) {
      prepareFallbacks(
        sentenceString, tokens, false, dump, "PUNCTUATED")
    } else {
      val questionString = sentenceString + LABEL_QUESTION_MARK
      prepareFallbacks(
        questionString, tokens :+ LABEL_QUESTION_MARK,
        true, dump, "GUESSED QUESTION")
    }
  }

  private def prepareFallbacks(
    sentenceString : String, tokens : Seq[String],
    guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    val props = new Properties
    props.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    val props2 = new Properties
    props2.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
    val props3 = new Properties
    props3.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
    val capitalizedString = capitalize(sentenceString)
    def main() = prepareParser(
      capitalizedString, tokens, props, true, guessedQuestion,
      dump, dumpPrefix + " RNN")
    def fallback2() = prepareParser(
      capitalizedString, tokens, props2, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR")
    def fallback3() = prepareParser(
      capitalizedString, tokens, props3, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK PCFG")
    def fallback4() = prepareParser(
      sentenceString, tokens, props2, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR CASELESS")
    new ShlurdFallbackParser(Seq(
      main, fallback2, fallback3, fallback4))
  }

  private def prepareParser(
    sentenceString : String, tokens : Seq[String], props : Properties,
    needDependencies : Boolean, guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    val sentence = tokenize(sentenceString).head
    if (needDependencies) {
      // It's important to analyze dependencies BEFORE parsing in
      // order to get the best parse
      analyzeDependencies(sentence)
    }
    val corenlp = sentence.parse(props)
    if (dump) {
      println(dumpPrefix + " PARSE = " + corenlp)
    }
    corenlp.indexLeaves(0, true)
    val lemmas = sentence.lemmas.asScala
    val syntaxTree = ShlurdSyntaxRewrite.rewriteAbstract(
      new CorenlpTreeWrapper(corenlp, tokens, lemmas))
    val rewrittenTree = ShlurdSyntaxRewrite.rewriteEither(syntaxTree)
    if (dump) {
      println(dumpPrefix + " REWRITE = " + rewrittenTree)
    }
    new ShlurdSingleParser(rewrittenTree, tokens, lemmas, guessedQuestion)
  }

  private def analyzeDependencies(sentence : Sentence) =
  {
    val props = new Properties
    props.setProperty(
      "depparse.model",
      "edu/stanford/nlp/models/parser/nndep/english_SD.gz")
    sentence.dependencyGraph(props)
  }

  def getResourcePath(resource : String) =
    getClass.getResource(resource).getPath

  def getResourceFile(resource : String) =
    new File(getResourcePath(resource))

  def readResource(resource : String) : String =
    Source.fromFile(getResourcePath(resource)).
      getLines.mkString("\n")

  def apply(input : String) : ShlurdParser =
  {
    val sentences = tokenize(input)
    if (sentences.size == 1) {
      prepareOne(sentences.head)
    } else {
      new ShlurdMultipleParser(sentences.map(prepareOne(_)))
    }
  }
}
