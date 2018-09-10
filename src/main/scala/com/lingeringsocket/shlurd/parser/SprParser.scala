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

import com.lingeringsocket.shlurd.ilang._

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.io._
import scala.collection._
import scala.collection.JavaConverters._

import java.io._
import java.util.Properties

import SprPennTreebankLabels._
import SprUtils._

trait SprParser
{
  def parseOne() : SilSentence

  def parseFirst() : SilSentence

  def parseAll() : Stream[SilSentence]
}

class SprFallbackParser(
  parsers : Seq[() => SprParser])
    extends SprParser
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

  override def parseAll() = Stream(parseOne)
}

class SprSingleParser(
  tree : SprSyntaxTree, guessedQuestion : Boolean)
    extends SprParser
{
  private def parseRoot(tree : SprSyntaxTree) =
  {
    tree match {
      case SptROOT(sentenceSyntaxTree) => {
        val parsingRewriter = new SprPhraseRewriter(
          new SprEnglishSyntaxAnalyzer(guessedQuestion))
        val parsed = parsingRewriter.parseSentence(sentenceSyntaxTree)
        val normalizationRewriter = new SprNormalizationRewriter
        normalizationRewriter.normalize(parsed)
      }
      case _ => SilUnrecognizedSentence(tree)
    }
  }

  override def parseOne() = parseRoot(tree)

  override def parseFirst() = parseOne

  override def parseAll() = Stream(parseOne)
}

class SprMultipleParser(singles : Stream[SprParser])
    extends SprParser
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
  corenlp : Tree, tokens : Seq[String], lemmas : Seq[String],
  incomingDeps : Seq[String])
    extends SprAbstractSyntaxTree
{
  private val wrappedChildren =
    corenlp.children.map(
      new CorenlpTreeWrapper(_, tokens, lemmas, incomingDeps))

  override def label =
    corenlp.label.value.split("-").head

  override def tags =
    corenlp.label.value.split("-").tail.toSet

  override def lemma =
    lemmas(corenlp.label.asInstanceOf[HasIndex].index)

  override def token = tokens(corenlp.label.asInstanceOf[HasIndex].index)

  override def incomingDep =
    incomingDeps(corenlp.label.asInstanceOf[HasIndex].index)

  override def children = wrappedChildren
}

object SprParser
{
  case class CacheKey(
    sentence : String,
    config : String
  )

  type CacheValue = SprSyntaxTree

  private val terminators = Set(
    LABEL_DOT, LABEL_QUESTION_MARK, LABEL_EXCLAMATION_MARK)

  private var cache : Option[mutable.Map[CacheKey, CacheValue]] = None

  private var cacheOnly : Boolean = false

  private var cacheDirty : Boolean = false

  private var cacheFile : Option[File] = None

  def getEmptyDocument() = new Document("")

  def debug(s : String)
  {
    tokenize(s).foreach(sentence => {
      val parser = prepareOne(sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }

  def enableCache(file : Option[File] = None)
  {
    cacheFile = file
    cacheDirty = false
    cache = file.filter(_.exists).map(loadCache).orElse(
      Some(new concurrent.TrieMap[CacheKey, CacheValue]))
  }

  def lockCache() : Map[CacheKey, CacheValue] =
  {
    cacheOnly = true
    cache.get
  }

  private def loadCache(file : File) =
  {
    val fileIn = new FileInputStream(file)
    try {
      // https://stackoverflow.com/a/22375260/2913158
      val objIn = new ObjectInputStream(fileIn) {
        override def resolveClass(
          desc: java.io.ObjectStreamClass): Class[_] =
        {
          try {
            Class.forName(desc.getName, false, getClass.getClassLoader)
          } catch {
            case ex : ClassNotFoundException => super.resolveClass(desc)
          }
        }
      }
      objIn.readObject.asInstanceOf[mutable.Map[CacheKey, CacheValue]]
    } finally {
      fileIn.close
    }
  }

  def saveCache()
  {
    if (cacheDirty) {
      cacheFile.foreach(file => {
        cache.foreach(c => {
          val fileOut = new FileOutputStream(file)
          try {
            val objOut = new ObjectOutputStream(fileOut)
            objOut.writeObject(c)
            objOut.flush
            cacheDirty = false
          } finally {
            fileOut.close
          }
        })
      })
    }
  }

  private def cacheParse(
    key : CacheKey,
    parse : () => CacheValue) : CacheValue =
  {
    cache.map(_.getOrElseUpdate(key, {
      cacheDirty = true
      parse()
    })).getOrElse(parse())
  }

  def tokenize(input : String) : Seq[Sentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala
  }

  def isTerminator(token : String) : Boolean =
  {
    terminators.contains(token)
  }

  private def prepareOne(
    sentence : Sentence, dump : Boolean = false) : SprParser =
  {
    val tokens = sentence.originalTexts.asScala
    val sentenceString = sentence.text
    if (isTerminator(tokens.last)) {
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
    val propsSR = new Properties
    propsSR.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
    val propsPCFG = new Properties
    propsPCFG.setProperty(
      "parse.model",
      "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
    val capitalizedString = capitalize(sentenceString)
    def main() = prepareParser(
      capitalizedString, tokens, props, true, guessedQuestion,
      dump, dumpPrefix + " RNN")
    def fallbackSR() = prepareParser(
      capitalizedString, tokens, propsSR, true, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR")
    def fallbackPCFG() = prepareParser(
      capitalizedString, tokens, propsPCFG, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK PCFG")
    def fallbackSRCASELESS() = prepareParser(
      sentenceString, tokens, propsSR, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR CASELESS")
    new SprFallbackParser(Seq(
      main, fallbackSR, fallbackPCFG, fallbackSRCASELESS))
  }

  private def prepareParser(
    sentenceString : String, tokens : Seq[String], props : Properties,
    preDependencies : Boolean, guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    def corenlpParse() : SprSyntaxTree = {
      if (cacheOnly) {
        val oops = "OOPS"
        return SprSyntaxLeaf(oops, oops, oops)
      }
      var deps : Seq[String] = Seq.empty
      val sentence = tokenize(sentenceString).head
      if (preDependencies) {
        // when preDependencies is requested, it's important to analyze
        // dependencies BEFORE parsing in order to get the best parse
        deps = analyzeDependencies(sentence)
      }
      val corenlp = sentence.parse(props)
      if (dump) {
        println(dumpPrefix + " PARSE = " + corenlp)
      }
      corenlp.indexLeaves(0, true)
      if (!preDependencies) {
        // when preDependencies is not requested, it's important to analyze
        // dependencies AFTER parsing in order to get the best parse
        deps = analyzeDependencies(sentence)
      }
      val lemmas = sentence.lemmas.asScala
      if (dump) {
        println(dumpPrefix + " DEPS = " + tokens.zip(deps))
      }
      SprSyntaxRewriter.rewriteAbstract(
        new CorenlpTreeWrapper(corenlp, tokens, lemmas, deps))
    }

    val syntaxTree = cacheParse(
      CacheKey(sentenceString, dumpPrefix), corenlpParse)
    val rewrittenTree = SprSyntaxRewriter.rewriteWarts(syntaxTree)
    if (dump) {
      println(dumpPrefix + " REWRITTEN SYNTAX = " + rewrittenTree)
    }
    new SprSingleParser(rewrittenTree, guessedQuestion)
  }

  private def analyzeDependencies(sentence : Sentence) : Seq[String] =
  {
    val props = new Properties
    props.setProperty(
      "depparse.model",
      "edu/stanford/nlp/models/parser/nndep/english_SD.gz")
    sentence.incomingDependencyLabels(props).asScala.map(_.orElse(""))
  }

  def getResourcePath(resource : String) =
    getClass.getResource(resource).getPath

  def getResourceFile(resource : String) =
    new File(getResourcePath(resource))

  def readResource(resource : String) : String =
    Source.fromFile(getResourcePath(resource)).
      getLines.mkString("\n")

  def apply(input : String) : SprParser =
  {
    val sentences = tokenize(input)
    if (sentences.size == 1) {
      prepareOne(sentences.head)
    } else {
      new SprMultipleParser(sentences.toStream.map(prepareOne(_)))
    }
  }
}
