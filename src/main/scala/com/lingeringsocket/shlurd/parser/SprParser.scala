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
import com.lingeringsocket.shlurd.ilang._

import scala.collection._

import java.io._

import SprPennTreebankLabels._

trait SprParser
{
  def parseOne() : SilSentence

  def parseFirst() : SilSentence

  def parseAll() : Stream[SilSentence] = parseAllPositional.map(_._1)

  def parseAllPositional() : Stream[(SilSentence, Int, Int)]

  def parseOnePositional() = Stream(tupleN((parseOne, 0, 0)))
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
        bestCount = count
      }
    })
    best.get
  }

  override def parseFirst() = parseOne

  override def parseAllPositional() = parseOnePositional()
}

class SprSingleParser(
  context : SprContext,
  tree : SprSyntaxTree, guessedQuestion : Boolean)
    extends SprParser
{
  protected def normalize(sentence : SilSentence) : SilSentence =
  {
    val normalizationRewriter = new SprNormalizationRewriter(context)
    normalizationRewriter.normalize(sentence)
  }

  private def parseRoot(tree : SprSyntaxTree) =
  {
    tree match {
      case SptROOT(sentenceSyntaxTree) => {
        val parsingRewriter = new SprPhraseRewriter(
          context,
          new SprEnglishSyntaxAnalyzer(context.annotator, guessedQuestion))
        val parsed = parsingRewriter.parseSentence(sentenceSyntaxTree)
        normalize(parsed)
      }
      case _ => SilUnrecognizedSentence(tree)
    }
  }

  override def parseOne() = parseRoot(tree)

  override def parseFirst() = parseOne

  override def parseAllPositional() = parseOnePositional()
}

class SprSingleHeuristicParser(
  context : SprContext,
  tree : SprSyntaxTree, terminator : Option[String]
) extends SprSingleParser(context, tree, false)
{
  override def parseOne() =
  {
    val sentence = super.parseOne
    val (addInterrogative, addExclamation) = {
      terminator match {
        case Some(LABEL_QUESTION_MARK) => (true, false)
        case Some(LABEL_EXCLAMATION_MARK) => (false, true)
        case _ => (false, false)
      }
    }
    val tam = {
      if (addInterrogative) {
        sentence match {
          case _ : SilConditionalSentence => sentence.tam
          case _ => {
            sentence.tam.withMood(MOOD_INTERROGATIVE)
          }
        }
      } else {
        sentence.tam
      }
    }
    val formality = {
      if (addExclamation) {
        sentence.formality.copy(force = FORCE_EXCLAMATION)
      } else {
        sentence.formality
      }
    }
    normalize(sentence.withNewTamFormality(tam, formality))
  }
}

class SprAmbiguityParser(
  context : SprContext,
  singles : Seq[SprParser])
    extends SprParser
{
  override def parseOne() =
  {
    val alternatives = singles.map(_.parseOne)
    val ambiguous = SilAmbiguousSentence(alternatives)
    val resolver = new SprAmbiguityResolver(context)
    resolver.resolveAmbiguousSentence(ambiguous)
  }

  override def parseFirst() = parseOne

  override def parseAllPositional() = parseOnePositional()
}

class SprDelimitedParser(
  context : SprContext,
  singles : Seq[SprParser],
  determiner : SilDeterminer,
  separator : SilSeparator)
    extends SprParser
{
  override def parseOne() =
  {
    val sentences = singles.map(_.parseOne)
    SilConjunctiveSentence(
      determiner,
      sentences,
      separator)
  }

  override def parseFirst() = parseOne

  override def parseAllPositional() = parseOnePositional()
}

class SprMultipleParser(singles : Stream[(SprParser, Int, Int)])
    extends SprParser
{
  override def parseOne() : SilSentence =
  {
    assert(singles.size == 1)
    parseFirst
  }

  override def parseFirst() = singles.head._1.parseOne

  override def parseAllPositional() = {
    singles.map(single => tupleN((single._1.parseOne, single._2, single._3)))
  }
}

trait SprParsingStrategy
{
  def newTokenizer : SprTokenizer

  def isCoreNLP : Boolean = false

  def prepareParser(
    context : SprContext,
    sentence : SprTokenizedSentence,
    dump : Boolean) : SprParser
}

object SprHeuristicParsingStrategy extends SprParsingStrategy
{
  override def newTokenizer = new SprIxaTokenizer

  override def prepareParser(
    context : SprContext,
    sentence : SprTokenizedSentence,
    dump : Boolean) =
  {
    SprParser.prepareHeuristic(context, sentence, dump, "HEURISTIC")
  }
}

object SprParser extends SprEnglishWordAnalyzer
{
  case class CacheKey(
    sentence : String,
    config : String
  )

  type CacheValue = SprSyntaxTree

  val ONCE_UPON_A_TIME = "once upon a time"

  private val terminators = Set(
    LABEL_DOT, LABEL_QUESTION_MARK, LABEL_EXCLAMATION_MARK)

  private var cache : Option[mutable.Map[CacheKey, CacheValue]] = None

  private var cacheOnly : Boolean = false

  private var cacheDirty : Boolean = false

  private var cacheFile : Option[File] = None

  private var strategy : SprParsingStrategy = SprHeuristicParsingStrategy

  def debug(s : String)
  {
    tokenize(s).foreach(sentence => {
      val parser = prepareOne(SprContext(), sentence, true)
      println("SHLURD = " + parser.parseOne)
    })
  }

  def isIgnorableLine(line : String) : Boolean =
  {
    val trimmed = line.trim
    trimmed.isEmpty || trimmed.startsWith("//")
  }

  def setStrategy(newStrategy : SprParsingStrategy)
  {
    strategy = newStrategy
  }

  def isCoreNLP : Boolean = strategy.isCoreNLP

  def enableCache(file : Option[File] = None)
  {
    this.synchronized {
      if (cache.isEmpty) {
        cacheFile = file
        cacheDirty = false
        cache = file.filter(_.exists).map(loadCache).orElse(
          Some(new concurrent.TrieMap[CacheKey, CacheValue]))
      }
    }
  }

  def lockCache() : Map[CacheKey, CacheValue] =
  {
    cacheOnly = true
    cache.get
  }

  def getCache() : Map[CacheKey, CacheValue] =
  {
    cache.get
  }

  private def loadCache(file : File) =
  {
    SerializationUtils.deserialize[mutable.Map[CacheKey, CacheValue]](file)
  }

  def saveCache()
  {
    this.synchronized {
      if (cacheDirty) {
        cacheFile.foreach(file => {
          cache.foreach(c => {
            if (file.exists) {
              val oldCache = loadCache(file)
              c ++= oldCache
            }
            SerializationUtils.serialize(c, file)
            cacheDirty = false
          })
        })
      }
    }
  }

  // FIXME should take SprContext into account
  def cacheParse(
    key : CacheKey,
    parse : () => CacheValue) : CacheValue =
  {
    cache.map(_.getOrElseUpdate(key, {
      if (cacheOnly) {
        val oops = "OOPS"
        SprSyntaxLeaf(oops, oops, oops)
      } else {
        cacheDirty = true
        parse()
      }
    })).getOrElse(parse())
  }

  def tokenize(input : String) : Seq[SprTokenizedSentence] =
  {
    val tokenizer = strategy.newTokenizer
    tokenizer.tokenize(input)
  }

  def isTerminator(token : String) : Boolean =
  {
    terminators.contains(token)
  }

  private def prepareOne(
    context : SprContext,
    sentence : SprTokenizedSentence,
    dump : Boolean = false) : SprParser =
  {
    strategy.prepareParser(context, sentence, dump)
  }

  private[parser] def prepareHeuristic(
    context : SprContext,
    sentenceString : String,
    dump : Boolean, dumpDesc : String) : SprParser =
  {
    val sentence = tokenize(sentenceString).head
    prepareHeuristic(context, sentence, dump, dumpDesc)
  }

  private def collapseQuotations(
    sentence : SprTokenizedSentence) : Seq[String] =
  {
    collapseQuotations(sentence, sentence.tokens)
  }

  private def collapseQuotations(
    sentence : SprTokenizedSentence,
    tokens : Seq[SprToken]) : Seq[String] =
  {
    // FIXME deal with nested quotations?
    val tokensText = tokens.map(_.text)
    val left = tokensText.indexOf(LABEL_LQUOTE)
    if (left == -1) {
      tokensText
    } else {
      val right = tokensText.indexOf(LABEL_RQUOTE, left + 1)
      if (right == -1) {
        tokensText
      } else {
        val quotation = DQUOTE +
          sentence.offsetText.slice(tokens(left).end, tokens(right).start) +
          DQUOTE
        tokensText.take(left) ++
          Seq(quotation) ++
          collapseQuotations(sentence, tokens.drop(right + 1))
      }
    }
  }

  private def refineTokens(sentence : SprTokenizedSentence) : Seq[String] =
  {
    val seq = collapseQuotations(sentence)
    seq.flatMap(word => word.toLowerCase match {
      case "cannot" => {
        if (word.head.isUpper) {
          Seq("Can", "not")
        } else {
          Seq("can", "not")
        }
      }
      case _ => Seq(word)
    })
  }

  private[parser] def prepareHeuristic(
    context : SprContext,
    sentence : SprTokenizedSentence,
    dump : Boolean, dumpDesc : String) : SprParser =
  {
    val dumpPrefix = dumpDesc
    val allWords = refineTokens(sentence)
    val (unterminatedWords, terminator) = {
      if (isTerminator(allWords.last)) {
        tupleN((allWords.dropRight(1), Some(allWords.last)))
      } else {
        tupleN((allWords, None))
      }
    }
    def heuristicParseOne(words : Seq[String]) : SprSyntaxTree =
    {
      val synthesizer = {
        new SprHeuristicSynthesizer(
          context,
          SprHeuristicAcceptCompleteSentence,
          HEURISTIC_STAMINA_COMPLETE, words)
      }
      val analysis = synthesizer.analyzeWords
      if (dump) {
        println
        println("LEXICAL ANALYSIS")
        println
        words.zip(analysis).foreach {
          case (word, preTerminals) => {
            print(s"WORD:  " + word)
            preTerminals.foreach(pt => {
              print(pt)
              print(" -> ")
              print(pt.firstChild.lemma)
            })
            println
            println
          }
        }
      }
      val treeSet = new mutable.HashSet[SprSyntaxTree]
      // FIXME handle TOO SLOW excn
      synthesizer.synthesize(analysis).foreach(tree => {
        if (!treeSet.contains(tree)) {
          treeSet += tree
        }
      })
      if (false) {
        synthesizer.displayGraph(treeSet)
      }
      if (dump) {
        println("COST = " + synthesizer.getCost)
      }
      if (dump) {
        println(dumpPrefix + " PARSE = " + treeSet)
      }
      if (treeSet.isEmpty) {
        SptROOT(SptS(SprHeuristicSynthesizer.npSomething))
      } else if (treeSet.size == 1) {
        SptROOT(treeSet.head)
      } else {
        SptAMBIGUOUS(treeSet.toSeq:_*)
      }
    }
    def heuristicParse() : SprSyntaxTree =
    {
      val splitters = unterminatedWords.indices.filter(
        i => unterminatedWords(i) == ";")
      if (splitters.isEmpty) {
        heuristicParseOne(unterminatedWords)
      } else {
        val brackets = (Seq(-1) ++ splitters ++ Seq(unterminatedWords.size))
        val trees = brackets.sliding(2).map {
          case Seq(iBefore, iAfter) => {
            val sub = unterminatedWords.slice(iBefore + 1, iAfter)
            heuristicParseOne(sub)
          }
        }
        val semi = SptSEMICOLON(makeLeaf(";"))
        val delimited = trees.toSeq.flatMap(tree => {
          val unwrapped = tree match {
            case SptROOT(s) => s
            case _ => tree
          }
          Seq(unwrapped, semi)
        }).dropRight(1)
        SptROOT(SptS(delimited:_*))
      }
    }
    val cachedRoot = cacheParse(
      CacheKey(sentence.text, dumpPrefix), () => heuristicParse)
    def createParser(root : SprSyntaxTree) : SprParser =
    {
      root match {
        case SptAMBIGUOUS(trees @ _*) => {
          new SprAmbiguityParser(context,
            trees.map(tree =>
              new SprSingleHeuristicParser(context, SptROOT(tree), terminator)))
        }
        case SptROOT(
          SptS(trees @ _*)
        ) if (trees.exists(_.isInstanceOf[SptAMBIGUOUS])) => {
          val singles = trees.filterNot(_.isInstanceOf[SptSEMICOLON]).map(
            tree => createParser(tree)
          )
          new SprDelimitedParser(
            context,
            singles,
            DETERMINER_UNSPECIFIED,
            SEPARATOR_SEMICOLON)
        }
        case _ : SptROOT => {
          new SprSingleHeuristicParser(
            context,
            root,
            terminator)
        }
        case other => {
          new SprSingleHeuristicParser(
            context,
            SptROOT(other),
            terminator)
        }
      }
    }
    createParser(cachedRoot)
  }

  // FIXME Mickey Mouse
  def interpretTemporal(ref : SilReference) : Int =
  {
    ref match {
      case SilMandatorySingular(
        SilNounReference(
          SilWordLemma(lemma)
        )
      ) => {
        lemma.toLowerCase match {
          case ONCE_UPON_A_TIME => Int.MinValue
          case "yesterday" => -1
          case _ => throw new IllegalArgumentException
        }
      }
      case SilGenitiveReference(
        SilPronounReference(
          PERSON_THIRD, GENDER_N, COUNT_SINGULAR, DISTANCE_HERE),
        SilMandatorySingular(
          SilNounReference(
            SilWordLemma(lemma)
          )
        )
      ) => {
        lemma.toLowerCase match {
          case "morning" => 1
          case "afternoon" => 2
          case "evening" => 3
          case _ => throw new IllegalArgumentException
        }
      }
      case _ => {
        throw new IllegalArgumentException
      }
    }
  }

  def apply(input : String) : SprParser =
    apply(input, SprContext())

  def apply(
    input : String,
    context : SprContext) : SprParser =
  {
    val sentences = tokenize(input)
    if (sentences.size == 1) {
      prepareOne(context, sentences.head)
    } else {
      new SprMultipleParser(sentences.toStream.map(tokenizedSentence => {
        val start = tokenizedSentence.tokens.head.start
        val end = tokenizedSentence.tokens.last.end
        tupleN((prepareOne(context, tokenizedSentence), start, end))
      }))
    }
  }
}
