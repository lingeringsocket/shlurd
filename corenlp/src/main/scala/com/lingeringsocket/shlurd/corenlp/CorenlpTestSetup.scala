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
package com.lingeringsocket.shlurd.corenlp

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.nlang._

import edu.stanford.nlp.simple._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.ling._
import edu.stanford.nlp.simple.Document

import scala.collection.JavaConverters._

import java.util._

import SprPennTreebankLabels._
import SnlEnglishLemmas._
import SprUtils._

class CorenlpTestSetup
{
  SprParser.setStrategy(CorenlpParsingStrategy)
}

class CorenlpTokenizedSentence(val corenlpSentence : Sentence)
    extends SprTokenizedSentence
{
  override def text = corenlpSentence.text

  override def tokens = corenlpSentence.tokens.asScala.map(t =>
    SprToken(t.originalText, t.beginPosition, t.endPosition))

  // FIXME
  override def offsetText = text

  def lemmas = corenlpSentence.lemmas.asScala

  def incomingDeps =
  {
    val props = new Properties
    props.setProperty(
      "depparse.model",
      "edu/stanford/nlp/models/parser/nndep/english_SD.gz")
    corenlpSentence.incomingDependencyLabels(props).asScala.map(_.orElse(""))
  }
}

class CorenlpTokenizer extends SprTokenizer
{
  override def tokenize(input : String) : Seq[CorenlpTokenizedSentence] =
  {
    val doc = new Document(input)
    doc.sentences.asScala.map(sentence => {
      new CorenlpTokenizedSentence(sentence)
    })
  }
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

object CorenlpParsingStrategy extends SprParsingStrategy
{
  override def newTokenizer = new CorenlpTokenizer

  override def isCoreNLP : Boolean = true

  private implicit val tongue = SnlUtils.defaultTongue

  override def prepareParser(
    context : SprContext,
    sentence : SprTokenizedSentence,
    dump : Boolean) =
  {
    val tokens = sentence.tokens
    val sentenceString = sentence.text
    if (SprParser.isTerminator(tokens.last.text)) {
      prepareCorenlpFallbacks(
        context,
        sentenceString, tokens.map(_.text), false, dump, "CORENLP")
    } else {
      val questionString = sentenceString + LABEL_QUESTION_MARK
      prepareCorenlpFallbacks(
        context,
        questionString, tokens.map(_.text) :+ LABEL_QUESTION_MARK,
        true, dump, "CORENLP")
    }
  }

  private def prepareCorenlpFallbacks(
    context : SprContext,
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
    def main() = prepareCorenlp(
      context,
      capitalizedString, tokens, props, true, guessedQuestion,
      dump, dumpPrefix + " RNN")
    def fallbackSR() = prepareCorenlp(
      context,
      capitalizedString, tokens, propsSR, true, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR")
    def fallbackPCFG() = prepareCorenlp(
      context,
      capitalizedString, tokens, propsPCFG, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK PCFG")
    def fallbackSRCASELESS() = prepareCorenlp(
      context,
      sentenceString, tokens, propsSR, false, guessedQuestion,
      dump, dumpPrefix + " FALLBACK SR CASELESS")
    new SprFallbackParser(Seq(
      () => main, () => fallbackSR, () => fallbackPCFG,
      () => fallbackSRCASELESS))
  }

  private def prepareCorenlp(
    context : SprContext,
    sentenceString : String, tokens : Seq[String], props : Properties,
    preDependencies : Boolean, guessedQuestion : Boolean,
    dump : Boolean, dumpPrefix : String) =
  {
    def corenlpParse() : SprSyntaxTree = {
      var deps : Seq[String] = Seq.empty
      val sentence = tokenizeCorenlp(sentenceString).head
      if (preDependencies) {
        // when preDependencies is requested, it's important to analyze
        // dependencies BEFORE parsing in order to get the best parse
        deps = sentence.incomingDeps
      }
      val corenlpTree = sentence.corenlpSentence.parse(props)
      if (dump) {
        println(dumpPrefix + " PARSE = " + corenlpTree)
      }
      corenlpTree.indexLeaves(0, true)
      if (!preDependencies) {
        // when preDependencies is not requested, it's important to analyze
        // dependencies AFTER parsing in order to get the best parse
        deps = sentence.incomingDeps
      }
      val lemmas = sentence.lemmas
      if (dump) {
        println(dumpPrefix + " DEPS = " + tokens.zip(deps))
      }
      SprSyntaxRewriter.rewriteAbstract(
        new CorenlpTreeWrapper(corenlpTree, tokens, lemmas, deps))
    }

    val syntaxTree = SprParser.cacheParse(
      SprParser.CacheKey(sentenceString, dumpPrefix), () => corenlpParse)
    val rewrittenTree = rewriteWarts(syntaxTree)
    if (dump) {
      println(dumpPrefix + " REWRITTEN SYNTAX = " + rewrittenTree)
    }
    new SprSingleParser(context, rewrittenTree, guessedQuestion)
  }

  private def tokenizeCorenlp(input : String)
      : Seq[CorenlpTokenizedSentence] =
  {
    val tokenizer = new CorenlpTokenizer
    tokenizer.tokenize(input)
  }

  def rewriteWarts = SprSyntaxRewriter.rewrite {
    case np @ SptNP(children @ _*) if (children.count(_.isDeterminer) == 2)=> {
      val iFirst = children.indexWhere(_.isDeterminer)
      assert(iFirst >= 0)
      val iSecond = children.indexWhere(_.isDeterminer, iFirst + 1)
      assert(iSecond >= 0)
      if (iSecond == iFirst + 1) {
        np
      } else {
        SptNP(
          SptNP(children.take(iSecond):_*),
          SptNP(children.drop(iSecond):_*)
        )
      }
    }
    case SptVP(
      vbz @ SptVBZ(vb),
      SptNP(
        np : SptNP,
        pp @ SptPP(
          _ : SptTO,
          _
        )
      )
    ) if (vb.lemma == LEMMA_BE) => {
      SptVP(vbz, np, pp)
    }
    case vp @ SptVP(children @ _*) => {
      def pullUpNP(child : SprSyntaxTree) = {
        child match {
          case SptNP(grand @ _*) if (grand.forall(_.isNounPhrase)) => {
            grand
          }
          case _ => Seq(child)
        }
      }
      SptVP(children.flatMap(pullUpNP):_*)
    }
    case SptNP(
      SptNP(SptCC(dt), n1),
      SptCC(cc),
      n2
    ) if (dt.hasLemma(LEMMA_EITHER)) => {
      SptNP(
        SptCC(dt),
        n1,
        SptCC(cc),
        n2)
    }
    case np : SptNP if (np.containsIncomingDependency("tmod")) => {
      SptTMOD(np)
    }
    case SptNP(
      SptNP(nn : SptNN),
      pp : SptPP
    ) => {
      SptADVP(
        SptNP(nn),
        pp
      )
    }
    case SptS(
      SptVP(
        SptSQ(
          vb : SprSyntaxVerb,
          SptS(children @ _*)
        )
      ),
      remainder @ _*
    ) => {
      SptSQ((Seq(vb) ++ children ++ remainder):_*)
    }
    case SptS(
      SptVP(
        SptVBG(vbg),
        SptSBAR(
          dem : SptIN,
          SptS(children @ _*))
      ),
      remainder @ _*
    ) if (dem.isDemonstrative) => {
      SptS(
        (Seq(SptPP(
          SptIN(vbg),
          SptDT(dem.child)
        )) ++ children ++ remainder):_*
      )
    }
  }
}
