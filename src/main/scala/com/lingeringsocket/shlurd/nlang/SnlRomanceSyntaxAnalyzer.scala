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
package com.lingeringsocket.shlurd.nlang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.parser._

import SprUtils._

case class SnlRomanceSyntaxFrame(
  subjectOpt : Option[SprSyntaxTree],
  auxVerbOpt : Option[SprSyntaxTree],
  mainVerb : SprSyntaxTree,
  rhsOpt : Option[SprSyntaxTree],
  modifiers : Seq[SprSyntaxTree] = Seq.empty
)

abstract class SnlRomanceSyntaxAnalyzer(
  context : SprContext,
  guessedQuestion : Boolean,
  strictness : SprStrictness,
  enforceTransitive : Boolean
) extends SnlSyntaxAnalyzer(
  context, guessedQuestion, strictness, enforceTransitive)
{
  private def detectVerb(tree : SprSyntaxTree) : Boolean =
  {
    tree.isVerbNode && tree.unwrapPhrase.isVerb
  }

  private def detectNominative(tree : SprSyntaxTree) : Boolean =
  {
    if (tree.isNounPhrase && (tree.children.size == 1)) {
      detectNominative(tree.unwrapPhrase)
    } else if (tree.isQueryNounPhrase || tree.isNoun ||
      tree.isNounPhrase || tree.isDemonstrative)
    {
      true
    } else if (tree.isPronoun) {
      val lemma = tree.firstChild.lemma
      val inflection = tongue.analyzePronoun(lemma)._4
      (inflection == INFLECT_NOMINATIVE)
    } else {
      false
    }
  }

  private def detectAdjective(tree : SprSyntaxTree) : Boolean =
  {
    tree.isAdjective || tree.isAdjectivePhrase
  }

  private def detectAdposition(tree : SprSyntaxTree) : Boolean =
  {
    tree.isAdpositionalPhrase || tree.isQueryAdpositionPhrase
  }

  private def unwrapVerbPhrases(s : Seq[SprSyntaxTree]) : Seq[SprSyntaxTree] =
  {
    s.flatMap(c => {
      if (c.isVerbPhrase) {
        unwrapVerbPhrases(c.children)
      } else {
        Seq(c)
      }
    })
  }

  protected def detectImperative(children : Seq[SprSyntaxTree]) :
      (Boolean, SilGender, SilCount, SilPoliteness)

  override protected def analyzeSentenceChildren(
    tree : SprSyntaxTree, children : Seq[SprSyntaxTree],
    mood : SilMood, force : SilForce) =
  {
    val (isNegativeAbove, extracted) =
      extractNegative(unwrapSinglePhrase(children))
    val seq = unwrapSinglePhrase(extracted)
    // FIXME support explicit (vocative) subject for imperatives,
    // possibly providing gender
    val (
      isImperative, imperativeGender, imperativeCount, imperativePoliteness
    ) = detectImperative(seq)
    if (isImperative) {
      // FIXME use isNegativeAbove
      expectCommand(
        tree,
        children.head,
        SilFormality(force, imperativePoliteness),
        imperativeGender,
        imperativeCount)
    } else {
      val (isNegativeBelow, unwrapped) =
        extractNegative(unwrapVerbPhrases(seq))
      val isNegative = combineNegatives(isNegativeAbove, isNegativeBelow)
      val iVerbs = unwrapped.zipWithIndex.filter(z => detectVerb(z._1))
      val iNoms = unwrapped.zipWithIndex.filter(z => detectNominative(z._1))
      val iAdjs = unwrapped.zipWithIndex.filter(z => detectAdjective(z._1))
      val iAdps = unwrapped.zipWithIndex.filter(z => detectAdposition(z._1))
      // FIXME deal with presence of more than one question
      val queryOpt = unwrapped.find(_.isQueryPhrase)
      val questionOpt = queryOpt.flatMap(queryPhrase => {
        // FIXME adpositions
        maybeQuestionFor(unwrapQuery(queryPhrase.children)).map(_._1)
      })
      val frames = {
        val bonusFrames = tupleN((iNoms.size, iAdjs.size)) match {
          case (0 | 1, 0) => {
            iAdps.flatMap(iAdp => {
              produceFrames(unwrapped, iVerbs, iNoms, Seq(iAdp), mood)
            })
          }
          case _ => Seq.empty
        }
        produceFrames(unwrapped, iVerbs, iNoms, iAdjs, mood) ++ bonusFrames
      }
      val tam = {
        if (questionOpt.nonEmpty) {
          SilTam.interrogative
        } else {
          SilTam.indicative.withMood(mood)
        }
      }
      val tamPolarized = tam.withPolarity(!isNegative)
      val sentences = frames.flatMap(frame => {
        analyzeFrame(
          tree, unwrapped, frame,
          questionOpt, tamPolarized, force)
      })
      if (sentences.isEmpty) {
        SilUnrecognizedSentence(tree)
      } else if (sentences.size == 1) {
        sentences.head
      } else {
        SilAmbiguousSentence(sentences)
      }
    }
  }

  private def analyzeNom(tree : SprSyntaxTree) : SprSyntaxTree =
  {
    if (tree.isQueryPhrase) {
      val seq = unwrapQuery(tree.children)
      maybeQuestionFor(seq).map({
        case (question, adpositionOpt, questionChildren) => {
          // FIXME adpositionOpt
          nounPhraseForQuestion(question, questionChildren)
        }
      }).getOrElse(tree)
    } else {
      tree
    }
  }

  private def produceFrames(
    seq : Seq[SprSyntaxTree],
    iVerbs : Seq[(SprSyntaxTree, Int)],
    iNoms : Seq[(SprSyntaxTree, Int)],
    iAdjs : Seq[(SprSyntaxTree, Int)],
    mood : SilMood
  ) : Seq[SnlRomanceSyntaxFrame] =
  {
    def firstVerb = iVerbs(0)._1.unwrapPhrase
    def secondVerb = iVerbs(1)._1.unwrapPhrase
    def firstNom = analyzeNom(iNoms(0)._1)
    def secondNom = analyzeNom(iNoms(1)._1)
    def firstAdj = iAdjs(0)._1

    val modalAdpositionPos = {
      if (iVerbs.size == 2) {
        val firstVerbPos = iVerbs(0)._2
        val secondVerbPos = iVerbs(1)._2
        val succ = seq(firstVerbPos + 1)
        if (succ.isAdposition && (secondVerbPos == firstVerbPos + 2)) {
          if (tongue.adpositionForAux(firstVerb.firstChild.lemma) ==
            succ.firstChild.lemma)
          {
            firstVerbPos + 1
          } else {
            return Seq.empty
          }
        } else {
          -1
        }
      } else {
        -1
      }
    }

    // order of produced frames is significant in that in case
    // of unresolvable ambiguity, we will take the former
    val full = tupleN((iVerbs.size, iNoms.size, iAdjs.size)) match {
      case (1, 0, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, None, firstVerb, None)
        )
      }
      case (1, 1, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), None, firstVerb, None),
          SnlRomanceSyntaxFrame(
            None, None, firstVerb, Some(firstNom))
        )
      }
      case (1, 2, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), None, firstVerb, Some(secondNom)),
          SnlRomanceSyntaxFrame(
            Some(secondNom), None, firstVerb, Some(firstNom))
        )
      }
      case (2, 0, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, Some(firstVerb), secondVerb, None)
        )
      }
      case (2, 1, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), Some(firstVerb), secondVerb, None),
          SnlRomanceSyntaxFrame(
            None, Some(firstVerb), secondVerb, Some(firstNom))
        )
      }
      case (2, 2, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), Some(firstVerb), secondVerb, Some(secondNom)),
          SnlRomanceSyntaxFrame(
            Some(secondNom), Some(firstVerb), secondVerb, Some(firstNom))
        )
      }
      case (1, 0, 1) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, None, firstVerb, Some(firstAdj))
        )
      }
      case (1, 1, 1) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), None, firstVerb, Some(firstAdj))
        )
      }
      case (2, 0, 1) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, Some(firstVerb), secondVerb, Some(firstAdj))
        )
      }
      case (2, 1, 1) => {
        Seq(
          SnlRomanceSyntaxFrame(
            Some(firstNom), Some(firstVerb), secondVerb, Some(firstAdj))
        )
      }
      case _ => {
        Seq.empty
      }
    }
    val unmodified = mood match {
      case MOOD_IMPERATIVE => {
        full.take(1)
      }
      case _ => {
        full
      }
    }
    val nonModPos = (iVerbs ++ iNoms ++ iAdjs).
      map(_._2).toSet + modalAdpositionPos
    val verbModifiers = seq.zipWithIndex.filterNot(
      z => nonModPos.contains(z._2)).map(_._1)
    unmodified.map(_.copy(modifiers = verbModifiers))
  }

  private def analyzeFrame(
    tree : SprSyntaxTree,
    seq : Seq[SprSyntaxTree],
    frame : SnlRomanceSyntaxFrame,
    questionOpt : Option[SilQuestion],
    tam : SilTam,
    force : SilForce
  ) : Option[SilSentence] =
  {
    frame.auxVerbOpt match {
      case Some(aux) if (
        aux.isProgressiveAux &&
          frame.mainVerb.isProgressiveVerb
      ) => {
        analyzeMainVerb(
          tree, seq,
          frame.copy(auxVerbOpt = None),
          questionOpt, tam.progressive, aux, force)
      }
      case Some(aux) if (aux.isModal) => {
        val tamModal = tongue.tamForAuxLemma(
          aux.firstChild.lemma,
          frame.mainVerb.firstChild.lemma
        )
        if (tamModal.modality != MODAL_NEUTRAL) {
          analyzeMainVerb(
            tree, seq,
            frame.copy(auxVerbOpt = None),
            questionOpt, tam.withModality(tamModal.modality),
            aux, force)
        } else {
          None
        }
      }
      case Some(aux) => None
      case _ => analyzeMainVerb(
        tree, seq, frame, questionOpt, tam,
        frame.mainVerb, force)
    }
  }

  private def analyzeMainVerb(
    tree : SprSyntaxTree,
    seq : Seq[SprSyntaxTree],
    frame : SnlRomanceSyntaxFrame,
    questionOpt : Option[SilQuestion],
    tam : SilTam,
    inflectedVerb : SprSyntaxTree,
    force : SilForce
  ) : Option[SilSentence] =
  {
    assert(frame.auxVerbOpt.isEmpty)
    if (frame.mainVerb.children.size != 1) {
      return None
    }
    val mainVerbLeaf = requireLeaf(frame.mainVerb.children)
    val mainVerb = getWord(mainVerbLeaf)
    val modifiers = expectVerbModifiers(frame.modifiers)
    val subject = frame.subjectOpt match {
      case Some(s) => s
      case _ => SptNP(SptNNE())
    }
    val predicateOpt = {
      if (frame.mainVerb.isImpersonalVerb) {
        if (frame.rhsOpt.isEmpty)
        {
          Some(SipUnresolvedStatePredicate(
            tree,
            expectReference(subject),
            mainVerb,
            expectExistenceState(SptNP(SptEX(mainVerbLeaf))),
            SilNullState(),
            modifiers))
        } else {
          None
        }
      } else {
        if (frame.mainVerb.isExistsVerb && frame.rhsOpt.isEmpty) {
          Some(SipUnresolvedStatePredicate(
            tree,
            expectReference(subject),
            mainVerb,
            expectExistenceState(frame.mainVerb),
            SilNullState(),
            modifiers))
        } else if ((frame.mainVerb.isRelationshipVerb ||
          frame.mainVerb.isBeingVerb) && frame.rhsOpt.nonEmpty)
        {
          val rhs = frame.rhsOpt.get
          if (rhs.isNounOrPronoun) {
            Some(SipUnresolvedRelationshipPredicate(
              tree,
              expectReference(subject),
              expectReference(rhs),
              mainVerb,
              modifiers))
          } else if (frame.mainVerb.isBeingVerb) {
            Some(SipUnresolvedStatePredicate(
              tree,
              expectReference(subject),
              mainVerb,
              expectComplementState(rhs),
              SilNullState(),
              modifiers))
          } else {
            None
          }
        } else if (frame.mainVerb.isBeingVerb && frame.rhsOpt.nonEmpty) {
          val rhs = frame.rhsOpt.get
          if (rhs.isNounOrPronoun) {
            None
          } else {
            Some(SipUnresolvedStatePredicate(
              tree,
              expectReference(subject),
              mainVerb,
              expectComplementState(rhs),
              SilNullState(),
              modifiers))
          }
        } else if (!frame.mainVerb.isPossessionVerb &&
          (!frame.mainVerb.isBeingVerb || questionOpt.isEmpty))
        {
          Some(SipUnresolvedActionPredicate(
            tree,
            expectReference(subject),
            mainVerb,
            frame.rhsOpt.map(expectReference),
            None,
            modifiers))
        } else {
          None
        }
      }
    }
    val tamTensed = extractTense(inflectedVerb, tam)
    val sentenceOpt = predicateOpt.map(predicate => {
      rememberPredicateInflection(predicate, inflectedVerb)
      questionOpt match {
        case Some(question) => {
          SilPredicateQuery(
            predicate,
            question,
            // FIXME
            INFLECT_NOMINATIVE,
            tamTensed,
            SilFormality(force))
        }
        case _ => {
          SilPredicateSentence(
            predicate,
            tamTensed,
            SilFormality(force))
        }
      }
    })
    sentenceOpt.foreach(s => s.rememberSyntaxTree(tree))
    sentenceOpt
  }

  override def analyzeSBARQ(tree : SptSBARQ)
      : SilSentence =
  {
    SilUnrecognizedSentence(tree)
  }

  override def analyzeSQ(
    tree : SprSyntaxTree, forceSQ : Boolean)
      : SilSentence =
  {
    SilUnrecognizedSentence(tree)
  }
}
