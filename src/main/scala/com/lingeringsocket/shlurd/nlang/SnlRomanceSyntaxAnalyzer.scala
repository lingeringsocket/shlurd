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

import scala.collection._

case class SnlRomanceSyntaxFrame(
  subjectOpt : Option[SprSyntaxTree],
  auxVerbOpt : Option[SprSyntaxTree],
  mainVerb : SprSyntaxTree,
  rhsOpt : Option[SprSyntaxTree],
  modifiers : Seq[SilVerbModifier] = Seq.empty
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
    if (tree.isNounPhrase && tree.isUnwrappable) {
      detectNominative(tree.unwrapPhrase)
    } else if (tree.isQueryNounPhrase ||
      tree.isQueryAdverbPhrase || tree.isNoun ||
      tree.isNounPhrase || tree.isDemonstrative)
    {
      // FIXME isQueryAdverbPhrase is for dónde, but it shouldn't really
      // be interpreted as a noun
      true
    } else if (tree.isPronoun) {
      val lemma = tree.firstChild.lemma
      val inflections = tongue.analyzePronoun(lemma)._4
      inflections.contains(INFLECT_NOMINATIVE)
    } else {
      false
    }
  }

  private def detectAccusative(tree : SprSyntaxTree) : Boolean =
  {
    if (tree.isNounPhrase && tree.isUnwrappable) {
      detectAccusative(tree.unwrapPhrase)
    } else if (tree.isPronoun) {
      val lemma = tree.firstChild.lemma
      val inflections = tongue.analyzePronoun(lemma)._4
      inflections.contains(INFLECT_ACCUSATIVE)
    } else {
      false
    }
  }

  private def detectDative(tree : SprSyntaxTree) : Boolean =
  {
    if (tree.isNounPhrase && tree.isUnwrappable) {
      detectDative(tree.unwrapPhrase)
    } else if (tree.isPronoun) {
      val lemma = tree.firstChild.lemma
      val inflections = tongue.analyzePronoun(lemma)._4
      inflections.contains(INFLECT_DATIVE)
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

  protected def detectImperative(
    children : Seq[SprSyntaxTree], isNegated : Boolean) :
      (Boolean, SilGender, SilCount, SilPoliteness)

  override protected def analyzeSentenceChildren(
    tree : SprSyntaxTree, children : Seq[SprSyntaxTree],
    mood : SilMood, force : SilForce) : SilSentence =
  {
    val (isNegativeAbove, extracted) =
      extractNegative(unwrapSinglePhrase(children))
    val seq = unwrapSinglePhrase(extracted)
    // FIXME support explicit (vocative) subject for imperatives,
    // possibly providing gender
    val (
      isImperative, imperativeGender, imperativeCount, imperativePoliteness
    ) = detectImperative(seq, isNegativeAbove)
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
      val iFirstVerb = iVerbs.headOption.map(_._2).getOrElse(unwrapped.size)
      val beforeFirstVerb = unwrapped.take(iFirstVerb)
      val iNoms = unwrapped.zipWithIndex.filter(z => detectNominative(z._1))
      // note that iDirects and iIndirects may overlap due to
      // pronoun usage collisions (e.g. "te" in Spanish)
      val iDirects = beforeFirstVerb.zipWithIndex.filter(
        z => detectAccusative(z._1))
      val iIndirects = beforeFirstVerb.zipWithIndex.filter(
        z => detectDative(z._1))
      val iAdjs = unwrapped.zipWithIndex.filter(z => detectAdjective(z._1))
      val iAdps = unwrapped.zipWithIndex.filter(z => detectAdposition(z._1))

      // FIXME deal with presence of more than one question
      val queryOpt = unwrapped.find(_.isQueryPhrase)
      val questionOpt = queryOpt.flatMap(queryPhrase => {
        // FIXME adpositions
        maybeQuestionFor(unwrapQuery(queryPhrase.children)).map(_._1)
      })
      val frames = {
        val bonusFrames = tupleN(iNoms.size, iAdjs.size) match {
          case (0 | 1, 0) => {
            iAdps.flatMap(iAdp => {
              produceFrames(
                unwrapped, iVerbs, iNoms, iDirects,
                iIndirects, Seq(iAdp), mood
              )
            })
          }
          case _ => Seq.empty
        }
        produceFrames(
          unwrapped, iVerbs, iNoms, iDirects, iIndirects, iAdjs, mood
        ) ++ bonusFrames
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

  private def checkNoDirect(
    rhsDirect : Option[SprSyntaxTree],
    seq : Seq[SnlRomanceSyntaxFrame]
  ) : Seq[SnlRomanceSyntaxFrame] =
  {
    if (rhsDirect.isEmpty) {
      seq
    } else {
      Seq.empty
    }
  }

  private def swappableSubject(
    frame : SnlRomanceSyntaxFrame
  ) : Seq[SnlRomanceSyntaxFrame] =
  {
    if (frame.rhsOpt.nonEmpty) {
      Seq(frame)
    } else {
      Seq(
        frame,
        SnlRomanceSyntaxFrame(
          None, frame.auxVerbOpt, frame.mainVerb,
          frame.subjectOpt,
          frame.modifiers)
      )
    }
  }

  private def swappableVerb(
    frame : SnlRomanceSyntaxFrame
  ) : Seq[SnlRomanceSyntaxFrame] =
  {
    if (frame.mainVerb.isBeingVerb) {
      Seq(frame)
    } else {
      Seq(
        frame,
        SnlRomanceSyntaxFrame(
          frame.rhsOpt,
          frame.auxVerbOpt,
          frame.mainVerb,
          frame.subjectOpt,
          frame.modifiers
        )
      )
    }
  }

  private def produceFrames(
    seq : Seq[SprSyntaxTree],
    iVerbs : Seq[(SprSyntaxTree, Int)],
    iNoms : Seq[(SprSyntaxTree, Int)],
    iDirects : Seq[(SprSyntaxTree, Int)],
    iIndirects : Seq[(SprSyntaxTree, Int)],
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

    def firstDirect = iDirects.headOption.map(_._1)
    def secondDirect = iDirects.drop(1).headOption.map(_._1)
    def firstIndirect = iIndirects.headOption.map(_._1)
    val (rhsDirect, dativeObj) = tupleN(
      iDirects.size, iIndirects.size
    ) match {
      case (0, 0) => {
        tupleN(None, None)
      }
      case (1, 0) => {
        tupleN(firstDirect, None)
      }
      case (0, 1) => {
        tupleN(None, firstIndirect)
      }
      case (1, 1) => {
        if (iDirects == iIndirects) {
          // FIXME some verbs may allow for an indirect object with no
          // explicit direct object (e.g. "ella me dice"); in that case
          // we should interpret the sole object as indirect, not direct
          tupleN(firstDirect, None)
        } else {
          tupleN(firstDirect, firstIndirect)
        }
      }
      case (2, 1) => {
        if (iDirects.head == iIndirects.head) {
          // "ella me lo cuenta"
          tupleN(secondDirect, firstIndirect)
        } else {
          return Seq.empty
        }
      }
      case _ => return Seq.empty
    }

    // order of produced frames is significant in that in case
    // of unresolvable ambiguity, we will take the former
    val full = tupleN(iVerbs.size, iNoms.size, iAdjs.size) match {
      case (1, 0, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, None, firstVerb, rhsDirect)
        )
      }
      case (1, 1, 0) => {
        swappableSubject(
          SnlRomanceSyntaxFrame(
            Some(firstNom), None, firstVerb, rhsDirect)
        )
      }
      case (1, 2, 0) => {
        checkNoDirect(
          rhsDirect,
          swappableVerb(
            SnlRomanceSyntaxFrame(
              Some(firstNom), None, firstVerb, Some(secondNom))))
      }
      case (2, 0, 0) => {
        Seq(
          SnlRomanceSyntaxFrame(
            None, Some(firstVerb), secondVerb, rhsDirect)
        )
      }
      case (2, 1, 0) => {
        swappableSubject(
          SnlRomanceSyntaxFrame(
            Some(firstNom), Some(firstVerb), secondVerb, rhsDirect)
        )
      }
      case (2, 2, 0) => {
        checkNoDirect(
          rhsDirect,
          swappableVerb(
            SnlRomanceSyntaxFrame(
              Some(firstNom), Some(firstVerb), secondVerb, Some(secondNom))))
      }
      case (1, 0, 1) => {
        checkNoDirect(
          rhsDirect,
          Seq(
            SnlRomanceSyntaxFrame(
              None, None, firstVerb, Some(firstAdj))
          )
        )
      }
      case (1, 1, 1) => {
        checkNoDirect(
          rhsDirect,
          Seq(
            SnlRomanceSyntaxFrame(
              Some(firstNom), None, firstVerb, Some(firstAdj))
          )
        )
      }
      case (2, 0, 1) => {
        checkNoDirect(
          rhsDirect,
          Seq(
            SnlRomanceSyntaxFrame(
              None, Some(firstVerb), secondVerb, Some(firstAdj))
          )
        )
      }
      case (2, 1, 1) => {
        checkNoDirect(
          rhsDirect,
          Seq(
            SnlRomanceSyntaxFrame(
              Some(firstNom), Some(firstVerb), secondVerb, Some(firstAdj))
          )
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
    val nonModPos = (iVerbs ++ iNoms ++ iAdjs ++ iDirects ++ iIndirects).
      map(_._2).toSet + modalAdpositionPos
    val verbModifiers = expectVerbModifiers(seq.zipWithIndex.filterNot(
      z => nonModPos.contains(z._2)
    ).map(_._1)) ++ dativeObj.map(indirectObj => {
      val modifier = SilAdpositionalVerbModifier(
        SprPredefAdposition(PD_DATIVE_TO),
        expectReference(indirectObj))
      modifier.rememberSyntaxTree(indirectObj)
      modifier
    })
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
    val modifiers = frame.modifiers
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
            // this is incorrect, but we fix it later
            // during normalization
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
