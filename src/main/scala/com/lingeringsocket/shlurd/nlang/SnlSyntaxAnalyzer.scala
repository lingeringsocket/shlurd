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

import SprPennTreebankLabels._
import SprUtils._

import scala.collection._

abstract class SnlSyntaxAnalyzer(
  context : SprContext,
  guessedQuestion : Boolean,
  strictness : SprStrictness,
  enforceTransitive : Boolean)
    extends SprAbstractSyntaxAnalyzer(context, strictness)
{
  protected implicit val tongue = context.getTongue

  override def analyzeSentence(tree : SptS)
      : SilSentence =
  {
    val hasQuestionMark =
      tree.children.last.hasTerminalLabel(LABEL_DOT, LABEL_QUESTION_MARK)
    val isQuestion = hasQuestionMark && !guessedQuestion
    val force = {
      if (tree.children.last.hasTerminalLabel(
        LABEL_DOT, LABEL_EXCLAMATION_MARK))
      {
        FORCE_EXCLAMATION
      } else {
        FORCE_NEUTRAL
      }
    }
    val (semiSplits, semiSeparator) = splitSemicolons(tree.children)
    if (semiSplits.size > 1) {
      if (!semiSplits.forall(_.size == 1)) {
        return SilUnrecognizedSentence(tree)
      }
      return SilConjunctiveSentence(
        DETERMINER_ABSENT,
        semiSplits.map(split => SipExpectedSentence(split.head)),
        semiSeparator)
    }
    splitCoordinatingConjunction(tree.children) match {
      case (DETERMINER_ABSENT, _, _) => ;
      case (determiner, separator, splits) => {
        val subs = splits.map(split => {
          split match {
            case Seq(s : SptS) => Some(s)
            case _ => None
          }
        })
        if (subs.forall(_.nonEmpty)) {
          return SilConjunctiveSentence(
            determiner,
            subs.flatten.map(s => SipExpectedSentence(s)),
            separator)
        }
      }
    }
    val children = stripPauses(tree)
    extractAntecedent(children) match {
      case Some((conjunction, antecedent)) => {
        SipExpectedConditionalSentence(
          tree,
          conjunction,
          SipExpectedSentence(antecedent),
          SipExpectedSentence(
            SptS(children.tail.filterNot(
              c => (c.isThen || c.isEquivalently)).toSeq:_*)),
          children.tail.exists(c =>
            (c.isEquivalently || c.children.exists(_.isEquivalently))),
          SilFormality(force))
      }
      case _ => {
        val mood = {
          if (isQuestion) {
            MOOD_INTERROGATIVE
          } else {
            MOOD_INDICATIVE
          }
        }
        analyzeSentenceChildren(
          tree, children, mood, force)
      }
    }
  }

  protected def analyzeSentenceChildren(
    tree : SprSyntaxTree, children : Seq[SprSyntaxTree],
    mood : SilMood, force : SilForce) : SilSentence

  protected def unwrapQuery(seq : Seq[SprSyntaxTree]) : Seq[SprSyntaxTree] =
  {
    if (seq.head.hasLabel(LABEL_LPAREN) &&
      seq.last.hasLabel(LABEL_RPAREN))
    {
      unwrapQuery(seq.dropRight(1).drop(1))
    } else if (seq.head.hasLabel(LABEL_LCURLY) &&
      seq.last.hasLabel(LABEL_RCURLY))
    {
      unwrapQuery(seq.dropRight(1).drop(1))
    } else if ((seq.size == 1) && seq.head.isQueryPhrase) {
      seq.head.children
    } else {
      seq
    }
  }

  protected def nounPhraseForQuestion(
    question : SilQuestion,
    questionChildren : Seq[SprSyntaxTree]) : SptNP =
  {
    question match {
      // FIXME for QUESTION_WHAT, there are two flavors (plain "what
      // do you want?" and also "what beer is most delicious?")
      case QUESTION_WHERE  | QUESTION_WHAT => {
        SptNP(SptNN(requireLeaf(questionChildren)))
      }
      case QUESTION_WHO => {
        if ((questionChildren.size == 1) && questionChildren.head.isLeaf) {
          val leaf = requireLeaf(questionChildren)
          if (leaf.lemma != leaf.token.toLowerCase) {
            SptNP(SptNNS(leaf))
          } else {
            SptNP(SptNN(leaf))
          }
        } else {
          SptNP(questionChildren.toSeq:_*)
        }
      }
      case QUESTION_WHICH => {
        // FIXME likewise, these have two flavors "which do you want?"
        // and "which flavor do you want?"
        questionChildren match {
          case Seq(SptNP(first : SptNP, second)) => {
            SptNP(
              SptNP((SptDT(makeLeaf(PD_WHICH.toLemma)) +:
                unwrapSinglePhrase(first.children)).toSeq:_*),
              second
            )
          }
          case _ => {
            SptNP((SptDT(makeLeaf(PD_WHICH.toLemma)) +:
              unwrapSinglePhrase(questionChildren)).toSeq:_*)
          }
        }
      }
      case QUESTION_HOW_MANY => {
        // FIXME likewise, these have two flavors "how many trees are there"
        // and "how many are still alive"
        SptNP(questionChildren.toSeq:_*)
      }
    }
  }

  override def analyzeNounPhrase(
    tree : SptNP) : SilReference =
  {
    val seqIn = tree.children
    if (seqIn.isEmpty) {
      return SilUnrecognizedReference(tree)
    }
    val seq = seqIn.map(_.unwrapPhrase)
    if (seq.size == 1) {
      return expectReference(seq.head)
    }
    if (seq.head.hasLabel(LABEL_LPAREN) && seq.last.hasLabel(LABEL_RPAREN)) {
      return annotator.parenthesizedRef(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_PAREN
      )
    }
    if (seq.head.hasLabel(LABEL_LCURLY) && seq.last.hasLabel(LABEL_RCURLY)) {
      return annotator.parenthesizedRef(
        expectReference(seq.dropRight(1).drop(1)),
        BRACKET_CURLY
      )
    }
    if (seq.head.lastChild.isPossessiveClitic) {
      return annotator.genitiveRef(
        expectReference(seq.head.children.dropRight(1)),
        expectReference(seq.tail))
    }
    if ((seq.size == 2) && seq.last.isNounPhrase &&
      seq.last.firstChild.hasLabel(LABEL_LPAREN)
    ) {
      return annotator.appositionalRef(
        expectReference(seq.head),
        expectReference(seq.last)
      )
    }
    splitCoordinatingConjunction(seq) match {
      case (DETERMINER_ABSENT, _, _) => {
      }
      case (determiner, separator, split) => {
        return annotator.conjunctiveRef(
          determiner, split.map(expectReference), separator)
      }
    }
    val (determiner, components) = {
      val first = seq.head.unwrapPhrase
      first match {
        // FIXME we should allow coordinating determiners in the absence
        // of conjunctions, e.g. "both lions are asleep"
        case pt @ (
          _ : SptDT | _ : SptCD
        ) if (
          !pt.isDemonstrative &&
            !tongue.isCoordinatingDeterminer(pt.firstChild.lemma)
        ) => {
          tupleN(determinerFor(requireLeaf(pt.children)), seq.drop(1))
        }
        case _ => {
          tupleN(DETERMINER_ABSENT, seq)
        }
      }
    }
    if ((components.size == 2) &&
      (components.head.isPossessivePronoun ||
        components.head.isDemonstrative))
    {
      val pronounReference = SipExpectedPossessiveReference(components.head)
      val entityReference = expectNounReference(
        tree, components.last, determiner)
      annotator.genitiveRef(pronounReference, entityReference)
    } else if (components.last.isCompoundAdpositionalPhrase) {
      annotator.stateSpecifiedRef(
        expectReference(seqIn.dropRight(1)),
        SipExpectedAdpositionalState(components.last, false))
    } else if ((components.size == 2) && components.head.isNounPhrase) {
      val entityReference = expectReference(components.head)
      expectRelativeReference(tree, entityReference, components.last)
    } else {
      analyzeQualifiedNounPhrase(tree, determiner, components)
    }
  }

  private def findNounPhraseHead(components : Seq[SprSyntaxTree]) : Int =
  {
    tongue.getAdjectivePosition match {
      case MOD_BEFORE_ALWAYS => {
        components.size - 1
      }
      case MOD_BEFORE_DEFAULT => {
        components.lastIndexWhere(isNounPhraseHead)
      }
      case MOD_AFTER_ALWAYS => {
        0
      }
      case MOD_AFTER_DEFAULT => {
        components.indexWhere(isNounPhraseHead)
      }
    }
  }

  private def analyzeQualifiedNounPhrase(
    tree : SptNP,
    determiner : SilDeterminer,
    components : Seq[SprSyntaxTree]) : SilReference =
  {
    val iHead = findNounPhraseHead(components)
    if (iHead == -1) {
      SilUnrecognizedReference(tree)
    } else {
      val head = components(iHead)
      val adjComponents = components.patch(iHead, Seq.empty, 1)
      if (isNounPhraseHead(head) && adjComponents.forall(
        c => isNounPhraseModifier(c, head)))
      {
        val qr = {
          val entityReference = expectNounReference(
            tree, head, DETERMINER_ABSENT)
          if (components.size > 1) {
            val qualifiedReference = annotator.stateQualifiedRef(
              entityReference,
              adjComponents.map(expectPropertyState))
            qualifiedReference matchPartial {
              case sr @ SilStateSpecifiedReference(
                _, state
              ) => {
                sr.rememberSyntaxTree(tree)
                state matchPartial {
                  case cs : SilConjunctiveState => {
                    rememberSyntheticADJP(cs, adjComponents)
                  }
                }
              }
            }
            qualifiedReference
          } else {
            entityReference
          }
        }
        annotator.determinedRef(qr, determiner)
      } else {
        SilUnrecognizedReference(tree)
      }
    }
  }

  protected def combineNegatives(n1 : Boolean, n2 : Boolean) : Boolean

  override def expectAdpositionalState(
    tree : SprSyntaxTree, extracted : Boolean)
    : SilState =
  {
    val seq = tree.children
    val adpTree = seq.head.unwrapPhrase
    if (seq.size == 2) {
      extractAdposition(adpTree) match {
        // "in the car"
        case Some(adposition) => {
          val valid = {
            if (extracted) {
              // FIXME ugh
              adposition match {
                case SprPredefAdposition(
                  PD_OF | PD_GENITIVE_OF | PD_TO | PD_DATIVE_TO) => false
                case _ => true
              }
            } else {
              true
            }
          }
          if (valid) {
            val obj = seq.last
            if (isAdpositionable(obj.unwrapPhrase)) {
              SilAdpositionalState(adposition, expectReference(obj))
            } else {
              SilUnrecognizedState(tree)
            }
          } else {
            SilUnrecognizedState(tree)
          }
        }
        case _ => {
          if (seq.last.isAdpositionalPhrase) {
            // "south of the border"
            expectAdpositionalState(seq.last, false) match {
              case SilAdpositionalState(SilAdposition(word), ref) => {
                val unwrapped = adpTree.unwrapPhrase
                if ((unwrapped.children.size == 1) &&
                  unwrapped.children.head.isLeaf &&
                  !unwrapped.isInstanceOf[SptNNQ])
                {
                  SilAdpositionalState(
                    SilAdposition(
                      (getWord(
                        requireLeaf(unwrapped.children)) +: word.decomposed
                      ).toSeq
                    ),
                    ref)
                } else {
                  SilUnrecognizedState(tree)
                }
              }
              case _ => {
                SilUnrecognizedState(tree)
              }
            }
          } else {
            SilUnrecognizedState(tree)
          }
        }
      }
    } else if ((seq.size == 1) && seq.head.isNounPhrase) {
      SilAdpositionalState(
        SprPredefAdposition(PD_ADVERBIAL_TMP), expectReference(seq.head))
    } else {
      SilUnrecognizedState(tree)
    }
  }

  override def expectVerbModifierPhrase(
    tree : SprSyntaxPhrase)
      : SilVerbModifier =
  {
    if (tree.children.size == 1) {
      expectVerbModifier(tree.firstChild)
    } else {
      val qualified = tree.firstChild match {
        case s : SprSyntaxSimpleAdverb => {
          // FIXME full list of qualifying adverbs
          s.child.lemma match {
            case SnlEnglishLemmas.LEMMA_NO | "very" => true
            case _ => false
          }
        }
        case _ => false
      }
      if (!qualified) {
        SilUnrecognizedVerbModifier(tree)
      } else {
        val words = tree.children.flatMap(_ match {
          case adverb : SprSyntaxSimpleAdverb => {
            Seq(getWord(adverb.child))
          }
          case particle : SptRP => {
            Seq(getWord(particle.child))
          }
          case rbc : SptRBC => {
            getCompoundWord(rbc).components
          }
          case _ => return expectAdpositionalVerbModifier(tree)
        })
        SilBasicVerbModifier(SilCompoundWord(words))
      }
    }
  }

  override def expectAdpositionalVerbModifier(
    tree : SprSyntaxTree) =
  {
    tree match {
      case SptPP(pt : SprSyntaxPreTerminal) => {
        if (tongue.isAdposition(pt.child.lemma)) {
          SilDanglingVerbModifier(
            SilAdposition(getWord(pt.child)))
        } else {
          SilUnrecognizedVerbModifier(tree)
        }
      }
      case _ => {
        expectAdpositionalState(tree, false) match {
          case SilAdpositionalState(adposition, objRef) => {
            SilAdpositionalVerbModifier(adposition, objRef)
          }
          case _ => {
            SilUnrecognizedVerbModifier(tree)
          }
        }
      }
    }
  }

  override def expectPropertyComplementState(
    tree : SprSyntaxTree) : SilState =
  {
    val seq = tree.children
    if (isStrict) {
      tree match {
        case _ : SptVP => SilUnrecognizedState(tree)
        case _ : SptADVP => SilUnrecognizedState(tree)
        case SptADJP(_ : SptRB) => SilUnrecognizedState(tree)
        case SptADJP(nn) if (nn.isNounOrPronoun) => SilUnrecognizedState(tree)
        case ap : SptADJP if (
          ap.children.exists(
            child => child.isInstanceOf[SptPP] && (child.numChildren < 2))
        ) =>
          SilUnrecognizedState(tree)
        case _ => {
          splitCoordinatingConjunction(seq) match {
            case (DETERMINER_ABSENT, _, _) => {
              analyzePropertyComplementState(tree, seq)
            }
            case (determiner, separator, split) => {
              val state = SilConjunctiveState(
                determiner,
                split.map(x => {
                  analyzePropertyComplementState(tree, unwrapSinglePhrase(x))
                }),
                separator)
              state
            }
          }
        }
      }
    } else {
      analyzePropertyComplementState(tree, seq)
    }
  }

  private def analyzePropertyComplementState(
    tree : SprSyntaxTree,
    seq : Seq[SprSyntaxTree]) : SilState =
  {
    def state = expectPropertyState(seq.head)
    if (seq.size == 1) {
      state
    } else {
      // FIXME generalize this, but needs semantic analysis, e.g.
      // "happy in the bathtub" is different from "west of the bathtub"
      val compoundAdposition = seq.last match {
        case SptPP(
          SptIN(adp),
          _
        ) if (SilWord(adp.lemma) == SprPredefWord(PD_OF)) => {
          Some(adp)
        }
        case _ => None
      }
      if ((seq.size == 2) && !compoundAdposition.isEmpty) {
        expectAdpositionalState(tree, false)
      } else {
        SilConjunctiveState(
          DETERMINER_ABSENT,
          Seq(state) ++ seq.tail.map(
            component => expectComplementState(component)))
      }
    }
  }

  private def extractAntecedent(children : Seq[SprSyntaxTree])
      : Option[(SilWord, SptS)] =
  {
    children.headOption match {
      case Some(SptSBAR(SptIN(leaf), antecedent : SptS)) => {
        SilWord(leaf.lemma) match {
          case SprPredefWord(PD_IF | PD_WHEN | PD_WHENEVER |
              PD_BEFORE | PD_AFTER) =>
            Some(tupleN(getWord(leaf), antecedent))
          case _ =>
            None
        }
      }
      case Some(SptSBAR(SptWHADVP(SptWRB(leaf)), antecedent : SptS)) => {
        SilWord(leaf.lemma) match {
          case SprPredefWord(PD_WHEN | PD_WHENEVER) =>
            Some(tupleN(getWord(leaf), antecedent))
          case _ => None
        }
      }
      case _ => None
    }
  }

  protected def maybeQuestionFor(
    seq : Seq[SprSyntaxTree])
      : Option[(SilQuestion, Option[SprSyntaxTree], Seq[SprSyntaxTree])] =
  {
    val tree = seq.head
    tree match {
      case SptWHADJP(children @ _*) => {
        if (children.forall(_.isPreTerminal)) {
          val lemma = children.map(_.firstChild.lemma).mkString(" ")
          SilWord(lemma) match {
            case SprPredefWord(PD_HOW_MANY) => {
              // FIXME for HOW_MANY, force the corresponding noun reference
              // to plural
              Some((QUESTION_HOW_MANY, None, seq.tail))
            }
            case _ => None
          }
        } else {
          None
        }
      }
      case SptWRB(where) => {
        SilWord(where.lemma) match {
          case SprPredefWord(PD_WHERE) =>
            Some((QUESTION_WHERE, None, tree.children))
          case _ => None
        }
      }
      case SptWDT(wdt) => {
        SilWord(wdt.lemma) match {
          case SprPredefWord(PD_WHICH | PD_WHAT) =>
            Some((QUESTION_WHICH, None, seq.tail))
          case _ => None
        }
      }
      case SptWP_POS(wpp) => {
        Some((QUESTION_WHO, None,
          Seq(SptNP(
            (SptNP(SptNN(makeLeaf(PD_WHO.toLemma)), SptPOS(makeLeaf("'s"))) +:
              seq.tail).toSeq:_*))))
      }
      case SptWP(wp) => {
        SilWord(wp.lemma) match {
          case SprPredefWord(PD_WHO | PD_WHOM) =>
            Some((QUESTION_WHO, None, tree.children))
          case SprPredefWord(PD_WHAT) =>
            Some((QUESTION_WHAT, None, tree.children))
          case _ => None
        }
      }
      case adp : SprSyntaxAdposition => {
        if (seq.size == 2) {
          val whnp = seq.last
          whnp match {
            case _ : SptWHNP => {
              maybeQuestionFor(whnp.children) match {
                case Some((question, None, questionChildren)) => {
                  Some((question, Some(adp), questionChildren))
                }
                case _ => None
              }
            }
            case _ => None
          }
        } else {
          None
        }
      }
      case _ => None
    }
  }

  protected def splitCoordinatingConjunction(
    components : Seq[SprSyntaxTree])
      : (SilDeterminer, SilSeparator, Seq[Seq[SprSyntaxTree]]) =
  {
    if (isSinglePhrase(components)) {
      return splitCoordinatingConjunction(components.head.children)
    }
    val pos = components.indexWhere(t => t.isCoordinatingConjunction, 1)
    if (pos == -1) {
      val (commaSplit, commaSeparator) = splitCommas(components)
      commaSeparator match {
        case SEPARATOR_COMMA | SEPARATOR_OXFORD_COMMA => {
          splitCoordinatingConjunction(commaSplit.last) match {
            case (DETERMINER_ABSENT, _, _) => {
              tupleN(DETERMINER_ABSENT, SEPARATOR_CONJOINED, Seq.empty)
            }
            case (determiner, separator, subSplit) => {
              // FIXME:  deal with coordinating determiner in
              // first sub-phrase
              val seq = commaSplit.dropRight(1) ++ subSplit
              tupleN(determiner, commaSeparator, seq)
            }
          }
        }
        case _ => {
          tupleN(DETERMINER_ABSENT, SEPARATOR_CONJOINED, Seq.empty)
        }
      }
    } else {
      var determiner = components(pos) match {
        case SptCC(leaf) => {
          determinerFor(leaf)
        }
        case _ => throw new AssertionError("CC required")
      }
      val prefix = {
        if (isCoordinatingDeterminer(components.head, determiner)) {
          if (determiner.isInstanceOf[SilUnlimitedDeterminer]) {
            determiner = DETERMINER_DEFINITE
          }
          components.take(pos).drop(1)
        } else {
          components.take(pos)
        }
      }
      val (commaSplit, commaSeparator) = splitCommas(prefix)
      val suffix = components.drop(pos + 1)
      splitCoordinatingConjunction(suffix) match {
        case (DETERMINER_ABSENT, _, _) => {
          tupleN(determiner, commaSeparator, commaSplit ++ Seq(suffix))
        }
        case (subDeterminer, _, subSplit) => {
          val seq = {
            if (determiner == subDeterminer) {
              commaSplit ++ subSplit
            } else {
              commaSplit ++ Seq(suffix)
            }
          }
          tupleN(determiner, commaSeparator, seq.filterNot(_.isEmpty))
        }
      }
    }
  }

  override def isProhibitedPropertyState(
    preTerminal : SprSyntaxPreTerminal) : Boolean =
  {
    val lemma = getWord(preTerminal.child).lemma
    (
      tongue.isBeingLemma(lemma)
    ) || (
      isStrict && preTerminal.isAdposition &&
        (tongue.isAdposition(lemma) ||
          tongue.isSubordinatingConjunction(lemma))
    )
  }

  protected def expectCommand(
    tree : SprSyntaxTree,
    vp : SprSyntaxTree,
    formality : SilFormality,
    gender : SilGender = GENDER_SOMEONE,
    count : SilCount = COUNT_SINGULAR) : SilSentence =
  {
    val pronounLemma = tongue.pronounLemma(
      PERSON_SECOND, gender, count,
      PROXIMITY_ENTITY, formality.politeness, INFLECT_NOMINATIVE)
    val np = SptNP(SptPRP(makeLeaf(pronounLemma)))
    val sentence = analyzeSentenceChildren(
      tree, Seq(np, vp), MOOD_IMPERATIVE, formality.force)
    val tam = sentence.tam
    if ((tam.modality == MODAL_NEUTRAL) &&
      (tam.aspect == ASPECT_SIMPLE) &&
      (tam.tense == TENSE_PRESENT))
    {
      sentence.withNewTamFormality(
        tam.withMood(MOOD_IMPERATIVE),
        formality)
    } else {
      SilUnrecognizedSentence(tree)
    }
  }
}
