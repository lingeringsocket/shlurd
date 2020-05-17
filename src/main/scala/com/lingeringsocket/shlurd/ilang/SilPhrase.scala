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
package com.lingeringsocket.shlurd.ilang

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.parser._

import scala.collection._

sealed trait SilPhrase
{
  def children : Seq[SilPhrase] = Seq.empty

  def hasUnknown : Boolean = children.exists(_.hasUnknown)

  def hasUnresolved : Boolean = hasUnresolvedChildren

  def hasUnresolvedChildren : Boolean = children.exists(_.hasUnresolved)

  def isUninterpretable : Boolean =
    hasUnknown || children.exists(_.isUninterpretable)

  def isConjunctive : Boolean = false

  override def toString = SprPrettyPrinter.prettyPrint(this)

  def maybeSyntaxTree : Option[SprSyntaxTree] = None

  def toWordString : String =
  {
    maybeSyntaxTree match {
      case Some(syntaxTree) => syntaxTree.toWordString
      case _ => toString
    }
  }

  def countUnknownSyntaxLeaves : Int =
  {
    children.map(_.countUnknownSyntaxLeaves).sum
  }

  def childReferences : Seq[SilReference] =
  {
    children.filter(_.isInstanceOf[SilReference]).
      map(_.asInstanceOf[SilReference])
  }
}

sealed trait SilSentence extends SilPhrase
{
  def tam : SilTam

  def formality : SilFormality

  def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence
}

sealed trait SilPredicate extends SilPhrase
{
  def getSubject : SilReference

  def getVerb : SilWord

  private var inflectedCount : SilCount = COUNT_SINGULAR

  def getInflectedCount = inflectedCount

  def setInflectedCount(count : SilCount)
  {
    inflectedCount = count
  }

  def getModifiers : Seq[SilVerbModifier] = Seq.empty

  def withNewSubject(ref : SilReference) : SilPredicate

  def withNewModifiers(newModifiers : Seq[SilVerbModifier]) : SilPredicate
}

sealed trait SilReference extends SilPhrase
{
  def acceptsSpecifiers : Boolean = true

  def descendantReferences : Set[SilReference] = {
    Set(this) ++ childReferences.flatMap(_.descendantReferences)
  }
}

sealed trait SilState extends SilPhrase
{
}

sealed trait SilVerbModifier extends SilPhrase
{
}

sealed trait SilUnknownPhrase extends SilPhrase
{
  override def hasUnknown = true

  def syntaxTree : SprSyntaxTree

  override def maybeSyntaxTree = Some(syntaxTree)

  override def countUnknownSyntaxLeaves : Int =
  {
    syntaxTree.countLeaves
  }
}

sealed trait SilUnknownSentence
    extends SilSentence with SilUnknownPhrase
{
  override def tam = SilTam.indicative

  override def formality = SilFormality.DEFAULT

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

sealed trait SilUnknownPredicate
    extends SilPredicate with SilUnknownPhrase
{
  override def getSubject : SilReference = SilUnrecognizedReference(syntaxTree)

  override def getVerb = SilWord("<UNKNOWN>")

  override def withNewSubject(reference : SilReference) = this

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) = this
}

sealed trait SilUnknownReference
    extends SilReference with SilUnknownPhrase
{
}

sealed trait SilUnknownState
    extends SilState with SilUnknownPhrase
{
}

sealed trait SilUnknownVerbModifier
    extends SilVerbModifier with SilUnknownPhrase
{
}

sealed trait SilUnrecognizedPhrase extends SilPhrase
{
}

sealed trait SilUnresolvedPhrase extends SilPhrase
{
  override def hasUnresolved = true
}

sealed abstract class SilTransformedPhrase extends SilPhrase
{
  protected var syntaxTreeOpt : Option[SprSyntaxTree] = None

  def rememberSyntaxTree(syntaxTree : SprSyntaxTree)
  {
    syntaxTreeOpt = Some(syntaxTree)
  }

  def hasSyntaxTree = !syntaxTreeOpt.isEmpty

  override def maybeSyntaxTree = syntaxTreeOpt
}

sealed abstract class SilAnnotatedReference
    extends SilTransformedPhrase with SilReference
{
  private var annotatorOpt : Option[SilAnnotator] = None

  private var annotationId = 0

  private[ilang] def registerAnnotation(
    annotator : SilAnnotator, id : Int)
  {
    assert(annotatorOpt.isEmpty)
    annotatorOpt = Some(annotator)
    annotationId = id
  }

  def hasAnnotation() : Boolean =
  {
    annotatorOpt.nonEmpty
  }

  def maybeAnnotator() : Option[SilAnnotator] =
  {
    annotatorOpt
  }

  def getAnnotator() : SilAnnotator =
  {
    annotatorOpt.get
  }

  def getAnnotationId() : Int =
  {
    assert(hasAnnotation)
    annotationId
  }
}

sealed trait SilAdpositionalPhrase extends SilTransformedPhrase
{
  def adposition : SilAdposition

  def objRef : SilReference

  override def children = Seq(objRef)
}

case class SilUnparsedSentence(
  text : String
) extends SilSentence with SilUnrecognizedPhrase
{
  override def hasUnknown = true

  override def toString = text

  override def toWordString = text

  override def countUnknownSyntaxLeaves = Int.MaxValue

  override def tam = SilTam.indicative

  override def formality = SilFormality.DEFAULT

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

case class SilUnrecognizedSentence(
  syntaxTree : SprSyntaxTree
) extends SilUnknownSentence with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedPredicate(
  syntaxTree : SprSyntaxTree
) extends SilUnknownPredicate with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedReference(
  syntaxTree : SprSyntaxTree
) extends SilUnknownReference with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnrecognizedPhrase
{
}

case class SilUnrecognizedVerbModifier(
  syntaxTree : SprSyntaxTree
) extends SilUnknownVerbModifier with SilUnrecognizedPhrase
{
}

case class SilExpectedSentence(
  syntaxTree : SprSyntaxTree,
  forceSQ : Boolean = false
) extends SilUnknownSentence with SilUnresolvedPhrase
{
}

case class SilExpectedConditionalSentence(
  syntaxTree : SprSyntaxTree,
  conjunction : SilWord,
  antecedent : SilSentence,
  consequent : SilSentence,
  biconditional : Boolean,
  expectedFormality : SilFormality
) extends SilUnknownSentence with SilUnresolvedPhrase
{
  override def formality = expectedFormality
}

case class SilExpectedPredicate(
  syntaxTree : SprSyntaxTree
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
}

case class SilExpectedReference(
  syntaxTree : SprSyntaxTree
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedPossessiveReference(
  syntaxTree : SprSyntaxTree
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedNounlikeReference(
  syntaxTree : SprSyntaxTree,
  preTerminal : SprSyntaxTree,
  determiner : SilDeterminer
) extends SilUnknownReference with SilUnresolvedPhrase
{
}

case class SilExpectedComplementState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedAdpositionalState(
  syntaxTree : SprSyntaxTree,
  extracted : Boolean
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedPropertyState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedExistenceState(
  syntaxTree : SprSyntaxTree
) extends SilUnknownState with SilUnresolvedPhrase
{
}

case class SilExpectedVerbModifier(
  syntaxTree : SprSyntaxTree
) extends SilUnknownVerbModifier with SilUnresolvedPhrase
{
}

case class SilUnresolvedStatePredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  state : SilState,
  specifiedState : SilState,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, state, specifiedState) ++ modifiers

  override def withNewSubject(reference : SilReference) =
    copy(subject = reference)

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilUnresolvedActionPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  verb : SilWord,
  directObject : Option[SilReference],
  adpositionObject : Option[SilReference],
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ adpositionObject ++ modifiers

  override def withNewSubject(reference : SilReference) =
    copy(subject = reference)

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilUnresolvedRelationshipPredicate(
  syntaxTree : SprSyntaxTree,
  subject : SilReference,
  complement : SilReference,
  verb : SilWord,
  modifiers : Seq[SilVerbModifier]
) extends SilUnknownPredicate with SilUnresolvedPhrase
{
  override def getSubject = subject

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilPredicateSentence(
  predicate : SilPredicate,
  tam : SilTam  = SilTam.indicative,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tam = newTam, formality = newFormality)
  }
}

case class SilConditionalSentence(
  conjunction : SilWord,
  antecedent : SilPredicate,
  consequent : SilPredicate,
  tamAntecedent : SilTam,
  tamConsequent : SilTam,
  biconditional : Boolean,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(antecedent, consequent)

  override def tam = tamConsequent

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tamConsequent = newTam, formality = newFormality)
  }
}

case class SilPredicateQuery(
  predicate : SilPredicate,
  question : SilQuestion,
  answerInflection : SilInflection,
  tam : SilTam,
  formality : SilFormality = SilFormality.DEFAULT
) extends SilTransformedPhrase with SilSentence
{
  override def children = Seq(predicate)

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(tam = newTam, formality = newFormality)
  }
}

case class SilConjunctiveSentence(
  determiner : SilDeterminer,
  sentences : Seq[SilSentence],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilSentence
{
  override def children = sentences

  override def isConjunctive : Boolean = true

  // not really sure there's any more meaningful implementation
  override def tam = sentences.head.tam

  override def formality = sentences.head.formality

  override def withNewTamFormality(
    newTam : SilTam, newFormality : SilFormality) =
  {
    copy(sentences = sentences.headOption.map(
      _.withNewTamFormality(newTam, newFormality)).toSeq ++ sentences.tail)
  }
}

case class SilAmbiguousSentence(
  alternatives : Seq[SilSentence],
  done : Boolean = false
) extends SilTransformedPhrase with SilSentence
{
  override def children = alternatives

  override def tam = alternatives.head.tam

  override def formality = alternatives.head.formality

  def isRipe = !hasUnresolvedChildren && !done

  override def withNewTamFormality(newTam : SilTam, newFormality : SilFormality)
      : SilSentence = this
}

case class SilStatePredicate(
  subject : SilReference,
  verb : SilWord,
  state : SilState,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children = Seq(subject, state) ++ modifiers

  override def withNewSubject(reference : SilReference) =
  {
    copy(subject = reference)
  }

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilRelationshipPredicate(
  subject : SilReference,
  verb : SilWord,
  complement : SilReference,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children = Seq(subject, complement) ++ modifiers

  override def withNewSubject(reference : SilReference) =
  {
    copy(subject = reference)
  }

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

case class SilActionPredicate(
  subject : SilReference,
  verb : SilWord,
  directObject : Option[SilReference] = None,
  modifiers : Seq[SilVerbModifier] = Seq.empty
) extends SilTransformedPhrase with SilPredicate
{
  override def getSubject = subject

  override def getVerb = verb

  override def getModifiers = modifiers

  override def children =
    Seq(subject) ++ directObject ++ modifiers

  override def withNewSubject(reference : SilReference) =
    copy(subject = reference)

  override def withNewModifiers(newModifiers : Seq[SilVerbModifier]) =
    copy(modifiers = newModifiers)
}

object SilParenthesizedReference
{
  private def apply(
    reference : SilReference, bracket : SilBracket
  ) = new SilParenthesizedReference(reference, bracket)

  def unannotated(reference : SilReference, bracket : SilBracket) =
    SilParenthesizedReference(reference, bracket)
}

case class SilParenthesizedReference private(
  reference : SilReference,
  bracket : SilBracket
) extends SilAnnotatedReference
{
  override def children = Seq(reference)

  override def acceptsSpecifiers = reference.acceptsSpecifiers
}

object SilAppositionalReference
{
  private def apply(
    primary : SilReference, secondary : SilReference
  ) = new SilAppositionalReference(primary, secondary)

  def unannotated(
    primary : SilReference, secondary : SilReference
  ) = SilAppositionalReference(primary, secondary)
}

case class SilAppositionalReference private(
  primary : SilReference,
  secondary : SilReference
) extends SilAnnotatedReference
{
  override def children = Seq(primary, secondary)

  override def acceptsSpecifiers = primary.acceptsSpecifiers
}

object SilStateSpecifiedReference
{
  private def apply(
    reference : SilReference, state : SilState
  ) = new SilStateSpecifiedReference(reference, state)

  def unannotated(
    reference : SilReference, state : SilState
  ) = SilStateSpecifiedReference(reference, state)
}

case class SilStateSpecifiedReference private(
  reference : SilReference,
  state : SilState
) extends SilAnnotatedReference
{
  override def children = Seq(reference, state)

  override def acceptsSpecifiers : Boolean = {
    // FIXME:  weird special case for "3 of them"
    state matchPartial {
      case SilAdpositionalState(SilAdposition.OF, pn : SilPronounReference) => {
        reference matchPartial {
          case SilOptionallyDeterminedReference(
            SilNounLemmaReference(lemma),
            _
          ) => {
            if (lemma.forall(Character.isDigit)) {
              return false
            }
          }
        }
      }
    }
    reference.acceptsSpecifiers
  }
}

object SilGenitiveReference
{
  private def apply(
    possessor : SilReference, possessee : SilReference
  ) = new SilGenitiveReference(possessor, possessee)

  def unannotated(
    possessor : SilReference, possessee : SilReference
  ) = SilGenitiveReference(possessor, possessee)
}

case class SilGenitiveReference private(
  possessor : SilReference,
  possessee : SilReference
) extends SilAnnotatedReference
{
  override def children = Seq(possessor, possessee)
}

object SilPronounReference
{
  private def apply(
    person : SilPerson, gender : SilGender,
    count : SilCount, distance : SilDistance
  ) = new SilPronounReference(person, gender, count, distance)

  def unannotated(
    person : SilPerson, gender : SilGender,
    count : SilCount, distance : SilDistance
  ) = SilPronounReference(person, gender, count, distance)
}

case class SilPronounReference private(
  person : SilPerson,
  gender : SilGender,
  count : SilCount,
  distance : SilDistance
) extends SilAnnotatedReference
{
  override def acceptsSpecifiers = false

  def word() : Option[SilWord] =
  {
    maybeAnnotator.flatMap(annotator => {
      annotator.getBasicNote(this).getWord
    })
  }

  def clearWord()
  {
    maybeAnnotator.foreach(annotator => {
      annotator.getBasicNote(this).clearWord
    })
  }

  def pronounMap() : SilPronounMap =
  {
    maybeAnnotator.map(_.getBasicNote(this).getPronounMap).
      getOrElse(SilPronounMap())
  }

  def isDemonstrative() : Boolean =
  {
    distance match {
      case DISTANCE_HERE | DISTANCE_THERE => true
      case _ => false
    }
  }

  def isReflexive() : Boolean =
  {
    distance == DISTANCE_REFLEXIVE
  }
}

object SilConjunctiveReference
{
  private def apply(
    determiner : SilDeterminer,
    references : Seq[SilReference],
    separator : SilSeparator
  ) =
  {
    assert(determiner match {
      case DETERMINER_NONE | DETERMINER_ANY | DETERMINER_DEFINITE |
          DETERMINER_ALL | DETERMINER_ABSENT => true
      case _ => false
    })
    new SilConjunctiveReference(determiner, references, separator)
  }

  def unannotated(
    determiner : SilDeterminer,
    references : Seq[SilReference],
    separator : SilSeparator
  ) = SilConjunctiveReference(determiner, references, separator)
}

case class SilConjunctiveReference private(
  determiner : SilDeterminer,
  references : Seq[SilReference],
  separator : SilSeparator
) extends SilAnnotatedReference
{
  override def children = references

  override def isConjunctive : Boolean = true

  override def acceptsSpecifiers = children.exists(_.acceptsSpecifiers)
}

object SilDeterminedReference
{
  private def apply(
    reference : SilReference, determiner : SilDeterminer
  ) = new SilDeterminedReference(reference, determiner)

  def unannotated(
    reference : SilReference, determiner : SilDeterminer
  ) = SilDeterminedReference(reference, determiner)
}

case class SilDeterminedReference private(
  reference : SilReference,
  determiner : SilDeterminer
) extends SilAnnotatedReference
{
  override def children = Seq(reference)

  override def acceptsSpecifiers = reference.acceptsSpecifiers
}

object SilCountedNounReference
{
  def unapply(ref : SilNounReference) =
  {
    Some(tupleN((ref.noun, ref.count)))
  }
}

object SilNounLemmaReference
{
  def unapply(ref : SilNounReference) =
  {
    Some(ref.noun.toNounLemma)
  }
}

object SilNounReference
{
  private def apply(noun : SilWord) = new SilNounReference(noun)

  def unannotated(noun : SilWord) = SilNounReference(noun)
}

case class SilNounReference private(
  noun : SilWord
) extends SilAnnotatedReference
{
  def count : SilCount =
  {
    maybeAnnotator.map(_.getBasicNote(this).getCount).getOrElse(COUNT_SINGULAR)
  }
}

object SilMappedReference
{
  private def apply(key : String, determiner : SilDeterminer) =
    new SilMappedReference(key, determiner)

  def unannotated(key : String, determiner : SilDeterminer) =
    SilMappedReference(key, determiner)
}

case class SilMappedReference private(
  key : String,
  determiner : SilDeterminer
) extends SilAnnotatedReference with SilUnknownReference
{
  override def syntaxTree = SprSyntaxLeaf(key, key, key)
}

object SilQuotationReference
{
  private def apply(quotation : String, bracket : SilBracket) =
    new SilQuotationReference(quotation, bracket)

  def unannotated(quotation : String, bracket : SilBracket) =
    SilQuotationReference(quotation, bracket)
}

case class SilQuotationReference private(
  quotation : String,
  bracket : SilBracket
) extends SilAnnotatedReference
{
}

case class SilExistenceState(
  existentialPronoun : Option[SilWord] = None
) extends SilTransformedPhrase with SilState
{
}

case class SilNullState(
) extends SilTransformedPhrase with SilState
{
  override def hasUnknown = true
}

case class SilPropertyQueryState(
  propertyName : String
) extends SilTransformedPhrase with SilState
{
}

case class SilPropertyState(
  state : SilWord
) extends SilTransformedPhrase with SilState
{
}

case class SilAdpositionalState(
  adposition : SilAdposition,
  objRef : SilReference
) extends SilAdpositionalPhrase with SilState
{
}

case class SilConjunctiveState(
  determiner : SilDeterminer,
  states : Seq[SilState],
  separator : SilSeparator = SEPARATOR_CONJOINED
) extends SilTransformedPhrase with SilState
{
  override def children = states

  override def isConjunctive : Boolean = true
}

case class SilBasicVerbModifier(
  word : SilWord
) extends SilTransformedPhrase with SilVerbModifier
{
}

case class SilDanglingVerbModifier(
  adposition : SilAdposition
) extends SilTransformedPhrase with SilVerbModifier
{
}

case class SilAdpositionalVerbModifier(
  adposition : SilAdposition,
  objRef : SilReference
) extends SilAdpositionalPhrase with SilVerbModifier
{
}

trait SilWord
{
  def senseId : String = ""

  def isProper : Boolean

  def withSense(senseId : String) : SilWord

  def decomposed : Seq[SilSimpleWord]

  def recompose(seq : Seq[String]) : String

  def toLemma : String

  def toNounLemma : String = toLemma

  def toUnfoldedLemma : String

  def toUninflected : SilWord

  def toNounUninflected = toUninflected
}

case class SilSimpleWord(
  inflected : String,
  lemmaUnfolded : String,
  override val senseId : String = ""
) extends SilWord
{
  def lemma = lemmaUnfolded.toLowerCase

  override def toUnfoldedLemma = lemmaUnfolded

  override def isProper =
    lemmaUnfolded.head.isUpper || lemmaUnfolded.contains(' ')

  def uninflected = SilSimpleWord("", lemmaUnfolded, senseId)

  override def withSense(senseId : String) =
    SilSimpleWord(inflected, lemmaUnfolded, senseId)

  override def decomposed = Seq(this)

  override def toLemma = lemma

  override def toUninflected =
  {
    SilWord.uninflected(lemma).withSense(senseId)
  }

  override def recompose(seq : Seq[String]) =
  {
    seq.mkString(" ")
  }
}

case class SilCompoundWord(
  components : Seq[SilSimpleWord],
  style : SilCompoundStyle = COMPOUND_OPEN,
  override val senseId : String = ""
) extends SilWord
{
  override def isProper = components.exists(_.isProper)

  override def withSense(senseId : String) =
    SilCompoundWord(components, style, senseId)

  override def decomposed = components

  override def toNounLemma =
    recompose(components.dropRight(1).map(_.inflected) :+
      components.last.lemma)

  override def toLemma =
    recompose(components.map(_.lemma))

  override def toNounUninflected =
  {
    SilCompoundWord(
      components.dropRight(1) :+
        SilWord.uninflected(components.last.lemma),
      style,
      senseId)
  }

  override def toUninflected =
  {
    SilCompoundWord(
      components.map(c => SilWord.uninflected(c.lemma)),
      style,
      senseId)
  }

  override def toUnfoldedLemma =
    recompose(components.map(_.lemmaUnfolded))

  override def recompose(seq : Seq[String]) =
  {
    seq.mkString(separator)
  }

  def separator : String =
  {
    style match {
      case COMPOUND_OPEN => " "
      case COMPOUND_CLOSED => ""
      case COMPOUND_HYPHENATED => "-"
    }
  }
}

object SilWord
{
  def apply(s : String) : SilSimpleWord = SilSimpleWord(s, s)

  def apply(
    inflected : String,
    lemmaUnfolded : String,
    senseId : String = "") : SilSimpleWord =
  {
    SilSimpleWord(inflected, lemmaUnfolded, senseId)
  }

  def uninflected(s : String) = SilSimpleWord("", s)

  def withSense(s : String, senseId : String) =
    SilSimpleWord(s, s, senseId)
}

object SilWordLemma
{
  def unapply(w : SilSimpleWord) =
  {
    Some(w.lemma)
  }
}

object SilWordInflected
{
  def unapply(w : SilSimpleWord) =
  {
    Some(w.inflected)
  }
}

// use unapply with caution since it defeats match exhaustivity checking
// https://nrinaudo.github.io/scala-best-practices/unsafe/custom_extractors.html
object SilStackedStateReference
{
  def apply(
    annotator : SilAnnotator,
    ref : SilReference, states : Seq[SilState]) : SilReference =
  {
    if (states.isEmpty) {
      ref
    } else {
      annotator.stateSpecifiedRef(
        SilStackedStateReference(annotator, ref, states.tail),
        states.head)
    }
  }

  def unapply(ref : SilReference) : Option[(SilReference, Seq[SilState])] =
  {
    ref match {
      case SilStateSpecifiedReference(
        SilStackedStateReference(sub, states),
        state
      ) => {
        Some((sub, state +: states))
      }
      case _ : SilNounReference => Some((ref, Seq.empty))
      case _ => None
    }
  }
}

object SilOptionallyDeterminedReference
{
  def unapply(ref : SilReference) =
  {
    ref match {
      case SilDeterminedReference(
        sub,
        determiner
      ) => {
        Some((sub, determiner))
      }
      case _ => {
        Some((ref, DETERMINER_ABSENT))
      }
    }
  }
}
