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

import scala.collection._

import java.util.concurrent.atomic._

abstract class SilAbstractRefNote(
  val ref : SilAnnotatedReference
) {
  def getCount() : SilCount

  def setCount(count : SilCount)

  def hasCount() : Boolean

  def maybeGender() : Option[SilGender]

  def setGender(gender : SilGender)

  def hasGender() : Boolean

  def getWord() : Option[SilWord]

  def setWord(word : SilWord)

  def clearWord()

  def getPronounMap() : SilPronounMap

  def setPronounMap(pronounMap : SilPronounMap)

  def getContext() : Option[SilReferenceContext]

  def setContext(context : SilReferenceContext)

  def getPredicate() : Option[SilPredicate]

  def setPredicate(predicate : SilPredicate)

  def getRef() : SilReference = ref

  def mergeFrom(oldNote : SilAbstractRefNote)
}

class SilBasicRefNote(
  ref : SilAnnotatedReference
) extends SilAbstractRefNote(ref)
{
  private var count : Option[SilCount] = None

  private var gender : Option[SilGender] = None

  private var word : Option[SilWord] = None

  private var pronounMap : SilPronounMap = SilPronounMap()

  private var context : Option[SilReferenceContext] = None

  private var predicate : Option[SilPredicate] = None

  override def hasCount() : Boolean =
  {
    count.nonEmpty
  }

  override def getCount() : SilCount =
  {
    count.getOrElse {
      val newCount = SilUtils.deriveCount(ref)
      count = Some(newCount)
      newCount
    }
  }

  override def setCount(newCount : SilCount)
  {
    count = Some(newCount)
  }

  override def hasGender() : Boolean =
  {
    gender.nonEmpty
  }

  override def maybeGender() : Option[SilGender] =
  {
    gender
  }

  override def setGender(newGender : SilGender)
  {
    gender = Some(newGender)
  }

  override def getPronounMap() : SilPronounMap = pronounMap

  override def setPronounMap(newPronounMap : SilPronounMap)
  {
    pronounMap = newPronounMap
  }

  override def getWord() : Option[SilWord] =
  {
    word
  }

  override def setWord(newWord : SilWord)
  {
    word = Some(newWord)
  }

  override def clearWord()
  {
    word = None
  }

  override def getContext() : Option[SilReferenceContext] = context

  override def setContext(newContext : SilReferenceContext)
  {
    context = Some(newContext)
  }

  override def getPredicate() : Option[SilPredicate] = predicate

  override def setPredicate(newPredicate : SilPredicate)
  {
    predicate = Some(newPredicate)
  }

  override def mergeFrom(
    oldNote : SilAbstractRefNote)
  {
    oldNote matchPartial {
      case basic : SilBasicRefNote => {
        if (count.isEmpty) {
          count = basic.count
        }
        if (gender.isEmpty) {
          gender = basic.gender
        }
        if (word.isEmpty && (ref == oldNote.ref)) {
          word = basic.word
        }
        if (pronounMap.isEmpty) {
          pronounMap = basic.pronounMap
        }
        if (!context.isDefined) {
          context = basic.context
        }
        if (!predicate.isDefined) {
          predicate = basic.predicate
        }
      }
    }
  }
}

trait SilAnnotator
{
  def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType,
    newId : Int = generateId) : ReferenceType

  def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType,
    copyOptions : SilPhraseCopyOptions = SilPhraseCopyOptions()
  ) : ReferenceType

  def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType)

  def getBasicNote(ref : SilAnnotatedReference) : SilAbstractRefNote

  def generateId : Int

  def copy[PhraseType <: SilPhrase](
    phrase : PhraseType,
    copyOptions : SilPhraseCopyOptions = SilPhraseCopyOptions()
  ) : PhraseType =
  {
    val rewriter = new SilPhraseRewriter(this)
    rewriter.deepclone(phrase, copyOptions)
  }

  def nounRef(
    noun : SilWord,
    count : SilCount = COUNT_SINGULAR,
    genderOpt : Option[SilGender] = None
  ) : SilNounReference =
  {
    val newRef = register(SilNounReference.unannotated(noun))
    val basic = getBasicNote(newRef)
    basic.setCount(count)
    genderOpt.foreach(gender => basic.setGender(gender))
    newRef
  }

  def determinedNounRef(
    noun : SilWord, determiner : SilDeterminer,
    count : SilCount = COUNT_SINGULAR,
    genderOpt : Option[SilGender] = None
  ) : SilReference =
  {
    val ref = nounRef(noun, count, genderOpt)
    determinedRef(ref, determiner)
  }

  def mappedRef(key : String, determiner : SilDeterminer, gender : SilGender) =
  {
    val newRef = register(SilMappedReference.unannotated(key, determiner))
    val basic = getBasicNote(newRef)
    basic.setGender(gender)
    newRef
  }

  def parenthesizedRef(reference : SilReference, bracket : SilBracket) =
  {
    register(SilParenthesizedReference.unannotated(reference, bracket))
  }

  def appositionalRef(primary : SilReference, secondary : SilReference) =
  {
    register(SilAppositionalReference.unannotated(primary, secondary))
  }

  def stateSpecifiedRef(reference : SilReference, state : SilState) =
  {
    register(SilStateSpecifiedReference.unannotated(reference, state))
  }

  def genitiveRef(possessor : SilReference, possessee : SilReference) =
  {
    register(SilGenitiveReference.unannotated(possessor, possessee))
  }

  def pronounRef(
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    genderAnalyzer : SilGenderAnalyzer,
    proximity : SilProximity = PROXIMITY_ENTITY,
    politeness : SilPoliteness = SilPoliteness.DEFAULT,
    word : Option[SilWord] = None,
    pronounMap : SilPronounMap = SilPronounMap()) =
  {
    val newRef = register(SilPronounReference.unannotated(
      person, genderAnalyzer.canonicalGender(gender), count, proximity,
      politeness))
    val note = getBasicNote(newRef)
    word.foreach(w => note.setWord(w))
    note.setPronounMap(pronounMap)
    newRef
  }

  def basicPronounRef(
    person : SilPerson,
    gender : SilGender,
    count : SilCount,
    proximity : SilProximity = PROXIMITY_ENTITY,
    politeness : SilPoliteness = SilPoliteness.DEFAULT,
    word : Option[SilWord] = None,
    pronounMap : SilPronounMap = SilPronounMap()) =
  {
    pronounRef(
      person, gender, count, SilGenderPreserver, proximity,
      politeness, word, pronounMap)
  }

  def conjunctiveRef(
    determiner : SilDeterminer,
    references : Seq[SilReference],
    separator : SilSeparator = SEPARATOR_CONJOINED) =
  {
    register(SilConjunctiveReference.unannotated(
      determiner, references, separator))
  }

  def determinedRef(
    reference : SilReference,
    determiner : SilDeterminer) =
  {
    determiner match {
      case DETERMINER_ABSENT => reference
      case _ => register(
        SilDeterminedReference.unannotated(reference, determiner))
    }
  }

  def quotationRef(
    quotation : String,
    bracket : SilBracket = BRACKET_DQUOTE) =
  {
    register(SilQuotationReference.unannotated(quotation, bracket))
  }

  def stateQualifiedRef(
    reference : SilReference,
    qualifiers : Seq[SilState])
      : SilReference =
  {
    val (sub, determiner) = reference match {
      case SilDeterminedReference(s, d) => tupleN((s, d))
      case _ => tupleN((reference, DETERMINER_ABSENT))
    }
    val rewritten = {
      if (qualifiers.isEmpty) {
        sub
      } else if (qualifiers.size == 1) {
        stateSpecifiedRef(
          sub, qualifiers.head)
      } else {
        stateSpecifiedRef(
          sub,
          SilConjunctiveState(
            DETERMINER_ALL,
            qualifiers,
            SEPARATOR_CONJOINED))
      }
    }
    determinedRef(rewritten, determiner)
  }

  def qualifiedRef(
    reference : SilReference,
    qualifiers : Seq[SilWord])
      : SilReference =
  {
    stateQualifiedRef(reference, qualifiers.map(SilPropertyState))
  }
}

trait SilAnnotation[NoteType <: SilAbstractRefNote]
{
  def getNote(ref : SilAnnotatedReference) : NoteType
}

class SilTypedAnnotator[NoteType <: SilAbstractRefNote](
  noteSupplier : (SilAnnotatedReference) => NoteType
) extends SilAnnotator with SilAnnotation[NoteType]
{
  private val nextId = new AtomicInteger

  private val map = new mutable.LinkedHashMap[Int, NoteType]

  def getMap() : Map[Int, NoteType] = map

  override def generateId : Int = nextId.incrementAndGet

  override def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType,
    newId : Int) : ReferenceType =
  {
    if (!ref.hasAnnotation) {
      ref.registerAnnotation(this, newId)
    }
    ref
  }

  override def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType,
    copyOptions : SilPhraseCopyOptions = SilPhraseCopyOptions()
  ) : ReferenceType =
  {
    val newId = {
      if (copyOptions.preserveIds) {
        oldRef.getAnnotationId
      } else {
        generateId
      }
    }
    val annotatedRef = register(ref, newId)
    if (copyOptions.preserveNotes) {
      preserveNote(oldRef, annotatedRef)
    } else if (copyOptions.preserveBasicNotes) {
      preserveBasicNote(oldRef, annotatedRef)
    }
    annotatedRef
  }

  private def preserveBasicNote(
    oldRef : SilAnnotatedReference,
    newRef : SilAnnotatedReference)
  {
    if (oldRef.hasAnnotation) {
      val oldNote = oldRef.getAnnotator.getBasicNote(oldRef)
      val newNote = getBasicNote(newRef)
      if (oldNote.hasCount) {
        newNote.setCount(oldNote.getCount)
      }
      if (oldNote.hasGender) {
        newNote.setGender(oldNote.maybeGender.get)
      }
      oldNote.getWord.foreach(word => {
        newNote.setWord(word)
      })
      newNote.setPronounMap(oldNote.getPronounMap)
    }
  }

  override def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType
  ) {
    if ((oldRef ne newRef) && oldRef.hasAnnotation) {
      val oldNote = oldRef.getAnnotator.getBasicNote(oldRef)
      val newNote = newRef.getAnnotator.getBasicNote(newRef)
      newNote.mergeFrom(oldNote)
    }
  }

  override def getNote(ref : SilAnnotatedReference) : NoteType =
  {
    register(ref)
    map.getOrElseUpdate(ref.getAnnotationId, noteSupplier(ref))
  }

  override def getBasicNote(ref : SilAnnotatedReference) : SilAbstractRefNote =
  {
    getNote(ref)
  }
}

object SilBasicAnnotator
{
  def apply() =
  {
    new SilTypedAnnotator[SilBasicRefNote](
      (ref) => new SilBasicRefNote(ref))
  }
}

object SilAnnotator extends SilPhraseQuerier
{
  private def checkRef(
    annotator : SilAnnotator,
    idSet : mutable.Set[Int]
  ) = queryMatcher {
    case annotatedRef : SilAnnotatedReference => {
      assert(annotatedRef.hasAnnotation,
        "Annotation lost for " + annotatedRef)
      assert(annotatedRef.getAnnotator == annotator,
        "Annotator mismatch for " + annotatedRef)
      val id = annotatedRef.getAnnotationId
      assert(!idSet.contains(id),
        s"Duplicate annotation id $id for " + annotatedRef)
      idSet += id
    }
  }

  def sanityCheck(annotator : SilAnnotator, phrase : SilPhrase)
  {
    val idSet = new mutable.HashSet[Int]
    query(
      checkRef(annotator, idSet),
      phrase)
  }
}
