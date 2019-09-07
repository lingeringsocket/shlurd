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
  val ref : SilReference
) {
  def getCount() : SilCount

  def setCount(count : SilCount)

  def getRef() : SilReference = ref
}

class SilBasicRefNote(
  ref : SilReference
) extends SilAbstractRefNote(ref)
{
  private var count : Option[SilCount] = None

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
}

trait SilAnnotator
{
  def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType) : ReferenceType

  def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType) : ReferenceType

  def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType)

  def getBasicNote(ref : SilAnnotatedReference) : SilAbstractRefNote

  def generateId : Int

  def copy[PhraseType <: SilPhrase](phrase : PhraseType) : PhraseType =
  {
    val rewriter = new SilPhraseRewriter(this)
    rewriter.deepclone(phrase)
  }

  def nounRef(
    noun : SilWord, count : SilCount = COUNT_SINGULAR
  ) : SilNounReference =
  {
    val newRef = register(SilNounReference.unannotated(noun))
    getBasicNote(newRef).setCount(count)
    newRef
  }

  def determinedNounRef(
    noun : SilWord, determiner : SilDeterminer,
    count : SilCount = COUNT_SINGULAR
  ) : SilReference =
  {
    val ref = nounRef(noun, count)
    determinedRef(ref, determiner)
  }

  def mappedRef(key : String, determiner : SilDeterminer) =
  {
    register(SilMappedReference.unannotated(key, determiner))
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
    person : SilPerson, gender : SilGender,
    count : SilCount, distance : SilDistance = DISTANCE_UNSPECIFIED) =
  {
    register(SilPronounReference.unannotated(person, gender, count, distance))
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
      case DETERMINER_UNSPECIFIED => reference
      case _ => register(
        SilDeterminedReference.unannotated(reference, determiner))
    }
  }

  def quotationRef(
    quotation : String) =
  {
    register(SilQuotationReference.unannotated(quotation))
  }

  def stateQualifiedRef(
    reference : SilReference,
    qualifiers : Seq[SilState])
      : SilReference =
  {
    val (sub, determiner) = reference match {
      case SilDeterminedReference(s, d) => tupleN((s, d))
      case _ => tupleN((reference, DETERMINER_UNSPECIFIED))
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
  noteSupplier : (SilReference) => NoteType
) extends SilAnnotator with SilAnnotation[NoteType]
{
  private val nextId = new AtomicInteger

  private val map = new mutable.LinkedHashMap[Int, NoteType]

  def getMap() : Map[Int, NoteType] = map

  override def generateId : Int = nextId.incrementAndGet

  override def register[ReferenceType <: SilAnnotatedReference](
    ref : ReferenceType) : ReferenceType =
  {
    if (!ref.hasAnnotation) {
      ref.registerAnnotation(this, generateId)
    }
    ref
  }

  override def transform[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    ref : ReferenceType) : ReferenceType =
  {
    val annotatedRef = register(ref)
    preserveNote(oldRef, annotatedRef)
    annotatedRef
  }

  override def preserveNote[ReferenceType <: SilAnnotatedReference](
    oldRef : SilAnnotatedReference,
    newRef : ReferenceType
  ) {
    val newId = newRef.getAnnotationId
    // FIXME merge in case both old and new notes are present
    if (oldRef.hasAnnotation && !map.contains(newId)) {
      map.get(oldRef.getAnnotationId).foreach(
        note => map.put(newId, note))
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
      /*
      assert(annotatedRef.getAnnotator == annotator,
        "Annotator mismatch for " + annotatedRef)
      val id = annotatedRef.getAnnotationId
      assert(!idSet.contains(id),
        s"Duplicate annotation id $id for " + annotatedRef)
      idSet += id
       */
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
