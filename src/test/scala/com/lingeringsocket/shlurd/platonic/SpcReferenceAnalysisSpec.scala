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
package com.lingeringsocket.shlurd.platonic

import com.lingeringsocket.shlurd._
import com.lingeringsocket.shlurd.ilang._
import com.lingeringsocket.shlurd.mind._

import scala.collection._

class SpcReferenceAnalysisSpec extends SpcProcessingSpecification
{
  trait AnalysisContext extends ProcessingContext
  {
    protected def startSequence()
    {
      mind.startConversation
      mind.startNarrative
    }

    protected def stopSequence()
    {
      mind.stopNarrative
      mind.stopConversation
    }

    protected def analyze(
      input : String, response : String)
        : Map[SilReference, (SilReferenceContext, Set[SpcEntity])] =
    {
      val isolated = !mind.isConversing
      if (isolated) {
        startSequence
      }
      process(
        input,
        ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams(verbosity = RESPONSE_TERSE),
        okExecutor
      ) must be equalTo response
      val result = mind.getConversation.getUtterances.
        dropRight(1).last.refMap
      if (isolated) {
        stopSequence
      }
      result.map {
        case (ref, entities) => {
          val contextOpt = ref match {
            case ar : SilAnnotatedReference => {
              SpcAnnotator(ar.getAnnotator).getNote(ar).getContext
            }
            case _ => None
          }
          tupleN((ref, tupleN((contextOpt.getOrElse(REF_SUBJECT), entities))))
        }
      }
    }

    protected def analyze(input : String)
        : Map[SilReference, (SilReferenceContext, Set[SpcEntity])] =
    {
      analyze(input, "OK.")
    }

    protected def okExecutor() =
    {
      new SmcExecutor[SpcEntity] {
        override def executeAction(
          predicate : SilActionPredicate,
          refMap : SpcRefMap
        ) : Option[String] =
        {
          Some("OK.")
        }

        override def executeImperative(
          predicate : SilPredicate,
          refMap : SpcRefMap
        ) : Option[String] =
        {
          Some("OK.")
        }
      }
    }
  }

  "SpcReferenceAnalysis" should
  {
    "analyze references" in new AnalysisContext
    {
      SpcPrimordial.initCosmos(cosmos)
      val ivan = "Ivan"
      val boris = "Boris"
      val natasha = "Natasha"
      val annotator = SpcAnnotator(mind)
      val ivanRef = annotator.nounRef(SilWord(ivan))
      val borisRef = annotator.nounRef(SilWord(boris))
      val natashaRef = annotator.nounRef(SilWord(natasha))
      val beastRef = annotator.nounRef(SilWord("beast"))
      val ownerRef = annotator.nounRef(SilWord("owner"))
      analyze("Masculine is a kind of gender")
      analyze("Feminine is a kind of gender")
      analyze(s"$natasha is a woman")
      analyze("Feminine is Woman's spc-gender")
      val natashaEntity = expectProperName(natasha)
      val natashaSet = Set(natashaEntity)
      analyze("a mule is a kind of animal") must beEmpty
      analyze("Masculine is Mule's spc-gender")
      analyze("a mule may be grumpy or happy") must beEmpty
      analyze("if a mule kicks an spc-entity, " +
        "then equivalently the mule is grumpy")
      analyze("a woman's beast must be a mule")
      val sheRef = annotator.pronounRef(
        PERSON_THIRD, GENDER_FEMININE, COUNT_SINGULAR)
      val heRef = annotator.pronounRef(
        PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR)
      val himselfRef = annotator.pronounRef(
        PERSON_THIRD, GENDER_MASCULINE, COUNT_SINGULAR, DISTANCE_REFLEXIVE)
      val isaResult = analyze(s"$ivan is a mule")
      val ivanEntity = expectProperName(ivan)
      val ivanSet = Set(ivanEntity)
      // FIXME why is this REF_COMPLEMENT instead of REF_SUBJECT?
      isaResult must be equalTo Map(
        ivanRef -> tupleN((REF_COMPLEMENT, ivanSet))
      )
      analyze("if a mule becomes happy, " +
        s"then the mule kicks $ivan")
      analyze(s"$boris is a mule")
      val borisEntity = expectProperName(boris)
      val borisSet = Set(borisEntity)
      analyze(s"$ivan is grumpy") must be equalTo Map(
        ivanRef -> tupleN((REF_SUBJECT, ivanSet))
      )
      analyze(s"is $ivan a mule", "Yes.") must be equalTo Map(
        ivanRef -> tupleN((REF_COMPLEMENT, ivanSet))
      )
      analyze(s"does $ivan kick $boris", "Yes.") must be equalTo Map(
        ivanRef -> tupleN((REF_SUBJECT, ivanSet)),
        borisRef -> tupleN((REF_DIRECT_OBJECT, borisSet))
      )
      analyze(s"shout at $boris") must be equalTo Map(
        borisRef -> tupleN((REF_ADPOSITION_OBJ, borisSet))
      )
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> tupleN((REF_SUBJECT, borisSet))
      )
      analyze(s"${natasha}'s beast kicks her") must be equalTo Map(
        natashaRef -> tupleN((REF_GENITIVE_POSSESSOR, natashaSet)),
        sheRef -> tupleN((REF_DIRECT_OBJECT, natashaSet)),
        annotator.genitiveRef(natashaRef, beastRef) ->
          tupleN((REF_SUBJECT, Set()))
      )

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> tupleN((REF_SUBJECT, borisSet))
      )
      analyze(s"he loves $natasha") must be equalTo Map(
        natashaRef -> tupleN((REF_DIRECT_OBJECT, natashaSet)),
        heRef -> tupleN((REF_SUBJECT, borisSet))
      )
      analyze(s"he loves himself") must be equalTo Map(
        heRef -> tupleN((REF_SUBJECT, borisSet)),
        himselfRef -> tupleN((REF_DIRECT_OBJECT, borisSet))
      )
      stopSequence

      if (false) {
        startSequence
        analyze(s"$boris is happy") must be equalTo Map(
          borisRef -> tupleN((REF_SUBJECT, borisSet))
        )
        analyze(s"$ivan loves him") must be equalTo Map(
          ivanRef -> tupleN((REF_SUBJECT, ivanSet)),
          heRef -> tupleN((REF_DIRECT_OBJECT, borisSet))
        )
        analyze(s"$ivan loves himself") must be equalTo Map(
          ivanRef -> tupleN((REF_SUBJECT, ivanSet)),
          himselfRef -> tupleN((REF_DIRECT_OBJECT, ivanSet))
        )
        stopSequence
      }

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> tupleN((REF_SUBJECT, borisSet))
      )
      analyze(s"$ivan loves his owner") must be equalTo Map(
        ivanRef -> tupleN((REF_SUBJECT, ivanSet)),
        heRef -> tupleN((REF_GENITIVE_POSSESSOR, ivanSet)),
        annotator.genitiveRef(heRef, ownerRef) ->
          tupleN((REF_DIRECT_OBJECT, Set()))
      )
      stopSequence
    }
  }
}
