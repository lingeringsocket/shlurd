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
        : SpcRefMap =
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
      result
    }

    protected def analyze(input : String)
        : SpcRefMap =
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
      isaResult must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyze("if a mule becomes happy, " +
        s"then the mule kicks $ivan")
      analyze(s"$boris is a mule")
      val borisEntity = expectProperName(boris)
      val borisSet = Set(borisEntity)
      analyze(s"$ivan is grumpy") must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyze(s"is $ivan a mule", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyze(s"does $ivan kick $boris", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet,
        borisRef -> borisSet
      )
      analyze(s"shout at $boris") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"${natasha}'s beast kicks her") must be equalTo Map(
        natashaRef -> natashaSet,
        sheRef -> natashaSet,
        annotator.genitiveRef(natashaRef, beastRef) -> Set()
      )

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"he loves $natasha") must be equalTo Map(
        natashaRef -> natashaSet,
        heRef -> borisSet
      )
      analyze(s"he loves himself") must be equalTo Map(
        heRef -> borisSet,
        himselfRef -> borisSet
      )
      stopSequence

      if (false) {
        startSequence
        analyze(s"$boris is happy") must be equalTo Map(
          borisRef -> borisSet
        )
        analyze(s"$ivan loves him") must be equalTo Map(
          ivanRef -> ivanSet,
          heRef -> borisSet
        )
        analyze(s"$ivan loves himself") must be equalTo Map(
          ivanRef -> ivanSet,
          himselfRef -> ivanSet
        )
        stopSequence
      }

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"$ivan loves his owner") must be equalTo Map(
        ivanRef -> ivanSet,
        heRef -> ivanSet,
        annotator.genitiveRef(heRef, ownerRef) -> Set()
      )
      stopSequence
    }
  }
}
