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
      val ivanRef = SilNounReference(SilWord(ivan))
      val borisRef = SilNounReference(SilWord(boris))
      val natashaRef = SilNounReference(SilWord(natasha))
      val sheRef = SilPronounReference(PERSON_THIRD, GENDER_F, COUNT_SINGULAR)
      val heRef = SilPronounReference(PERSON_THIRD, GENDER_M, COUNT_SINGULAR)
      val beastRef = SilNounReference(SilWord("beast"))
      val ownerRef = SilNounReference(SilWord("owner"))
      analyze("a woman's gender must be feminine")
      analyze(s"$natasha is a woman")
      val natashaEntity = expectProperName(natasha)
      val natashaSet = Set(natashaEntity)
      analyze("a mule is a kind of animal") must beEmpty
      analyze("a mule's gender must be masculine")
      analyze("a mule may be grumpy or happy") must beEmpty
      analyze("if a mule kicks an spc-entity, " +
        "then equivalently the mule is grumpy")
      analyze("a woman's beast must be a mule")
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
        SilGenitiveReference(natashaRef, beastRef) -> Set()
      )

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"he loves $natasha") must be equalTo Map(
        natashaRef -> natashaSet,
        heRef -> borisSet
      )
      stopSequence

      startSequence
      analyze(s"$boris is happy") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"$ivan loves his owner") must be equalTo Map(
        ivanRef -> ivanSet,
        heRef -> ivanSet,
        SilGenitiveReference(heRef, ownerRef) -> Set()
      )
      stopSequence
    }
  }
}
