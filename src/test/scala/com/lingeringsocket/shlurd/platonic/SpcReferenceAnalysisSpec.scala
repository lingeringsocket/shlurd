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
import com.lingeringsocket.shlurd.parser._

import scala.collection._

class SpcReferenceAnalysisSpec extends SpcProcessingSpecification
{
  trait AnalysisContext extends ProcessingContext
  {
    protected def analyze(input : String, response : String) =
    {
      mind.startConversation
      mind.startNarrative
      process(
        input,
        ACCEPT_MODIFIED_BELIEFS,
        SmcResponseParams(verbosity = RESPONSE_TERSE),
        okExecutor) must be equalTo response
      val result = mind.getConversation.getUtterances.head.referenceMap
      mind.stopNarrative
      mind.stopConversation
      result
    }

    protected def analyzeBelief(input : String) =
    {
      analyze(input, "OK.")
    }

    protected def okExecutor() =
    {
      new SmcExecutor[SpcEntity] {
        override def executeAction(
          predicate : SilActionPredicate,
          referenceMap : Map[SilReference, Set[SpcEntity]]
        ) : Option[String] =
        {
          Some("OK.")
        }

        override def executeImperative(
          predicate : SilPredicate,
          referenceMap : Map[SilReference, Set[SpcEntity]]
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
      if (SprParser.isCoreNLP) {
        skipped("CoreNLP not supported")
      }
      val ivan = "Ivan"
      val boris = "Boris"
      val ivanRef = SilNounReference(SilWord(ivan))
      val borisRef = SilNounReference(SilWord(boris))
      analyzeBelief("a mule is a kind of animal") must beEmpty
      analyzeBelief("a mule may be grumpy or happy") must beEmpty
      analyzeBelief("if a mule kicks a kick-target, " +
        "then equivalently the mule is grumpy")
      val isaResult = analyzeBelief(s"$ivan is a mule")
      val ivanEntity = expectProperName(ivan)
      val ivanSet = Set(ivanEntity)
      isaResult must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyzeBelief("if a mule is happy, " +
        s"then the mule kicks $ivan")
      analyzeBelief(s"$boris is a mule")
      val borisEntity = expectProperName(boris)
      val borisSet = Set(borisEntity)
      analyzeBelief(s"$ivan is grumpy") must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyze(s"is $ivan a mule", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet
      )
      analyze(s"does $ivan kick $boris", "Yes.") must be equalTo Map(
        ivanRef -> ivanSet,
        borisRef -> borisSet
      )
      analyze(s"shout at $boris", "OK.") must be equalTo Map(
        borisRef -> borisSet
      )
      analyze(s"$boris is happy", "OK.") must be equalTo Map(
        borisRef -> borisSet
      )
    }
  }
}
