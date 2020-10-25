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
package com.lingeringsocket.shlurd.corenlp

import com.lingeringsocket.shlurd.parser._
import com.lingeringsocket.shlurd.nlang._
import com.lingeringsocket.shlurd.mind._

// we selectively import suites known to work with CoreNLP

class CorenlpSentencePrinterSpec extends SnlEnglishSentencePrinterSpec
{
}

class CorenlpSentenceTranslatorSpec extends SnlKoreanTranslatorSpec
{
}

class CorenlpParserSpec extends SprParserSpec
{
}

class CorenlpEnglishVerbSpec extends SnlEnglishVerbSpec
{
}

class CorenlpResponderSpec extends SmcResponderSpec
{
}
