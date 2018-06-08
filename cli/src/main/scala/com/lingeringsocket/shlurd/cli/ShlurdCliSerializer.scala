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
package com.lingeringsocket.shlurd.cli

import com.twitter.chill.ScalaKryoInstantiator
import com.esotericsoftware.kryo.io._

import java.io._

class ShlurdCliSerializer
{
  private val instantiator = new ScalaKryoInstantiator
  instantiator.setRegistrationRequired(false)
  private val kryo = instantiator.newKryo

  def save(mind : ShlurdCliMind, file : File)
  {
    val fos = new FileOutputStream(file)
    val output = new Output(fos)
    kryo.writeObject(output, mind)
    output.close
  }

  def load(file : File) : ShlurdCliMind =
  {
    val fis = new FileInputStream(file)
    val input = new Input(fis)
    val oldMind = kryo.readObject(input, classOf[ShlurdCliMind])
    input.close
    oldMind
  }
}
