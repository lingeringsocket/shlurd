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
package com.lingeringsocket.shlurd

import java.io._

object SerializationUtils
{
  def serialize[T](obj : T, file : File) : Unit =
  {
    val fileOut = new FileOutputStream(file)
    try {
      val objOut = new ObjectOutputStream(fileOut)
      objOut.writeObject(obj)
      objOut.flush
    } finally {
      fileOut.close
    }
  }

  def deserialize[T](file : File) : T =
  {
    val fileIn = new FileInputStream(file)
    try {
      // https://stackoverflow.com/a/22375260/2913158
      val objIn = new ObjectInputStream(fileIn) {
        override def resolveClass(
          desc: java.io.ObjectStreamClass): Class[_] =
        {
          try {
            Class.forName(desc.getName, false, getClass.getClassLoader)
          } catch {
            case ex : ClassNotFoundException => super.resolveClass(desc)
          }
        }
      }
      objIn.readObject.asInstanceOf[T]
    } finally {
      fileIn.close
    }
  }
}
