/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package magnolia.examples

import magnolia.{Magnolia, ReadOnlyCaseClass, SealedTrait}

import scala.language.experimental.macros

// Prints a type, only requires read access to fields
trait Print[T] {
  def print(t: T): String
}

trait GenericPrint {
  type Typeclass[T] = Print[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Print[T] = { value =>
  if (ctx.isValueClass) {
    val param = ctx.parameters.head
    param.typeclass.print(param.dereference(value))
  }
  else {
    ctx.parameters.map { param =>
      param.typeclass.print(param.dereference(value))
    }.mkString(s"${ctx.typeName.short}(", ",", ")")
  }
  }


  def dispatch[T](ctx: SealedTrait[Print, T])(): Print[T] = { value =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  implicit def gen[T]: Print[T] = macro Magnolia.gen[T]

}

object Print extends GenericPrint {
  implicit val string: Print[String] = identity
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
}
