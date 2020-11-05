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
package magnolia.tests

import magnolia.{Magnolia, SealedTrait}

import scala.language.experimental.macros

trait NoCombine[A] {
  def nameOf(value: A): String
}

object NoCombine {
  type Typeclass[T] = NoCombine[T]
  implicit def gen[T]: NoCombine[T] = macro Magnolia.gen[T]

  def dispatch[T](ctx: SealedTrait[NoCombine, T]): NoCombine[T] = instance { value =>
    ctx.dispatch(value)(sub => sub.typeclass.nameOf(sub.cast(value)))
  }

  def instance[T](name: T => String): NoCombine[T] = new Typeclass[T] {
    def nameOf(value: T): String = name(value)
  }
}
