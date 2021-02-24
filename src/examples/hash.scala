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

import magnolia._

trait WeakHash[T] { def hash(value: T): Int }

object WeakHash extends MagnoliaDerivation[WeakHash] {

  def combine[T](ctx: CaseClass[WeakHash, T]): WeakHash[T] = new WeakHash[T] {
    def hash(value: T): Int = ctx.parameters.map { param =>
      param.typeclass.hash(param.dereference(value))
    }.foldLeft(0)(_ ^ _)
  }

  override def dispatch[T](ctx: SealedTrait[WeakHash, T]): WeakHash[T] = {
    new WeakHash[T] {
      def hash(value: T): Int = ctx.dispatch(value) { sub =>
        sub.typeclass.hash(value)
      }
    }
  }

  given string: WeakHash[String] = _.map(_.toInt).sum
  given int: WeakHash[Int] = identity
  given double: WeakHash[Double] = _.toInt
}
