/* Magnolia, version 0.7.1. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package magnolia.examples

import magnolia._
import scala.language.experimental.macros

trait WeakHash[T] { def hash(value: T): Int }

object WeakHash {

  type Typeclass[T] = WeakHash[T]

  def combine[T](ctx: CaseClass[WeakHash, T]): WeakHash[T] = new WeakHash[T] {
    def hash(value: T): Int = ctx.parameters.map { param =>
      param.typeclass.hash(param.dereference(value))
    }.foldLeft(0)(_ ^ _)
  }

  implicit val string: WeakHash[String] = _.map(_.toInt).sum
  implicit val int: WeakHash[Int] = identity
  implicit val double: WeakHash[Double] = _.toInt

  implicit def gen[T]: WeakHash[T] = macro Magnolia.gen[T]
}
