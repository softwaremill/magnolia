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

import magnolia.*

trait Eq[T]:
  def equal(value: T, value2: T): Boolean

object Eq extends Derivation[Eq]:
  def join[T](ctx: CaseClass[Eq, T]): Eq[T] = (v1, v2) =>
    ctx.params.forall { p => p.typeclass.equal(p.deref(v1), p.deref(v2)) }

  override def split[T](ctx: SealedTrait[Eq, T]): Eq[T] = (v1, v2) => ctx.choose(v1) {
    sub => sub.typeclass.equal(sub.value, sub.cast(v2))
  }

  given Eq[String] = _ == _
  given Eq[Int] = _ == _

  given [T: Eq]: Eq[Option[T]] =
    case (Some(v1), Some(v2)) => summon[Eq[T]].equal(v1, v2)
    case (None, None)         => true
    case _                    => false

  given [T: Eq, C[x] <: Iterable[x]]: Eq[C[T]] = (v1, v2) =>
    v1.size == v2.size && (v1.iterator zip v2.iterator).forall((summon[Eq[T]].equal).tupled)