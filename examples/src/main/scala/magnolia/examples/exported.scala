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

import scala.language.experimental.macros

class ExportedTypeclass[T]()

object ExportedTypeclass {
  type Typeclass[T] = ExportedTypeclass[T]
  case class Exported[T]() extends ExportedTypeclass[T]
  def combine[T](ctx: CaseClass[Typeclass, T]): Exported[T] = Exported()
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Exported[T] = Exported()

  implicit val intInstance: Typeclass[Int] = new ExportedTypeclass()
  implicit def seqInstance[T: Typeclass]: Typeclass[Seq[T]] = new ExportedTypeclass()
  def gen[T]: Exported[T] = macro Magnolia.gen[T]
}
