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

trait Csv[A]:
  def apply(a: A): List[String]

object Csv extends Derivation[Csv]:
  
  def join[A](ctx: CaseClass[Csv, A]): Csv[A] = new Csv[A]:
    def apply(a: A): List[String] =
      ctx.params.foldLeft(List[String]()) { (acc, p) => acc ++ p.typeclass(p.dereference(a)) }

  override def split[A](ctx: SealedTrait[Csv, A]): Csv[A] = new Csv[A]:
    def apply(a: A): List[String] = ctx.split(a)(sub => sub.typeclass(sub.cast(a)))

  given csvStr: Csv[String] with
    def apply(a: String): List[String] = List(a)