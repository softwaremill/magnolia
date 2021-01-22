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

/* automatically derived only for wrapper types (unary product types) */
trait ToString[A] {
  def str(a: A): String
}

object ToString {
  def apply[A: ToString]: ToString[A] = implicitly

  type Typeclass[A] = ToString[A]

  @typeValidation.minMembers(1)
  @typeValidation.maxMembers(1)
  def combine[A](ctx: ReadOnlyCaseClass[ToString, A]): ToString[A] =
    (a: A) => {
      val param = ctx.parameters.head
      param.typeclass.str(param.dereference(a))
    }

  implicit def derive[A]: ToString[A] = macro Magnolia.gen[A]

  implicit val str: ToString[String] = (a: String) => a
  implicit val int: ToString[Int] = (a: Int) => a.toString
}


trait FromString[A] {
  def fromStr(str: String): A
}

object FromString {
  def apply[A: FromString]: FromString[A] = implicitly

  type Typeclass[A] = FromString[A]

  @typeValidation.minMembers(1)
  @typeValidation.maxMembers(1)
  def combine[A](ctx: CaseClass[FromString, A]): FromString[A] =
    (str: String) => ctx.construct(p => p.typeclass.fromStr(str))

  implicit def derive[A]: FromString[A] = macro Magnolia.gen[A]

  implicit val str: FromString[String] = (a: String) => a
  implicit val int: FromString[Int] = (a: String) => a.toInt

}
