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

/** Demonstrate some flexibility for the user-facing API of your type classes
  *
  * - how to export multiple type class derivations behind one underscore import
  * - how to offer both auto and semiauto derivations through different imports
  */
object proxies {
  // if wanted these can go in a trait and be reexported through other paths as well
  object auto extends auto

  trait auto {
    @proxy(ToString1)
    implicit def deriveToString1[A]: ToString1[A] = macro Magnolia.gen[A]

    @proxy(ToString2)
    implicit def deriveToString2[A]: ToString2[A] = macro Magnolia.gen[A]
  }

  // or directly in an object, or in the companion objects or wherever
  object semiauto {
    @proxy(ToString1)
    def deriveToString1[A]: ToString1[A] = macro Magnolia.gen[A]

    @proxy(ToString2)
    def deriveToString2[A]: ToString2[A] = macro Magnolia.gen[A]
  }

  implicit class Syntax[T](private val t: T) extends AnyVal {
    def str1(implicit ev: ToString1[T]): String = ev.str1(t)
    def str2(implicit ev: ToString2[T]): String = ev.str2(t)
  }

  // an example type class which just stringifies data in a non-useful way
  trait ToString1[A] {
    def str1(a: A): String
  }

  object ToString1 {
    def apply[A: ToString1]: ToString1[A] = implicitly

    type Typeclass[A] = ToString1[A]

    def combine[A](ctx: ReadOnlyCaseClass[ToString1, A]): ToString1[A] =
      (a: A) => ctx.parameters.map(param => param.typeclass.str1(param.dereference(a))).mkString(", ")

    implicit val str: ToString1[String] = (a: String) => a
    implicit val int: ToString1[Int] = (a: Int) => a.toString
  }

  // this is just here to test that we can export more than one derivation
  trait ToString2[A] {
    def str2(a: A): String
  }

  object ToString2 {
    def apply[A: ToString2]: ToString2[A] = implicitly

    type Typeclass[A] = ToString2[A]

    def combine[A](ctx: ReadOnlyCaseClass[ToString2, A]): ToString2[A] =
      (a: A) => ctx.parameters.map(param => param.typeclass.str2(param.dereference(a))).mkString("; ")

    implicit val str: ToString2[String] = (a: String) => a
    implicit val int: ToString2[Int] = (a: Int) => a.toString
  }
}
