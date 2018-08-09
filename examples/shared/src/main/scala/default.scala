/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
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

import magnolia._, mercator._
import scala.language.experimental.macros

/** typeclass for providing a default value for a particular type */
trait Default[T] { def default: Either[String, T] }

/** companion object and derivation object for [[Default]] */
object Default {

  type Typeclass[T] = Default[T]

  /** constructs a default for each parameter, using the constructor default (if provided),
    *  otherwise using a typeclass-provided default */
  def combine[T](ctx: CaseClass[Default, T]): Default[T] = new Default[T] {
    def default = ctx.constructMonadic { param =>
      param.default match {
        case Some(arg) => Right(arg)
        case None => param.typeclass.default
      }
    }
  }

  /** chooses which subtype to delegate to */
  def dispatch[T](ctx: SealedTrait[Default, T])(): Default[T] = new Default[T] {
    def default = ctx.subtypes.headOption match {
      case Some(sub) => sub.typeclass.default
      case None => Left("no subtypes")
    }
  }

  /** default value for a string; the empty string */
  implicit val string: Default[String] = new Default[String] { def default = Right("") }

  /** default value for ints; 0 */
  implicit val int: Default[Int] = new Default[Int] { def default = Right(0) }

  /** oh, no, there is no default Boolean... whatever will we do? */
  implicit val boolean: Default[Boolean] = new Default[Boolean] { def default = Left("truth is a lie") }

  /** default value for sequences; the empty sequence */
  implicit def seq[A]: Default[Seq[A]] = new Typeclass[Seq[A]] { def default = Right(Seq.empty) }

  /** generates default instances of [[Default]] for case classes and sealed traits */
  implicit def gen[T]: Default[T] = macro Magnolia.gen[T]
}
