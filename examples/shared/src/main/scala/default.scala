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
trait HasDefault[T] { def defaultValue: Either[String, T] }

/** companion object and derivation object for [[HasDefault]] */
object HasDefault {

  type Typeclass[T] = HasDefault[T]

  /** constructs a default for each parameter, using the constructor default (if provided),
    *  otherwise using a typeclass-provided default */
  def combine[T](ctx: CaseClass[HasDefault, T]): HasDefault[T] = new HasDefault[T] {
    def defaultValue = ctx.constructMonadic { param =>
      param.default match {
        case Some(arg) => Right(arg)
        case None => param.typeclass.defaultValue
      }
    }
  }

  /** chooses which subtype to delegate to */
  def dispatch[T](ctx: SealedTrait[HasDefault, T])(): HasDefault[T] = new HasDefault[T] {
    def defaultValue = ctx.subtypes.headOption match {
      case Some(sub) => sub.typeclass.defaultValue
      case None => Left("no subtypes")
    }
  }

  /** default value for a string; the empty string */
  implicit val string: HasDefault[String] = new HasDefault[String] { def defaultValue = Right("") }

  /** default value for ints; 0 */
  implicit val int: HasDefault[Int] = new HasDefault[Int] { def defaultValue = Right(0) }

  /** oh, no, there is no default Boolean... whatever will we do? */
  implicit val boolean: HasDefault[Boolean] = new HasDefault[Boolean] { def defaultValue = Left("truth is a lie") }

  /** default value for sequences; the empty sequence */
  implicit def seq[A]: HasDefault[Seq[A]] = new Typeclass[Seq[A]] { def default = Right(Seq.empty) }

  /** generates default instances of [[HasDefault]] for case classes and sealed traits */
  implicit def gen[T]: HasDefault[T] = macro Magnolia.gen[T]
}
