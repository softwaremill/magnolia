package magnolia.examples

import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.language.experimental.macros

/** typeclass for providing a default value for a particular type */
trait Default[T] { def default: T }

/** companion object and derivation object for [[Default]] */
object Default {

  type Typeclass[T] = Default[T]
  
  /** constructs a default for each parameter, using the constructor default (if provided),
   *  otherwise using a typeclass-provided default */
  def combine[T](ctx: CaseClass[Default, T]): Default[T] = new Default[T] {
    def default = ctx.construct { param => param.default.getOrElse(param.typeclass.default) }
  }

  /** chooses which subtype to delegate to */
  def dispatch[T](ctx: SealedTrait[Default, T])(): Default[T] = new Default[T] {
    def default: T = ctx.subtypes.head.typeclass.default
  }

  /** default value for a string; the empty string */
  implicit val string: Default[String] = new Default[String] { def default = "" }

  /** default value for ints; 0 */
  implicit val int: Default[Int] = new Default[Int] { def default = 0 }

  /** generates default instances of [[Default]] for case classes and sealed traits */
  implicit def gen[T]: Default[T] = macro Magnolia.gen[T]
}
