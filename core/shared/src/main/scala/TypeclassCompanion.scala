package magnolia

import scala.language.experimental.macros
import scala.language.higherKinds

trait TypeclassCompanion[C[_]] {

  /** Required type constructor for new instances of the typeclass */
  type Typeclass[T] = C[T]

  /** The typeclass constructor */
  def apply[T](implicit c: C[T]): C[T] = c

  /** Defines how a typeclass should be constructed for a given case class */
  def combine[T](ctx: CaseClass[C, T]): C[T]

  /** Defines how to choose which subtype of a sealed trait to use for the
    * construction of the typeclass
    * */
  def dispatch[T](ctx: SealedTrait[C, T]): C[T]

  /** binds the Magnolia macro to the derivation object */
  implicit def gen[T]: C[T] = macro Magnolia.gen[T]

}
