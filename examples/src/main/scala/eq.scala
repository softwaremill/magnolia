package magnolia.examples

import scala.language.higherKinds

import magnolia._
import scala.language.experimental.macros

trait Eq[T] { def equal(value: T, value2: T): Boolean }

object Eq {
  type Typeclass[T] = Eq[T]
  def combine[T](ctx: CaseClass[Eq, T]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T) =
      ctx.parameters.forall { param => param.typeclass.equal(param.dereference(value1), param.dereference(value2)) }
  }

  def dispatch[T](ctx: SealedTrait[Eq, T]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T): Boolean =
      ctx.dispatch(value1) { case sub => sub.typeclass.equal(sub.cast(value1), sub.cast(value2)) }
  }

  implicit val string: Eq[String] = new Eq[String] { def equal(v1: String, v2: String) = v1 == v2 }
  implicit val int: Eq[Int] = new Eq[Int] { def equal(v1: Int, v2: Int) = v1 == v2 }
  implicit def gen[T]: Eq[T] = macro Magnolia.gen[T]
}
