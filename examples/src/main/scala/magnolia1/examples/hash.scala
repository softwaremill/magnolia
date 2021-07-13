package magnolia1.examples

import magnolia1.{CaseClass, Magnolia}

import scala.language.experimental.macros

trait WeakHash[T] { def hash(value: T): Int }

object WeakHash {

  type Typeclass[T] = WeakHash[T]

  def combine[T](ctx: CaseClass[WeakHash, T]): WeakHash[T] = new WeakHash[T] {
    def hash(value: T): Int = ctx.parameters
      .map { param =>
        param.typeclass.hash(param.dereference(value))
      }
      .foldLeft(0)(_ ^ _)
  }

  implicit val string: WeakHash[String] = _.map(_.toInt).sum
  implicit val int: WeakHash[Int] = identity
  implicit val double: WeakHash[Double] = _.toInt

  implicit def gen[T]: WeakHash[T] = macro Magnolia.gen[T]
}
