package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait}

import scala.language.experimental.macros

trait SemiDefault[A] {
  def default: A
}
object SemiDefault {
  @inline def apply[A](implicit A: SemiDefault[A]): SemiDefault[A] = A

  type Typeclass[T] = SemiDefault[T]

  def join[T](ctx: CaseClass[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.construct(p => p.default.getOrElse(p.typeclass.default))
  }
  def split[T](ctx: SealedTrait[SemiDefault, T])(): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.subtypes.head.typeclass.default
  }
  implicit val string: SemiDefault[String] = new SemiDefault[String] { def default = "" }
  implicit val int: SemiDefault[Int] = new SemiDefault[Int] { def default = 0 }

  // NOT IMPLICIT
  def gen[T]: SemiDefault[T] = macro Magnolia.gen[T]
}
