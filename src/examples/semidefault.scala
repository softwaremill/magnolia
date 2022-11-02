package magnolia1.examples

import magnolia1._

trait SemiDefault[A]:
  def default: A

object SemiDefault extends AutoDerivation[SemiDefault]:
  @inline def apply[A](implicit A: SemiDefault[A]): SemiDefault[A] = A

  type Typeclass[T] = SemiDefault[T]

  def join[T](ctx: CaseClass[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.construct(p => p.default.getOrElse(p.typeclass.default))
  }

  override def split[T](ctx: SealedTrait[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.subtypes.head.typeclass.default
  }

  given string: SemiDefault[String] = new SemiDefault[String] { def default = "" }

  given int: SemiDefault[Int] = new SemiDefault[Int] { def default = 0 }
