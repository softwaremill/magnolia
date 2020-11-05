package magnolia.tests

import magnolia.{Magnolia, SealedTrait}

import scala.language.experimental.macros

trait NoCombine[A] {
  def nameOf(value: A): String
}

object NoCombine {
  type Typeclass[T] = NoCombine[T]
  implicit def gen[T]: NoCombine[T] = macro Magnolia.gen[T]

  def dispatch[T](ctx: SealedTrait[NoCombine, T]): NoCombine[T] = instance { value =>
    ctx.dispatch(value)(sub => sub.typeclass.nameOf(sub.cast(value)))
  }

  def instance[T](name: T => String): NoCombine[T] = new Typeclass[T] {
    def nameOf(value: T): String = name(value)
  }
}
