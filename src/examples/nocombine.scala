package magnolia1.examples

import magnolia1.*

trait NoCombine[A]:
  def nameOf(value: A): String

object NoCombine extends AutoDerivation[NoCombine]:
  type Typeclass[T] = NoCombine[T]

  def join[T](ctx: CaseClass[magnolia1.examples.NoCombine, T]): NoCombine[T] =
    instance { value =>
      ctx.typeInfo.short
    }

  override def split[T](ctx: SealedTrait[NoCombine, T]): NoCombine[T] =
    instance { value =>
      ctx.choose(value)(sub => sub.typeclass.nameOf(sub.cast(value)))
    }

  def instance[T](name: T => String): NoCombine[T] = new Typeclass[T] {
    def nameOf(value: T): String = name(value)
  }
