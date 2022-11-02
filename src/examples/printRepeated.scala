package magnolia1.examples

import magnolia1.*

trait PrintRepeated[T]:
  def print(t: T): String


object PrintRepeated extends AutoDerivation[PrintRepeated]:
  def join[T](ctx: CaseClass[Typeclass, T]): PrintRepeated[T] = _ =>
    ctx.params.filter(_.repeated).map(_.label).toList.toString

  override def split[T](ctx: SealedTrait[PrintRepeated, T]): PrintRepeated[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }

  given PrintRepeated[String] = _ => ""
  given seq[T](using printT: PrintRepeated[T]): PrintRepeated[Seq[T]] = _ => ""
