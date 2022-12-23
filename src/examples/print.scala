package magnolia1.examples

import magnolia1._

// Prints a type, only requires read access to fields
trait Print[T] {
  def print(t: T): String
}

trait GenericPrint extends AutoDerivation[Print]:
  def join[T](ctx: CaseClass[Typeclass, T]): Print[T] = value =>
    ctx.params
      .map { param =>
        param.typeclass.print(param.deref(value))
      }
      .mkString(s"${ctx.typeInfo.short}(", ",", ")")

  override def split[T](ctx: SealedTrait[Print, T]): Print[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }

object Print extends GenericPrint:
  given Print[String] = identity(_)
  given Print[Int] = _.toString
  given seq[T](using printT: Print[T]): Print[Seq[T]] =
    _.map(printT.print).mkString("[", ",", "]")
