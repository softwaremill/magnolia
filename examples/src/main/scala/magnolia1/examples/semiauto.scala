package magnolia1.examples

import scala.language.experimental.macros
import magnolia1._

trait SemiPrint[A]:
  def print(a: A): String

object SemiPrint extends Derivation[SemiPrint]:
  def join[T](ctx: CaseClass[Typeclass, T]): SemiPrint[T] = value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.print(param.deref(value))
    else
      ctx.params
        .map { param =>
          param.typeclass.print(param.deref(value))
        }
        .mkString(s"${ctx.typeInfo.short}(", ",", ")")

  override def split[T](ctx: SealedTrait[SemiPrint, T]): SemiPrint[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }

  given SemiPrint[String] with
    def print(s: String) = s

  given SemiPrint[Int] with
    def print(i: Int) = i.toString

  given seq[T](using spt: SemiPrint[T]): SemiPrint[Seq[T]] with
    def print(t: Seq[T]) = t.map(spt.print).mkString(", ")
