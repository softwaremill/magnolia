package magnolia1.examples

import magnolia1.{Magnolia, ReadOnlyCaseClass, SealedTrait}

import scala.language.experimental.macros

// Prints a type, only requires read access to fields
trait Print[T] {
  def print(t: T): String
}

trait GenericPrint {
  type Typeclass[T] = Print[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Print[T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.print(param.dereference(value))
    } else {
      ctx.parameters
        .map { param =>
          param.typeclass.print(param.dereference(value))
        }
        .mkString(s"${ctx.typeName.short}(", ",", ")")
    }
  }

  def split[T](ctx: SealedTrait[Print, T])(): Print[T] = { value =>
    ctx.split(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  implicit def gen[T]: Print[T] = macro Magnolia.gen[T]

}

object Print extends GenericPrint {
  implicit val string: Print[String] = identity
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
}
