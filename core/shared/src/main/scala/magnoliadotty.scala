package magnolia
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.summonInline


object Magnolia {
  inline def materialize[A <: Tuple]: List[Print[_]] = inline erasedValue[A] match {
    case _: Unit => Nil
    case _: (h *: t) => summonInline[Print[h]] :: materialize[t]
  }

  inline transparent def gen[T](using inline m: Mirror.Of[T]): Print[T] = {
    val elems = materialize[m.MirroredElemTypes]
    ???
  }
}

// Prints a type, only requires read access to fields
trait Print[T] {
  def print(t: T): String
}

object Print {
  type Typeclass[T] = Print[T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Print[T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.print(param.dereference(value))
    }
    else {
      ctx.parameters.map { param =>
        param.typeclass.print(param.dereference(value))
      }.mkString(s"${ctx.typeName.short}(", ",", ")")
    }
  }


  def dispatch[T](ctx: SealedTrait[Print, T])(): Print[T] = { value =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  // implicit def gen[T]: Print[T] = macro Magnolia.gen[T]

  inline def derived[T: Mirror.Of]: Print[T] = Magnolia.gen[T]

  implicit val string: Print[String] = a => a
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
}

enum MyList derives Print:
  case Cons(h: Int, t: MyList)
  case End
  

@main
def run = println(summon[Print[MyList]].print(MyList.Cons(1, MyList.End)))
