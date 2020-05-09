package magnolia
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.constValue
import scala.compiletime.summonInline
import scala.compiletime.summonFrom


object Magnolia {
  inline def materialize[A <: Tuple]: List[Print[_]] = inline erasedValue[A] match {
    case _: Unit => Nil
    case _: (h *: t) => summonInline[Print[h]] :: materialize[t]
  }

  inline def subtypesOf[Parent, T <: Tuple](tpeName: String, idx: Int)(using m: Mirror.SumOf[Parent]): List[Subtype[Print, Parent]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: ((h, label) *: t) =>
        //todo: better way of getting type names https://github.com/lampepfl/dotty/issues/8739

        val childName = inline constValue[label] match {
          case childName: String => childName
        }

        val headSubtype = Subtype[Print, Parent, Parent](
          name = TypeName(tpeName, childName, Nil),
          idx = idx,
          anns = Array(),
          tc = CallByNeed(summonInline[Print[h]].asInstanceOf[Print[Parent]]),
          isType = m.ordinal(_) == idx,
          asType = a => a
        )

        headSubtype :: subtypesOf[Parent, t](tpeName, idx + 1)
    }

  inline def dispatchInternal[T](using m: Mirror.SumOf[T]): Print[T] = {
    val tpeName = constValue[m.MirroredLabel]

    val subtypes = subtypesOf[T, Tuple.Zip[m.MirroredElemTypes, m.MirroredElemLabels]](tpeName, 0)

    //todo parent type
    val st: SealedTrait[Print, T]  = new SealedTrait[Print, T](
      TypeName("", tpeName, Nil),
      subtypes.toArray,
      Array()
    )

    Print.dispatch(st)
  }

  inline def parametersOf[Parent, T <: Tuple](idx: Int)(using m: Mirror.ProductOf[Parent]): List[ReadOnlyParam[Print, Parent]] = {
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: ((h, label) *: t) =>
        val paramName = constValue[label] match {
          case paramName: String => paramName
        }

        val param: ReadOnlyParam[Print, Parent] =
          ReadOnlyParam[Print, Parent, h](
            name = paramName,
            idx = idx,
            isRepeated = false, //todo
            typeclassParam = CallByNeed(summonInline[Print[h]]),
            annotationsArrayParam = Array()
          )

        param :: parametersOf[Parent, t](idx + 1)
    }
  }
  
  inline def combineInternal[T](using m: Mirror.ProductOf[T]): Print[T] = {
    val tpeName = constValue[m.MirroredLabel]

    val cc: ReadOnlyCaseClass[Print, T] = 
      new ReadOnlyCaseClass[Print, T](
        //todo parent type
        typeName = TypeName("", tpeName, Nil),
        isObject = false,
        isValueClass = false,
        parametersArray = parametersOf[T, Tuple.Zip[m.MirroredElemTypes, m.MirroredElemLabels]](0).toArray,
        annotationsArray = Array()
      ){}

    Print.combine(cc)
  }

  inline transparent def gen[T](using m: Mirror.Of[T]): Print[T] = {
    inline m match {
      case sum: Mirror.SumOf[T] => dispatchInternal[T](using sum)
      case prod: Mirror.ProductOf[T] => combineInternal[T](using prod)
    }
  }.asInstanceOf[Print[T]]
}

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
        param.label + " = " + param.typeclass.print(param.dereference(value))
      }.mkString(s"${ctx.typeName.short}(", ", ", ")")
    }
  }


  def dispatch[T](ctx: SealedTrait[Print, T]): Print[T] = { value =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  inline implicit def derived[T: Mirror.Of]: Print[T] = Magnolia.gen[T]

  implicit val string: Print[String] = a => a
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
}

enum MyList derives Print:
  case Cons(h: Int, t: String)
  case End

@main
def run = println(summon[Print[MyList]].print(MyList.Cons(1, "foo")))
// def run = println(summon[Print[MyList]].print(MyList.Cons(1)))
