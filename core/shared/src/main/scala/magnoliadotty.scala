package magnolia
import scala.deriving.Mirror
import scala.compiletime.erasedValue
import scala.compiletime.summonInline


// object Magnolia {
//   inline def materialize[A <: Tuple]: List[Print[_]] = inline erasedValue[A] match {
//     case _: Unit => Nil
//     case _: (h *: t) => summonInline[Print[h]] :: materialize[t]
//   }

//   inline transparent def gen[T](using inline m: Mirror.Of[T]): Print[T] = {
//     val elems = materialize[m.MirroredElemTypes]
//     ???
//   }
// }

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


  def dispatch[T](ctx: SealedTrait[Print, T]): Print[T] = { value =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.print(sub.cast(value))
    }
  }

  // implicit def gen[T]: Print[T] = macro Magnolia.gen[T]

  // inline implicit def derived[T: Mirror.Of]: Print[T] = Magnolia.gen[T]

  implicit val string: Print[String] = a => a
  implicit val int: Print[Int] = _.toString

  implicit def seq[T](implicit printT: Print[T]): Print[Seq[T]] = { values =>
    values.map(printT.print).mkString("[", ",", "]")
  }
}

enum MyList/*  derives Print */:
  case Cons(h: Int, t: MyList)
  case End

object MyList {
  lazy val printCons: Print[MyList.Cons] = Print.combine(ccCons)
  lazy val printEnd: Print[MyList.End.type] = Print.combine(ccEnd)
  implicit lazy val printList: Print[MyList] = Print.dispatch(st)
  
  val ccCons: ReadOnlyCaseClass[Print, MyList.Cons] =
    new ReadOnlyCaseClass[Print, MyList.Cons](
      typeName = TypeName("MyList", "Cons", Nil),
      isObject = false,
      isValueClass = false,
      parametersArray = Array(
        ReadOnlyParam.valueParam[Print, MyList.Cons, Int](
          name = "h",
          deref = _.h,
          isRepeated = false,
          typeclassParam = CallByNeed(Print.int),
          annotationsArrayParam = Array()
        ),
        ReadOnlyParam.valueParam[Print, MyList.Cons, MyList](
          name = "t",
          deref = _.t,
          isRepeated = false,
          typeclassParam = CallByNeed(printList),
          annotationsArrayParam = Array()
        )
      ),
      annotationsArray = Array()
    ){}

  val ccEnd: ReadOnlyCaseClass[Print, MyList.End.type] =
    new ReadOnlyCaseClass[Print, MyList.End.type](
      typeName = TypeName("MyList", "End", Nil),
      isObject = true,
      isValueClass = false,
      parametersArray = Array(),
      annotationsArray = Array()
    ){}

  val st: SealedTrait[Print, MyList] = new SealedTrait[Print, MyList](
    TypeName("", "MyList", Nil),
    Array(
      Subtype[Print, MyList, MyList.Cons](
        name = TypeName("MyList", "Cons", Nil),
        idx = 0,
        anns = Array(),
        tc = CallByNeed(MyList.printCons),
        isType = _.isInstanceOf[MyList.Cons],
        asType = _.asInstanceOf[MyList.Cons]
      ),
      Subtype[Print, MyList, MyList.End.type](
        name = TypeName("MyList", "End", Nil),
        idx = 1,
        anns = Array(),
        tc = CallByNeed(MyList.printEnd),
        isType = _ eq MyList.End,
        asType = _ => MyList.End
      )
    ),
    Array()
  )
}
  

@main
def run = println(summon[Print[MyList]].print(MyList.Cons(1, MyList.End)))
