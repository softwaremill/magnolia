package magnolia

object TestApp extends App {
  case class MyAnnotation(a: Int) extends scala.annotation.Annotation

  @MyAnnotation(1) case class MyCaseClass[A](@MyAnnotation(2) @MyAnnotation(10) i: A @MyAnnotation(3) ,@MyAnnotation(4) s: String @MyAnnotation(5))

  trait Show[T] {
    def show(t: T): String
  }

  object Show extends MagnoliaDerivation[Show] {
    def combine[T](ctx: CaseClass[Show, T]): Show[T] = new Show[T] {
      def show(value: T): String = ctx.parameters.map { p =>
        s"${p.label}=${p.typeclass.show(p.dereference(value))}"
      }.mkString("{", ",", "}")
    }

    def dispatch[T](ctx: SealedTrait[Show, T]): Show[T] = {
      new Show[T] {
        def show(value: T): String = ctx.dispatch(value) { sub =>
          sub.typeclass.show(sub.cast(value))
        }
      }
    }

    given IntShow as Show[Int] {
      def show(t: Int): String = t.toString
    }

    given StringShow as Show[String] {
      def show(t: String): String = t
    }
  }

  List(
    MyCaseClass[Int](1, "a")
  ).map(Show.derived[MyCaseClass[Int]].show).foreach(println)

}
