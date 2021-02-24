package magnolia

object TestApp {
  case class MyAnnotation(a: Int) extends scala.annotation.Annotation

  @MyAnnotation(1) case class MyCaseClass[A](@MyAnnotation(2) @MyAnnotation(10) i: A @MyAnnotation(3) ,@MyAnnotation(4) s: String @MyAnnotation(5))

  trait Show[T] {
    def show(t: T): String
  }

  object Show extends MagnoliaDerivation[Show] {
    def combine[T](ctx: CaseClass[Show, T]): Show[T] = new Show[T] {
      def show(value: T): String = ctx.typeInfo.short + ctx.parameters.map { p =>
        s"${p.label}=${p.typeclass.show(p.dereference(value))}"
      }.mkString("{", ",", "}")
    }

    override def dispatch[T](ctx: SealedTrait[Show, T]): Show[T] = {
      new Show[T] {
        def show(value: T): String = ctx.dispatch(value) { sub =>
          sub.typeclass.show(value)
        }
      }
    }

    given IntShow: Show[Int] with {
      def show(t: Int): String = t.toString
    }

    given StringShow: Show[String] with {
      def show(t: String): String = t
    }
  }
}

object Main extends App {
  import TestApp._

  enum Tree[T] derives Show:
   case Branch(left: Tree[T], right: Tree[T])
   case Leaf(elem: T)

  sealed trait STree[T] derives Show
  final case class SBranch[T](left: STree[T], right: STree[T]) extends STree[T]
  final case class SLeaf[T](elem: T) extends STree[T]

  case class Number(value: Int) extends AnyVal

  opaque type Num = Int

  object Num {
    def apply(i: Int): Num = i
  }

  List(
    MyCaseClass[Int](1, "a")
  ).map(Show.derived[MyCaseClass[Int]].show).foreach(println)

  List(
    Num(1)
  ).map(summon[Show[Num]].show).foreach(println)

  import Tree._

  List(
    Leaf(1),
    Branch(Leaf(1),  Leaf(2))
  ).map(summon[Show[Tree[Int]]].show).foreach(println)

  List[STree[Int]]( 
    SLeaf(1),
    SBranch(SLeaf(1),  SLeaf(2)),
    SBranch(SBranch(SLeaf(1),  SLeaf(2)),  SLeaf(3))
  ).map(summon[Show[STree[Int]]].show).foreach(println)

  sealed trait A derives Show
  case class AB(b: B) extends A
  case class AEnd(n: Num) extends A

  sealed trait B derives Show
  case class BA(a: A) extends B

  List( 
    AB(BA(AB(BA(AEnd(Num(2))))))
  ).map(summon[Show[A]].show).foreach(println)

}
