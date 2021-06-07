package magnolia.tests

import magnolia.examples.*

case class NewRecursive(children: Seq[NewRecursive])
case class NonRecursive(children: Seq[Int])

class newTests extends munit.FunSuite {
  test("print non-recursive") {
    val res = SemiPrint.derived[NonRecursive].print(NonRecursive(Seq(1,2,3)))

    assertEquals(res, """NonRecursive(1, 2, 3)""")
  }
  
  test("print recursive") {
    val res = SemiPrint.derived[NewRecursive].print(NewRecursive(Seq.empty))

    assertEquals(res, """NewRecursive()""")
  }

  test("basic use case") {
    case class X(a: Int, b: String)
    case class Y(c: String)
    val instance = SemiPrint.derived[X]

    // val res = instance.print(X(1, Y("s")))
    val res = instance.print(X(1, "s"))
    assertEquals(res, """Y(1, X(s))""")
  }
}
