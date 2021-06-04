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

}
