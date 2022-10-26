package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

class RecursiveTypesTests extends munit.FunSuite:
  import RecursiveTypesTests.*

  test("serialize a Leaf") {
    val res = summon[Show[String, Leaf[String]]].show(Leaf("testing"))
    assertEquals(res, "Leaf[String](value=testing)")
  }

  // TODO not working - not serializing the concrete type down the hierarchy: "T" instead of String
  // test("serialize a Branch") {
  //   val res = summon[Show[String, Branch[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
  //   assertEquals(res, "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")
  // }

  // TODO not working - not serializing the concrete type down the hierarchy: "T" instead of String
  // test("serialize a Branch") {
  //   val res = summon[Show[String, Tree[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
  //   assertEquals(res, "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")
  // }

  test("test branch equality true") {
    val res = Eq
      .derived[Tree[String]]
      .equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
    assert(res)
  }

  test("serialize self recursive type in base case") {
    val res = summon[Show[String, GPerson]].show(GPerson(Nil))
    assertEquals(res, "GPerson(children=[])")
  }

  test("serialize self recursive type in nonbase case") {
    val alice = RPerson(0, "Alice", Nil)
    val bob = RPerson(0, "Bob", Nil)
    val granny = GPerson(List(RPerson(1, "Mama", List(alice, bob))))

    val res = summon[Show[String, GPerson]].show(granny)
    println(s"GRAND: $res")
    assertEquals(
      res,
      "GPerson(children=[RPerson(age=1,name=Mama,children=[RPerson(age=0,name=Alice,children=[]),RPerson(age=0,name=Bob,children=[])])])"
    )
  }

  test("construct a semi print for recursive hierarchy") {
    given instance: SemiPrint[Recursive] = SemiPrint.derived
    val res = instance.print(Recursive(Seq(Recursive(Seq.empty))))

    assertEquals(res, "Recursive(Recursive())")
  }

  test("construct a semmi print for a recursive, generic type") {
    given instance: SemiPrint[Tree[Int]] = SemiPrint.derived
    val res = instance.print(Branch(Branch(Leaf(0), Leaf(1)), Leaf(2)))
    println(s"SEMI RES = $res")
    assertEquals(res, "Branch(Branch(Leaf(0),Leaf(1)),Leaf(2))")
  }

  test("equality of Wrapper") {
    val res = Eq
      .derived[Wrapper]
      .equal(
        Wrapper(Some(KArray(KArray(Nil) :: Nil))),
        Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil)))
      )
    assert(!res)
  }

  test("construction of Show instance for Tree") {
    val error = compileErrors("summon[Show[String, Tree[String]]]")
    assert(error.isEmpty)
  }

  test("construction of Show instance for Leaf") {
    val error = compileErrors("summon[Show[String, Leaf[String]]]")
    assert(error.isEmpty)
  }

  test("show a recursive case class") {
    val res = Show.derived[Recursive].show(Recursive(Seq(Recursive(Nil))))
    assertEquals(res, "Recursive(children=[Recursive(children=[])])")
  }

  test("manually derive a recursive case class instance") {
    val res = Recursive.showRecursive.show(Recursive(Seq(Recursive(Nil))))
    assertEquals(res, "Recursive(children=[Recursive(children=[])])")
  }

  // 
  test("no support for arbitrary derivation result type for recursive classes yet") {
    val error = compileErrors("ExportedTypeclass.derived[Recursive]")
    println(s"ERR rec: ${error}")
    val expectedError = 
      """|No given instance of type magnolia1.examples.ExportedTypeclass[
         |  Seq[magnolia1.tests.RecursiveTypesTests.Recursive]
         |] was found.
         |""".stripMargin
    assert(error contains expectedError)
  }

object RecursiveTypesTests:

  sealed trait Tree[+T] derives Eq
  object Tree:
    given [T: [X] =>> Show[String, X]]: Show[String, Tree[T]] = Show.derived
  case class Leaf[+L](value: L) extends Tree[L]
  case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

  case class RPerson(age: Int, name: String, children: Seq[RPerson])
  object RPerson:
    given Show[String, RPerson] = Show.derived
  case class GPerson(children: Seq[RPerson])

  case class Recursive(children: Seq[Recursive])
  object Recursive:
    given showRecursive: Show[String, Recursive] = Show.derived[Recursive]

  case class KArray(value: List[KArray]) derives Eq
  case class Wrapper(v: Option[KArray])
