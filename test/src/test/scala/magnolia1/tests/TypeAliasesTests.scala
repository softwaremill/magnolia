package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

import scala.annotation.StaticAnnotation

class TypeAliasesTests extends munit.FunSuite:
  import TypeAliasesTests.*
  test("show a type aliased case class") {
    type T = Person
    val res = Show.derived[T].show(Person("Donald Duck", 313))
    assertEquals(res, "Person(name=Donald Duck,age=313)")
  }
  test("resolve aliases for type names") {
    type LO[X] = Leaf[Seq[X]]
    val res = Show.derived[LO[String]].show(Leaf(Seq("hi")))
    assertEquals(res, "Leaf[Seq[String]](value=[hi])")
  }
  test("preserve defaults") {
    assertEquals(List(("i", 1)), Macro.defaultValue[AliasA].map((n, v) => (n, v.get.apply())))
  }
  test("opaques should remain opaque") {
    assertEquals(Nil, Macro.defaultValue[OpaqueA])
  }
  test("collect annotations from a union type alias") {
    assertEquals(Macro.anns[AnnotatedUnion].map(_.toString), List("MyAnnotation(0)"))
  }
  test("combine annotations on a type alias with those on the dealiased type") {
    assertEquals(Macro.anns[AnnotatedAlias].map(_.toString), List("MyAnnotation(1)", "MyAnnotation(2)"))
  }
end TypeAliasesTests
object TypeAliasesTests:
  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)

  sealed trait Tree[+T] derives Eq
  object Tree:
    given [T: [X] =>> Show[String, X]]: Show[String, Tree[T]] = Show.derived
  case class Leaf[+L](value: L) extends Tree[L]
  case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

  case class A(i: Int = 1)
  opaque type OpaqueA = A
  type AliasA = A

  case class MyAnnotation(order: Int) extends StaticAnnotation

  @MyAnnotation(0)
  type AnnotatedUnion = Company | Person

  @MyAnnotation(2)
  case class B(i: Int)

  @MyAnnotation(1)
  type AnnotatedAlias = B
end TypeAliasesTests
