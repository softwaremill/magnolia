package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

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
    assertEquals(res, "Leaf[String](value=[hi])") // todo should be Seq[String]
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
end TypeAliasesTests
