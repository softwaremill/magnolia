package magnolia2.tests

import magnolia2.*
import magnolia2.examples.*

class TypeAliasesTests extends munit.FunSuite:

  import TypeAliasesTests.*

  // TODO not working: Cannot get a tree of no symbol
  // test("show a type aliased case class") {
  //   type T = Person
  //   val res = Show.derived[T].show(Person("Donald Duck", 313))
  //   assertEquals(res, "Person(name=Donald Duck,age=313)")
  // }

  // TODO - not working: assertion failed: Cannot get tree of no symbol
  // test("resolve aliases for type names") {
  //     type LO[X] = Leaf[Option[X]]

  //     val res = Show.derived[LO[String]].show(Leaf(None))
  //     assertEquals(res,"Leaf[Option[String]](value=None())")
  //   }

object TypeAliasesTests:

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)
