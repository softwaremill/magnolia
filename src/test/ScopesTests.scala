package magnolia2.tests

import magnolia2.*
import magnolia2.examples.*

class ScopesTests extends munit.FunSuite:
  import ScopesTests.*

  test("local implicit beats Magnolia") {
    given showPerson: Show[String, Person] = _ => "nobody"
    val res = summon[Show[String, Address]].show(
      Address("Home", Person("John Smith", 44))
    )
    assertEquals(res, "Address(line1=Home,occupant=nobody)")
  }

  test("even low-priority implicit beats Magnolia for nested case") {
    val res =
      summon[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
    assertEquals(res, "Lunchbox(fruit=apple,drink=lemonade)")
  }

  test("low-priority implicit beats Magnolia when not nested") {
    val res = summon[Show[String, Fruit]].show(Fruit("apple"))
    assertEquals(res, "apple")
  }

  test("low-priority implicit beats Magnolia when chained") {
    val res = summon[Show[String, FruitBasket]].show(
      FruitBasket(Fruit("apple"), Fruit("banana"))
    )
    assertEquals(res, "FruitBasket(fruits=[apple,banana])")
  }

  test("typeclass implicit scope has lower priority than ADT implicit scope") {
    val res = summon[Show[String, Fruit]].show(Fruit("apple"))
    assertEquals(res, "apple")
  }

object ScopesTests:

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity

  case class Address(line1: String, occupant: Person)

  case class Fruit(name: String)
  object Fruit:
    given showFruit: Show[String, Fruit] = (f: Fruit) => f.name

  case class FruitBasket(fruits: Fruit*)

  case class Lunchbox(fruit: Fruit, drink: String)
