package magnolia2.tests

import magnolia2.*
import magnolia2.examples.*

class ModifiersTests extends munit.FunSuite:
  import ModifiersTests.*

  test("construct a Show instance for product with partially private fields") {
    val res = Show.derived[Abc].show(Abc(12, 54, "pm"))
    assertEquals(res, "Abc(a=12,b=54L,c=pm)")
  }

  test("serialize case class with protected constructor") {
    val res = ProtectedCons.show.show(ProtectedCons("dada", "phil"))
    assertEquals(res, "ProtectedCons(name=dada phil)")
  }

  test(
    "read-only typeclass can serialize case class with protected constructor"
  ) {
    val res = summon[Print[ProtectedCons]].print(ProtectedCons("dada", "phil"))
    assertEquals(res, "ProtectedCons(dada phil)")
  }

  test(
    "read-only typeclass can serialize case class with inaccessible private constructor"
  ) {
    val res = summon[Print[PrivateCons]].print(PrivateCons("dada", "phil"))
    assertEquals(res, "PrivateCons(dada phil)")
  }

  test("serialize case class with accessible private constructor") {
    val res = PrivateCons.show.show(PrivateCons("dada", "phil"))
    assertEquals(res, "PrivateCons(name=dada phil)")
  }

object ModifiersTests:

  final case class Abc(private val a: Int, private val b: Long, c: String)

  case class ProtectedCons protected (name: String)

  object ProtectedCons:
    def apply(firstName: String, familyName: String): ProtectedCons =
      new ProtectedCons(firstName + " " + familyName)
    given show: Show[String, ProtectedCons] = Show.derived

  case class PrivateCons private (name: String)

  object PrivateCons:
    def apply(firstName: String, familyName: String): PrivateCons =
      new PrivateCons(firstName + " " + familyName)
    given show: Show[String, PrivateCons] = Show.derived
