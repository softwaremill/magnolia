package magnolia2.tests

import magnolia2.*
import magnolia2.examples.*

class DefaultValuesTests extends munit.FunSuite:
  import DefaultValuesTests.*

  test("construct a Show instance for a product with multiple default values") {
    val res = Show.derived[ParamsWithDefault].show(ParamsWithDefault())
    assertEquals(res, "ParamsWithDefault(a=3,b=4)")
  }

  test("decode using default") {
    val res = summon[Decoder[WithDefault]].decode(
      """WithDefault()"""
    )
    assertEquals(res, WithDefault(x = 2))
  }

  // TODO - will not work if object is in external scope
  test("decode not using default") {
    val res = summon[Decoder[WithDefault]].decode(
      """WithDefault(x=1)"""
    )
    assertEquals(res, WithDefault(x = 1))
  }

  test("construct a failed NoDefault") {
    val res = HasDefault.derived[NoDefault].defaultValue
    assertEquals(res, Left("truth is a lie"))
  }

  // TODO - will not work if object is in external scope
  test("access default constructor values") {
    val res = summon[HasDefault[Item]].defaultValue
    assertEquals(res, Right(Item("", 1, 0)))
  }

  // TODO - not working
  // test("construct a HasDefault instance for a generic product with default values") {
  //   val res = HasDefault.derived[ParamsWithDefaultGeneric[String, Int]].defaultValue
  //   assertEquals(res, Right(ParamsWithDefaultGeneric("A", 0)))
  // }

object DefaultValuesTests:

  case class ParamsWithDefault(a: Int = 3, b: Int = 4)

  case class ParamsWithDefaultGeneric[A, B](a: A = "A", b: B = "B")

  case class Item(name: String, quantity: Int = 1, price: Int)

  case class WithDefault(x: Int = 2)

  case class NoDefault(value: Boolean)
