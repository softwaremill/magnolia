package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
import scala.util.control.NonFatal

class OtherTests extends munit.FunSuite:

  import OtherTests.*

  test("show error stack") {
    val error = compileErrors("""
      case class Alpha(integer: Double)
      case class Beta(alpha: Alpha)
      Show.derived[Beta]
    """)
    assert(
      clue(error) contains "No given instance of type magnolia1.examples.Show[String, Alpha] was found."
    )
  }

  test("not attempt to instantiate Unit when producing error stack") {
    val error = compileErrors("""
      case class Gamma(unit: Unit)
      Show.derived[Gamma]
    """)
    assert(
      clue(error) contains "No given instance of type magnolia1.examples.Show[String, Unit] was found."
    )
  }

  test("not attempt to derive instances for refined types") {
    val error = compileErrors("Show.derived[Character]")
    assert(
      clue(
        error
      ) contains "No given instance of type magnolia1.examples.Show[String, Long & magnolia1.tests.OtherTests.Character.Tag] was found."
    )
  }

  test("derive instances for types with refined types if implicit provided") {
    val error = compileErrors("Show.derived[AnotherCharacter]")
    assert(error.isEmpty)
  }

// TODO - not working: "Maximal number of successive inlines (32) exceeded"
//  test("not attempt to derive instances for Java enums") {
//    val error = compileErrors("Show.derived[WeekDay]")
//    assert(error contains "No given instance of type deriving.Mirror.Of[magnolia1.tests.WeekDay] was found for parameter x$1 of method derived in trait Derivation.")
//  }

  test("patch a Person via a Patcher[Entity]") {
    given Patcher[String] = Patcher.forSingleValue[String]
    given Patcher[Int] = Patcher.forSingleValue[Int]
    val person = Person("Bob", 42)
    val res = summon[Patcher[Entity]].patch(person, Seq(null, 21))

    assertEquals(res, Person("Bob", 21))
  }

  test("throw on an illegal patch attempt with field count mismatch") {
    // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
    given Patcher[String] = Patcher.forSingleValue[String]
    given Patcher[Int] = Patcher.forSingleValue[Int]

    val res =
      try {
        val person = Person("Bob", 42)
        summon[Patcher[Entity]].patch(person, Seq(null, 21, "killer"))
      } catch {
        case NonFatal(e) => e.getMessage
      }
    assertEquals(res, "Cannot patch value `Person(Bob,42)`, expected 2 fields but got 3")
  }

// TODO - test hanging
//  test("throw on an illegal patch attempt with field type mismatch") {
//    // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
//    given Patcher[String] = Patcher.forSingleValue[String]
//    given Patcher[Int] = Patcher.forSingleValue[Int]
//
//    val res = try {
//      val person = Person("Bob", 42)
//      summon[Patcher[Entity]].patch(person, Seq(null, "killer"))
//      "it worked"
//    } catch {
//      case NonFatal(e) => e.getMessage
//    }
//    assert(res.contains("java.lang.String cannot be cast to"))
//    assert(res.contains("java.lang.Integer"))
//  }

object OtherTests:
  case class Character(id: Character.Id)
  object Character:
    trait Tag extends Any
    type Id = Long with Tag

  case class AnotherCharacter(id: AnotherCharacter.Id)
  object AnotherCharacter:
    trait Tag extends Any
    type Id = Long with Tag
    given Show[String, Id] = _.toString

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)
