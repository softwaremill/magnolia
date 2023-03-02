package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
import scala.annotation.StaticAnnotation

object SumsTests:

  case class MyAnnotation(order: Int) extends StaticAnnotation
  case class MyTypeAnnotation(order: Int) extends StaticAnnotation

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  case object Orange extends Color
  case object Pink extends Color

  sealed trait Y
  case object A extends Y
  case class B(s: String) extends Y

  enum Size:
    case S, M, L


  sealed trait Sport
  case object Boxing extends Sport
  case class Soccer(players: Int) extends Sport

  sealed trait Complex
  object Complex:
    case object Object extends G
    sealed trait A extends Complex
    sealed trait B extends A
    case object ObjectC extends Complex
    case object ObjectD extends A
    case object ObjectE extends B
    case object ObjectF extends A with Complex
    sealed trait G extends B
    case class ClassH(i: Int) extends A with G
    object Scoped:
      case object Object extends A
  end Complex

  object ExtendingTraits:
    trait One
    trait Two

  enum ExtendingTraits:
    case A extends ExtendingTraits with ExtendingTraits.One
    case B extends ExtendingTraits with ExtendingTraits.Two
    case C extends ExtendingTraits with ExtendingTraits.Two

  sealed trait Parent
  trait BadChild extends Parent // escape hatch!
  sealed trait GoodChild extends Parent
  final case class Huey(height: Int) extends GoodChild
  class Dewey(val height: Int) extends GoodChild
  final case class Louie(height: Int) extends BadChild

  sealed abstract class Halfy
  final case class Lefty() extends Halfy
  object Lefty:
    given NoCombine[Lefty] = NoCombine.instance(_ => "Lefty")
  final case class Righty() extends Halfy
  object Righty:
    given NoCombine[Righty] = NoCombine.instance(_ => "Righty")

end SumsTests

class SumsTests extends munit.FunSuite:

  import SumsTests.*

  test("serialize case object as a sealed trait") {
    val res = summon[Show[String, Color]].show(Blue)
    assertEquals(res, "Blue()")
  }

  test("construct a Show coproduct instance") {
    val res = Show.derived[Entity].show(Person("John Smith", 34))
    assertEquals(res, "Person(name=John Smith,age=34)")
  }

  test("construct a default value") {
    val res = HasDefault.derived[Entity].defaultValue
    assertEquals(res, Right(Company("")))
  }

  test("decode a Person as an Entity") {
    val res = summon[Decoder[Entity]].decode(
      """magnolia1.tests.SumsTests.Person(name=John Smith,age=32)"""
    )
    assertEquals(res, Person("John Smith", 32))
  }

  test("construct a semi print for sealed hierarchy") {
    val res = SemiPrint.derived[Y].print(A)
    assertEquals(res, "A()")
  }

  test("not find a given for semi print") {
    val res = compileErrors("""summon[SemiPrint[Y]].print(A)""")
    assert(res.nonEmpty)
  }

  test("isEnum field in SubtypeInfo should be true for enum") {
    val derivedSubtypeInfo = SubtypeInfo.derived[Size]
    assertEquals(derivedSubtypeInfo.isEnum, true)
  }

  test("isEnum field in SubtypeInfo should be false for sealed trait") {
    val derivedSubtypeInfo = SubtypeInfo.derived[Sport]
    assertEquals(derivedSubtypeInfo.isEnum, false)
  }

  test("construct a Show instance for an enum") {
    val res = Show.derived[Size].show(Size.S)
    assertEquals(res, "S()")
  }

  test("choose a enum") {
    val res = Passthrough.derived[Size].ctx.get.toOption.get
    List(
      Size.S,
      Size.M,
      Size.L
    ).foreach { o =>
      val chosen = res.choose(o)(identity)
      assertEquals(chosen.value, o)
      assertEquals(
        chosen.typeInfo.short,
        o.toString
      )
    }
  }

  test("should derive Show for a enum extending a trait") {
    val res = Show.derived[ExtendingTraits.A.type].show(ExtendingTraits.A)
    assertEquals(res, "A()")
  }

  test("sealed trait enumeration should detect isObject") {
    val subtypeIsObjects = SubtypeInfo.derived[Color].subtypeIsObject
    assertEquals(subtypeIsObjects, Seq(true, true, true, true, true))
  }

  test("sealed trait subtypes should be ordered") {
    val res = TypeNameInfo.derived[Color].subtypeNames.map(_.short)
    assertEquals(res, Seq("Red", "Green", "Blue", "Orange", "Pink"))
  }

  test("sealed trait subtypes should detect isObject") {
    val subtypeIsObjects = SubtypeInfo.derived[Sport].subtypeIsObject
    assertEquals(subtypeIsObjects, Seq(true, false))
  }

  test("sealed trait typeName should be complete and unchanged") {
    val res = TypeNameInfo.derived[Color].name
    assertEquals(res.full, "magnolia1.tests.SumsTests.Color")
  }

  test(
    "report an error when an abstract member of a sealed hierarchy is not sealed"
  ) {
    val error = compileErrors("Show.derived[Parent]")
    assert(
      error contains "No given instance of type deriving.Mirror.Of[magnolia1.tests.SumsTests.Parent] was found for parameter x$1 of method derived in trait Derivation."
    )
    assert(
      error contains "trait Parent is not a generic sum because its child trait BadChild is not a generic product because it is not a case class"
    )
  }

  test(
    "report an error when a concrete member of a sealed hierarchy is neither final nor a case class"
  ) {
    val error = compileErrors("Show.derived[GoodChild]")
    assert(
      error contains "trait GoodChild is not a generic sum because its child class Dewey is not a generic product because it is not a case class"
    )
  }

  test("not assume full auto derivation of external coproducts") {
    case class LoggingConfig(o: Option[String])
    object LoggingConfig:
      given SemiDefault[LoggingConfig] = SemiDefault.derived

    val res = summon[SemiDefault[LoggingConfig]].default
    assertEquals(res, LoggingConfig(None))
  }

  test("half auto derivation of sealed families") {
    val res = SemiDefault.derived[Halfy].default
    assertEquals(res, Lefty())
  }

   test("derive all subtypes in complex hierarchy") {
     val res = Passthrough.derived[Complex].ctx.get.toOption.get
     assertEquals(res.subtypes.size, 7)
     List(
       Complex.ObjectE,
       Complex.Object,
       Complex.Scoped.Object,
       Complex.ClassH(1),
       Complex.ObjectD,
       Complex.ObjectF,
       Complex.ObjectC
     ).foreach { o =>
       val chosen = res.choose(o)(identity)
       assertEquals(chosen.value, o)
       assertEquals(
         chosen.typeInfo.short,
         o.getClass.getSimpleName.replace("$", "")
       )
     }
   }

  test("support split without join") {
    val res = summon[NoCombine[Halfy]].nameOf(Righty())
    assertEquals(res, "Righty")
  }
