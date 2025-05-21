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

  // format: off
  enum VeryLong:
    case _1, _2, _3, _4, _5, _6, _7, _8, _9, _10,
       _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
       _21, _22, _23, _24, _25, _26, _27, _28, _29, _30,
       _31, _32, _33, _34, _35, _36, _37, _38, _39, _40,
       _41, _42, _43, _44, _45, _46, _47, _48, _49, _50,
       _51, _52, _53, _54, _55, _56, _57, _58, _59, _60,
       _61, _62, _63, _64, _65, _66, _67, _68, _69, _70,
       _71, _72, _73, _74, _75, _76, _77, _78, _79, _80,
       _81, _82, _83, _84, _85, _86, _87, _88, _89, _90,
       _91, _92, _93, _94, _95, _96, _97, _98, _99, _100,
       _101, _102, _103, _104, _105, _106, _107, _108, _109, _110,
       _111, _112, _113, _114, _115, _116, _117, _118, _119, _120,
       _121, _122, _123, _124, _125, _126, _127, _128, _129, _130,
       _131, _132, _133, _134, _135, _136, _137, _138, _139, _140,
       _141, _142, _143, _144, _145, _146, _147, _148, _149, _150,
       _151, _152, _153, _154, _155, _156, _157, _158, _159, _160,
       _161, _162, _163, _164, _165, _166, _167, _168, _169, _170,
       _171, _172, _173, _174, _175, _176, _177, _178, _179, _180,
       _181, _182, _183, _184, _185, _186, _187, _188, _189, _190,
       _191, _192, _193, _194, _195, _196, _197, _198, _199, _200,
       _201, _202, _203, _204, _205, _206, _207, _208, _209, _210,
       _211, _212, _213, _214, _215, _216, _217, _218, _219, _220,
       _221, _222, _223, _224, _225, _226, _227, _228, _229, _230,
       _231, _232, _233, _234, _235, _236, _237, _238, _239, _240,
       _241, _242, _243, _244, _245, _246, _247, _248, _249, _250,
       _251, _252, _253, _254
  // format: on
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

  test("construct a Show instance for very long enum") {
    val res = Show.derived[VeryLong].show(VeryLong._254)
    assertEquals(res, "_254()")
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
    assertEquals(res, Seq("Blue", "Green", "Orange", "Pink", "Red"))
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
      clue(
        error
      ) contains "No given instance of type scala.deriving.Mirror.Of[magnolia1.tests.SumsTests.Parent] was found for parameter x$1 of method derived in trait Derivation."
    )
    assert(
      clue(
        error
      ) contains "trait Parent is not a generic sum because its child trait BadChild is not a generic product because it is not a case class"
    )
  }

  test(
    "report an error when a concrete member of a sealed hierarchy is neither final nor a case class"
  ) {
    val error = compileErrors("Show.derived[GoodChild]")
    assert(
      clue(
        error
      ) contains "trait GoodChild is not a generic sum because its child class Dewey is not a generic product because it is not a case class"
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

    val pkg = "magnolia1.tests.SumsTests.Complex"
    val expected = List(
      s"$pkg.ClassH",
      s"$pkg.Object",
      s"$pkg.ObjectC",
      s"$pkg.ObjectD",
      s"$pkg.ObjectE",
      s"$pkg.ObjectF",
      s"$pkg.Scoped.Object"
    )

    assertEquals(res.subtypes.map(_.typeInfo.full).toList, expected)
  }

  test("support split without join") {
    val res = summon[NoCombine[Halfy]].nameOf(Righty())
    assertEquals(res, "Righty")
  }
