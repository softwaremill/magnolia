package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
import scala.annotation.StaticAnnotation

/** Supports mirrorless value classes derivation for non-generic products with annotations.
 * TODO:
    1) non-product derivation
    2) generic derivation
  */
class ValueClassesTests extends munit.FunSuite:
  import ValueClassesTests.*

  test("Print foo") {
    given Print[Int] = _.toString
    val res = Print.derived[VC].print(VC(555))
    assertEquals(res, "VC(555)")
  }

  test("Print bar") {
    given Print[Int] = _.toString
    val res = Print.derived[Bar].print(Bar(666))
    assertEquals(res, "Bar(666)")
  }

  test("semi default for VC") {
    given SemiDefault[VC] = SemiDefault.derived
    val res = summon[SemiDefault[VC]].default
    assertEquals(res, VC(0))
  }

  test("SemiDefault for BigBox value class") {
    given SemiDefault[BigBox] = SemiDefault.derived
    val res = summon[SemiDefault[BigBox]].default 
    assertEquals(res, BigBox(NormalBox(SmallBox(TinyBox(0)))))
  }

  test("Semi default for Foo") {
    given SemiDefault[Foo] = SemiDefault.derived
    val res = summon[SemiDefault[Foo]].default
    assertEquals(res, Foo(0))
  }

  test("show for Foo") {
    val res = Show.derived[Foo].show(Foo(33))
    assertEquals(res, "Foo{MyAnnotation(0)}(k{MyAnnotation(1)}{MyAnnotation(2)}=33)")
  }

  test("Print default") {
    given Print[Int] = _.toString
    val res = Print.derived[WithDefault].print(WithDefault())
    assertEquals(res, "WithDefault(123)")
  }

  test("Semi default") {
    val res = SemiDefault.derived[WithDefault].default
    assertEquals(res, WithDefault(123))
  }

  // test("serialize a value class") {
  //   val res = Show.derived[Length].show(new Length(100))
  //   assertEquals(res, "100")
  // }

  // test("construct a Show instance for value case class") {
  //   val res = Show.derived[ServiceName1].show(ServiceName1("service"))
  //   assertEquals(res, "service")
  // }

  // test("read-only typeclass can serialize value case class with inaccessible private constructor") {
  //   val res = implicitly[Print[PrivateValueClass]].print(PrivateValueClass(42))
  //   assertEquals(res, "42")
  // }

  // test("not assume full auto derivation of external value classes") {
  //   val error = compileErrors("""
  //     case class LoggingConfig(n: ServiceName1)
  //     object LoggingConfig {
  //       implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
  //     }
  //   """)
  //   assert(error contains """
  //     |magnolia: could not find SemiDefault.Typeclass for type magnolia1.tests.ServiceName1
  //     |    in parameter 'n' of product type LoggingConfig
  //     |""".stripMargin)
  // }

  //   test("serialize value case class with accessible private constructor") {
  //   class PrivateValueClass private (val value: Int) extends AnyVal
  //   object PrivateValueClass {
  //     def apply(l: Int) = new PrivateValueClass(l)
  //     implicit val show: Show[String, PrivateValueClass] = Show.derived[PrivateValueClass]
  //   }
  //   val res = PrivateValueClass.show.show(PrivateValueClass(42))
  //   assertEquals(res, "42")
  // }

  // test("allow derivation result to have arbitrary type") {
  //    val res = (ExportedTypeclass.derived[Length], ExportedTypeclass.derived[Color])
  //    assertEquals(res, (ExportedTypeclass.Exported[Length](), ExportedTypeclass.Exported[Color]()))
  //  }

object ValueClassesTests:

  case class TinyBox(size: Int)
  case class SmallBox(tinyBox: TinyBox)
  case class NormalBox(smallBox: SmallBox)
  case class BigBox(normalBox: NormalBox) extends AnyVal

  case class Bar(k: Int) 

  case class WrappedVC(bar: Bar) extends AnyVal

  case class MyAnnotation(order: Int) extends scala.annotation.Annotation

  case class MyTypeAnnotation(order: Int) extends StaticAnnotation

  case class VC(k: Int) extends AnyVal

  case class WithDefault(k: Int = 123) extends AnyVal

  @MyAnnotation(0)
  case class Foo(
    @MyAnnotation(1) k: Int @MyAnnotation(2)
    ) extends AnyVal

  class Length(val value: Int) extends AnyVal

  final case class ServiceName1(value: String) extends AnyVal

  class PrivateValueClass private (val value: Int) extends AnyVal
  object PrivateValueClass {
    def apply(l: Int) = new PrivateValueClass(l)
    // given Show[String, PrivateValueClass] = Show.derived
  }

