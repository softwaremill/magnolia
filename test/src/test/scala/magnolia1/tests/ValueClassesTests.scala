package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
//import magnolia1.tests.SerializationTests.Color

/** TODO: Support for value classes is missing for scala3 branch. Eventually refactor and uncomment the tests below once the feature is
  * implemented.
  */
class ValueClassesTests extends munit.FunSuite:
  import ValueClassesTests.given
  import ValueClassesTests.*

  test("serialize a value class") {
    val res = ParamaterisedShow.derived[Length].show(new Length(100))
    assertEquals(res, "100")
  }

  test("construct a Show instance for value case class") {
    val res = ParamaterisedShow.derived[ServiceName1].show(ServiceName1("service"))
    assertEquals(res, "service")
  }

//   test("read-only typeclass can serialize value case class with inaccessible private constructor") {
//     val res = implicitly[ParamaterisedGenericShow[PrivateValueClass]].show(PrivateValueClass(42))
//     assertEquals(res, "42")
//   }

//   test("not assume full auto derivation of external value classes") {
//     val error = compileErrors("""
//       case class LoggingConfig(n: ServiceName1)
//       object LoggingConfig {
//         implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
//       }
//     """)
//     println(s"error = ${error}")
//     assert(error contains """
//       |magnolia: could not find SemiDefault.Typeclass for type magnolia1.tests.ServiceName1
//       |    in parameter 'n' of product type LoggingConfig
//       |""".stripMargin)
//   }

//     test("serialize value case class with accessible private constructor") {
//     val res = PrivateValueClass.show.show(PrivateValueClass(42))
//     assertEquals(res, "42")
//   }

//   test("allow derivation result to have arbitrary type") {
//      val res = (ExportedTypeclass.derived[Length], ExportedTypeclass.derived[Color])
//      assertEquals(res, (ExportedTypeclass.Exported[Length](), ExportedTypeclass.Exported[Color]()))
//    }

object ValueClassesTests:

  case class Length(val value: Int) extends AnyVal

  final case class ServiceName1(value: String) extends AnyVal

  class PrivateValueClass private (val value: Int) extends AnyVal
  object PrivateValueClass {
    def apply(l: Int) = new PrivateValueClass(l)
    given ParamaterisedShow[String, PrivateValueClass] = ParamaterisedShow.derived
  }
