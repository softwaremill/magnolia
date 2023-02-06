package magnolia2.tests

import magnolia2.*
import magnolia2.examples.*
import scala.annotation.StaticAnnotation
import scala.runtime.Static

/** Supports mirrorless value classes derivation for non-generic products with
  * annotations. TODO: 1) non-product derivation 2) generic derivation 3) access
  * modifiers
  */
class ValueClassesTests extends munit.FunSuite:
  import ValueClassesTests.*

  test("Derive Print TC for simple value class") {
    given Print[Int] = _.toString
    val res = Print.derived[SimpleVC].print(SimpleVC(555))
    assertEquals(res, "SimpleVC(555)")
  }

  test(
    "Derive SemiDefault TC for simple value class with no default argument"
  ) {
    given SemiDefault[SimpleVC] = SemiDefault.derived
    val res = summon[SemiDefault[SimpleVC]].default
    assertEquals(res, SimpleVC(0))
  }

  test("Derive SemiDefault for nested BigBox value class") {
    given SemiDefault[BigBox] = SemiDefault.derived
    val res = summon[SemiDefault[BigBox]].default
    assertEquals(res, BigBox(NormalBox(SmallBox(TinyBox(0)))))
  }

  test("Derive SemiDefault for heavily annotated value class") {
    given SemiDefault[HeavilyAnnotated] = SemiDefault.derived
    val res = summon[SemiDefault[HeavilyAnnotated]].default
    assertEquals(res, HeavilyAnnotated(0))
  }

  test("Derive Show for heavily annotated value class") {
    val res = Show.derived[HeavilyAnnotated].show(HeavilyAnnotated(33))
    assertEquals(
      res,
      "HeavilyAnnotated{MyAnnotation(0)}{MyTypeAnnotation(3)}(k{MyParamAnnotation(1)}{MyTypeAnnotation(2)}=33)"
    )
  }

  test(
    "Derive Print for a value class with default argument value of basic type"
  ) {
    given Print[Int] = _.toString
    val res =
      Print.derived[ValueClassWithDefault].print(ValueClassWithDefault())
    assertEquals(res, "ValueClassWithDefault(123)")
  }

  test(
    "Derive SemiDefault for a value class with default value of basic type"
  ) {
    val res = SemiDefault.derived[ValueClassWithDefault].default
    assertEquals(res, ValueClassWithDefault(123))
  }

  test("Derive SemiDefault for a wrapped value class with default") {
    val res = SemiDefault[WrappedValueClassWithDefault].default
    assertEquals(res, WrappedValueClassWithDefault(ValueClassWithDefault(123)))
  }

  test(
    "Derive SemiDefault for a value class wrapper of plain case class with default "
  ) {
    val res = SemiDefault[WrappedPlainWithDefault].default
    assertEquals(res, WrappedPlainWithDefault(PlainWithDefault("abrakadabra")))

  }

  test("Derive HasDefault for a value class with default value") {
    val res = HasDefault.derived[ValueClassWithDefault].defaultValue
    assertEquals(res, Right(ValueClassWithDefault()))
  }

  test("Derive Show for a value class with nested annotations") {
    val res = Show.derived[BigBox].show(BigBox(NormalBox(SmallBox(TinyBox(7)))))
    val expected =
      "BigBox(normalBox=NormalBox{MyAnnotation(4)}(smallBox{MyAnnotation(5)}{MyAnnotation(6)}=SmallBox(tinyBox=TinyBox{MyAnnotation(1),MyAnnotation(0)}(size{MyAnnotation(2)}{MyAnnotation(3)}=7))))"
    assertEquals(res, expected)
  }

  test("Construct a Show instance for a final value case class") {
    val res = Show.derived[ServiceName].show(ServiceName("service"))
    assertEquals(res, "ServiceName(value=service)")
  }

  test("Assume full auto derivation of external value classes") {
    case class LogingConfig(n: ServiceName)
    val res = SemiDefault[LogingConfig].default
    assertEquals(res, LogingConfig(ServiceName("")))
  }

  // support for non-product value classes is not yet supported
  // test("serialize a value class") {
  //   val res = Show.derived[Length].show(new Length(100))
  //   assertEquals(res, "100")
  // }

  // support for non-product value classes is not yet supported
  // test("read-only typeclass can serialize value case class with inaccessible private constructor") {
  //   val res = implicitly[Print[PrivateValueClass]].print(PrivateValueClass(42))
  //   assertEquals(res, "42")
  // }

  // support for non-product value classes is not yet supported
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

  case class SimpleVC(k: Int) extends AnyVal

  @MyAnnotation(0)
  @MyAnnotation(1)
  case class TinyBox(@MyAnnotation(2) size: Int @MyAnnotation(3))

  case class SmallBox(tinyBox: TinyBox)

  @MyAnnotation(4)
  case class NormalBox(@MyAnnotation(5) smallBox: SmallBox @MyAnnotation(6))

  case class BigBox(normalBox: NormalBox) extends AnyVal

  case class ValueClassWithDefault(k: Int = 123) extends AnyVal

  case class PlainWithDefault(name: String = "abrakadabra")

  case class WrappedValueClassWithDefault(withDefault: ValueClassWithDefault)

  case class WrappedPlainWithDefault(PlainWithDefault: PlainWithDefault)
      extends AnyVal

  @MyAnnotation(0)
  case class HeavilyAnnotated(
      @MyParamAnnotation(1) k: Int @MyTypeAnnotation(2)
  ) extends AnyVal @MyTypeAnnotation(3)

  class Length(val value: Int) extends AnyVal

  final case class ServiceName(value: String) extends AnyVal

  class PrivateValueClass private (val value: Int) extends AnyVal

  object PrivateValueClass:
    def apply(l: Int) = new PrivateValueClass(l)
    // given Show[String, PrivateValueClass] = Show.derived

  case class MyAnnotation(order: Int) extends scala.annotation.Annotation

  case class MyParamAnnotation(order: Int) extends StaticAnnotation

  case class MyTypeAnnotation(order: Int) extends StaticAnnotation
