package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
import scala.annotation.StaticAnnotation

class AnnotationsTests extends munit.FunSuite:
  import AnnotationsTests.*

  test("capture attributes against params") {
    val res = summon[Show[String, Attributed]].show(Attributed("xyz", 100))
    assertEquals(
      res,
      "Attributed{MyAnnotation(0)}{MyTypeAnnotation(2)}(p1{MyAnnotation(1)}{MyTypeAnnotation(0)}=xyz,p2{MyAnnotation(2)}{MyTypeAnnotation(1)}=100)"
    )
  }

  test("show the scala.deprecated annotation on a field") {
    val res = summon[Show[String, Deprecated]].show(Deprecated(10))
    assert(res.contains("MyAnnotation(0)"))
    assert(res.contains("scala.deprecated"))
  }

  test("inherit annotations from parent trait") {
    val res = Show.derived[Pet].show(Dog("Alex", 10, likesMeat = true))
    assertEquals(
      res,
      "{MyTypeAnnotation(2),MyTypeAnnotation(1)}Dog{MyTypeAnnotation(2),MyTypeAnnotation(1)}(name{MyAnnotation(1)}=Alex,age{MyAnnotation(2)}=10,likesMeat{MyAnnotation(3)}=true)"
    )
  }

  test("inherit annotations from all parent traits in hierarchy") {
    val res = Show
      .derived[Rodent]
      .show(Hamster("Alex", 10, likesNuts = true, likesVeggies = true))
    assertEquals(
      res,
      "{MyTypeAnnotation(1)}Hamster{MyTypeAnnotation(1)}(name{MyAnnotation(1)}=Alex,age{MyAnnotation(2)}=10,likesNuts{MyAnnotation(3)}=true,likesVeggies{MyAnnotation(4)}=true)"
    )
  }

  test("inherit annotations from base class constructor parameters") {
    val res = Show.derived[Foo].show(Foo("foo"))
    assertEquals(res, "Foo(foo{MyAnnotation(2),MyAnnotation(1)}=foo)")
  }

  test(
    "inherit annotations from all base class constructor parameters in hierarchy"
  ) {
    val res = Show.derived[Bar].show(Bar("foo", "bar"))
    assertEquals(
      res,
      "Bar(foo{MyAnnotation(2),MyAnnotation(1)}=foo,bar{MyAnnotation(2),MyAnnotation(1)}=bar)"
    )
  }

  test("capture attributes against subtypes") {
    val res = Show.derived[AttributeParent].show(Attributed("xyz", 100))
    assertEquals(
      res,
      "{MyAnnotation(0)}Attributed{MyAnnotation(0)}{MyTypeAnnotation(2)}(p1{MyAnnotation(1)}{MyTypeAnnotation(0)}=xyz,p2{MyAnnotation(2)}{MyTypeAnnotation(1)}=100)"
    )
  }

  test("sealed trait enumeration should provide trait annotations") {
    val traitAnnotations = SubtypeInfo.derived[Sport].traitAnnotations.map(_.toString)
    assertEquals(traitAnnotations.mkString, "MyAnnotation(0)")
  }

  test("sealed trait enumeration should provide subtype annotations") {
    val subtypeAnnotations = SubtypeInfo.derived[Sport].subtypeAnnotations
    assertEquals(subtypeAnnotations(0).mkString, "MyAnnotation(1)")
    assertEquals(subtypeAnnotations(1).mkString, "MyAnnotation(2)")
  }

  // TODO - not compiling
  // test("serialize case class with Java annotations by skipping them") {
  //   val res = Show.derived[MyDto].show(MyDto("foo", 42))
  //   assertEquals(res, "MyDto{MyAnnotation(0)}(foo=foo,bar=42)")
  // }

  // TODO - not compiling
  // test("serialize case class with Java annotations which comes from external module by skipping them") {
  //   val res = Show.derived[JavaAnnotatedCase].show(JavaAnnotatedCase(1))
  //   assertEquals(res, "JavaAnnotatedCase(v=1)")
  // }

object AnnotationsTests:

  case class MyAnnotation(order: Int) extends StaticAnnotation

  case class MyTypeAnnotation(order: Int) extends StaticAnnotation

  sealed trait AttributeParent
  @MyAnnotation(0) 
  case class Attributed(
      @MyAnnotation(1) p1: String @MyTypeAnnotation(0),
      @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
  ) extends AttributeParent @MyTypeAnnotation(2)

  case class Deprecated(@MyAnnotation(0) @deprecated f: Int)

  class Base(
      @MyAnnotation(1)
      val foo: String
  )

  case class Foo(
      @MyAnnotation(2)
      override val foo: String
  ) extends Base(foo)

  class Base2(
      override val foo: String,
      @MyAnnotation(1)
      val bar: String
  ) extends Base(foo)

  case class Bar(
      @MyAnnotation(2)
      override val foo: String,
      @MyAnnotation(2)
      override val bar: String
  ) extends Base2(foo, bar)

  @MyAnnotation(0)
  sealed trait Sport

  @MyAnnotation(1)
  case object Boxing extends Sport

  @MyAnnotation(2)
  case class Soccer(players: Int) extends Sport

  @MyAnnotation(0)
  @SuppressWarnings(Array("deprecation"))
  @JavaExampleAnnotation(description = "Some model")
  case class MyDto(foo: String, bar: Int)
