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
    assert(clue(res).contains("MyAnnotation(0)"))
    assert(clue(res).contains("scala.deprecated"))
  }

  test("inherit annotations from parent trait") {
    val res = Show.derived[Pet].show(Dog("Alex", 10, likesMeat = true))
    assertEquals(
      res,
      "{MyTypeAnnotation(2),MyTypeAnnotation(1)}Dog{MyTypeAnnotation(2),MyTypeAnnotation(1)}(name{[i]MyAnnotation(1)}=Alex,age{[i]MyAnnotation(2)}=10,likesMeat{MyAnnotation(3)}=true)"
    )
  }

  test("inherit annotations from all parent traits in hierarchy") {
    val res = Show
      .derived[Rodent]
      .show(Hamster("Alex", 10, likesNuts = true, likesVeggies = true))
    assertEquals(
      res,
      "{MyTypeAnnotation(1)}Hamster{MyTypeAnnotation(1)}(name{[i]MyAnnotation(1)}=Alex,age{MyAnnotation(6),[i]MyAnnotation(2)}=10,likesNuts{[i]MyAnnotation(3)}=true,likesVeggies{MyAnnotation(4)}=true)"
    )
  }

  test("inherit annotations from base class constructor parameters") {
    val res = Show.derived[Foo].show(Foo("foo"))
    assertEquals(res, "Foo(foo{MyAnnotation(2),[i]MyAnnotation(1)}=foo)")
  }

  test(
    "inherit annotations from all base class constructor parameters in hierarchy"
  ) {
    val res = Show.derived[Bar].show(Bar("foo", "bar"))
    assertEquals(
      res,
      "Bar(foo{MyAnnotation(2),[i]MyAnnotation(1)}=foo,bar{MyAnnotation(2),[i]MyAnnotation(1)}=bar)"
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
    val traitAnnotations =
      SubtypeInfo.derived[Sport].traitAnnotations.map(_.toString)
    assertEquals(traitAnnotations.mkString, "MyAnnotation(0)")
  }

  test("sealed trait enumeration should provide subtype annotations") {
    val subtypeAnnotations = SubtypeInfo.derived[Sport].subtypeAnnotations
    assertEquals(subtypeAnnotations.head.mkString, "MyAnnotation(1)")
    assertEquals(subtypeAnnotations(1).mkString, "MyAnnotation(2)")
  }

  test("sealed trait enumeration should provide subtype inherited annotations") {
    val subtypeAnnotations = SubtypeInfo.derived[Sport].subtypeInheritedAnnotations
    assertEquals(subtypeAnnotations.head.map(_.toString).mkString, "MyAnnotation(0)") // Boxing
    assertEquals(subtypeAnnotations(1).map(_.toString).mkString, "MyAnnotation(0)") // Soccer
  }

  test("sealed trait enumeration should provide subtype type annotations") {
    val subtypeTypeAnnotations =
      SubtypeInfo.derived[AttributeParent].subtypeTypeAnnotations
    assertEquals(subtypeTypeAnnotations.head.map(_.toString).mkString, "MyTypeAnnotation(2)") // Attributed
  }

  test("serialize case class with Java annotations by skipping them") {
    val res = Show.derived[MyDto].show(MyDto("foo", 42))
    assertEquals(res, "MyDto{MyAnnotation(0)}(foo=foo,bar=42)")
  }

  test("serialize case class with Java annotations which comes from external module by skipping them") {
    val res = Show.derived[JavaAnnotatedCase].show(JavaAnnotatedCase(1))
    assertEquals(res, "JavaAnnotatedCase(v=1)")
  }

  test("Scala 3 enum should provide annotations on enum") {
    val traitAnnotations = SubtypeInfo.derived[Size].traitAnnotations.map(_.toString)
    assertEquals(traitAnnotations.mkString, "MyAnnotation(0)")
  }

  test("Scala 3 enum should provide annotations on parameterless enum cases") {
    val subtypeAnnotations = SubtypeInfo.derived[Size].subtypeAnnotations
    // Subtypes are sorted alphabetically by full type name
    assertEquals(subtypeAnnotations.head.mkString, "MyAnnotation(3)") // L
    assertEquals(subtypeAnnotations(1).mkString, "MyAnnotation(2)") // M
    assertEquals(subtypeAnnotations(2).mkString, "MyAnnotation(1)") // S
  }

  test("Scala 3 enum should provide inherited annotations on parameterless enum cases") {
    val inherited = SubtypeInfo.derived[Size].subtypeInheritedAnnotations
    assertEquals(inherited.head.mkString, "MyAnnotation(0)") // L inherits from Size
    assertEquals(inherited(1).mkString, "MyAnnotation(0)") // M inherits from Size
    assertEquals(inherited(2).mkString, "MyAnnotation(0)") // S inherits from Size
  }

  test("Scala 3 enum case with params should provide param annotations") {
    val show = Show.derived[Shape].show(Shape.Square(5))
    // Square's own @MyAnnotation(3), inherited @MyAnnotation(0) from Shape, and @MyAnnotation(4) on side param
    assertEquals(show, "{MyAnnotation(3),MyAnnotation(0)}Square{MyAnnotation(3),MyAnnotation(0)}(side{MyAnnotation(4)}=5)")

    val info = SubtypeInfo.derived[Shape]
    assertEquals(info.traitAnnotations.map(_.toString).mkString, "MyAnnotation(0)")
    // Subtypes are sorted alphabetically by full type name
    assertEquals(info.subtypeAnnotations.head.map(_.toString).mkString, "MyAnnotation(1)") // Circle
    assertEquals(info.subtypeAnnotations(1).map(_.toString).mkString, "MyAnnotation(3)") // Square
    assertEquals(info.subtypeInheritedAnnotations.head.map(_.toString).mkString, "MyAnnotation(0)") // Circle inherits from Shape
    assertEquals(info.subtypeInheritedAnnotations(1).map(_.toString).mkString, "MyAnnotation(0)") // Square inherits from Shape
  }

  test("Scala 3 enum case with params should provide inherited param annotations") {
    val show = Show.derived[Tagged].show(Tagged.Sized("low", 5))
    assertEquals(show, "Sized(name=low,value{[i]MyAnnotation(10)}=5)")
  }

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

  @MyTypeAnnotation(1)
  sealed trait Pet {
    @MyAnnotation(1)
    def name: String
    @MyAnnotation(2)
    def age: Int
  }

  @MyTypeAnnotation(2)
  case class Dog(name: String, age: Int, @MyAnnotation(3) likesMeat: Boolean) extends Pet

  sealed trait Rodent extends Pet {
    @MyAnnotation(3)
    def likesNuts: Boolean
  }

  case class Hamster(
      name: String,
      @MyAnnotation(6)
      age: Int,
      likesNuts: Boolean,
      @MyAnnotation(4) likesVeggies: Boolean
  ) extends Rodent

  @MyAnnotation(0)
  enum Size:
    @MyAnnotation(1)
    case S extends Size
    @MyAnnotation(2)
    case M extends Size
    @MyAnnotation(3)
    case L extends Size

  @MyAnnotation(0)
  enum Shape:
    @MyAnnotation(1)
    case Circle(@MyAnnotation(2) radius: Int) extends Shape
    @MyAnnotation(3)
    case Square(@MyAnnotation(4) side: Int) extends Shape

  sealed trait HasValue:
    def name: String
    @MyAnnotation(10)
    def value: Int

  enum Tagged extends HasValue:
    case Sized(name: String, value: Int) extends Tagged
