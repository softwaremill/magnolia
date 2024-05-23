package magnolia1.tests

import magnolia1.TypeName
import magnolia1.examples._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.time.LocalDate
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.util.control.NonFatal

sealed trait Tree[+T]
case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Path[+A]
case class Destination[+A](value: A) extends Path[A]
case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
case class OffRoad[+A](path: Option[Path[A]]) extends Path[A]

sealed trait Entity

case class Company(name: String) extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)

class Length(val value: Int) extends AnyVal

case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: String)
case class Fruit(name: String)
object Fruit {
  implicit val showFruit: Show[String, Fruit] = (f: Fruit) => f.name
}

case class Item(name: String, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Orange extends Color
case object Pink extends Color

case class MyAnnotation(order: Int) extends StaticAnnotation
case class MyTypeAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
  @MyAnnotation(1) p1: String @MyTypeAnnotation(0),
  @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
) extends AttributeParent @MyTypeAnnotation(2)

case class `%%`(`/`: Int, `#`: String)

case class Param(a: String, b: String)
case class TestEntry(param: Param)
object TestEntry {
  def apply(): TestEntry = TestEntry(Param("", ""))

  def apply(a: String)(implicit b: Int): TestEntry = TestEntry(Param(a, b.toString))

  def apply(a: String, b: String): TestEntry = TestEntry(Param(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S) extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

sealed trait Box[+A]
case class SimpleBox[+A](value: A) extends Box[A]
case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

case class Account(id: String, emails: String*)

case class Portfolio(companies: Company*)

case class Recursive(children: Seq[Recursive])

// This tests compilation.
class GenericCsv[A: Csv]
object ParamCsv extends GenericCsv[Param]


class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: String) extends AnyVal
final case class ServiceName2(value: String)

sealed abstract class Halfy
final case class Lefty() extends Halfy
object Lefty {
  implicit val noCombine: NoCombine[Lefty] =
    NoCombine.instance(_ => "Lefty")
}
final case class Righty() extends Halfy
object Righty {
  implicit val noCombine: NoCombine[Righty] =
    NoCombine.instance(_ => "Righty")
}

@MyAnnotation(0)
@SuppressWarnings(Array("deprecation"))
@JavaExampleAnnotation(description = "Some model")
case class MyDto(foo: String, bar: Int)

@SerialVersionUID(42) case class Schedule(events: Seq[Event])
case class Event(date: LocalDate)

case class RPerson(age: Int, name: String, children: Seq[RPerson])
case class GPerson(children: Seq[RPerson])

case class ProtectedCons protected (name: String)
object ProtectedCons {
  def apply(firstName: String, familyName: String): ProtectedCons =
    new ProtectedCons(firstName + " " + familyName)
  implicit val show: Show[String, ProtectedCons] = Show.gen
}

case class PrivateCons private (name: String)
object PrivateCons {
  def apply(firstName: String, familyName: String): PrivateCons =
    new PrivateCons(firstName + " " + familyName)
  implicit val show: Show[String, PrivateCons] = Show.gen
}

class PrivateValueClass private (val value: Int) extends AnyVal
object PrivateValueClass {
  def apply(l: Int) = new PrivateValueClass(l)
  implicit val show: Show[String, PrivateValueClass] = Show.gen
}

case class KArray(value: List[KArray])
case class Wrapper(v: Option[KArray])

case class VeryLong(
  p1: String,
  p2: String,
  p3: String,
  p4: String,
  p5: String,
  p6: String,
  p7: String,
  p8: String,
  p9: String,
  p10: String,
  p11: String,
  p12: String,
  p13: String,
  p14: String,
  p15: String,
  p16: String,
  p17: String,
  p18: String,
  p19: String,
  p20: String,
  p21: String,
  p22: String,
  p23: String
)

case class Character(id: Character.Id)
object Character {
  trait Tag extends Any
  type Id = Long with Tag
}

case class AnotherCharacter(id: AnotherCharacter.Id)
object AnotherCharacter {
  trait Tag extends Any
  type Id = Long with Tag

  implicit val idShow: Show[String, Id] = _.toString
}

final case class Abc(
  private val a: Int,
  private val b: Long,
  c: String
)

sealed trait Covariant[+A]
sealed trait Contravariant[-A]
sealed trait Exactly[A] extends Covariant[A] with Contravariant[A]

object Exactly {
  case object Any extends Exactly[Any]
  case class Custom[A](value: A) extends Exactly[A]
  case object Int extends Exactly[Int]
  case object Nothing extends Exactly[Nothing]
  case object String extends Exactly[String]
}

case class ParamsWithDefault(a: Int = 3, b: Int = 4)
case class ParamsWithDefaultGeneric[A, B](a: A = "A", b: B = "B")
case class ParamsWithDynamicDefault(a: Double = scala.math.random())

sealed trait Parent
trait BadChild extends Parent // escape hatch!
sealed trait GoodChild extends Parent
final case class Huey(height: Int) extends GoodChild
class Dewey(val height: Int) extends GoodChild
final case class Louie(height: Int) extends BadChild

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

case class Hamster(name: String, age: Int, likesNuts: Boolean, @MyAnnotation(4) likesVeggies: Boolean) extends Rodent

// not serializable outer
class Outer {
  val showAddress: Show[String, Address] = Show.gen
  val showColor: Show[String, Color] = Show.gen
}

sealed trait NoNameCollision
object NoNameCollision {
  case class Foo(a: Boolean) extends NoNameCollision
  case class Bar(b: Int) extends NoNameCollision
  case class Array(c: String) extends NoNameCollision

  val showNoNameCollision: Show[String, NoNameCollision] = Show.gen[NoNameCollision]
}

class Tests extends munit.FunSuite {

    test("construct a Show product instance with alternative apply functions") {
      val res = Show.gen[TestEntry].show(TestEntry("a", "b"))
      assertEquals(res, """TestEntry(param=Param(a=a,b=b))""")
    }

    test("construct a Show product instance") {
      val res = Show.gen[Person].show(Person("John Smith", 34))
      assertEquals(res, """Person(name=John Smith,age=34)""")
    }

    test("construct a Show coproduct instance") {
      val res = Show.gen[Person].show(Person("John Smith", 34))
      assertEquals(res, "Person(name=John Smith,age=34)")
    }

    test("construct a Show instance for product with partially private fields") {
      val res = Show.gen[Abc].show(Abc(12, 54, "pm"))
      assertEquals(res, "Abc(a=12,b=54L,c=pm)")
    }

    test("construct a Show instance for value case class") {
      val res = Show.gen[ServiceName1].show(ServiceName1("service"))
      assertEquals(res, "service")
    }

    test("construct a Show instance for a product with multiple default values") {
      val res = Show.gen[ParamsWithDefault].show(ParamsWithDefault())
      assertEquals(res, "ParamsWithDefault(a=3,b=4)")
    }

    test("construct a HasDefault instance for a generic product with default values") {
      val res = HasDefault.gen[ParamsWithDefaultGeneric[String, Int]].defaultValue
      assertEquals(res, Right(ParamsWithDefaultGeneric("A", 0)))
    }

    test("construct a HasDefault instance for a generic product with dynamic default values") {
      val res1 = HasDefault.gen[ParamsWithDynamicDefault].getDynamicDefaultValueForParam("a")
      val res2 = HasDefault.gen[ParamsWithDynamicDefault].getDynamicDefaultValueForParam("a")

      assertEquals(res1.isDefined, true)
      assertEquals(res2.isDefined, true)

      for {
        firstParam <- res1
        secondParam <- res2
        res = assertNotEquals(firstParam, secondParam)
      } yield res
    }

    test("serialize a Branch") {
      val res = implicitly[Show[String, Branch[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
      assertEquals(res, "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")
    }

    test("local implicit beats Magnolia") {
      implicit val showPerson: Show[String, Person] = _ => "nobody"
      val res = implicitly[Show[String, Address]].show(Address("Home", Person("John Smith", 44)))
      assertEquals(res, "Address(line1=Home,occupant=nobody)")
    }

    test("even low-priority implicit beats Magnolia for nested case") {
      val res = implicitly[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
      assertEquals(res, "Lunchbox(fruit=apple,drink=lemonade)")
    }

    test("low-priority implicit beats Magnolia when not nested") {
      val res = implicitly[Show[String, Fruit]].show(Fruit("apple"))
      assertEquals(res, "apple")
    }

    test("low-priority implicit beats Magnolia when chained") {
      val res = implicitly[Show[String, FruitBasket]].show(FruitBasket(Fruit("apple"), Fruit("banana")))
      assertEquals(res, "FruitBasket(fruits=[apple,banana])")
    }

    test("typeclass implicit scope has lower priority than ADT implicit scope") {
      val res = implicitly[Show[String, Fruit]].show(Fruit("apple"))
      assertEquals(res, "apple")
    }

    test("test equality false") {
      val res = Eq.gen[Entity].equal(Person("John Smith", 34), Person("", 0))
      assertEquals(res, false)
    }

    test("test equality true") {
      val res = Eq.gen[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
      assertEquals(res, true)
    }

    test("test branch equality true") {
      val res = Eq.gen[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
      assertEquals(res, true)
    }

    test("construct a default value") {
      val res = HasDefault.gen[Entity].defaultValue
      assertEquals(res, Right(Company("")))
    }

    test("construction of Show instance for Leaf") {
      val error = compileErrors("implicitly[Show[String, Leaf[String]]]")
      assert(error.isEmpty)
    }

    test("construction of Show instance for Tree") {
      val error = compileErrors("implicitly[Show[String, Tree[String]]]")
      assert(error.isEmpty)
    }

    test("serialize a Leaf") {
      val res = implicitly[Show[String, Leaf[String]]].show(Leaf("testing"))
      assertEquals(res, "Leaf[String](value=testing)")
    }

    test("serialize a Branch as a Tree") {
      val res = implicitly[Show[String, Tree[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
      assertEquals(res, "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")
    }

    test("serialize case object") {
      val res = implicitly[Show[String, Red.type]].show(Red)
      assertEquals(res, "Red()")
    }

    test("serialize self recursive type") {
      val res = implicitly[Show[String, GPerson]].show(GPerson(Nil))
      assertEquals(res, "GPerson(children=[])")
    }

    test("access default constructor values") {
      val res = implicitly[HasDefault[Item]].defaultValue
      assertEquals(res, Right(Item("", 1, 0)))
    }

    test("serialize case object as a sealed trait") {
      val res = implicitly[Show[String, Color]].show(Blue)
      assertEquals(res, "Blue()")
    }

    test("serialize case class with protected constructor") {
      val res = ProtectedCons.show.show(ProtectedCons("dada", "phil"))
      assertEquals(res, "ProtectedCons(name=dada phil)")
    }

    test("serialize case class with accessible private constructor") {
      val res = PrivateCons.show.show(PrivateCons("dada", "phil"))
      assertEquals(res, "PrivateCons(name=dada phil)")
    }

    test("serialize value case class with accessible private constructor") {
      val res = PrivateValueClass.show.show(PrivateValueClass(42))
      assertEquals(res, "42")
    }

    test("read-only typeclass can serialize case class with inaccessible private constructor") {
      val res = implicitly[Print[PrivateCons]].print(PrivateCons("dada", "phil"))
      assertEquals(res, "PrivateCons(dada phil)")
    }

    test("read-only typeclass can serialize value case class with inaccessible private constructor") {
      val res = implicitly[Print[PrivateValueClass]].print(PrivateValueClass(42))
      assertEquals(res, "42")
    }

    test("read-only typeclass can serialize case class with protected constructor") {
      val res = implicitly[Print[ProtectedCons]].print(ProtectedCons("dada", "phil"))
      assertEquals(res, "ProtectedCons(dada phil)")
    }

    test("decode a company") {
      val res = Decoder.gen[Company].decode("""Company(name=Acme Inc)""")
      assertEquals(res, Company("Acme Inc"))
    }

    test("decode a Person as an Entity") {
      val res = implicitly[Decoder[Entity]].decode("""magnolia1.tests.Person(name=John Smith,age=32)""")
      assertEquals(res, Person("John Smith", 32))
    }

    test("decode a nested product") {
      val res = implicitly[Decoder[Address]].decode(
        """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
      )
      assertEquals(res, Address("53 High Street", Person("Richard Jones", 44)))
    }

    test("show error stack") {
      val error = compileErrors("""
        case class Alpha(integer: Double)
        case class Beta(alpha: Alpha)
        Show.gen[Beta]
      """)
      assert(error contains """
        |magnolia: could not find Show.Typeclass for type Double
        |    in parameter 'integer' of product type Alpha
        |    in parameter 'alpha' of product type Beta
        |""".stripMargin)
    }

    test("serialize case class with Java annotations by skipping them") {
      val res = Show.gen[MyDto].show(MyDto("foo", 42))
      assertEquals(res, "MyDto{MyAnnotation(0)}(foo=foo,bar=42)")
    }

    test("serialize case class with Java annotations which comes from external module by skipping them") {
      val res = Show.gen[JavaAnnotatedCase].show(JavaAnnotatedCase(1))
      assertEquals(res, "JavaAnnotatedCase(v=1)")
    }

    test("not attempt to instantiate Unit when producing error stack") {
      val error = compileErrors("""
        case class Gamma(unit: Unit)
        Show.gen[Gamma]
      """)
      assert(error contains """
        |magnolia: could not find Show.Typeclass for type Unit
        |    in parameter 'unit' of product type Gamma
        |""".stripMargin)
    }

    test("not assume full auto derivation of external value classes") {
      val error = compileErrors("""
        case class LoggingConfig(n: ServiceName1)
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
      """)
      assert(error contains """
        |magnolia: could not find SemiDefault.Typeclass for type magnolia1.tests.ServiceName1
        |    in parameter 'n' of product type LoggingConfig
        |""".stripMargin)
    }

    test("not assume full auto derivation of external products") {
      val error = compileErrors("""
        case class LoggingConfig(n: ServiceName2)
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
      """)
      assert(error contains """
        |magnolia: could not find SemiDefault.Typeclass for type magnolia1.tests.ServiceName2
        |    in parameter 'n' of product type LoggingConfig
        |""".stripMargin)
    }

    test("not assume full auto derivation of external coproducts") {
      val error = compileErrors("""
        case class LoggingConfig(o: Option[String])
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
      """)
      assert(error contains """
        |magnolia: could not find SemiDefault.Typeclass for type Option[String]
        |    in parameter 'o' of product type LoggingConfig
        |""".stripMargin)
    }

    test("half auto derivation of sealed families") {
      val res = SemiDefault.gen[Halfy].default
      assertEquals(res, Lefty())
    }

    test("typenames and labels are not encoded") {
      val res = implicitly[Show[String, `%%`]].show(`%%`(1, "two"))
      assertEquals(res, "%%(/=1,#=two)")
    }

    test("serialize a tuple") {
      val tupleDerivation = implicitly[Show[String, (Int, String)]]
      val res = tupleDerivation.show((42, "Hello World"))
      assertEquals(res, "Tuple2[Int,String](_1=42,_2=Hello World)")
    }

    test("serialize a value class") {
      val res = Show.gen[Length].show(new Length(100))
      assertEquals(res, "100")
    }

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
    test("show a Politician with covariant lobby") {
      val res = Show.gen[Politician[String]].show(Corrupt("wall", Seq(Company("Alice Inc"))))
      assertEquals(res, "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")
    }

    // LabelledBox being invariant in L <: String prohibits the derivation for LabelledBox[Int, _]
    test("can't show a Box with invariant label") {
      val error = compileErrors("Show.gen[Box[Int]]")
      assert(error contains """
        |magnolia: could not find Show.Typeclass for type L
        |    in parameter 'label' of product type magnolia1.tests.LabelledBox[Int, _ <: String]
        |    in coproduct type magnolia1.tests.Box[Int]
        |""".stripMargin)
    }


    test("patch a Person via a Patcher[Entity]") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      val person = Person("Bob", 42)
      val res = implicitly[Patcher[Entity]].patch(person, Seq(null, 21))
      assertEquals(res, Person("Bob", 21))
    }

    test("throw on an illegal patch attempt with field count mismatch") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      val res = try {
        val person = Person("Bob", 42)
        implicitly[Patcher[Entity]].patch(person, Seq(null, 21, 'killer))
      } catch {
        case NonFatal(e) => e.getMessage
      }
      assertEquals(res, "Cannot patch value `Person(Bob,42)`, expected 2 fields but got 3")
    }

    test("throw on an illegal patch attempt with field type mismatch") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      val res = try {
        val person = Person("Bob", 42)
        implicitly[Patcher[Entity]].patch(person, Seq(null, 'killer))
        "it worked"
      } catch {
        case NonFatal(e) => e.getMessage
      }

      assert(res.contains("scala.Symbol cannot be cast to"))
      assert(res.contains("java.lang.Integer"))
    }

    class ParentClass {
      case class InnerClass(name: String)
      case class InnerClassWithDefault(name: String = "foo")

      def testInner(): Unit = {
        test("serialize a case class inside another class") {
          val res = implicitly[Show[String, InnerClass]].show(InnerClass("foo"))
          assertEquals(res, "InnerClass(name=foo)")
        }

        test("construct a default case class inside another class") {
          val res = HasDefault.gen[InnerClassWithDefault].defaultValue
          assertEquals(res, Right(InnerClassWithDefault()))
        }

        ()
      }

      def testLocal(): Unit = {
        case class LocalClass(name: String)
        case class LocalClassWithDefault(name: String = "foo")

        test("serialize a case class inside a method") {
          val res = implicitly[Show[String, LocalClass]].show(LocalClass("foo"))
          assertEquals(res, "LocalClass(name=foo)")
        }

        test("construct a default case class inside a method") {
          val res = HasDefault.gen[LocalClassWithDefault].defaultValue
          assertEquals(res, Right(LocalClassWithDefault()))
        }

        ()
      }
    }

    val parent = new ParentClass()
    parent.testInner()
    parent.testLocal()

    test("show an Account") {
      val res = Show.gen[Account].show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
      assertEquals(res, "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")
    }

    test("construct a default Account") {
      val res = HasDefault.gen[Account].defaultValue
      assertEquals(res, Right(Account("")))
    }

    test("construct a failed NoDefault") {
      val res = HasDefault.gen[NoDefault].defaultValue
      assertEquals(res, Left("truth is a lie"))
    }

    test("show a Portfolio of Companies") {
      val res = Show.gen[Portfolio].show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
      assertEquals(res, "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")
    }

    test("show a List[Int]") {
      val res = Show.gen[List[Int]].show(List(1, 2, 3))
      // in Scala 2.13, the List definition has changed, what used to be "tl" is now called "next"
      assertEquals(res.replaceAll("next", "tl"), "::[Int](head=1,tl=::[Int](head=2,tl=::[Int](head=3,tl=Nil())))")
    }

    test("sealed trait typeName should be complete and unchanged") {
      val res = TypeNameInfo.gen[Color].name
      assertEquals(res.full, "magnolia1.tests.Color")
    }

    test("sealed trait subtypes should be ordered") {
      val res = TypeNameInfo.gen[Color].subtypeNames
      assertEquals(res.map(_.short), Seq("Blue", "Green", "Orange", "Pink", "Red"))
    }

    test("case class typeName should be complete and unchanged") {
      implicit val stringTypeName: TypeNameInfo[String] = new TypeNameInfo[String] {
        def name = ???
        def subtypeNames = ???
      }
      val res = TypeNameInfo.gen[Fruit].name
      assertEquals(res.full, "magnolia1.tests.Fruit")
    }

    test("show chained error stack") {
      val error = compileErrors("Show.gen[(Int, Seq[(Double, String)])]")
      assert(error contains """
        |magnolia: could not find Show.Typeclass for type Double
        |    in parameter '_1' of product type (Double, String)
        |    in chained implicit Show.Typeclass for type Seq[(Double, String)]
        |    in parameter '_2' of product type (Int, Seq[(Double, String)])
        |""".stripMargin)
    }

    test("show chained error stack when leaf instance is missing") {
      val error = compileErrors("Show.gen[Schedule]")
      assert(error contains """
        |magnolia: could not find Show.Typeclass for type java.time.LocalDate
        |    in parameter 'date' of product type magnolia1.tests.Event
        |    in chained implicit Show.Typeclass for type Seq[magnolia1.tests.Event]
        |    in parameter 'events' of product type magnolia1.tests.Schedule
        |""".stripMargin)
    }

    test("show a recursive case class") {
      val res = Show.gen[Recursive].show(Recursive(Seq(Recursive(Nil))))
      assertEquals(res, "Recursive(children=[Recursive(children=[])])")
    }

    test("manually derive a recursive case class instance") {
      implicit lazy val showRecursive: Show[String, Recursive] = Show.gen[Recursive]
      val res = showRecursive.show(Recursive(Seq(Recursive(Nil))))
      assertEquals(res, "Recursive(children=[Recursive(children=[])])")
    }

    test("show a type aliased case class") {
      type T = Person
      val res = Show.gen[T].show(Person("Donald Duck", 313))
      assertEquals(res, "Person(name=Donald Duck,age=313)")
    }

    test("dependencies between derived type classes") {
      implicit def showDefaultOption[A](
                                         implicit showA: Show[String, A],
                                         defaultA: HasDefault[A]
                                       ): Show[String, Option[A]] = (optA: Option[A]) => showA.show(optA.getOrElse(defaultA.defaultValue.right.get))

      val res = Show.gen[Path[String]].show(OffRoad(Some(Crossroad(Destination("A"), Destination("B")))))
      assertEquals(res, "OffRoad[String](path=Crossroad[String](left=Destination[String](value=A),right=Destination[String](value=B)))")
    }

    test("resolve aliases for type names") {
      type LO[X] = Leaf[Option[X]]

      val res = Show.gen[LO[String]].show(Leaf(None))
      assertEquals(res,"Leaf[Option[String]](value=None())")
    }

    test("capture attributes against params") {
      val res = Show.gen[Attributed].show(Attributed("xyz", 100))
      assertEquals(res, "Attributed{MyAnnotation(0)}{MyTypeAnnotation(2)}(p1{MyAnnotation(1)}{MyTypeAnnotation(0)}=xyz,p2{MyAnnotation(2)}{MyTypeAnnotation(1)}=100)")
    }

    test("capture attributes against subtypes") {
      val res = Show.gen[AttributeParent].show(Attributed("xyz", 100))
      assertEquals(res, "{MyAnnotation(0)}Attributed{MyAnnotation(0)}{MyTypeAnnotation(2)}(p1{MyAnnotation(1)}{MyTypeAnnotation(0)}=xyz,p2{MyAnnotation(2)}{MyTypeAnnotation(1)}=100)")
    }

    test("show underivable type with fallback") {
      val res = TypeNameInfo.gen[NotDerivable].name
      assertEquals(res, TypeName("", "Unknown Type", Seq.empty))
    }

    test("allow no-coproduct derivation definitions") {
      val error = compileErrors("WeakHash.gen[Person]")
      assert(error.isEmpty)
    }

    test("disallow coproduct derivations without split method") {
      val error = compileErrors("WeakHash.gen[Entity]")
      assert(error contains "magnolia: the method `split` must be defined on the derivation object WeakHash to derive typeclasses for sealed traits")
    }

    test("equality of Wrapper") {
      val res = Eq.gen[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
      assertEquals(res, false)
    }

    test("very long") {
      val vl =
        VeryLong("p1",
          "p2",
          "p3",
          "p4",
          "p5",
          "p6",
          "p7",
          "p8",
          "p9",
          "p10",
          "p11",
          "p12",
          "p13",
          "p14",
          "p15",
          "p16",
          "p17",
          "p18",
          "p19",
          "p20",
          "p21",
          "p22",
          "p23")
      val res = Eq.gen[VeryLong].equal(vl, vl)
      assertEquals(res, true)
    }

    test("not attempt to derive instances for refined types") {
      val error = compileErrors("Show.gen[Character]")
      assert(error contains "magnolia: could not infer Show.Typeclass for refined type magnolia1.tests.Character.Id")
    }

    test("derive instances for types with refined types if implicit provided") {
      val error = compileErrors("Show.gen[AnotherCharacter]")
      assert(error.isEmpty)
    }

    test("not attempt to derive instances for Java enums") {
      val error = compileErrors("Show.gen[WeekDay]")
      assert(error contains "magnolia: could not infer Show.Typeclass for type magnolia1.tests.WeekDay")
    }

    test("determine subtypes of Exactly[Int]") {
      implicit def hideFallbackWarning: TypeNameInfo[Int] = TypeNameInfo.fallback[Int]

      val res = TypeNameInfo.gen[Exactly[Int]].subtypeNames.map(_.short).mkString(" | ")
      assertEquals(res, "Custom | Int")
    }

    test("determine subtypes of Covariant[String]") {
      implicit def hideFallbackWarning: TypeNameInfo[String] = TypeNameInfo.fallback[String]

      val res = TypeNameInfo.gen[Covariant[String]].subtypeNames.map(_.short).mkString(" | ")
      assertEquals(res, "Custom | Nothing | String")
    }

    test("determine subtypes of Contravariant[Double]") {
      implicit def hideFallbackWarning: TypeNameInfo[Double] = TypeNameInfo.fallback[Double]

      val res = TypeNameInfo.gen[Contravariant[Double]].subtypeNames.map(_.short).mkString(" | ")
      assertEquals(res, "Any | Custom")
    }

    test("allow derivation result to have arbitrary type") {
      val res = (ExportedTypeclass.gen[Length], ExportedTypeclass.gen[Color])
      assertEquals(res, (ExportedTypeclass.Exported[Length](), ExportedTypeclass.Exported[Color]()))
    }

    test("no support for arbitrary derivation result type for recursive classes yet") {
      val error = compileErrors("ExportedTypeclass.gen[Recursive]")
      assert(error contains """
        |magnolia: could not find ExportedTypeclass.Typeclass for type Seq[magnolia1.tests.Recursive]
        |    in parameter 'children' of product type magnolia1.tests.Recursive
        |""".stripMargin)
    }

    test("report an error when an abstract member of a sealed hierarchy is not sealed") {
      val error = compileErrors("Show.gen[Parent]")
      assert(error contains "magnolia: child trait BadChild of trait Parent is not sealed")
    }

    test("report an error when a concrete member of a sealed hierarchy is neither final nor a case class") {
      val error = compileErrors("Show.gen[GoodChild]")
      assert(error contains "magnolia: child class Dewey of trait GoodChild is neither final nor a case class")
    }

    test("support split without join") {
      val res = implicitly[NoCombine[Halfy]].nameOf(Righty())
      assertEquals(res, "Righty")
    }

    test("inherit annotations from parent trait") {
      val res = Show.gen[Pet].show(Dog("Alex", 10, likesMeat = true))
      assertEquals(
        res,
        "{MyTypeAnnotation(2),MyTypeAnnotation(1)}Dog{MyTypeAnnotation(2),MyTypeAnnotation(1)}(name{MyAnnotation(1)}=Alex,age{MyAnnotation(2)}=10,likesMeat{MyAnnotation(3)}=true)"
      )
    }

    test("inherit annotations from all parent traits in hierarchy") {
      val res = Show.gen[Rodent].show(Hamster("Alex", 10, likesNuts = true, likesVeggies = true))
      assertEquals(
        res,
        "{MyTypeAnnotation(1)}Hamster{MyTypeAnnotation(1)}(name{MyAnnotation(1)}=Alex,age{MyAnnotation(2)}=10,likesNuts{MyAnnotation(3)}=true,likesVeggies{MyAnnotation(4)}=true)"
      )
    }

    test("inherit annotations from base class constructor parameters") {
      val res = Show.gen[Foo].show(Foo("foo"))
      assertEquals(res, "Foo(foo{MyAnnotation(2),MyAnnotation(1)}=foo)")
    }

    test("inherit annotations from all base class constructor parameters in hierarchy") {
      val res = Show.gen[Bar].show(Bar("foo", "bar"))
      assertEquals(res, "Bar(foo{MyAnnotation(2),MyAnnotation(1)}=foo,bar{MyAnnotation(2),MyAnnotation(1)}=bar)")
    }

  private def serializeToByteArray(value: Serializable): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(buffer)
    oos.writeObject(value)
    buffer.toByteArray
  }

  private def deserializeFromByteArray(encodedValue: Array[Byte]): AnyRef = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(encodedValue))
    ois.readObject()
  }

  def ensureSerializable[T <: Serializable](value: T): T =
    deserializeFromByteArray(serializeToByteArray(value)).asInstanceOf[T]

  test("generate serializable type-classes") {
    ensureSerializable(new Outer().showAddress)
    ensureSerializable(new Outer().showColor)
  }
}
