/* Magnolia, version 0.7.1. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package magnolia.tests

import language.experimental.macros
import estrapade.{TestApp, test}
import contextual.data.scalac._
import contextual.data.fqt._
import contextual.data.txt._
import magnolia.examples._
import magnolia.TypeName

import java.time.LocalDate

import scala.annotation.StaticAnnotation
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

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
  @MyAnnotation(1) p1: String,
  @MyAnnotation(2) p2: Int
) extends AttributeParent

case class `%%`(`/`: Int, `#`: String)

case class Param(a: String, b: String)
case class Test(param: Param)
object Test {
  def apply(): Test = Test(Param("", ""))

  def apply(a: String)(implicit b: Int): Test = Test(Param(a, b.toString))

  def apply(a: String, b: String): Test = Test(Param(a, b))
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
final case class Righty() extends Halfy

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

final case class Abc(
  private val a: Int,
  private val b: Long,
  c: String
)

object Tests extends TestApp {

  def tests(): Unit = for (_ <- 1 to 1) {
    test("construct a Show product instance with alternative apply functions") {
      Show.gen[Test].show(Test("a", "b"))
    }.assert(_ == """Test(param=Param(a=a,b=b))""")

    test("construct a Show product instance") {
      Show.gen[Person].show(Person("John Smith", 34))
    }.assert(_ == """Person(name=John Smith,age=34)""")

    test("construct a Show coproduct instance") {
      Show.gen[Person].show(Person("John Smith", 34))
    }.assert(_ == "Person(name=John Smith,age=34)")

    test("construct a Show instance for product with partially private fields") {
      implicit val showLong: Show[String, Long] = _.toString
      Show.gen[Abc].show(Abc(12, 54L, "pm"))
    }.assert(_ == "Abc(a=12,b=54,c=pm)")

    test("serialize a Branch") {
      implicitly[Show[String, Branch[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")

    test("local implicit beats Magnolia") {
      implicit val showPerson: Show[String, Person] = new Show[String, Person] {
        def show(p: Person) = "nobody"
      }
      implicitly[Show[String, Address]].show(Address("Home", Person("John Smith", 44)))
    }.assert(_ == "Address(line1=Home,occupant=nobody)")

    test("even low-priority implicit beats Magnolia for nested case") {
      implicitly[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
    }.assert(_ == "Lunchbox(fruit=apple,drink=lemonade)")

    test("low-priority implicit beats Magnolia when not nested") {
      implicitly[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "apple")

    test("low-priority implicit beats Magnolia when chained") {
      implicitly[Show[String, FruitBasket]].show(FruitBasket(Fruit("apple"), Fruit("banana")))
    }.assert(_ == "FruitBasket(fruits=[apple,banana])")

    test("typeclass implicit scope has lower priority than ADT implicit scope") {
      implicitly[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "apple")

    test("test equality false") {
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("", 0))
    }.assert(_ == false)

    test("test equality true") {
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
    }.assert(_ == true)

    test("test branch equality true") {
      Eq.gen[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
    }.assert(_ == true)

    test("construct a default value") {
      HasDefault.gen[Entity].defaultValue
    }.assert(_ == Right(Company("")))

    test("construction of Show instance for Leaf") {
      scalac"""
        implicitly[Show[String, Leaf[java.lang.String]]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Leaf[String]]"))

    test("construction of Show instance for Tree") {
      scalac"""
        implicitly[Show[String, Tree[String]]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Tree[String]]"))

    test("serialize a Leaf") {
      implicitly[Show[String, Leaf[String]]].show(Leaf("testing"))
    }.assert(_ == "Leaf[String](value=testing)")

    test("serialize a Branch as a Tree") {
      implicitly[Show[String, Tree[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))")

    test("serialize case object") {
      implicitly[Show[String, Red.type]].show(Red)
    }.assert(_ == "Red()")

    test("serialize self recursive type") {
      implicitly[Show[String, GPerson]].show(GPerson(Nil))
    }.assert(_ == "GPerson(children=[])")

    test("access default constructor values") {
      implicitly[HasDefault[Item]].defaultValue
    }.assert(_ == Right(Item("", 1, 0)))

    test("serialize case object as a sealed trait") {
      implicitly[Show[String, Color]].show(Blue)
    }.assert(_ == "Blue()")

    test("serialize case class with protected constructor") {
      ProtectedCons.show.show(ProtectedCons("dada", "phil"))
    }.assert(_ == "ProtectedCons(name=dada phil)")

    test("serialize case class with accessible private constructor") {
      PrivateCons.show.show(PrivateCons("dada", "phil"))
    }.assert(_ == "PrivateCons(name=dada phil)")

    test("serialize value case class with accessible private constructor") {
      PrivateValueClass.show.show(PrivateValueClass(42))
    }.assert(_ == "42")

    test("read-only typeclass can serialize case class with inaccessible private constructor") {
      implicitly[Print[PrivateCons]].print(PrivateCons("dada", "phil"))
    }.assert(_ == "PrivateCons(dada phil)")

    test("read-only typeclass can serialize value case class with inaccessible private constructor") {
      implicitly[Print[PrivateValueClass]].print(PrivateValueClass(42))
    }.assert(_ == "42")

    test("read-only typeclass can serialize case class with protected constructor") {
      implicitly[Print[ProtectedCons]].print(ProtectedCons("dada", "phil"))
    }.assert(_ == "ProtectedCons(dada phil)")

    test("decode a company") {
      Decoder.gen[Company].decode("""Company(name=Acme Inc)""")
    }.assert(_ == Company("Acme Inc"))

    test("decode a Person as an Entity") {
      implicitly[Decoder[Entity]].decode("""magnolia.tests.Person(name=John Smith,age=32)""")
    }.assert(_ == Person("John Smith", 32))

    test("decode a nested product") {
      implicitly[Decoder[Address]].decode(
        """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
      )
    }.assert(_ == Address("53 High Street", Person("Richard Jones", 44)))

    test("show error stack") {
      scalac"""
        case class Alpha(integer: Double)
        case class Beta(alpha: Alpha)
        Show.gen[Beta]
      """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Double
        |    in parameter 'integer' of product type Alpha
        |    in parameter 'alpha' of product type Beta
        |"""))

    test("serialize case class with Java annotations by skipping them") {
      Show.gen[MyDto].show(MyDto("foo", 42))
    }.assert(_ == "MyDto{MyAnnotation(0)}(foo=foo,bar=42)")

    test("serialize case class with Java annotations which comes from external module by skipping them") {
      Show.gen[JavaAnnotatedCase].show(JavaAnnotatedCase(1))
    }.assert(_ == "JavaAnnotatedCase(v=1)")

    test("not attempt to instantiate Unit when producing error stack") {
      scalac"""
        case class Gamma(unit: Unit)
        Show.gen[Gamma]
      """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Unit
        |    in parameter 'unit' of product type Gamma
        |"""))

    test("not assume full auto derivation of external value classes") {
      scalac"""
        case class LoggingConfig(n: ServiceName1)
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
        """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type magnolia.tests.ServiceName1
    in parameter 'n' of product type LoggingConfig
""") )

    test("not assume full auto derivation of external products") {
      scalac"""
        case class LoggingConfig(n: ServiceName2)
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
        """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type magnolia.tests.ServiceName2
    in parameter 'n' of product type LoggingConfig
""") )

    test("not assume full auto derivation of external coproducts") {
      scalac"""
        case class LoggingConfig(o: Option[String])
        object LoggingConfig {
          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
        }
        """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type Option[String]
    in parameter 'o' of product type LoggingConfig
""") )

    test("half auto derivation of sealed families") {
      SemiDefault.gen[Halfy].default
    }.assert(_ == Lefty())

    test("typenames and labels are not encoded") {
      implicitly[Show[String, `%%`]].show(`%%`(1, "two"))
    }.assert(_ == "%%(/=1,#=two)")

    val tupleDerivation = test("derive for a tuple") {
      implicitly[Show[String, (Int, String)]]
    }.returns()

    test("serialize a tuple") {
      tupleDerivation().show((42, "Hello World"))
    }.assert(_ == "Tuple2[Int,String](_1=42,_2=Hello World)")

    test("serialize a value class") {
      Show.gen[Length].show(new Length(100))
    }.assert(_ == "100")

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
    test("show a Politician with covariant lobby") {
      Show.gen[Politician[String]].show(Corrupt("wall", Seq(Company("Alice Inc"))))
    }.assert(_ == "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")

    // LabelledBox being invariant in L <: String prohibits the derivation for LabelledBox[Int, _]
    test("can't show a Box with invariant label") {
      scalac"Show.gen[Box[Int]]"
    }.assert { _ == TypecheckError(txt"""magnolia: could not find Show.Typeclass for type L
        |    in parameter 'label' of product type magnolia.tests.LabelledBox[Int, _ <: String]
        |    in coproduct type magnolia.tests.Box[Int]
        |""") }

    test("patch a Person via a Patcher[Entity]") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      val person = Person("Bob", 42)
      implicitly[Patcher[Entity]].patch(person, Seq(null, 21))
    }.assert(_ == Person("Bob", 21))

    test("throw on an illegal patch attempt with field count mismatch") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      try {
        val person = Person("Bob", 42)
        implicitly[Patcher[Entity]].patch(person, Seq(null, 21, 'killer))
      } catch {
        case NonFatal(e) => e.getMessage
      }
    }.assert(_ == "Cannot patch value `Person(Bob,42)`, expected 2 fields but got 3")


    test("throw on an illegal patch attempt with field type mismatch") {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher = Patcher.forSingleValue[Int]

      try {
        val person = Person("Bob", 42)
        implicitly[Patcher[Entity]].patch(person, Seq(null, 'killer))
        "it worked"
      } catch {
        case NonFatal(e) => e.getMessage
      }
    }.assert{x =>
      //tiny hack because Java 9 inserts the "java.base/" module name in the error message
      x.startsWith("scala.Symbol cannot be cast to") && x.endsWith("java.lang.Integer")
    }

    class ParentClass {
      case class InnerClass(name: String)
      case class InnerClassWithDefault(name: String = "foo")

      def testInner(): Unit = {
        test("serialize a case class inside another class") {
          implicitly[Show[String, InnerClass]].show(InnerClass("foo"))
        }.assert(_ == "InnerClass(name=foo)")

        test("construct a default case class inside another class") {
          HasDefault.gen[InnerClassWithDefault].defaultValue
        }.assert(_ == Right(InnerClassWithDefault()))

        ()
      }

      def testLocal(): Unit = {
        case class LocalClass(name: String)
        case class LocalClassWithDefault(name: String = "foo")

        test("serialize a case class inside a method") {
          implicitly[Show[String, LocalClass]].show(LocalClass("foo"))
        }.assert(_ == "LocalClass(name=foo)")

        test("construct a default case class inside a method") {
          HasDefault.gen[LocalClassWithDefault].defaultValue
        }.assert(_ == Right(LocalClassWithDefault()))

        ()
      }
    }

    val parent = new ParentClass()
    parent.testInner()
    parent.testLocal()

    test("show an Account") {
      Show.gen[Account].show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
    }.assert(_ == "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")

    test("construct a default Account") {
      HasDefault.gen[Account].defaultValue
    }.assert(_ == Right(Account("")))

    test("construct a failed NoDefault") {
      HasDefault.gen[NoDefault].defaultValue
    }.assert(_ == Left("truth is a lie"))

    test("show a Portfolio of Companies") {
      Show.gen[Portfolio].show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
    }.assert(_ == "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")

    test("show a List[Int]") {
      Show.gen[List[Int]].show(List(1, 2, 3))
    }.assert(_ == "::[Int](head=1,tl=::[Int](head=2,tl=::[Int](head=3,tl=Nil())))")

    test("sealed trait typeName should be complete and unchanged") {
      TypeNameInfo.gen[Color].name
    }.assert(_.full == "magnolia.tests.Color")

    test("sealed trait subtypes should be ordered") {
      TypeNameInfo.gen[Color].subtypeNames
    }.assert(_.map(_.short) == Seq("Blue", "Green", "Orange", "Pink", "Red"))

    test("case class typeName should be complete and unchanged") {
      implicit val stringTypeName: TypeNameInfo[String] = new TypeNameInfo[String] {
        def name = ???

        def subtypeNames = ???
      }
      TypeNameInfo.gen[Fruit].name
    }.assert(_.full == "magnolia.tests.Fruit")

    test("show chained error stack") {
      scalac"""
        Show.gen[(Int, Seq[(Long, String)])]
      """
    } assert { _ == TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Long
        |    in parameter '_1' of product type (Long, String)
        |    in chained implicit Show.Typeclass for type Seq[(Long, String)]
        |    in parameter '_2' of product type (Int, Seq[(Long, String)])
        |""") }

    test("show chained error stack when leaf instance is missing") {
      scalac"""Show.gen[Schedule]"""
    } assert { //
      _ == TypecheckError(txt"""magnolia: could not find Show.Typeclass for type java.time.LocalDate
        |    in parameter 'date' of product type magnolia.tests.Event
        |    in chained implicit Show.Typeclass for type Seq[magnolia.tests.Event]
        |    in parameter 'events' of product type magnolia.tests.Schedule
        |""")
    }

    test("show a recursive case class") {
      Show.gen[Recursive].show(Recursive(Seq(Recursive(Nil))))
    }.assert(_ == "Recursive(children=[Recursive(children=[])])")

    test("manually derive a recursive case class instance") {
      implicit lazy val showRecursive: Show[String, Recursive] = Show.gen[Recursive]
      showRecursive.show(Recursive(Seq(Recursive(Nil))))
    }.assert(_ == "Recursive(children=[Recursive(children=[])])")

    test("show a type aliased case class") {
      type T = Person
      Show.gen[T].show(Person("Donald Duck", 313))
    }.assert(_ == "Person(name=Donald Duck,age=313)")

    test("dependencies between derived type classes") {
      implicit def showDefaultOption[A](
        implicit showA: Show[String, A],
        defaultA: HasDefault[A]
      ): Show[String, Option[A]] = (optA: Option[A]) => showA.show(optA.getOrElse(defaultA.defaultValue.right.get))

      Show.gen[Path[String]].show(OffRoad(Some(Crossroad(Destination("A"), Destination("B")))))
    }.assert(_ == "OffRoad[String](path=Crossroad[String](left=Destination[String](value=A),right=Destination[String](value=B)))")

    test("capture attributes against params") {
      Show.gen[Attributed].show(Attributed("xyz", 100))
    }.assert(_ == "Attributed{MyAnnotation(0)}(p1{MyAnnotation(1)}=xyz,p2{MyAnnotation(2)}=100)")

    test("capture attributes against subtypes") {
      Show.gen[AttributeParent].show(Attributed("xyz", 100))
    }.assert(_ == "{MyAnnotation(0)}Attributed{MyAnnotation(0)}(p1{MyAnnotation(1)}=xyz,p2{MyAnnotation(2)}=100)")

    test("show underivable type with fallback") {
      TypeNameInfo.gen[NotDerivable].name
    }.assert(_ == TypeName("", "Unknown Type", Seq.empty))

    test("allow no-coproduct derivation definitions") {
      scalac"""
        WeakHash.gen[Person]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.WeakHash[magnolia.tests.Person]"))

    test("disallow coproduct derivations without dispatch method") {
      scalac"""
        WeakHash.gen[Entity]
      """
    }.assert(_ == TypecheckError("magnolia: the method `dispatch` must be defined on the derivation object WeakHash to derive typeclasses for sealed traits"))

    test("equality of Wrapper") {
      Eq.gen[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
    }.assert(_ == false)

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
      Eq.gen[VeryLong].equal(vl, vl)
    }.assert(_ == true)

    test("not attempt to derive instances for refined types") {
      scalac"Show.gen[Character]"
    }.assert(_ == TypecheckError(txt"magnolia: could not infer Show.Typeclass for refined type magnolia.tests.Character.Id"))

    test("not attempt to derive instances for Java enums") {
      scalac"Show.gen[WeekDay]"
    }.assert(_ == TypecheckError(txt"magnolia: could not infer Show.Typeclass for type magnolia.tests.WeekDay"))
  }
}
