/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package magnolia.tests

import magnolia.examples.*
import magnolia.TypeInfo

import java.time.LocalDate
import probably.*

import scala.annotation.StaticAnnotation

type ShowStr = [X] =>> Show[String, X ]

sealed trait Tree[+T] derives Eq
object Tree:
  given [T: [X] =>> Show[String, X]] : Show[String, Tree[T]] = Show.derived

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

object Fruit:
  given showFruit: Show[String, Fruit] = (f: Fruit) => f.name

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

  def apply(a: String)(using b: Int): TestEntry = TestEntry(Param(a, b.toString))

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
object Recursive {
  given showRecursive: Show[String, Recursive] = Show.derived[Recursive]
}

// This tests compilation.
// class GenericCsv[A: Csv]
// object ParamCsv extends GenericCsv[Param]


class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: String) extends AnyVal
final case class ServiceName2(value: String)

@MyAnnotation(0)
@SuppressWarnings(Array("deprecation"))
@JavaExampleAnnotation(description = "Some model")
case class MyDto(foo: String, bar: Int)

@SerialVersionUID(42) case class Schedule(events: Seq[Event])
case class Event(date: LocalDate)

case class RPerson(age: Int, name: String, children: Seq[RPerson])
object RPerson {
  given Show[String, RPerson] = Show.derived
}
case class GPerson(children: Seq[RPerson])

case class ProtectedCons protected (name: String)
object ProtectedCons {
  def apply(firstName: String, familyName: String): ProtectedCons =
    new ProtectedCons(firstName + " " + familyName)
  given show: Show[String, ProtectedCons] = Show.derived
}

case class PrivateCons private (name: String)
object PrivateCons {
  def apply(firstName: String, familyName: String): PrivateCons =
    new PrivateCons(firstName + " " + familyName)
  given show: Show[String, PrivateCons] = Show.derived
}

// class PrivateValueClass private (val value: Int) extends AnyVal
// object PrivateValueClass {
//   def apply(l: Int) = new PrivateValueClass(l)
//   implicit val show: Show[String, PrivateValueClass] = Show.derived
// }

case class KArray(value: List[KArray]) derives Eq
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

  given idShow: Show[String, Id] = _.toString
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

sealed trait Parent
trait BadChild extends Parent // escape hatch!
sealed trait GoodChild extends Parent
final case class Huey(height: Int) extends GoodChild
class Dewey(val height: Int) extends GoodChild
final case class Louie(height: Int) extends BadChild

object Tests extends Suite("Magnolia tests"):

  def run(using Runner) =
    test("construct a Show product instance with alternative apply functions") {
      Show.derived[TestEntry].show(TestEntry("a", "b"))
    }.assert(_ == """TestEntry(param=Param(a=a,b=b))""")

    test("construct a Show product instance") {
      Show.derived[Person].show(Person("John Smith", 34))
    }.assert(_ == """Person(name=John Smith,age=34)""")

    test("construct a Show coproduct instance") {
      Show.derived[Person].show(Person("John Smith", 34))
    }.assert(_ == "Person(name=John Smith,age=34)")

    test("construct a Show instance for product with partially private fields") {
      Show.derived[Abc].show(Abc(12, 54, "pm"))
    }.assert(_ == "Abc(a=12,b=54L,c=pm)")

    test("construct a Show instance for a product with multiple default values") {
      Show.derived[ParamsWithDefault].show(ParamsWithDefault())
    }.assert(_ == "ParamsWithDefault(a=3,b=4)")

    test("local implicit beats Magnolia") {
      given showPerson: Show[String, Person] = _ => "nobody"
      summon[Show[String, Address]].show(Address("Home", Person("John Smith", 44)))
    }.assert(_ == "Address(line1=Home,occupant=nobody)")

    test("even low-priority implicit beats Magnolia for nested case") {
      summon[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
    }.assert(_ == "Lunchbox(fruit=apple,drink=lemonade)")

    test("low-priority implicit beats Magnolia when not nested") {
      summon[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "apple")

    test("low-priority implicit beats Magnolia when chained") {
      summon[Show[String, FruitBasket]].show(FruitBasket(Fruit("apple"), Fruit("banana")))
    }.assert(_ == "FruitBasket(fruits=[apple,banana])")

    test("typeclass implicit scope has lower priority than ADT implicit scope") {
      summon[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "apple")

    test("test equality false") {
      Eq.derived[Entity].equal(Person("John Smith", 34), Person("", 0))
    }.assert(!_)

    test("test equality true") {
      Eq.derived[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
    }.assert(_ == true)

    test("test branch equality true") {
      Eq.derived[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
    }.assert(_ == true)

    test("construct a default value") {
      HasDefault.derived[Entity].defaultValue
    }.assert(_ == Right(Company("")))

    test("serialize a Leaf") {
      implicitly[Show[String, Leaf[String]]].show(Leaf("testing"))
    }.assert(_ == "Leaf[String](value=testing)")

    test("serialize case object") {
      summon[Show[String, Red.type]].show(Red)
    }.assert(_ == "Red()")

    test("serialize self recursive type") {
      summon[Show[String, GPerson]].show(GPerson(Nil))
    }.assert(_ == "GPerson(children=[])")

    test("serialize case object as a sealed trait") {
      summon[Show[String, Color]].show(Blue)
    }.assert(_ == "Blue()")

    test("serialize case class with protected constructor") {
      ProtectedCons.show.show(ProtectedCons("dada", "phil"))
    }.assert(_ == "ProtectedCons(name=dada phil)")

    test("serialize case class with accessible private constructor") {
      PrivateCons.show.show(PrivateCons("dada", "phil"))
    }.assert(_ == "PrivateCons(name=dada phil)")

    test("read-only typeclass can serialize case class with inaccessible private constructor") {
      summon[Print[PrivateCons]].print(PrivateCons("dada", "phil"))
    }.assert(_ == "PrivateCons(dada phil)")

    test("read-only typeclass can serialize case class with protected constructor") {
      summon[Print[ProtectedCons]].print(ProtectedCons("dada", "phil"))
    }.assert(_ == "ProtectedCons(dada phil)")

    test("decode a company") {
      Decoder.derived[Company].decode("""Company(name=Acme Inc)""")
    }.assert(_ == Company("Acme Inc"))

    test("decode a Person as an Entity") {
      summon[Decoder[Entity]].decode("""tests.Person(name=John Smith,age=32)""")
    }.assert(_ == Person("John Smith", 32))

    test("decode a nested product") {
      summon[Decoder[Address]].decode(
        """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
      )
    }.assert(_ == Address("53 High Street", Person("Richard Jones", 44)))

    test("typenames and labels are not encoded") {
      summon[Show[String, `%%`]].show(`%%`(1, "two"))
    }.assert(_ == "%%(/=1,#=two)")

    val tupleDerivation = summon[Show[String, (Int, String)]]

    test("serialize a tuple") {
      tupleDerivation.show((42, "Hello World"))
    }.assert(_ == "Tuple2[Int,String](_1=42,_2=Hello World)")

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
    test("show a Politician with covariant lobby") {
      Show.derived[Politician[String]].show(Corrupt("wall", Seq(Company("Alice Inc"))))
    }.assert(_ == "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")

    //test("patch a Person via a Patcher[Entity]") {
    //  val person = Person("Bob", 42)
    //  summon[Patcher[Entity]].patch(person, Seq(null, 21))
    //}.assert(_ == Person("Bob", 21))

    test("show an Account") {
      Show.derived[Account].show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
    }.assert(_ == "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")

    test("construct a default Account") {
      HasDefault.derived[Account].defaultValue
    }.assert(_ == Right(Account("")))

    test("construct a failed NoDefault") {
      HasDefault.derived[NoDefault].defaultValue
    }.assert(_ == Left("truth is a lie"))

    test("show a Portfolio of Companies") {
      Show.derived[Portfolio].show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
    }.assert(_ == "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")

    // test("show a List[Int]") {
    //   given [T: [X] =>> Show[String, X]] : Show[String, List[T]] = Show.derived

    //   Show.derived[List[Int]].show(List(1, 2, 3))
    //.assert(_ == "::[Int](head=1,tl=::[Int](head=2,tl=::[Int](head=3,tl=Nil())))")
    // }

    test("sealed trait typeName should be complete and unchanged") {
      TypeNameInfo.derived[Color].name
    }.assert(_.full == "tests.Color")

    test("sealed trait subtypes should be ordered") {
      TypeNameInfo.derived[Color].subtypeNames.map(_.short)
    }.assert(_ == Seq("Red", "Green", "Blue", "Orange", "Pink"))

    test("case class typeName should be complete and unchanged") {
      given stringTypeName: TypeNameInfo[String] with {
        def name = ???

        def subtypeNames = ???
      }
      TypeNameInfo.derived[Fruit].name
    }.assert(_.full == "tests.Fruit")

    test("show a recursive case class") {
      Show.derived[Recursive].show(Recursive(Seq(Recursive(Nil))))
    }.assert(_ == "Recursive(children=[Recursive(children=[])])")

    test("manually derive a recursive case class instance") {
      Recursive.showRecursive.show(Recursive(Seq(Recursive(Nil))))
    }.assert(_ == "Recursive(children=[Recursive(children=[])])")

    test("show underivable type with fallback") {
      summon[TypeNameInfo[NotDerivable]].name
    }.assert(_ == TypeInfo("", "Unknown Type", Seq.empty))

    test("equality of Wrapper") {
      Eq.derived[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
    }.assert(!_)

    test("very long") {
      val vl =
        VeryLong("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15",
            "p16", "p17", "p18", "p19", "p20", "p21", "p22", "p23")
      Eq.derived[VeryLong].equal(vl, vl)
    }.assert(_ == true)
