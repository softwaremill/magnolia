package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

import java.time.LocalDate
import scala.annotation.StaticAnnotation

type ShowStr = [X] =>> Show[String, X]

sealed trait Tree[+T] derives Eq
object Tree:
  given [T: [X] =>> Show[String, X]]: Show[String, Tree[T]] = Show.derived

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

case class WithDefault(x: Int = 2)

class Length(val value: Int) extends AnyVal

case class Fruit(name: String)
object Fruit:
  given showFruit: Show[String, Fruit] = (f: Fruit) => f.name
case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: String)

case class Item(name: String, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Orange extends Color
case object Pink extends Color

@MyAnnotation(0)
sealed trait Sport
@MyAnnotation(1)
case object Boxing extends Sport
@MyAnnotation(2)
case class Soccer(players: Int) extends Sport

case class MyAnnotation(order: Int) extends StaticAnnotation
case class MyTypeAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
    @MyAnnotation(1) p1: String @MyTypeAnnotation(0),
    @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
) extends AttributeParent @MyTypeAnnotation(2)

case class Deprecated(@MyAnnotation(0) @deprecated f: Int)

case class `%%`(`/`: Int, `#`: String)

case class Param(a: String, b: String)

case class TestEntry(param: Param)
object TestEntry {
  def apply(): TestEntry = TestEntry(Param("", ""))

  def apply(a: String)(using b: Int): TestEntry = TestEntry(
    Param(a, b.toString)
  )

  def apply(a: String, b: String): TestEntry = TestEntry(Param(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S) extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L)
    extends Politician[S]

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

// sealed trait Parent
// trait BadChild extends Parent // escape hatch!
// sealed trait GoodChild extends Parent
// final case class Huey(height: Int) extends GoodChild
// class Dewey(val height: Int) extends GoodChild
// final case class Louie(height: Int) extends BadChild

object Obj1:
  object Obj2:
    case class NestedInObjects(i: Int)

sealed trait Y
case object A extends Y
case class B(s: String) extends Y

enum Size:
  case S, M, L

@MyTypeAnnotation(1)
sealed trait Pet {
  @MyAnnotation(1)
  def name: String
  @MyAnnotation(2)
  def age: Int
}

@MyTypeAnnotation(2)
case class Dog(name: String, age: Int, @MyAnnotation(3) likesMeat: Boolean)
    extends Pet

sealed trait Rodent extends Pet {
  @MyAnnotation(3)
  def likesNuts: Boolean
}

case class Hamster(
    name: String,
    age: Int,
    likesNuts: Boolean,
    @MyAnnotation(4) likesVeggies: Boolean
) extends Rodent

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

object ExtendingTraits:
  trait One
  trait Two

enum ExtendingTraits:
  case A extends ExtendingTraits with ExtendingTraits.One
  case B extends ExtendingTraits with ExtendingTraits.Two
  case C extends ExtendingTraits with ExtendingTraits.Two

//

trait PrintRepeated[T] {
  def print(t: T): String
}

object PrintRepeated extends AutoDerivation[PrintRepeated]:
  def join[T](ctx: CaseClass[Typeclass, T]): PrintRepeated[T] = _ =>
    ctx.params.filter(_.repeated).map(_.label).toList.toString

  override def split[T](ctx: SealedTrait[PrintRepeated, T]): PrintRepeated[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }

  given PrintRepeated[String] = _ => ""
  given seq[T](using printT: PrintRepeated[T]): PrintRepeated[Seq[T]] = _ => ""

//

class Tests extends munit.FunSuite
