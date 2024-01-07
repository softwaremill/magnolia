package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

class VarianceTests extends munit.FunSuite:

  import VarianceTests.*

  // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
  test("show a Politician with covariant lobby") {
    val res = Show
      .derived[Politician[String]]
      .show(Corrupt("wall", Seq(Company("Alice Inc"))))
    assertEquals(
      res,
      "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])"
    )
  }

  test("show a Box with invariant label") {
    val res = summon[Show[String, Box[Int]]].show(LabelledBox(17, "justLabel"))
    assertEquals(res, "LabelledBox[Int,String](value=17,label=justLabel)")
  }

  // TODO: yields [Any | Custom | Int | Nothing | String]
  // test("determine subtypes of Exactly[Int]") {
  //   given TypeNameInfo[Int] = TypeNameInfo.fallback[Int]
  //   val res = TypeNameInfo.derived[Exactly[Int]].subtypeNames.map(_.short).mkString(" | ")
  //   assertEquals(res, "Custom | Int")
  // }

  // TODO: yields [Any | Custom | Int | Nothing | String]
  // test("determine subtypes of Covariant[String]") {
  //   given hideFallbackWarning: TypeNameInfo[String] = TypeNameInfo.fallback[String]

  //   val res = TypeNameInfo.derived[Covariant[String]].subtypeNames.map(_.short).mkString(" | ")
  //   assertEquals(res, "Custom | Nothing | String")
  // }

  // TODO: yields [Any | Custom | Int | Nothing | String]
  // test("determine subtypes of Contravariant[Double]") {
  //   given hideFallbackWarning: TypeNameInfo[Double] = TypeNameInfo.fallback[Double]

  //   val res = TypeNameInfo.derived[Contravariant[Double]].subtypeNames.map(_.short).mkString(" | ")
  //   assertEquals(res, "Any | Custom")
  // }

  // TODO - not working as expected
  // test("dependencies between derived type classes") {
  //   given [T: [X] =>> Show[String, X]] : Show[String, Path[T]] = Show.derived
  //   implicit def showDefaultOption[A](
  //                                       implicit showA: Show[String, A],
  //                                       defaultA: HasDefault[A]
  //                                     ): Show[String, Option[A]] = (optA: Option[A]) => showA.show(optA.getOrElse(defaultA.defaultValue.right.get))

  //   val res = Show.derived[Path[String]].show(OffRoad(Some(Crossroad(Destination("A"), Destination("B")))))
  //   val destinationA = Destination("A")
  //   val destinationB = Destination("B")
  //   val crossroad = Crossroad(destinationA, destinationB)
  //   val offroad = OffRoad(Some(crossroad))

  //   val destinationAShow = summon[Show[String, Destination[String]]].show(destinationA)
  //   val crossroadShow = summon[Show[String, Crossroad[String]]].show(crossroad)
  //   val crossroadShow2 = summon[Show[String, Path[String]]].show(crossroad)
  //   val offroadShow = summon[Show[String, Path[String]]].show(offroad)

  //   assertEquals(res, "OffRoad[String](path=Crossroad[String](left=Destination[String](value=A),right=Destination[String](value=B)))")
  // }

object VarianceTests:

  sealed trait Covariant[+A]

  sealed trait Contravariant[-A]

  sealed trait Exactly[A] extends Covariant[A], Contravariant[A]

  object Exactly:
    case object Any extends Exactly[Any]
    case class Custom[A](value: A) extends Exactly[A]
    case object Int extends Exactly[Int]
    case object Nothing extends Exactly[Nothing]
    case object String extends Exactly[String]

  sealed trait Politician[+S]
  case class Accountable[+S](slogan: S) extends Politician[S]
  case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)

  sealed trait Box[+A]
  case class SimpleBox[+A](value: A) extends Box[A]
  case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

  sealed trait Path[+A] derives Print
  case class Destination[+A](value: A) extends Path[A]
  case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
  case class OffRoad[+A](path: Option[Path[A]]) extends Path[A]
