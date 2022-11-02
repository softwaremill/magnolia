package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

class VarianceTests extends munit.FunSuite:
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

  // TODO:
  // LabelledBox being invariant in L <: String prohibits the derivation for LabelledBox[Int, _]
  // test("can't show a Box with invariant label") {
  //   val error = compileErrors("Show.derived[Box[Int]]")
  //   println(s"ERR: $error")
  //   assert(error contains """
  //       |magnolia: could not find Show.Typeclass for type L
  //       |    in parameter 'label' of product type magnolia1.tests.LabelledBox[Int, _ <: String]
  //       |    in coproduct type magnolia1.tests.Box[Int]
  //       |""".stripMargin)
  // }

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

  // TODO - not working
  // test("dependencies between derived type classes") {
  //   given [T: [X] =>> Show[String, X]] : Show[String, Path[T]] = Show.derived
  //   implicit def showDefaultOption[A](
  //                                       implicit showA: Show[String, A],
  //                                       defaultA: HasDefault[A]
  //                                     ): Show[String, Option[A]] = (optA: Option[A]) => showA.show(optA.getOrElse(defaultA.defaultValue.right.get))

  //   val res = Show.derived[Path[String]].show(OffRoad(Some(Crossroad(Destination("A"), Destination("B")))))
  //   assertEquals(res, "OffRoad[String](path=Crossroad[String](left=Destination[String](value=A),right=Destination[String](value=B)))")
  // }

object VarianceTests:
  sealed trait Politician[+S]
  case class Accountable[+S](slogan: S) extends Politician[S]
  case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L)
      extends Politician[S]

  sealed trait Box[+A]
  case class SimpleBox[+A](value: A) extends Box[A]
  case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]
