package adt1

import scalaz._
import Scalaz._

// @deriving(Show, Equal) sealed trait Tree
// @deriving(Show, Equal) case class Leaf(value: String) extends Tree
// @deriving(Show, Equal) case class Branch(left: Tree, right: Tree) extends Tree

// @deriving(Show, Equal) sealed trait GTree[T]
// @deriving(Show, Equal) case class GLeaf[T](value: String) extends GTree[T]
// @deriving(Show, Equal) case class GBranch[T](left: GTree[T], right: GTree[T]) extends GTree[T]

// @deriving(Show, Equal) sealed trait Entity

// @deriving(Show, Equal) case class Company(name: String) extends Entity
// @deriving(Show, Equal) case class Human(name: String, age: Int) extends Entity
// @deriving(Show, Equal) case class Address(line1: String, occupant: Human)

@deriving(Show, Equal) sealed trait Alphabet

@deriving(Show, Equal) case class Greek(άλφα: Letter,
                 βήτα: Letter,
                 γάμα: Letter,
                 δέλτα: Letter,
                 έψιλον: Letter,
                 ζήτα: Letter,
                 ήτα: Letter,
                 θήτα: Letter)
    extends Alphabet

@deriving(Show, Equal) case class Cyrillic(б: Letter, в: Letter, г: Letter, д: Letter, ж: Letter, з: Letter)
    extends Alphabet

@deriving(Show, Equal) case class Latin(a: Letter,
                 b: Letter,
                 c: Letter,
                 d: Letter,
                 e: Letter,
                 f: Letter,
                 g: Letter,
                 h: Letter,
                 i: Letter,
                 j: Letter,
                 k: Letter,
                 l: Letter,
                 m: Letter,
                 n: Letter,
                 o: Letter,
                 p: Letter,
                 q: Letter,
                 r: Letter,
                 s: Letter,
                 t: Letter,
                 u: Letter,
                 v: Letter)
    extends Alphabet

//@deriving(Show, Equal) case class Letter(name: String, phonetic: String)
//@deriving(Show, Equal) case class Country(name: String, language: Language, leader: Person, existence: DateRange)
//@deriving(Show, Equal) case class Language(name: String, code: String, alphabet: Alphabet)
//@deriving(Show, Equal) case class Person(name: String, dateOfBirth: Date)
//@deriving(Show, Equal) case class Date(year: Int, month: Month, day: Int)
//@deriving(Show, Equal) case class DateRange(from: Date, toDate: Date)

// @deriving(Show, Equal) sealed trait Month
// @deriving(Show, Equal) case object Jan extends Month
// @deriving(Show, Equal) case object Feb extends Month
// @deriving(Show, Equal) case object Mar extends Month
// @deriving(Show, Equal) case object Apr extends Month
// @deriving(Show, Equal) case object May extends Month
// @deriving(Show, Equal) case object Jun extends Month
// @deriving(Show, Equal) case object Jul extends Month
// @deriving(Show, Equal) case object Aug extends Month
// @deriving(Show, Equal) case object Sep extends Month
// @deriving(Show, Equal) case object Oct extends Month
// @deriving(Show, Equal) case object Nov extends Month
// @deriving(Show, Equal) case object Dec extends Month
