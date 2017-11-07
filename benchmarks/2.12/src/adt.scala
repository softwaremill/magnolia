package adt

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Branch(left: Tree, right: Tree) extends Tree

sealed trait GTree[+T]
case class GLeaf[+T](value: String) extends GTree[T]
case class GBranch[+T](left: GTree[T], right: GTree[T]) extends GTree[T]

sealed trait Entity

case class Company(name: String) extends Entity
case class Human(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Human)

sealed trait Alphabet

case class Greek(άλφα: Letter, βήτα: Letter, γάμα: Letter, δέλτα: Letter, έψιλον: Letter, ζήτα: Letter, ήτα: Letter, θήτα: Letter) extends Alphabet
case class Cyrillic(б: Letter, в: Letter, г: Letter, д: Letter, ж: Letter, з: Letter) extends Alphabet
case class Latin(a: Letter, b: Letter, c: Letter, d: Letter, e: Letter, f: Letter, g: Letter, h: Letter, i: Letter, j: Letter, k: Letter, l: Letter, m: Letter, n: Letter, o: Letter, p: Letter, q: Letter, r: Letter, s: Letter, t: Letter, u: Letter, v: Letter) extends Alphabet

case class Letter(name: String, phonetic: String)
case class Country(name: String, language: Language, leader: Person, existence: DateRange)
case class Language(name: String, code: String, alphabet: Alphabet)
case class Person(name: String, dateOfBirth: Date)
case class Date(year: Int, month: Month, day: Int)

case class DateRange(from: Date, toDate: Date)

sealed trait Month
case object Jan extends Month
case object Feb extends Month
case object Mar extends Month
case object Apr extends Month
case object May extends Month
case object Jun extends Month
case object Jul extends Month
case object Aug extends Month
case object Sep extends Month
case object Oct extends Month
case object Nov extends Month
case object Dec extends Month
