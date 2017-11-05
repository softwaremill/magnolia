package magnolia.examples

import scala.collection.immutable.ListMap
import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.reflect._
import scala.reflect.macros._
import scala.language.experimental.macros
import scala.annotation.unchecked.uncheckedVariance


object Show {
  type Typeclass[T] = Show[String, T]
  def join[T](context: JoinContext[Typeclass, T]): Show[String, T] = new Show[String, T] {
    def show(value: T) = context.parameters.map { param =>
      s"${param.label}=${param.typeclass.show(param.dereference(value))}"
    }.mkString(s"${context.typeName.split("\\.").last}(", ",", ")")
  }

  def dispatch[T](subclasses: Seq[Subclass[Typeclass, T]])(value: T): String =
    subclasses.map { sub => sub.cast.andThen { value =>
      sub.typeclass.show(sub.cast(value))
    } }.reduce(_ orElse _)(value)

  implicit val string: Show[String, String] = identity
  implicit val int: Show[String, Int] = new Show[String, Int] { def show(s: Int): String = s.toString }
  implicit def generic[T]: Show[String, T] = macro Magnolia.generic[T]
}

trait Show[Out, T] { def show(value: T): Out }

object Eq {
  type Typeclass[T] = Eq[T]
  def join[T](context: JoinContext[Eq, T]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T) =
      context.parameters.forall { param => param.typeclass.equal(param.dereference(value1), param.dereference(value2)) }
  }

  def dispatch[T](subclasses: Seq[Subclass[Eq, T]]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T) =
      subclasses.map { case subclass =>
        subclass.cast.andThen { value => subclass.typeclass.equal(subclass.cast(value1), subclass.cast(value2)) }
      }.reduce(_ orElse _)(value1)
  }

  implicit val string: Eq[String] = _ == _
  implicit val int: Eq[Int] = _ == _
  implicit def generic[T]: Eq[T] = macro Magnolia.generic[T]
}

trait Eq[T] { def equal(value: T, value2: T): Boolean }

object Default {
  type Typeclass[T] = Default[T]
  def join[T](context: JoinContext[Default, T]): Default[T] = new Default[T] {
    def default = context.construct { param => param.typeclass.default }
  }

  def dispatch[T](subclasses: Seq[Subclass[Default, T]])(): Default[T] = new Default[T] {
    def default = subclasses.head.typeclass.default
  }

  implicit val string: Default[String] = new Default[String] { def default = "" }
  implicit val int: Default[Int] = new Default[Int] { def default = 0 }
  implicit def generic[T]: Default[T] = macro Magnolia.generic[T]
}

trait Default[T] { def default: T }

object Decoder {
  def join[T](context: JoinContext[Decoder, T])(value: String): T =
    context.construct { param => param.typeclass.decode(value) }

  def dispatch[T](subclasses: Seq[Subclass[Decoder, T]])(param: String): T =
    subclasses.map { subclass =>
      { case _ if decodes(subclass.typeclass, param) => subclass.typeclass.decode(param) }: PartialFunction[String, T]
    }.reduce(_ orElse _)(param)

  def decodes[T](tc: Decoder[T], s: String): Boolean = try { tc.decode(s); true } catch { case e: Exception => false }
  
  implicit val string: Decoder[String] = new Decoder[String] { def decode(str: String): String = str }
  implicit val int: Decoder[Int] = new Decoder[Int] { def decode(str: String): Int = str.toInt }
  implicit def generic[T]: Decoder[T] = macro Magnolia.generic[T]
}

trait Decoder[T] { def decode(str: String): T }

sealed trait Tree[+T]
case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Entity

case class Company(name: String) extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)



sealed trait Alphabet

case class Greek(άλφα: Letter, βήτα: Letter, γάμα: Letter, δέλτα: Letter, έψιλον: Letter, ζήτα: Letter, ήτα: Letter, θήτα: Letter) extends Alphabet
case class Cyrillic(б: Letter, в: Letter, г: Letter, д: Letter, ж: Letter, з: Letter) extends Alphabet
case class Latin(a: Letter, b: Letter, c: Letter, d: Letter, e: Letter, f: Letter, g: Letter, h: Letter, i: Letter, j: Letter, k: Letter, l: Letter, m: Letter) extends Alphabet

case class Letter(name: String, phonetic: String)
case class Country(name: String, language: Language, leader: Person)
case class Language(name: String, code: String, alphabet: Alphabet)
//case class Person(name: String, dateOfBirth: Date)
case class Date(year: Int, month: Month, day: Int)

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

