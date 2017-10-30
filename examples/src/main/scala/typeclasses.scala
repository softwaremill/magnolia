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
  def join[T](construct: Any, className: String, elems: List[Param[Show, T]])(value: T): String =
    elems.map { call => s"${call.label}=${call.typeclass.show(call.dereference(value))}" }.mkString(s"{", ",", "}")

  def split[T](subclasses: List[Magnolia.Subclass[Show, T]])(value: T): String =
    subclasses.map { sub => sub.cast.andThen { value =>
      sub.typeclass.show(sub.cast(value))
    } }.reduce(_ orElse _)(value)

  implicit val string: Show[String] = identity
  implicit val int: Show[Int] = new Show[Int] { def show(s: Int): String = s.toString }
  implicit def generic[T]: Show[T] = macro Magnolia.generic[T]
}

trait Show[T] { def show(value: T): String }

object Eq {
  def join[T](construct: Any, className: String, elems: List[Param[Eq, T]])(param1: T, param2: T): Boolean =
    elems.forall { case call => call.typeclass.equal(call.dereference(param1), call.dereference(param2)) }

  def split[T](subclasses: List[Magnolia.Subclass[Eq, T]])(param1: T, param2: T): Boolean =
    subclasses.map { case subclass =>
      subclass.cast.andThen { value => subclass.typeclass.equal(subclass.cast(param1), subclass.cast(param2)) }
    }.reduce(_ orElse _)(param1)

  implicit val string: Eq[String] = _ == _
  implicit val int: Eq[Int] = _ == _
  implicit def generic[T]: Eq[T] = macro Magnolia.generic[T]
}

trait Eq[T] { def equal(value: T, value2: T): Boolean }

object Default {
  case class Call[T](label: String, typeclass: Default[T])
  case class Subclass[T](label: String, typeclass: Default[T], cast: PartialFunction[_ >: T, T])
  
  def join[T](construct: ((Call[R] => R) forSome { type R }) => T, className: String, elems: List[Call[_]]): T =
    construct { call: Call[_] => call.typeclass.default }

  def split[T](subclasses: List[Subclass[T]])(param: T): T = subclasses.head.typeclass.default


  implicit val string: Default[String] = new Default[String] { def default: String = "" }
  implicit val int: Default[Int] = new Default[Int] { def default: Int = 0 }
  implicit def generic[T]: Default[T] = macro Magnolia.generic[T]
}

trait Default[T] { def default: T }

object Decoder {
  case class Call[T](label: String, typeclass: Decoder[T], value: String)
  
  case class Subclass[T](label: String, typeclass: Decoder[T], cast: PartialFunction[_ >: T, T])

  def join[T](construct: ((Call[R] => R) forSome { type R }) => T, className: String, elems: List[Call[_]]): T =
    construct { call: Call[_] => call.typeclass.decode(call.value) }

  def split[T](subclasses: List[Subclass[T]])(param: String): T =
    subclasses.map { case Subclass(name, typeclass, cast) =>
      PartialFunction[String, T] { case _ if decodes(typeclass, param) => typeclass.decode(param) }
    }.reduce(_ orElse _)(param)

  def decodes[T](tc: Decoder[T], s: String): Boolean = try { decodes(tc, s); true } catch { case e: Exception => false }
  
  implicit val string: Decoder[String] = new Decoder[String] { def decode(str: String): String = str }
  implicit val int: Decoder[Int] = new Decoder[Int] { def decode(str: String): Int = str.toInt }
  implicit def generic[T]: Decoder[T] = macro Magnolia.generic[T]
}

trait Decoder[T] { def decode(str: String): T }

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Branch(left: Tree, right: Tree) extends Tree

sealed trait Entity

case class Company(name: String) extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)
