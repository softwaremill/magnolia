package magnolia.examples

import scala.collection.immutable.ListMap
import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.reflect._
import scala.reflect.macros._
import scala.language.experimental.macros
import scala.annotation.unchecked.uncheckedVariance

trait Show[T] { def show(value: T): String }
object Show {
  implicit val string: Show[String] = identity
  implicit val int: Show[Int] = new Show[Int] { def show(s: Int): String = s.toString }
}
object DerivedShow {
  def join[T](context: JoinContext[Show, T])(value: T): String = context.parameters.map { param =>
    s"${param.label}=${param.typeclass.show(param.dereference(value))}"
  }.mkString(s"${context.typeName.split("\\.").last}(", ",", ")")

  def split[T](subclasses: List[Subclass[Show, T]])(value: T): String =
    subclasses.map { sub => sub.cast.andThen { value =>
      sub.typeclass.show(sub.cast(value))
    } }.reduce(_ orElse _)(value)

  def gen[T]: Show[T] = macro Magnolia.generic[T]
}

object Eq {
  def join[T](context: JoinContext[Eq, T])(value1: T, value2: T): Boolean =
    context.parameters.forall { param => param.typeclass.equal(param.dereference(value1), param.dereference(value2)) }

  def split[T](subclasses: List[Subclass[Eq, T]])(value1: T, value2: T): Boolean =
    subclasses.map { case subclass =>
      subclass.cast.andThen { value => subclass.typeclass.equal(subclass.cast(value1), subclass.cast(value2)) }
    }.reduce(_ orElse _)(value1)

  implicit val string: Eq[String] = _ == _
  implicit val int: Eq[Int] = _ == _
  implicit def generic[T]: Eq[T] = macro Magnolia.generic[T]
}

trait Eq[T] { def equal(value: T, value2: T): Boolean }

object Default {
  def join[T](context: JoinContext[Default, T]): Default[T] = new Default[T] {
    def default = context.construct { param => param.typeclass.default }
  }

  def split[T](subclasses: List[Subclass[Default, T]])(): Default[T] = new Default[T] {
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

  def split[T](subclasses: List[Subclass[Decoder, T]])(param: String): T =
    subclasses.map { subclass =>
      { case _ if decodes(subclass.typeclass, param) => subclass.typeclass.decode(param) }: PartialFunction[String, T]
    }.reduce(_ orElse _)(param)

  def decodes[T](tc: Decoder[T], s: String): Boolean = try { tc.decode(s); true } catch { case e: Exception => false }
  
  implicit val string: Decoder[String] = new Decoder[String] { def decode(str: String): String = str }
  implicit val int: Decoder[Int] = new Decoder[Int] { def decode(str: String): Int = str.toInt }
  implicit def generic[T]: Decoder[T] = macro Magnolia.generic[T]
}

trait Decoder[T] { def decode(str: String): T }
