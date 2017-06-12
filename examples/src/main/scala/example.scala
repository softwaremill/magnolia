package magnolia.examples

import magnolia._

import language.experimental.macros
import language.higherKinds

object `package` {
  implicit class Showable[T: Show](t: T) {
    def show: String = implicitly[Show[T]].show(t)
  }
  implicit val showString: Show[String] = identity
  implicit val showBool: Show[Boolean] = _.toString
  implicit def showList[T: Show]: Show[List[T]] = xs => xs.map { x => s"list:${implicitly[Show[T]].show(x)}" }.mkString(";")
  implicit def showSet[T: Show]: Show[Set[T]] = s => "set"
}

sealed trait Tree
case class Branch(left: Tree, right: Tree) extends Tree
case class Leaf(value: Int) extends Tree

sealed trait Entity
case class Person(name: String, address: Address) extends Entity
case class Organization(name: String, contacts: Set[Person]) extends Entity
case class Address(lines: List[String], country: Country)
case class Country(name: String, code: String, salesTax: Boolean)

trait Show[T] { def show(t: T): String }
object Show extends Show_1 {
  implicit val showInt: Show[Int] = _.toString
  implicit val derivation = new ContravariantDerivation[Show] {
    type Return = String
    def call[T](show: Show[T], value: T): String = show.show(value)
    def construct[T](body: T => String): Show[T] = body(_)
    def join(xs: List[String]): String = xs.mkString("(", ", ", ")")
  }
}

trait Show_1 {
  implicit def generic[T]: Show[T] = macro Macros.magnolia[T, Show[_]]
}
