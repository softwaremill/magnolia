package magnolia.examples

import magnolia._

import language.experimental.macros
import language.higherKinds
import collection.immutable.ListMap

object `package` {
  implicit class Showable[T: Show](t: T) {
    def show: String = implicitly[Show[T]].show(t)
  }
  implicit val showString: Show[String] = identity
  implicit val showBool: Show[Boolean] = _.toString
  implicit def showList[T: Show]: Show[List[T]] = xs => xs.map { x => s"list:${implicitly[Show[T]].show(x)}" }.mkString(";")
  implicit def showSet[T: Show]: Show[Set[T]] = s => "set"

  implicit class Equable[T: Eq](t: T) {
    def isEqualTo(other: T): Boolean = implicitly[Eq[T]].isEqual(t, other)
  }
  implicit val eqString: Eq[String] = _ == _
  implicit val eqBool: Eq[Boolean] = _ == _
  implicit def eqList[T: Eq]: Eq[List[T]] =
    (l1, l2) => l1.size == l2.size && (l1 zip l2).forall { case (e1, e2) => e1 isEqualTo e2 }
  implicit def eqSet[T: Eq]: Eq[Set[T]] =
    (s1, s2) => s1.size == s2.size && (s1 zip s2).forall { case (e1, e2) => e1 isEqualTo e2 }
}

sealed trait EmptyType

sealed trait Tree
case class Branch(left: Tree, right: Tree) extends Tree
case class Leaf(value: Int, no: String) extends Tree

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
    def join(xs: ListMap[String, String]): String = xs.map { case (k, v) => s"$k=$v" }.mkString("{", ", ", "}")
  }
}

trait Show_1 {
  implicit def generic[T]: Show[T] = macro Macros.magnolia[T, Show[_]]
}

trait Eq[T] { def isEqual(a: T, b: T): Boolean }

object Eq extends Eq_1 {

  implicit val eqInt: Eq[Int] = _ == _

  implicit val derivation = new ContravariantDerivation2[Eq] {
    type Return = Boolean
    def call[T](eq: Eq[T], value1: T, value2: T): Boolean =
      if(value1.getClass == value2.getClass) eq.isEqual(value1, value2) else false
    def construct[T](body: (T, T) => Boolean): Eq[T] = body(_, _)
    def join(elements: ListMap[String, Boolean]): Boolean = elements.forall(_._2)
  }
}

trait Eq_1 {
  implicit def generic[T]: Eq[T] = macro Macros.magnolia[T, Eq[_]]
}

