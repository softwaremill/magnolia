package magnolia.examples

import magnolia._

import language.experimental.macros
import collection.immutable.ListMap

object `package` {
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

sealed trait Tree
case class Branch(left: Tree, right: Tree) extends Tree
case class Leaf(value: Int) extends Tree

sealed trait Entity
case class Person(name: String, address: Address) extends Entity
case class Organization(name: String, contacts: Set[Person]) extends Entity
case class Address(lines: List[String], country: Country)
case class Country(name: String, code: String, salesTax: Boolean)

trait Eq[T] { def isEqual(a: T, b: T): Boolean }

object Eq {

  implicit val eqInt: Eq[Int] = _ == _

  val derivation: Coderivation2[Eq] = new Coderivation2[Eq] {
    type Return = Boolean
    
    def call[T](eq: Eq[T], value1: T, value2: T): Boolean =
      if(value1.getClass == value2.getClass) eq.isEqual(value1, value2) else false
    
    def construct[T](body: (T, T) => Boolean): Eq[T] = body(_, _)
    def join(className: String, elements: ListMap[String, Boolean]): Boolean =
      elements.forall(_._2)
  }
  
}
