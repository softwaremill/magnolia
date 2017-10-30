package magnolia.examples

import scala.collection.immutable.ListMap
import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.reflect._
import scala.reflect.macros._
import scala.language.experimental.macros
import scala.annotation.unchecked.uncheckedVariance
import stalactite._

sealed trait Tree
case class Leaf(value: String) extends Tree
case class Branch(left: Tree, right: Tree) extends Tree
case object Bud extends Tree

sealed trait Entity
case class Company(name: String) extends Entity
@deriving(Show)
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)

