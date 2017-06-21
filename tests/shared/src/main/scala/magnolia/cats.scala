package magnolia

import examples.{Address, Branch, Country, Entity, Leaf, Person}
import cats.instances.all._
import cats.syntax.all._
import examples.cats.instances._
import language.experimental.macros

object CatsMain {

  def main(args: Array[String]): Unit = {
    println(Branch(Branch(Leaf(1, "a"), Leaf(2, "b")), Leaf(3, "c")).show)
    println(List[Entity](Person("John Smith",
      Address(List("1 High Street", "London", "SW1A 1AA"),
        Country("UK", "GBR", false)))).show)
  }
}
