package magnolia

import examples.{Address, Branch, Country, Entity, Leaf, Person}
import cats.instances.all._
import cats.syntax.all._
import examples.catsShowDerivation._
import language.experimental.macros

object Main {

  def main(args: Array[String]): Unit = {
    val tree1 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    println(tree1.show)
    println(tree1 isEqualTo tree1)

    println(List[Entity](Person("John Smith",
      Address(List("1 High Street", "London", "SW1A 1AA"),
        Country("UK", "GBR", false)))).show)
  }
}
