package magnolia

import examples._

object Main {
  def main(args: Array[String]): Unit = {
    println(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show)
    println(List[Entity](Person("John Smith",
        Address(List("1 High Street", "London", "SW1A 1AA"),
        Country("UK", "GBR", false)))).show)
  }
}

