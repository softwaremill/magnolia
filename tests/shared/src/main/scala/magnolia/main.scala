package magnolia

import examples._

object Main {
  def main(args: Array[String]): Unit = {

    val tree1: Tree = Branch(Branch(Leaf(1, "abc"), Leaf(2, "def")), Leaf(3, "ghi"))
    val tree2: Tree = Branch(Leaf(1, "abc"), Leaf(2, "def"))

    println(tree1.show)
    println(tree1 isEqualTo tree1)
    println(tree1 isEqualTo tree2)

    println(List[Entity](Person("John Smith",
      Address(List("1 High Street", "London", "SW1A 1AA"),
        Country("UK", "GBR", false)))).show)

  }
}

