package magnolia

import examples._

object Main {
  def main(args: Array[String]): Unit = {
    println(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show)

    println(List[Entity](Person("Jon Pretty", Address(List("Home"), Country("UK", "GBR", false)))).show)
  
  }

}

