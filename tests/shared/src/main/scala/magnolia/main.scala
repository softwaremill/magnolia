package magnolia

sealed trait Tree

case class Branch(left: List[Leaf]) extends Tree
case class Leaf(node: List[String], right: List[Branch], left2: List[Branch], another: List[Leaf], broken: Double) extends Tree
case class Twig(alpha: List[Twig], beta: List[Leaf], gamma: Double, delta: List[Tree]) extends Tree

object Main {


  def main(args: Array[String]): Unit = {
    println(implicitly[Extractor[List[Twig]]].extract(Thing("42")))
  }

}

