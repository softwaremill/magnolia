package magnolia

sealed trait Tree

case class Branch(left: List[Twig]) extends Tree
case class Leaf(node: List[String], right: List[Branch], left2: List[Branch], another: List[Leaf], broken: Double) extends Tree
case class Twig(alpha: List[Twig], beta: List[Leaf], gamma: Double, delta: List[Tree]) extends Tree

object Main {


  def main(args: Array[String]): Unit = {
  
    println(implicitly[Serializer[List[Tree]]].serialize(List(Branch(List(Twig(Nil, Nil, 43, Nil))))))
    println(implicitly[Extractor[List[Tree]]].extract(Thing("42")))
  }

}

