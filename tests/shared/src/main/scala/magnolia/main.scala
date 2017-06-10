package magnolia

sealed trait Tree

case class Branch(left: List[Leaf]) extends Tree
case class Leaf(node: List[String], right: List[Branch], left2: List[Branch], another: List[Leaf]) extends Tree

object Main {


  def main(args: Array[String]): Unit = {
    println(implicitly[Extractor[List[Leaf]]].extract(Thing("42")))
  }

}

