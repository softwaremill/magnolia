package magnolia

sealed trait Bar

case class Foo(one: String) extends Bar
case class Quux(two: Double, three: List[Bar]) extends Bar
case class Bippy(four: String)
class Baz(val x: Bar) extends AnyVal

object Main {

  def main(args: Array[String]): Unit = {
    println(implicitly[Extractor[Bar]].extract(Thing("42")))
  
  
  
  }

}

