package magnolia

sealed trait Bar

case class Foo(one: String) extends Bar
case class Quux(two: String, three: Double, four: List[Bar]) extends Bar
case class Bippy(five: String, six: List[Quux]) extends Bar
class Baz(val x: Bar) extends AnyVal

object Main {


  def main(args: Array[String]): Unit = {
    println(implicitly[Extractor[Bar]].extract(Thing("42")))
  }

}

