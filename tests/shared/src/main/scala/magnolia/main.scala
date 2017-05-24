package magnolia

sealed trait Bar
case class Foo(one: Int) extends Bar
case class Quux(two: Int, bar: Bar) extends Bar
case class Bippy(four: Int, bar: Bar)
case class Baz(x: Bar) extends AnyVal

case class X(y: Y)
case class Y(x: X)

object Main {
  def main(args: Array[String]): Unit = {
    println(implicitly[Extractor[Bar]].extract("hello world"))

  }
}

