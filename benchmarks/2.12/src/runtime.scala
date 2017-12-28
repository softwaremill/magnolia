import adt._
import cats._
import derived._
import cats.Show
import cats.instances.all._
import estrapade._
import magnolia._

object Gen extends TestApp {

  val latin = Latin(
    Letter("one", "two"),
    Letter("three", "four"),
    Letter("five", "six"),
    Letter("seven", "eight"),
    Letter("nine", "ten"),
    Letter("eleven", "twelve"),
    Letter("one", "two"),
    Letter("three", "four"),
    Letter("five", "six"),
    Letter("seven", "eight"),
    Letter("nine", "ten"),
    Letter("eleven", "twelve"),
    Letter("one", "two"),
    Letter("three", "four"),
    Letter("five", "six"),
    Letter("seven", "eight"),
    Letter("nine", "ten"),
    Letter("eleven", "twelve"),
    Letter("one", "two"),
    Letter("three", "four"),
    Letter("five", "six"),
    Letter("seven", "eight")
  )

  def tests() = {
    println("Warming up JVM")

    var n = 0
    var s: Any = null
    while (n < 5000000) {
      s = derive.show[Latin] //.show(latin)
      s = magnolia.examples.Show.generic[Latin] //.show(latin)
      n += 1
    }

    println("Warm")
    test("Kittens") {
      var n = 0
      var s: Any = null
      while (n < 1000000) {
        s = derive.show[Latin] //.show(latin)
        n += 1
      }
    }.returns()

    test("Magnolia") {
      var n = 0
      var s: Any = null
      while (n < 1000000) {
        s = magnolia.examples.Show.generic[Latin] //.show(latin)
        n += 1
      }
    }.returns()
  }
}
