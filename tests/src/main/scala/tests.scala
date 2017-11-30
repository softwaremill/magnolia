package magnolia.tests

import magnolia._
import estrapade._
import contextual.data.scalac._
import contextual.data.fqt._
import contextual.data.txt._

sealed trait Tree[+T]
case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Entity

case class Company(name: String) extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)

class Length(val value: Int) extends AnyVal

case class Lunchbox(fruit: Fruit, drink: String)
object Fruit {
  import examples._
  implicit val showFruit: Show[String, Fruit] =
    new Show[String, Fruit] { def show(f: Fruit): String = f.name }
}
case class Fruit(name: String)

case class Item(name: String, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color

case class `%%`(`/`: Int, `#`: String)

case class Param(a: String, b: String)
case class Test(param: Param)
object Test {
  def apply(): Test = Test(Param("", ""))

  def apply(a: String)(implicit b: Int): Test = Test(Param(a, b.toString))

  def apply(a: String, b: String): Test = Test(Param(a, b))
}

case class Account(id: String, emails: String*)

case class Portfolio(companies: Company*)

object Tests extends TestApp {

  def tests() = for (i <- 1 to 1000) {
    import examples._

    test("construct a Show product instance with alternative apply functions") {
      import examples._
      Show.gen[Test].show(Test("a", "b"))
    }.assert(_ == """Test(param=Param(a=a,b=b))""")

    test("construct a Show product instance") {
      import examples._
      Show.gen[Person].show(Person("John Smith", 34))
    }.assert(_ == """Person(name=John Smith,age=34)""")

    test("construct a Show coproduct instance") {
      import examples._
      Show.gen[Person].show(Person("John Smith", 34))
    }.assert(_ == "Person(name=John Smith,age=34)")

    test("serialize a Branch") {
      import magnolia.examples._
      implicitly[Show[String, Branch[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch(left=Leaf(value=LHS),right=Leaf(value=RHS))")

    test("local implicit beats Magnolia") {
      import magnolia.examples._
      implicit val showPerson: Show[String, Person] = new Show[String, Person] {
        def show(p: Person) = "nobody"
      }
      implicitly[Show[String, Address]].show(Address("Home", Person("John Smith", 44)))
    }.assert(_ == "Address(line1=Home,occupant=nobody)")

    test("even low-priority implicit beats Magnolia for nested case") {
      import magnolia.examples._
      import Show.gen
      implicitly[Show[String, Lunchbox]].show(Lunchbox(Fruit("apple"), "lemonade"))
    }.assert(_ == "Lunchbox(fruit=apple,drink=lemonade)")

    test("low-priority implicit does not beat Magnolia when not nested") {
      import magnolia.examples._
      import Show.gen
      implicitly[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "Fruit(name=apple)")

    test("typeclass implicit scope has lower priority than ADT implicit scope") {
      import magnolia.examples._
      implicitly[Show[String, Fruit]].show(Fruit("apple"))
    }.assert(_ == "apple")

    test("test equality false") {
      import examples._
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("", 0))
    }.assert(_ == false)

    test("test equality true") {
      import examples._
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
    }.assert(_ == true)

    test("test branch equality true") {
      import examples._
      Eq.gen[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
    }.assert(_ == true)

    test("construct a default value") {
      Default.gen[Entity].default
    }.assert(_ == Company(""))

    test("construction of Show instance for Leaf") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[String, Leaf[java.lang.String]]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Leaf[String]]"))

    test("construction of Show instance for Tree") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[String, Tree[String]]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Tree[String]]"))

    test("serialize a Leaf") {
      implicitly[Show[String, Leaf[String]]].show(Leaf("testing"))
    }.assert(_ == "Leaf(value=testing)")

    test("serialize a Branch as a Tree") {
      implicitly[Show[String, Tree[String]]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch(left=Leaf(value=LHS),right=Leaf(value=RHS))")

    test("serialize case object") {
      implicitly[Show[String, Red.type]].show(Red)
    }.assert(_ == "Red()")

    test("access default constructor values") {
      implicitly[Default[Item]].default
    }.assert(_ == Item("", 1, 0))

    test("serialize case object as a sealed trait") {
      implicitly[Show[String, Color]].show(Blue)
    }.assert(_ == "Blue()")

    test("decode a company") {
      Decoder.gen[Company].decode("""Company(name=Acme Inc)""")
    }.assert(_ == Company("Acme Inc"))

    test("decode a Person as an Entity") {
      implicitly[Decoder[Entity]].decode("""magnolia.tests.Person(name=John Smith,age=32)""")
    }.assert(_ == Person("John Smith", 32))

    test("decode a nested product") {
      implicitly[Decoder[Address]].decode(
        """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
      )
    }.assert(_ == Address("53 High Street", Person("Richard Jones", 44)))

    test("show error stack") {
      scalac"""
        import magnolia.examples._
        case class Alpha(integer: Double)
        case class Beta(alpha: Alpha)
        Show.gen[Beta]
      """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find typeclass for type Double
                                      |    in parameter 'integer' of product type Alpha
                                      |    in parameter 'alpha' of product type Beta
                                      |"""))

    test("not attempt to instantiate Unit when producing error stack") {
      scalac"""
        import magnolia.examples._
        case class Gamma(unit: Unit)
        Show.gen[Gamma]
      """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find typeclass for type Unit
                                      |    in parameter 'unit' of product type Gamma
                                      |"""))

    test("typenames and labels are not encoded") {
      implicitly[Show[String, `%%`]].show(`%%`(1, "two"))
    }.assert(_ == "%%(/=1,#=two)")

    val tupleDerivation = test("derive for a tuple") {
      implicitly[Show[String, (Int, String)]]
    }.returns()

    test("serialize a tuple") {
      tupleDerivation().show((42, "Hello World"))
    }.assert(_ == "Tuple2(_1=42,_2=Hello World)")

    test("serialize a value class") {
      Show.gen[Length].show(new Length(100))
    }.assert(_ == "100")

    test("show an Account") {
      Show.gen[Account].show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
    }.assert(_ == "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")

    test("construct a default Account") {
      Default.gen[Account].default
    }.assert(_ == Account(""))

    test("show a Portfolio of Companies") {
      Show.gen[Portfolio].show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
    }.assert(_ == "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")
  }
}
