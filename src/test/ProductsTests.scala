package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*

class ProductsTests extends munit.FunSuite:
  import ProductsTests.*

  // TODO - whis will not work ?
  // test("serialize an object") {
  //   val res = summon[Show[String, JustObject.type]].show(JustObject)
  //   println(s"CO: $res")
  //   assertEquals(res, "JustObject()")
  // }

  // test("serialize a case object") {
  //   val res = summon[Show[String, JustCaseObject.type]].show(JustCaseObject)
  //   assertEquals(res, "JustCaseObject()")
  // }

  // test("serialize a class") {
  //   val res = summon[Show[String, JustClass]].show(JustClass())
  //   println(s"JUST CLASS : $res")
  // }

  test("serialize a case class") {
    val res = summon[Show[String, JustCaseClass]].show(
      (JustCaseClass(42, "Hello World", true))
    )
    assertEquals(res, "JustCaseClass(int=42,string=Hello World,boolean=true)")
  }

  test("construct a Show product instance") {
    val res = Show.derived[Person].show(Person("John Smith", 34))
    assertEquals(res, """Person(name=John Smith,age=34)""")
  }

  test("serialize a tuple") {
    val res = summon[Show[String, (Int, String)]].show((42, "Hello World"))
    assertEquals(res, "Tuple2[Int,String](_1=42,_2=Hello World)")
  }

  test("serialize case object within custom ADT") {
    val res = summon[Show[String, Red.type]].show(Red)
    assertEquals(res, "Red()")
  }

  test("construct a Show product instance with alternative apply functions") {
    val res = Show.derived[TestEntry].show(TestEntry("a", "b"))
    assertEquals(res, """TestEntry(param=Param(a=a,b=b))""")
  }

  test("decode a company") {
    val res = Decoder.derived[Company].decode("""Company(name=Acme Inc)""")
    assertEquals(res, Company("Acme Inc"))
  }

  test("test equality false") {
    val res = Eq.derived[Entity].equal(Person("John Smith", 34), Person("", 0))
    assert(!res)
  }

  test("test equality true") {
    val res = Eq
      .derived[Entity]
      .equal(Person("John Smith", 34), Person("John Smith", 34))
    assert(res)
  }

  test("decode a product nested in objects") {
    import Obj1.Obj2.*
    val res = summon[Decoder[NestedInObjects]].decode(
      """magnolia1.tests.Obj1.Obj2.NestedInObjects(i=42)"""
    )
    assertEquals(res, NestedInObjects(42))
  }

  test("decode a nested product") {
    val res = summon[Decoder[Address]].decode(
      """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
    )
    assertEquals(res, Address("53 High Street", Person("Richard Jones", 44)))
  }

  test("typenames and labels are not encoded") {
    val res = summon[Show[String, `%%`]].show(`%%`(1, "two"))
    assertEquals(res, "%%(/=1,#=two)")
  }

  test("very long") {
    val vl =
      VeryLong(
        "p1",
        "p2",
        "p3",
        "p4",
        "p5",
        "p6",
        "p7",
        "p8",
        "p9",
        "p10",
        "p11",
        "p12",
        "p13",
        "p14",
        "p15",
        "p16",
        "p17",
        "p18",
        "p19",
        "p20",
        "p21",
        "p22",
        "p23"
      )
    val res = Eq.derived[VeryLong].equal(vl, vl)
    assert(res)
  }

  test("show an Account") {
    val res = Show
      .derived[Account]
      .show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com"))
    assertEquals(
      res,
      "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])"
    )
  }

  test("construct a default Account") {
    val res = HasDefault.derived[Account].defaultValue
    assertEquals(res, Right(Account("")))
  }

  test("should print repeated") {
    val res =
      PrintRepeated.derived[Account].print(Account("id", "email1", "email2"))
    assertEquals(res, "List(emails)")
  }

  test("show underivable type with fallback") {
    val res = summon[TypeNameInfo[NotDerivable]].name
    assertEquals(res, TypeInfo("", "Unknown Type", Seq.empty))
  }

  test("show a Portfolio of Companies") {
    val res = Show
      .derived[Portfolio]
      .show(Portfolio(Company("Alice Inc"), Company("Bob & Co")))
    assertEquals(
      res,
      "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])"
    )
  }

  test("allow no-coproduct derivation definitions") {
    val error = compileErrors("WeakHash.derived[Person]")
    assert(error.isEmpty)
  }

  // TODO - no SemiDefault TC in scala3 branch
  // test("not assume full auto derivation of external products") {
  //   val error = compileErrors("""
  //     case class LoggingConfig(n: ServiceName2)
  //     object LoggingConfig {
  //       given semi: SemiDefault[LoggingConfig] = SemiDefault.derived
  //     }
  //   """)
  //   assert(error contains """
  //     |magnolia: could not find SemiDefault.Typeclass for type magnolia1.tests.ServiceName2
  //     |    in parameter 'n' of product type LoggingConfig
  //     |""".stripMargin)
  // }

  // TODO - not working as expected: showing T instead of Int
  // test("show a list of ints") {
  //   given [T: [X] =>> Show[String, X]]: Show[String, List[T]] = Show.derived
  //   val res = Show.derived[List[Int]].show(List(1, 2, 3))

  //   assertEquals(
  //     res,
  //     "::[Int](head=1,next$access$1=::[Int](head=2,next$access$1=::[Int](head=3,next$access$1=Nil())))"
  //   )
  // }

  test("case class typeName should be complete and unchanged") {
    given stringTypeName: TypeNameInfo[String] with {
      def name = ???

      def subtypeNames = ???
    }
    val res = TypeNameInfo.derived[Fruit].name
    assertEquals(res.full, "magnolia1.tests.Fruit")
  }

  // TODO - not sure what's the point of that test
  test("show chained error stack when leaf instance is missing") {
    val error = compileErrors("Show.derived[Schedule]")
    assert(
      error contains "No given instance of type magnolia1.examples.Show[String, Seq[magnolia1.tests.Event]] was found."
    )
    // assert(error contains """
    //   |magnolia: could not find Show.Typeclass for type java.time.LocalDate
    //   |    in parameter 'date' of product type magnolia1.tests.Event
    //   |    in chained implicit Show.Typeclass for type Seq[magnolia1.tests.Event]
    //   |    in parameter 'events' of product type magnolia1.tests.Schedule
    //   |""".stripMargin)
  }

  // TODO - not sure what's the point of that test
  test("show chained error stack") {
    val error = compileErrors("Show.derived[(Int, Seq[(Double, String)])]")
    assert(
      error contains "No given instance of type magnolia1.examples.Show[String, Seq[(Double, String)]] was found."
    )
  }

object ProductsTests:

  class NotDerivable

  class JustClass

  object JustObject

  case object JustCaseObject

  case class JustCaseClass(int: Int, string: String, boolean: Boolean)

  case class TestEntry(param: Param)
  object TestEntry:
    def apply(): TestEntry = TestEntry(Param("", ""))
    def apply(a: String)(using b: Int): TestEntry = TestEntry(
      Param(a, b.toString)
    )
    def apply(a: String, b: String): TestEntry = TestEntry(Param(a, b))

  sealed trait Entity
  case class Company(name: String) extends Entity
  case class Person(name: String, age: Int) extends Entity
  case class Address(line1: String, occupant: Person)

  case class Portfolio(companies: Company*)

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  case object Orange extends Color
  case object Pink extends Color

  object Obj1:
    object Obj2:
      case class NestedInObjects(i: Int)

  case class `%%`(`/`: Int, `#`: String)

  case class VeryLong(
      p1: String,
      p2: String,
      p3: String,
      p4: String,
      p5: String,
      p6: String,
      p7: String,
      p8: String,
      p9: String,
      p10: String,
      p11: String,
      p12: String,
      p13: String,
      p14: String,
      p15: String,
      p16: String,
      p17: String,
      p18: String,
      p19: String,
      p20: String,
      p21: String,
      p22: String,
      p23: String
  )

  case class Account(id: String, emails: String*)

  @SerialVersionUID(42) case class Schedule(events: Seq[Event])
