package magnolia1.tests

import magnolia1.*
import magnolia1.examples.*
import java.time.LocalDate

class ProductsTests extends munit.FunSuite:
  import ProductsTests.*

  test("serialize a case object") {
    val res = summon[Show[String, JustCaseObject.type]].show(JustCaseObject)
    assertEquals(res, "JustCaseObject()")
  }

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
      // format: off
      VeryLong(
        "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10",
        "p11", "p12", "p13", "p14", "p15", "p16", "p17", "p18", "p19", "p20",
        "p21", "p22", "p23", "p24", "p25", "p26", "p27", "p28", "p29", "p30",
        "p31", "p32", "p33", "p34", "p35", "p36", "p37", "p38", "p39", "p40",
        "p41", "p42", "p43", "p44", "p45", "p46", "p47", "p48", "p49", "p50",
        "p51", "p52", "p53", "p54", "p55", "p56", "p57", "p58", "p59", "p60",
        "p61", "p62", "p63", "p64", "p65", "p66", "p67", "p68", "p69", "p70",
        "p71", "p72", "p73", "p74", "p75", "p76", "p77", "p78", "p79", "p80",
        "p81", "p82", "p83", "p84", "p85", "p86", "p87", "p88", "p89", "p90",
        "p91", "p92", "p93", "p94", "p95", "p96", "p97", "p98", "p99", "p100",
        "p101", "p102", "p103", "p104", "p105", "p106", "p107", "p108", "p109", "p110",
        "p111", "p112", "p113", "p114", "p115", "p116", "p117", "p118", "p119", "p120",
        "p121", "p122", "p123", "p124", "p125", "p126", "p127", "p128", "p129", "p130",
        "p131", "p132", "p133", "p134", "p135", "p136", "p137", "p138", "p139", "p140",
        "p141", "p142", "p143", "p144", "p145", "p146", "p147", "p148", "p149", "p150",
        "p151", "p152", "p153", "p154", "p155", "p156", "p157", "p158", "p159", "p160",
        "p161", "p162", "p163", "p164", "p165", "p166", "p167", "p168", "p169", "p170",
        "p171", "p172", "p173", "p174", "p175", "p176", "p177", "p178", "p179", "p180",
        "p181", "p182", "p183", "p184", "p185", "p186", "p187", "p188", "p189", "p190",
        "p191", "p192", "p193", "p194", "p195", "p196", "p197", "p198", "p199", "p200",
        "p201", "p202", "p203", "p204", "p205", "p206", "p207", "p208", "p209", "p210",
        "p211", "p212", "p213", "p214", "p215", "p216", "p217", "p218", "p219", "p220",
        "p221", "p222", "p223", "p224", "p225", "p226", "p227", "p228", "p229", "p230",
        "p231", "p232", "p233", "p234", "p235", "p236", "p237", "p238", "p239", "p240",
        "p241", "p242", "p243", "p244", "p245", "p246", "p247", "p248", "p249", "p250",
        "p251", "p252", "p253", "p254"
      )
      // format: on

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

  test("assume full auto derivation of external products") {
    case class Input(value: String)
    case class LoggingConfig(input: Input)
    object LoggingConfig:
      given SemiDefault[LoggingConfig] = SemiDefault.derived

    val res = summon[SemiDefault[LoggingConfig]].default
    assertEquals(res, LoggingConfig(Input("")))

  }

  // TODO - not working as expected: showing "T" type instead of Int
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
    assertEquals(res.full, "magnolia1.tests.ProductsTests.Fruit")
  }

  test("case class parameter typeName should be dealiased") {
    given stringTypeName: TypeNameInfo[String] with {
      def name = ???

      def subtypeNames = ???
    }
    val res1 = TypeNameInfo.derived[Parameterized[Domain1.Type]].name
    val res2 = TypeNameInfo.derived[Parameterized[Domain2.Type]].name
    assertEquals(res1.typeParams.head.short, "Int")
    assertEquals(res2.typeParams.head.short, "String")
  }

  test("show chained error stack when leaf instance is missing") {
    val error = compileErrors("Show.derived[Schedule]")
    assert(
      clue(error) contains "No given instance of type magnolia1.examples.Show[String"
    )
  }

  test("show chained error stack") {
    val error = compileErrors("Show.derived[(Int, Seq[(Double, String)])]")
    assert(
      clue(error) contains "No given instance of type magnolia1.examples.Show[String, Seq[(Double, String)]] was found."
    )
  }

object ProductsTests:

  class NotDerivable

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

  // format: off
  case class VeryLong(
    p1: String, p2: String, p3: String, p4: String, p5: String, p6: String, p7: String, p8: String, p9: String, p10: String,
    p11: String, p12: String, p13: String, p14: String, p15: String, p16: String, p17: String, p18: String, p19: String, p20: String,
    p21: String, p22: String, p23: String, p24: String, p25: String, p26: String, p27: String, p28: String, p29: String, p30: String,
    p31: String, p32: String, p33: String, p34: String, p35: String, p36: String, p37: String, p38: String, p39: String, p40: String,
    p41: String, p42: String, p43: String, p44: String, p45: String, p46: String, p47: String, p48: String, p49: String, p50: String,
    p51: String, p52: String, p53: String, p54: String, p55: String, p56: String, p57: String, p58: String, p59: String, p60: String,
    p61: String, p62: String, p63: String, p64: String, p65: String, p66: String, p67: String, p68: String, p69: String, p70: String,
    p71: String, p72: String, p73: String, p74: String, p75: String, p76: String, p77: String, p78: String, p79: String, p80: String,
    p81: String, p82: String, p83: String, p84: String, p85: String, p86: String, p87: String, p88: String, p89: String, p90: String,
    p91: String, p92: String, p93: String, p94: String, p95: String, p96: String, p97: String, p98: String, p99: String, p100: String,
    p101: String, p102: String, p103: String, p104: String, p105: String, p106: String, p107: String, p108: String, p109: String, p110: String,
    p111: String, p112: String, p113: String, p114: String, p115: String, p116: String, p117: String, p118: String, p119: String, p120: String,
    p121: String, p122: String, p123: String, p124: String, p125: String, p126: String, p127: String, p128: String, p129: String, p130: String,
    p131: String, p132: String, p133: String, p134: String, p135: String, p136: String, p137: String, p138: String, p139: String, p140: String,
    p141: String, p142: String, p143: String, p144: String, p145: String, p146: String, p147: String, p148: String, p149: String, p150: String,
    p151: String, p152: String, p153: String, p154: String, p155: String, p156: String, p157: String, p158: String, p159: String, p160: String,
    p161: String, p162: String, p163: String, p164: String, p165: String, p166: String, p167: String, p168: String, p169: String, p170: String,
    p171: String, p172: String, p173: String, p174: String, p175: String, p176: String, p177: String, p178: String, p179: String, p180: String,
    p181: String, p182: String, p183: String, p184: String, p185: String, p186: String, p187: String, p188: String, p189: String, p190: String,
    p191: String, p192: String, p193: String, p194: String, p195: String, p196: String, p197: String, p198: String, p199: String, p200: String,
    p201: String, p202: String, p203: String, p204: String, p205: String, p206: String, p207: String, p208: String, p209: String, p210: String,
    p211: String, p212: String, p213: String, p214: String, p215: String, p216: String, p217: String, p218: String, p219: String, p220: String,
    p221: String, p222: String, p223: String, p224: String, p225: String, p226: String, p227: String, p228: String, p229: String, p230: String,
    p231: String, p232: String, p233: String, p234: String, p235: String, p236: String, p237: String, p238: String, p239: String, p240: String,
    p241: String, p242: String, p243: String, p244: String, p245: String, p246: String, p247: String, p248: String, p249: String, p250: String,
    p251: String, p252: String, p253: String, p254: String
  )
  // format: on

  case class Account(id: String, emails: String*)

  @SerialVersionUID(42) case class Schedule(events: Seq[Event])

  case class Event(date: LocalDate)

  case class Param(a: String, b: String)

  case class Fruit(name: String)

  case class Parameterized[T](t: T)

  object Domain1:
    type Type = Int

  object Domain2:
    type Type = String

  object Fruit:
    given showFruit: Show[String, Fruit] = (f: Fruit) => f.name
