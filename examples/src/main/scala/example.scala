package magnolia.examples

import magnolia._

import language.experimental.macros
import language.higherKinds



/*case class Thing(str: String) {
  def access(path: String): Thing = Thing(s"$str.$path")
}

trait Extractor[T] { ext =>
  
  def extract(src: Thing): T
  
  def orElse[TS >: T, T2 <: TS](ext2: Extractor[T2]): Extractor[TS] = new Extractor[TS] {
    def extract(src: Thing): TS = try ext.extract(src) catch {
      case e: Exception => ext2.extract(src)
    }
  }
}

object Extractor extends Extractor_1 {
  
  def apply[T](fn: Thing => T): Extractor[T] = new Extractor[T] {
    def extract(source: Thing): T = fn(source)
  }

  implicit val intExtractor: Extractor[Int] = Extractor(_.str.length)
  implicit val stringExtractor: Extractor[String] = Extractor(_.str)
  implicit val doubleExtractor: Extractor[Double] = Extractor(_.str.length.toDouble)

  implicit val derivation: CovariantDerivation[Extractor] { type Value = Thing } = new CovariantDerivation[Extractor] {
    type Value = Thing
    def dereference(value: Thing, param: String): Thing = value.access(param)
    def call[T](extractor: Extractor[T], value: Thing): T = extractor.extract(value)
    def combine[Supertype, Right <: Supertype](left: Extractor[_ <: Supertype],
        right: Extractor[Right]): Extractor[Supertype] = left.orElse(right)
    
    def construct[T](body: Thing => T): Extractor[T] = new Extractor[T] {
      def extract(source: Thing): T = body(source)
    }
  }
}

trait Extractor_1 extends Extractor_2 {
  implicit def listExtractor[T: Extractor]: Extractor[List[T]] = new Extractor[List[T]] {
    def extract(source: Thing): List[T] = List(implicitly[Extractor[T]].extract(source))
  }
}

trait Extractor_2 {
  implicit def generic[T]: Extractor[T] = macro Macros.magnolia[T, Extractor[_]]
}

trait Serializer[T] {
  def serialize(src: T): String
}

object Serializer extends Serializer_1 {
  implicit val deriv: ContravariantDerivation[Serializer] { type Return = String } = new ContravariantDerivation[Serializer] {
    type Return = String
    def call[T](typeclass: Serializer[T], value: T): String = typeclass.serialize(value)
    def construct[T](body: T => String): Serializer[T] = new Serializer[T] {
      def serialize(value: T): String = body(value)
    }
    def join(xs: List[String]): String = xs.mkString(", ")
  }
}

trait Serializer_1 extends Serializer_2 {
  implicit val intSerializer: Serializer[Int] =  { t => "int" }
  implicit val strSerializer: Serializer[String] = { t => "string" }
  implicit val doubleSerializer: Serializer[Double] = { t => "double" }
  implicit def listSerializer[T: Serializer]: Serializer[List[T]] = { ts =>
    println(ts)
    s"List[${ts.map { t => implicitly[Serializer[T]].serialize(t) }.mkString("-")}]"
  }
}

trait Serializer_2 {
  implicit def generic[T]: Serializer[T] = macro Macros.magnolia[T, Serializer[_]]
}
*/

object `package` {
  implicit class Showable[T: Show](t: T) {
    def show: String = implicitly[Show[T]].show(t)
  }
}

sealed trait Tree
case class Branch(left: Tree, right: Tree) extends Tree
case class Leaf(value: Int) extends Tree

trait Show[T] { def show(t: T): String }
object Show extends Show_1 {
  implicit val showInt: Show[Int] = _.toString
  implicit val derivation = new ContravariantDerivation[Show] {
    type Return = String
    def call[T](show: Show[T], value: T): String = show.show(value)
    def construct[T](body: T => String): Show[T] = body(_)
    def join(xs: List[String]): String = xs.mkString("(", ", ", ")")
  }
}

trait Show_1 {
  implicit def generic[T]: Show[T] = macro Macros.magnolia[T, Show[_]]
}
