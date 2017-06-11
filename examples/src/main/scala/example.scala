package magnolia

import language.experimental.macros
import language.higherKinds



case class Thing(str: String) {
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

  implicit val dereferencer: Dereferencer[Extractor] { type Value = Thing } = new Dereferencer[Extractor] {
    type Value = Thing
    def dereference(value: Thing, param: String): Thing = value.access(param)
    def delegate[T](extractor: Extractor[T], value: Thing): T = extractor.extract(value)
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
