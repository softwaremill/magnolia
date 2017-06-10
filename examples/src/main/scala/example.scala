package magnolia

import language.experimental.macros

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

}

trait Extractor_1 extends Extractor_2 {
  implicit def listExtractor[T: Extractor]: Extractor[List[T]] = new Extractor[List[T]] {
    def extract(source: Thing): List[T] = List(implicitly[Extractor[T]].extract(source))
  }
}
trait Extractor_2 {
  implicit def generic[T]: Extractor[T] = macro Macros.magnolia[T, Extractor[_]]
}
