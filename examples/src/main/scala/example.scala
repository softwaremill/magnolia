package magnolia

import language.experimental.macros

trait Extractor[T] {
  def extract(src: String): T
}

object Extractor extends Extractor_1 {
  
  def apply[T](fn: String => T): Extractor[T] = new Extractor[T] {
    def extract(source: String): T = fn(source)
  }

  implicit val intExtractor: Extractor[Int] = Extractor(_.toInt)
  implicit val stringExtractor: Extractor[String] = Extractor(identity)
  implicit val doubleExtractor: Extractor[Double] = Extractor(_.toDouble)

}

trait Extractor_1 extends Extractor_2 {
  implicit def listExtractor[T: Extractor]: Extractor[List[T]] = new Extractor[List[T]] {
    def extract(source: String): List[T] = List(implicitly[Extractor[T]].extract(source))
  }
}
trait Extractor_2 {
  implicit def generic[T]: Extractor[T] = macro Macros.generic[T, Extractor[_]]
}
