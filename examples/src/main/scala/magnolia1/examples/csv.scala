package magnolia1.examples

import magnolia1._

import scala.language.experimental.macros

trait Csv[A] {
  def apply(a: A): List[String]
}

object CsvConfig extends Config {
  type Proxy = Csv.type
  type Ignore = transient
  final val readOnly = true
  final val minFields = -1
  final val maxFields = -1
  final val minCases = -1
  final val maxCases = -1
}

object Csv {
  type Typeclass[A] = Csv[A]

  def join[A](ctx: CaseClass[Csv, A]): Csv[A] = new Csv[A] {
    def apply(a: A): List[String] =
      ctx.parameters.foldLeft(List[String]()) { (acc, p) =>
        acc ++ p.typeclass(p.dereference(a))
      }
  }

  def split[A](ctx: SealedTrait[Csv, A]): Csv[A] = new Csv[A] {
    def apply(a: A): List[String] = ctx.split(a)(sub => sub.typeclass(sub.cast(a)))
  }

  @transient
  val ignoreMe: String = "ignored value"

  implicit def deriveCsv[A]: Csv[A] = macro Magnolia.genWith[A, CsvConfig.type]

  implicit val csvStr: Csv[String] = new Csv[String] {
    def apply(a: String): List[String] = List(a)
  }

}
