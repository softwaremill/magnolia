package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait}

import scala.language.experimental.macros

trait Csv[A] {
  def apply(a: A): List[String]
}

object Csv {
  type Typeclass[A] = Csv[A]

  def combine[A](ctx: CaseClass[Csv, A]): Csv[A] = new Csv[A] {
    def apply(a: A): List[String] =
      ctx.parameters.foldLeft(List[String]()) { (acc, p) =>
        acc ++ p.typeclass(p.dereference(a))
      }
  }

  def split[A](ctx: SealedTrait[Csv, A]): Csv[A] = new Csv[A] {
    def apply(a: A): List[String] = ctx.split(a)(sub => sub.typeclass(sub.cast(a)))
  }

  implicit def deriveCsv[A]: Csv[A] = macro Magnolia.gen[A]

  implicit val csvStr: Csv[String] = new Csv[String] {
    def apply(a: String): List[String] = List(a)
  }
}
