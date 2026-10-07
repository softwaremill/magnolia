package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait}

class ExportedTypeclassAuto[T]()

object ExportedTypeclassAuto {
  type Typeclass[T] = ExportedTypeclassAuto[T]
  case class Exported[T]() extends ExportedTypeclassAuto[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Exported[T] = Exported()
  def split[T](ctx: SealedTrait[Typeclass, T]): Exported[T] = Exported()

  implicit val intInstance: Typeclass[Int] = new ExportedTypeclassAuto()
  implicit val stringInstance: Typeclass[String] = new ExportedTypeclassAuto()
  implicit def seqInstance[T: Typeclass]: Typeclass[Seq[T]] = new ExportedTypeclassAuto()

  implicit def gen[T]: Exported[T] = macro Magnolia.gen[T]
}
