package magnolia.examples

import magnolia._

class ExportedTypeclass[T]()

object ExportedTypeclass extends Derivation[ExportedTypeclass]:
  case class Exported[T]() extends ExportedTypeclass[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Exported[T] = Exported()
  override def split[T](ctx: SealedTrait[Typeclass, T]): Exported[T] = Exported()

  given Typeclass[Int] = new ExportedTypeclass()
  given Typeclass[String] = new ExportedTypeclass()
  given seqInstance[T: Typeclass]: Typeclass[Seq[T]] = new ExportedTypeclass()