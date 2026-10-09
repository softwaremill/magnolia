package magnolia1.examples

import magnolia1._

case class Passthrough[T](
    ctx: Option[Either[CaseClass[?, T], SealedTrait[?, T]]]
)
object Passthrough extends Derivation[Passthrough]:
  def join[T](ctx: CaseClass[Passthrough, T]) = Passthrough(Some(Left(ctx)))
  override def split[T](ctx: SealedTrait[Passthrough, T]) = Passthrough(
    Some(
      Right(ctx)
    )
  )

  given [T]: Passthrough[T] = Passthrough(None)
