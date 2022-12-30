package magnolia2.examples

import magnolia2._

case class Passthrough[T](
    ctx: Option[Either[CaseClass[_, T], SealedTrait[_, T]]]
)
object Passthrough extends Derivation[Passthrough]:
  def join[T](ctx: CaseClass[Passthrough, T]) = Passthrough(Some(Left(ctx)))
  override def split[T](ctx: SealedTrait[Passthrough, T]) = Passthrough(
    Some(
      Right(ctx)
    )
  )

  given [T]: Passthrough[T] = Passthrough(None)
