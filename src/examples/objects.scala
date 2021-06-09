package magnolia.examples

import magnolia._

trait ObjectInfo[T] {
  def subtypeIsObject: Seq[Boolean]
}

object ObjectInfo extends Derivation[ObjectInfo]:
  def join[T](ctx: CaseClass[ObjectInfo, T]): ObjectInfo[T] =
    new ObjectInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil

  override def split[T](ctx: SealedTrait[ObjectInfo, T]): ObjectInfo[T] =
    new ObjectInfo[T]:
      def subtypeIsObject: Seq[Boolean] = ctx.subtypes.map(_.isObject)

  given fallback[T]: ObjectInfo[T] =
    new ObjectInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil