package magnolia1.examples

import magnolia1._

trait SubtypeInfo[T] {
  def subtypeIsObject: Seq[Boolean]
  def traitAnnotations: Seq[Any]
  def subtypeAnnotations: Seq[Seq[Any]]
}

object SubtypeInfo extends Derivation[SubtypeInfo]:
  def join[T](ctx: CaseClass[SubtypeInfo, T]): SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil
      def traitAnnotations: List[Any] = Nil
      def subtypeAnnotations: List[List[Any]] = Nil

  override def split[T](ctx: SealedTrait[SubtypeInfo, T]): SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = ctx.subtypes.map(_.isObject)
      def traitAnnotations: Seq[Any] = ctx.annotations
      def subtypeAnnotations: Seq[Seq[Any]] = ctx.subtypes.map(_.annotations.toList).toList

  given fallback[T]: SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil
      def traitAnnotations: Seq[Any] = Nil
      def subtypeAnnotations: Seq[Seq[Any]] = Nil