package magnolia1.examples

import magnolia1._

trait SubtypeInfo[T] {
  def subtypeIsObject: Seq[Boolean]
  def traitAnnotations: Seq[Any]
  def traitInheritedAnnotations: Seq[Any]
  def traitTypeAnnotations: Seq[Any]
  def subtypeAnnotations: Seq[Seq[Any]]
  def subtypeInheritedAnnotations: Seq[Seq[Any]]
  def subtypeTypeAnnotations: Seq[Seq[Any]]
  def isEnum: Boolean
}

object SubtypeInfo extends Derivation[SubtypeInfo]:
  def join[T](ctx: CaseClass[SubtypeInfo, T]): SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil
      def traitAnnotations: List[Any] = Nil
      def traitInheritedAnnotations: List[Any] = Nil
      def traitTypeAnnotations: List[Any] = Nil
      def subtypeAnnotations: List[List[Any]] = Nil
      def subtypeInheritedAnnotations: List[List[Any]] = Nil
      def subtypeTypeAnnotations: List[List[Any]] = Nil
      def isEnum: Boolean = false

  override def split[T](ctx: SealedTrait[SubtypeInfo, T]): SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = ctx.subtypes.map(_.isObject)
      def traitAnnotations: Seq[Any] = ctx.annotations
      def traitInheritedAnnotations: Seq[Any] = ctx.inheritedAnnotations
      def traitTypeAnnotations: Seq[Any] = ctx.typeAnnotations
      def subtypeAnnotations: Seq[Seq[Any]] =
        ctx.subtypes.map(_.annotations.toList).toList
      def subtypeInheritedAnnotations: Seq[Seq[Any]] =
        ctx.subtypes.map(_.inheritedAnnotations.toList).toList
      def subtypeTypeAnnotations: Seq[Seq[Any]] =
        ctx.subtypes.map(_.typeAnnotations.toList).toList
      def isEnum: Boolean = ctx.isEnum

  given fallback[T]: SubtypeInfo[T] =
    new SubtypeInfo[T]:
      def subtypeIsObject: Seq[Boolean] = Nil
      def traitAnnotations: Seq[Any] = Nil
      def traitInheritedAnnotations: Seq[Any] = Nil
      def traitTypeAnnotations: Seq[Any] = Nil
      def subtypeAnnotations: Seq[Seq[Any]] = Nil
      def subtypeInheritedAnnotations: Seq[Seq[Any]] = Nil
      def subtypeTypeAnnotations: Seq[Seq[Any]] = Nil
      def isEnum: Boolean = false
