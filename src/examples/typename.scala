package magnolia.examples

import magnolia._

trait TypeNameInfo[T] {
  def name: TypeInfo

  def subtypeNames: Seq[TypeInfo]
}

object TypeNameInfo extends Derivation[TypeNameInfo]:
  def join[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = Nil

  override def split[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = ctx.subtypes.map(_.typeInfo)

  given fallback[T]: TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = TypeInfo("", "Unknown Type", Seq.empty)
      def subtypeNames: Seq[TypeInfo] = Nil