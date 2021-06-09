package magnolia.examples

import magnolia._

import scala.language.experimental.macros

trait TypeNameInfo[T] {
  def name: TypeName

  def subtypeNames: Seq[TypeName]
}

object TypeNameInfo {
  type Typeclass[T] = TypeNameInfo[T]
  def combine[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = ctx.typeName

      def subtypeNames: Seq[TypeName] = Nil
    }

  def dispatch[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = ctx.typeName

      def subtypeNames: Seq[TypeName] = ctx.subtypes.map(_.typeName)
    }

  def fallback[T]: TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = TypeName("", "Unknown Type", Seq.empty)

      def subtypeNames: Seq[TypeName] = Nil
    }

  implicit def gen[T]: TypeNameInfo[T] = macro Magnolia.gen[T]
}
