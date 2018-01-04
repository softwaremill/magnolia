package magnolia.examples

import language.experimental.macros

import magnolia._

trait TypeNameInfo[T] { def name: TypeName }

object TypeNameInfo {
  type Typeclass[T] = TypeNameInfo[T]
  def combine[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] { def name: TypeName = ctx.typeName }

  def dispatch[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] { def name: TypeName = ctx.typeName }

  implicit def gen[T]: TypeNameInfo[T] = macro Magnolia.gen[T]
}

