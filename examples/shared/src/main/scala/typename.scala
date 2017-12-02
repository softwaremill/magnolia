package magnolia.examples

import language.experimental.macros

import magnolia._

trait TypeName[T] { def name: String }

object TypeName {
  type Typeclass[T] = TypeName[T]
  def combine[T](ctx: CaseClass[TypeName, T, _]): TypeName[T] =
    new TypeName[T] { def name: String = ctx.typeName }

  def dispatch[T](ctx: SealedTrait[TypeName, T]): TypeName[T] =
    new TypeName[T] { def name: String = ctx.typeName }

  implicit def gen[T]: TypeName[T] = macro Magnolia.gen[T]
}

