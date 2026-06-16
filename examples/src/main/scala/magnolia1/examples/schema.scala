package magnolia1.examples.schema

import magnolia1._

// Slimmed down version of schema derivation from the Caliban library.

case class Derived[T](schema: T) extends AnyVal

object DerivedMagnolia {
  import magnolia1.Magnolia

  import scala.reflect.macros.whitebox

  def derivedMagnolia[TC[_], A](c: whitebox.Context)(implicit TC: c.WeakTypeTag[TC[_]], A: c.WeakTypeTag[A]): c.Expr[Derived[TC[A]]] = {
    val magnoliaTree = c.Expr[TC[A]](Magnolia.genNarrow[TC, A](c))
    c.universe.reify(Derived(magnoliaTree.splice))
  }
}

sealed trait SchemaDerivation {
  def join[R, T](ctx: ReadOnlyCaseClass[Schema[R, *], T]): Schema[R, T] = new Schema[R, T] {}
  def split[R, T](ctx: SealedTrait[Schema[R, *], T]): Schema[R, T] = new Schema[R, T] {}

  def genNarrow[R, T]: Schema[R, T] = macro Magnolia.genNarrow[Schema[R, *], T]
}

sealed trait Schema[-R, T]

object Schema extends SchemaDerivation {

  implicit val StringSchema: Schema[Any, String] = new Schema[Any, String] {}

  object auto extends SchemaDerivation {
    implicit def genMacro[R, T]: Derived[Schema[R, T]] =
      macro DerivedMagnolia.derivedMagnolia[Schema[R, *], T]

    def genAll[R0, T](implicit derived: Derived[Schema[R0, T]]): Schema[R0, T] = derived.schema
  }
}
