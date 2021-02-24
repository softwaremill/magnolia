package magnolia

import scala.quoted.*

object IsObject {
  inline def apply[T]: Boolean = ${ isObjectImpl[T] }

  def isObjectImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
  }
}
