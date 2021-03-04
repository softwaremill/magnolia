package magnolia

import scala.quoted.*
import scala.compiletime.erasedValue

object Annotations {
  inline def apply[T]: List[Any] = ${ annotationsImpl[T] }

  def annotationsImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[Any]] = {
    import qctx.reflect.*

    val tpe = TypeRepr.of[T]
    
    Expr.ofList(
      tpe
        .typeSymbol
        .annotations
        .filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        }
        .map(_.asExpr.asInstanceOf[Expr[Any]])
    )
  }
}