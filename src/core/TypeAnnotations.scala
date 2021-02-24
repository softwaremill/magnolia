package magnolia

import scala.quoted._

object TypeAnnotations {
  inline def apply[T]: List[Any] = ${ typeAnnotationsImpl[T] }

  def typeAnnotationsImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[Any]] = {
    import qctx.reflect._

    Expr.ofList(
      TypeRepr
        .of[T]
        .typeSymbol
        .annotations
        .filter(a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        ).map(_.asExpr.asInstanceOf[Expr[Any]])
    )
  }
}
