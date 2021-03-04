package magnolia

import scala.quoted._

//TODO dotty 3.0.0-RC1 regression?
object ParamAnnotations {
  inline def apply[T]: List[(String, List[Any])] = ${ paramAnnotationsImpl[T] }

  def paramAnnotationsImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, List[Any])]] = {
    import qctx.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList(
      tpe
      .typeSymbol
      .caseFields
      .map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        }
        .map(_.asExpr.asInstanceOf[Expr[Any]])
      }
      .filter(_._2.nonEmpty)
      .map{ case (name, annots) => Expr.ofTuple(name, Expr.ofList(annots)) }
    )
  }
}
