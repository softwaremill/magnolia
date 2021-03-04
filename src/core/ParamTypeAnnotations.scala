package magnolia

import scala.quoted._

object ParamTypeAnnotations {
  inline def apply[T]: List[(String, List[Any])] = ${ paramTypeAnnotationsImpl[T] }

  def paramTypeAnnotationsImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, List[Any])]] = {
    import qctx.reflect._

    val tpe = TypeRepr.of[T]

    def getAnnotations(t: TypeRepr): List[Term] = t match {
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _ => Nil
    }

    Expr.ofList(
      tpe
      .typeSymbol
      .caseFields
      .map { field =>
        val tpeRepr = field.tree match {
          case v: ValDef => v.tpt.tpe
          case d: DefDef => d.returnTpt.tpe
        }
        Expr(field.name) -> getAnnotations(tpeRepr).filter { a =>
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
