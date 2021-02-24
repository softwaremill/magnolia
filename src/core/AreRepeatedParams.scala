package magnolia

import scala.quoted.*
import scala.compiletime.erasedValue

object AreRepeatedParams {
  inline def apply[T]: List[(String, Boolean)] = ${ isDefaultParamsImpl[T] }

  def isDefaultParamsImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, Boolean)]] = {
    import qctx.reflect.*
    def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match {
      case a: AnnotatedType =>
        a.annotation.tpe match {
          case tr: TypeRef => tr.name == "Repeated"
          case _ => false
        }
      case _ => false
    }
    val tr = TypeRepr.of[T]
    val symbol = tr.typeSymbol
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    val areRepeated = constr.paramss.flatMap { clause =>
      clause.params.flatMap {
        case ValDef(name, tpeTree, _) =>
          Some(name -> isRepeated(tpeTree.tpe))
        case _ =>
          None
      }
    }
    Expr(areRepeated)
  }
}