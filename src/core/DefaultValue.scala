package magnolia

import scala.quoted.*
import scala.compiletime.erasedValue

object DefaultValues {
  inline def apply[T]: List[Option[Any]] = ${ defaultValuesImpl[T] }

  def defaultValuesImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[Option[Any]]] = {
    import qctx.reflect._
    val tr = TypeRepr.of[T]
    val symbol = tr.typeSymbol
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    constr.paramss.flatMap { clause =>
      clause.params.map { p =>
        val v = p.asInstanceOf[ValDef]
        println(v)
      }
    }
    Expr(List(None))
  }
}
