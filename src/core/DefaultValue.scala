package magnolia

import scala.quoted.*
import scala.compiletime.erasedValue

object DefaultValues {
  inline def apply[T]: List[(String, Option[Any])] = ${ defaultValuesImpl[T] }

  def defaultValuesImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, Option[Any])]] = {
    import qctx.reflect._
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol
    println(tpe.typeSymbol.caseFields.map)
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    Expr.ofList(
      tpe
        .typeSymbol
        .caseFields
        .map {
          case ValDef(name, _, rhs) => Expr(name -> None/*TODO rhs*/)
        }
    )
  }
}
