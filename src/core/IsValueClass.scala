package magnolia

import scala.quoted.*
import scala.compiletime.erasedValue

object IsValueClass { //TODO value classes can't work with mirrors yet, since https://github.com/lampepfl/dotty/pull/7023
  inline def apply[T]: Boolean = ${ isValueClassImpl[T] }

  def isValueClassImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] = {
    import qctx.reflect.*
    val anyVal: Symbol = Symbol.classSymbol("scala.AnyVal")
    val baseClasses = TypeRepr.of[T].baseClasses
    Expr(baseClasses.contains(anyVal))
  }
}