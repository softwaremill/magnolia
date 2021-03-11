/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
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