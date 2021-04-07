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