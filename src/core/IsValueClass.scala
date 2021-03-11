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

object IsValueClass { // TODO value classes can't work with mirrors yet https://github.com/lampepfl/dotty/pull/7023
  inline def apply[T]: Boolean = ${ isValueClassImpl[T] }

  def isValueClassImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] = {
    import qctx.reflect.*
    val anyVal: Symbol = Symbol.classSymbol("scala.AnyVal")
    val baseClasses = TypeRepr.of[T].baseClasses
    Expr(baseClasses.contains(anyVal))
  }
}