/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package magnolia.examples

import language.experimental.macros

import magnolia._

trait TypeNameInfo[T] {
  def name: TypeName

  def subtypeNames: Seq[TypeName]
}

object TypeNameInfo {
  type Typeclass[T] = TypeNameInfo[T]
  def combine[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = ctx.typeName

      def subtypeNames: Seq[TypeName] = Nil
    }

  def dispatch[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = ctx.typeName

      def subtypeNames: Seq[TypeName] = ctx.subtypes.map(_.typeName)
    }

  def fallback[T]: TypeNameInfo[T] =
    new TypeNameInfo[T] {
      def name: TypeName = TypeName("", "Unknown Type", Seq.empty)

      def subtypeNames: Seq[TypeName] = Nil
    }

  implicit def gen[T]: TypeNameInfo[T] = macro Magnolia.gen[T]
}
