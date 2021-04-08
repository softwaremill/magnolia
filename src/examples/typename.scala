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
package magnolia.examples

import magnolia._

trait TypeNameInfo[T] {
  def name: TypeInfo

  def subtypeNames: Seq[TypeInfo]
}

object TypeNameInfo extends Derivation[TypeNameInfo]:
  def join[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = Nil

  override def split[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = ctx.subtypes.map(_.typeInfo)

  given fallback[T]: TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo = TypeInfo("", "Unknown Type", Seq.empty)
      def subtypeNames: Seq[TypeInfo] = Nil