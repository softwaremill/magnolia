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

import language.experimental.macros
import scala.annotation.tailrec
import scala.reflect.*

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]):
  def full: String = s"$owner.$short"

trait Subtype[Typeclass[_], Type] extends Serializable:
  type SType
  
  def typeInfo: TypeInfo
  def index: Int
  def typeclass: Typeclass[SType & Type] = typeclassUnbound.asInstanceOf[Typeclass[SType & Type]]
  def typeclassUnbound: Typeclass[SType]
  def cast: PartialFunction[Type, SType & Type]
  def annotations: IArray[Any]
  def typeAnnotations: IArray[Any]
  override def toString: String = s"Subtype(${typeInfo.full})"

class SubtypeValue[Typeclass[_], Type](val subtype: Subtype[Typeclass, Type], v: Type):
  export subtype.{typeclass, typeAnnotations, annotations, cast, typeInfo}
  def value: subtype.SType & Type = cast(v)

object Subtype:
  def apply[Tc[_], T, S](info: TypeInfo, idx: Int, anns: IArray[Any], tpeAnns: IArray[Any], tc: CallByNeed[Tc[S]],
      isType: T => Boolean, asType: T => S & T): Subtype[Tc, T] =
    new Subtype[Tc, T] with PartialFunction[T, S & T]:
      type SType = S
      def typeInfo: TypeInfo = info
      def index: Int = idx
      def typeclassUnbound: Tc[SType] = tc.value
      def cast: PartialFunction[T, SType & T] = this
      def isDefinedAt(t: T) = isType(t)
      def apply(t: T): SType & T = asType(t)
      def annotations: IArray[Any] = anns
      def typeAnnotations: IArray[Any] = tpeAnns
      override def toString: String = s"Subtype(${typeInfo.full})"

trait Param[Typeclass[_], Type](val label: String, val index: Int,
    val repeated: Boolean, val annotations: IArray[Any], val typeAnnotations: IArray[Any]):
  
  type PType
  
  def typeclass: Typeclass[PType]
  def deref(param: Type): PType
  def default: Option[PType]
  override def toString: String = s"Param($label)"

object Param:
  def apply[Tc[_], T, P](name: String, idx: Int, repeated: Boolean, cbn: CallByNeed[Tc[P]],
      defaultVal: CallByNeed[Option[P]], annotations: IArray[Any],
      typeAnnotations: IArray[Any]): Param[Tc, T] = new Param[Tc, T](name, idx, repeated, annotations, typeAnnotations):

    type PType = P
    def default: Option[PType] = defaultVal.value
    def typeclass = cbn.value
    def deref(value: T): P = value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]

abstract class CaseClass[Typeclass[_], Type](
  val typeInfo: TypeInfo,
  val isObject: Boolean,
  val isValueClass: Boolean,
  val params: IArray[Param[Typeclass, Type]],
  val annotations: IArray[Any],
  val typeAnnotations: IArray[Any]
) extends Serializable:

  override def toString: String = s"CaseClass(${typeInfo.full}, ${params.mkString(",")})"
  def construct[PType](makeParam: Param[Typeclass, Type] => PType)(using ClassTag[PType]): Type

  def constructMonadic[Monad[_]: Monadic, PType: ClassTag]
                      (make: Param[Typeclass, Type] => Monad[PType]): Monad[Type]

  def constructEither[Err, PType: ClassTag]
                     (makeParam: Param[Typeclass, Type] => Either[Err, PType]): Either[List[Err], Type]

  def rawConstruct(fieldValues: Seq[Any]): Type

final class SealedTrait[Typeclass[_], Type]
                       (val typeInfo: TypeInfo,
                        val subtypes: IArray[Subtype[Typeclass, Type]],
                        val annotations: IArray[Any],
                        val typeAnnotations: IArray[Any]) extends Serializable:

  override def toString: String = s"SealedTrait($typeInfo, IArray[${subtypes.mkString(",")}])"

  def choose[Return](value: Type)(handle: SubtypeValue[Typeclass, Type] => Return): Return =
    @tailrec def rec(ix: Int): Return =
      if ix < subtypes.length then
        val sub = subtypes(ix)
        if (sub.cast.isDefinedAt(value)) handle(SubtypeValue[Typeclass, Type](sub, value)) else rec(ix + 1)
      else throw new IllegalArgumentException(s"The given value `$value` is not a sub type of `$typeInfo`")

    rec(0)

object CallByNeed:
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a)

final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable:
  lazy val value: A =
    val result = eval()
    eval = null
    result