/* Magnolia, version 0.7.0. Copyright 2018 Jon Pretty, Propensive Ltd.
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
package magnolia

import language.higherKinds
import scala.annotation.tailrec

/** represents a subtype of a sealed trait
  *
  *  @tparam Typeclass  type constructor for the typeclass being derived
  *  @tparam Type       generic type of this parameter */
trait Subtype[Typeclass[_], Type] {

  /** the type of subtype */
  type SType <: Type

  /** the [[TypeName]] of the subtype
    *
    *  This is the full name information for the type of subclass. */
  def typeName: TypeName

  /** the typeclass instance associated with this subtype
    *
    *  This is the instance of the type `Typeclass[SType]` which will have been discovered by
    *  implicit search, or derived by Magnolia. */
  def typeclass: Typeclass[SType]

  /** partial function defined the subset of values of `Type` which have the type of this subtype */
  def cast: PartialFunction[Type, SType]

  override def toString: String = s"Subtype(${typeName.full})"
}

/** represents a parameter of a case class
  *
  *  @tparam Typeclass  type constructor for the typeclass being derived
  *  @tparam Type       generic type of this parameter */
trait Param[Typeclass[_], Type] {

  /** the type of the parameter being represented
    *
    *  For example, for a case class,
    *  <pre>
    *  case class Person(name: String, age: Int)
    *  </pre>
    *  the [[Param]] instance corresponding to the `age` parameter would have a [[PType]] equal to
    *  the type [[scala.Int]]. However, in practice, this type will never be universally quantified.
    */
  type PType

  /** the name of the parameter */
  def label: String

  /** flag indicating a repeated (aka. vararg) parameter
    *
    * For example, for a case class,
    * <pre>
    * case class Account(id: String, emails: String*)
    * </pre>
    * the [[Param]] instance corresponding to the `emails` parameter would be `repeated` and have a
    * [[PType]] equal to the type `Seq[String]`. Note that only the last parameter of a case class
    * can be repeated. */
  def repeated: Boolean

  /** the typeclass instance associated with this parameter
    *
    *  This is the instance of the type `Typeclass[PType]` which will have been discovered by
    *  implicit search, or derived by Magnolia.
    *
    *  Its type is existentially quantified on this [[Param]] instance, and depending on the
    *  nature of the particular typeclass, it may either accept or produce types which are also
    *  existentially quantified on this same [[Param]] instance. */
  def typeclass: Typeclass[PType]

  /** provides the default value for this parameter, as defined in the case class constructor */
  def default: Option[PType]

  /** dereferences a value of the case class type, `Type`, to access the value of the parameter
    *  being represented
    *
    *  When programming generically, against an unknown case class, with unknown parameter names
    *  and types, it is not possible to directly access the parameter values without reflection,
    *  which is undesirable. This method, whose implementation is provided by the Magnolia macro,
    *  will dereference a case class instance to access the parameter corresponding to this
    *  [[Param]].
    *
    *  Whilst the type of the resultant parameter value cannot be universally known at the use, its
    *  type will be existentially quantified on this [[Param]] instance, and the return type of the
    *  corresponding `typeclass` method will be existentially quantified on the same value. This is
    *  sufficient for the compiler to determine that the two values are compatible, and the value may
    *  be applied to the typeclass (in whatever way that particular typeclass provides).
    *
    *  @param param  the instance of the case class to be dereferenced
    *  @return  the parameter value */
  def dereference(param: Type): PType

  def annotationsArray: Array[Any]

  /** a sequence of objects representing all of the annotations on the case class
    *
    *  For efficiency, this sequence is implemented by an `Array`, but upcast to a
    *  [[scala.collection.Seq]] to hide the mutable collection API. */
  final def annotations: Seq[Any] = annotationsArray

  override def toString: String = s"Param($label)"
}

/** represents a case class or case object and the context required to construct a new typeclass
  *  instance corresponding to it
  *
  *  Instances of [[CaseClass]] provide access to all of the parameters of the case class, the full
  *  name of the case class type, and a boolean to determine whether the type is a case class or case
  *  object.
  *
  *  @param typeName         the name of the case class
  *  @param isObject         true only if this represents a case object rather than a case class
  *  @param parametersArray  an array of [[Param]] values for this case class
  *  @param annotationsArray  an array of instantiated annotations applied to this case class
  *  @tparam Typeclass  type constructor for the typeclass being derived
  *  @tparam Type       generic type of this parameter */
abstract class CaseClass[Typeclass[_], Type] private[magnolia] (
  val typeName: TypeName,
  val isObject: Boolean,
  val isValueClass: Boolean,
  parametersArray: Array[Param[Typeclass, Type]],
  annotationsArray: Array[Any]
) {

  override def toString: String = s"CaseClass(${typeName.full}, ${parameters.mkString(",")})"
  /** constructs a new instance of the case class type
    *
    *  This method will be implemented by the Magnolia macro to make it possible to construct
    *  instances of case classes generically in user code, that is, without knowing their type
    *  concretely.
    *
    *  To construct a new case class instance, the method takes a lambda which defines how each
    *  parameter in the new case class should be constructed. See the [[Param]] class for more
    *  information on constructing parameter values from a [[Param]] instance.
    *
    *  @param makeParam  lambda for converting a generic [[Param]] into the value to be used for
    *                    this parameter in the construction of a new instance of the case class
    *  @return  a new instance of the case class */
  final def construct[Return](makeParam: Param[Typeclass, Type] => Return): Type =
    rawConstruct(parameters map makeParam)

  /** constructs a new instance of the case class type
    *
    *  Like [[construct]] this method is implemented by Magnolia and lets you construct case class
    *  instances generically in user code, without knowing their type concretely.
    *
    *  `rawConstruct`, however, is more low-level in that it expects you to provide a [[Seq]]
    *  containing all the field values for the case class type, in order and with the correct types.
    *
    * @param fieldValues contains the field values for the case class instance to be constructed,
    *                    in order and with the correct types.
    *  @return  a new instance of the case class
    *  @throws  IllegalArgumentException if the size of `paramValues` differs from the size of [[parameters]] */
  def rawConstruct(fieldValues: Seq[Any]): Type

  /** a sequence of [[Param]] objects representing all of the parameters in the case class
    *
    *  For efficiency, this sequence is implemented by an `Array`, but upcast to a
    *  [[scala.collection.Seq]] to hide the mutable collection API. */
  final def parameters: Seq[Param[Typeclass, Type]] = parametersArray

  /** a sequence of objects representing all of the annotations on the case class
    *
    *  For efficiency, this sequence is implemented by an `Array`, but upcast to a
    *  [[scala.collection.Seq]] to hide the mutable collection API. */
  final def annotations: Seq[Any] = annotationsArray
}

/** represents a sealed trait and the context required to construct a new typeclass instance
  *  corresponding to it
  *
  *  Instances of `SealedTrait` provide access to all of the component subtypes of the sealed trait
  *  which form a coproduct, and to the fully-qualified name of the sealed trait.
  *  @param typeName       the name of the sealed trait
  *  @param subtypesArray  an array of [[Subtype]] instances for each subtype in the sealed trait
  *  @param annotationsArray  an array of instantiated annotations applied to this case class
  *  @tparam Typeclass  type constructor for the typeclass being derived
  *  @tparam Type             generic type of this parameter */
final class SealedTrait[Typeclass[_], Type](
  val typeName: TypeName,
  subtypesArray: Array[Subtype[Typeclass, Type]],
  annotationsArray: Array[Any]
) {

  override def toString: String = s"SealedTrait($typeName, Array[${subtypes.mkString(",")}])"

  /** a sequence of all the subtypes of this sealed trait */
  def subtypes: Seq[Subtype[Typeclass, Type]] = subtypesArray

  /** convenience method for delegating typeclass application to the typeclass corresponding to the
    *  subtype of the sealed trait which matches the type of the `value`
    *
    *  @tparam Return  the return type of the lambda, which should be inferred
    *  @param value   the instance of the generic type whose value should be used to match on a
    *                 particular subtype of the sealed trait
    *  @param handle  lambda for applying the value to the typeclass for the particular subtype which
    *                 matches
    *  @return  the result of applying the `handle` lambda to subtype of the sealed trait which
    *           matches the parameter `value` */
  def dispatch[Return](value: Type)(handle: Subtype[Typeclass, Type] => Return): Return = {
    @tailrec def rec(ix: Int): Return =
      if (ix < subtypesArray.length) {
        val sub = subtypesArray(ix)
        if (sub.cast.isDefinedAt(value)) handle(sub) else rec(ix + 1)
      } else
        throw new IllegalArgumentException(
          s"The given value `$value` is not a sub type of `$typeName`"
        )
    rec(0)
  }

  /** a sequence of objects representing all of the annotations on the topmost trait
    *
    *  For efficiency, this sequence is implemented by an `Array`, but upcast to a
    *  [[scala.collection.Seq]] to hide the mutable collection API. */
  final def annotations: Seq[Any] = annotationsArray
}

/**
  * Provides the different parts of a type's class name.
  */
final case class TypeName(owner: String, short: String) {
  def full: String = s"$owner.$short"
}

/**
  * This annotation can be attached to the implicit `gen` method of a type class companion,
  * which is implemented by the `Magnolia.gen` macro.
  * It causes magnolia to dump the macro-generated code to the console during compilation.
  *
  * @param typeNamePart If non-empty restricts the output generation to types
  *                     whose full name contains the given [[String]]
  */
final class debug(typeNamePart: String = "") extends scala.annotation.StaticAnnotation
