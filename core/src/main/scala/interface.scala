package magnolia

import language.higherKinds

/** represents a subtype of a sealed trait
 *
 *  @tparam Typeclass  type constructor for the typeclass being derived
 *  @tparam Type       generic type of this parameter */
trait Subtype[Typeclass[_], Type] {
  
  /** the type of subtype */
  type SType <: Type

  /** the name of the subtype
   *
   *  This is the fully-qualified name of the type of subclass. */
  def label: String
  
  /** the typeclass instance associated with this subtype
   *
   *  This is the instance of the type `Typeclass[SType]` which will have been discovered by
   *  implicit search, or derived by Magnolia. */
  def typeclass: Typeclass[SType]

  /** partial function defined the subset of values of `Type` which have the type of this subtype */
  def cast: PartialFunction[Type, SType]
}

/** represents a parameter of a case class
 *
 *  @tparam Typeclass  type constructor for the typeclass being derived
 *  @tparam Type       generic type of this parameter */
trait Param[Typeclass[_], Type] {
  
  /** the type of the parameter being represented
   *
   *  For exmaple, for a case class,
   *  <pre>
   *  case class Person(name: String, age: Int)
   *  </pre>
   *  the [[Param]] instance corresponding to the `age` parameter would have a [[PType]] equal to
   *  the type [[Int]]. However, in practice, this type will never be universally quantified. */
  type PType
  
  /** the name of the parameter */
  def label: String

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
 *  @tparam Typeclass  type constructor for the typeclass being derived
 *  @tparam Type       generic type of this parameter */
abstract class CaseClass[Typeclass[_], Type] private[magnolia](
  val typeName: String,
  val isObject: Boolean,
  parametersArray: Array[Param[Typeclass, Type]]) {

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
  def construct[Return](makeParam: Param[Typeclass, Type] => Return): Type
  
  /** a sequence of [[Param]] objects representing all of the parameters in the case class
   *
   *  For efficiency, this sequence is implemented by an `Array`, but upcast to a [[Seq]] to hide
   *  the mutable collection API. */
  def parameters: Seq[Param[Typeclass, Type]] = parametersArray
}

/** represents a sealed trait and the context required to construct a new typeclass instance
 *  corresponding to it
 *
 *  Instances of `SealedTrait` provide access to all of the component subtypes of the sealed trait
 *  which form a coproduct, and to the fully-qualified name of the sealed trait.
 *
 *  @param typeName       the name of the sealed trait
 *  @param subtypesArray  an array of [[Subtype]] instances for each subtype in the sealed trait
 *  @tparam Typeclass  type constructor for the typeclass being derived
 *  @tparam Type             generic type of this parameter */
final class SealedTrait[Typeclass[_], Type](val typeName: String,
                                            subtypesArray: Array[Subtype[Typeclass, Type]]) {

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
  def dispatch[Return](value: Type)(handle: Subtype[Typeclass, Type] => Return): Return =
    subtypes.map { sub => sub.cast.andThen { v => handle(sub) } }.reduce(_ orElse _)(value)
}

