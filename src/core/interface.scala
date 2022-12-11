package magnolia1

import scala.annotation.tailrec
import scala.reflect.*
import scala.quoted.ToExpr.IArrayToExpr

case class TypeInfo(
    owner: String,
    short: String,
    typeParams: Iterable[TypeInfo]
):
  def full: String = s"$owner.$short"

object CaseClass:
  trait Param[Typeclass[_], Type](
      val label: String,
      val index: Int,
      val repeated: Boolean,
      val annotations: IArray[Any],
      val typeAnnotations: IArray[Any]
  ) extends Serializable:

    type PType

    /**
     * Gives the constructed typeclass for the parameter's type. Eg for a `case class Foo(bar: String, baz: Int)`,
     * where this [[Param]] denotes 'baz', the `typeclass` field returns an instance of `Typeclass[Int]`.
     */
    def typeclass: Typeclass[PType]

    /**
     * Get the value of this param out of the supplied instance of the case class.
     *
     * @param value an instance of the case class
     * @return the value of this parameter in the case class
     */
    def deref(param: Type): PType

    /** Requires compilation with `-Yretain-trees` on.
      * @return
      *   default argument value, if any
      */
    def default: Option[PType]
    def inheritedAnnotations: IArray[Any] = IArray.empty[Any]
    override def toString: String = s"Param($label)"

  object Param:

    def apply[F[_], T, P](
        name: String,
        idx: Int,
        repeated: Boolean,
        cbn: CallByNeed[F[P]],
        defaultVal: CallByNeed[Option[P]],
        annotations: List[Any],
        inheritedAnns: List[Any],
        typeAnnotations: List[Any]
    ): Param[F, T] =
      apply(
        name = name,
        idx = idx,
        repeated = repeated,
        cbn = cbn,
        defaultVal = defaultVal,
        annotations = IArray.from(annotations),
        inheritedAnns = IArray.from(inheritedAnns),
        typeAnnotations = IArray.from(typeAnnotations)
      )

    def apply[F[_], T, P](
        name: String,
        idx: Int,
        repeated: Boolean,
        cbn: CallByNeed[F[P]],
        defaultVal: CallByNeed[Option[P]],
        annotations: IArray[Any],
        inheritedAnns: IArray[Any],
        typeAnnotations: IArray[Any]
    ): Param[F, T] =
      new CaseClass.Param[F, T](
        name,
        idx,
        repeated,
        annotations,
        typeAnnotations
      ):
        type PType = P
        def default: Option[PType] = defaultVal.value
        def typeclass: F[PType] = cbn.value
        override def inheritedAnnotations = inheritedAnns
        def deref(value: T): P =
          value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]

    // for backward compatibility with v1.0.0
    def apply[F[_], T, P](
        name: String,
        idx: Int,
        repeated: Boolean,
        cbn: CallByNeed[F[P]],
        defaultVal: CallByNeed[Option[P]],
        annotations: IArray[Any],
        typeAnnotations: IArray[Any]
    ): Param[F, T] =
      new CaseClass.Param[F, T](
        name,
        idx,
        repeated,
        annotations,
        typeAnnotations
      ):
        type PType = P
        def default: Option[PType] = defaultVal.value
        def typeclass = cbn.value
        def deref(value: T): P =
          value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]
  end Param
end CaseClass


/**
 * In the terminology of Algebraic Data Types (ADTs), case classes are known as 'product types'.
 *
 * @param params an array giving information about the parameters of the case class. Each [[Param]] element
 *               has a very useful [[CaseClass.Param.typeclass]] field giving the constructed typeclass for the
 *               parameter's type. Eg for a `case class Foo(bar: String, baz: Int)`, you can
 *               obtain `Typeclass[String]`, `Typeclass[Int]`.
 */
abstract class CaseClass[Typeclass[_], Type](
    val typeInfo: TypeInfo,
    val isObject: Boolean,
    val isValueClass: Boolean,
    val params: IArray[CaseClass.Param[Typeclass, Type]],
    val annotations: IArray[Any],
    val inheritedAnnotations: IArray[Any] = IArray.empty[Any],
    val typeAnnotations: IArray[Any]
) extends Serializable:

  // for backward compatibility with v1.0.0
  def this(
      typeInfo: TypeInfo,
      isObject: Boolean,
      isValueClass: Boolean,
      params: IArray[CaseClass.Param[Typeclass, Type]],
      annotations: IArray[Any],
      typeAnnotations: IArray[Any]
  ) = this(
    typeInfo,
    isObject,
    isValueClass,
    params,
    annotations,
    IArray.empty[Any],
    typeAnnotations
  )

  type Param = CaseClass.Param[Typeclass, Type]

  override def toString: String =
    s"CaseClass(${typeInfo.full}, ${params.mkString(",")})"
  def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): Type
  def constructMonadic[Monad[_]: Monadic, PType: ClassTag](
      make: Param => Monad[PType]
  ): Monad[Type]
  def constructEither[Err, PType: ClassTag](
      makeParam: Param => Either[Err, PType]
  ): Either[List[Err], Type]
  def rawConstruct(fieldValues: Seq[Any]): Type

  def param[P](
      name: String,
      idx: Int,
      repeated: Boolean,
      cbn: CallByNeed[Typeclass[P]],
      defaultVal: CallByNeed[Option[P]],
      annotations: IArray[Any],
      inheritedAnns: IArray[Any],
      typeAnnotations: IArray[Any]
  ): Param =
    new CaseClass.Param[Typeclass, Type](
      name,
      idx,
      repeated,
      annotations,
      typeAnnotations
    ):
      type PType = P
      def default: Option[PType] = defaultVal.value
      def typeclass = cbn.value
      override def inheritedAnnotations = inheritedAnns
      def deref(value: Type): P =
        value.asInstanceOf[Product].productElement(idx).asInstanceOf[P]

  // for backward compatibility with v1.0.0
  def param[P](
      name: String,
      idx: Int,
      repeated: Boolean,
      cbn: CallByNeed[Typeclass[P]],
      defaultVal: CallByNeed[Option[P]],
      annotations: IArray[Any],
      typeAnnotations: IArray[Any]
  ): Param = param(
    name,
    idx,
    repeated,
    cbn,
    defaultVal,
    annotations,
    IArray.empty[Any],
    typeAnnotations
  )

end CaseClass

/**
 * Represents a Sealed-Trait or a Scala 3 Enum.
 *
 * In the terminology of Algebraic Data Types (ADTs), sealed-traits/enums are termed
 * 'sum types'.
 */
case class SealedTrait[Typeclass[_], Type](
    typeInfo: TypeInfo,
    subtypes: IArray[SealedTrait.Subtype[Typeclass, Type, _]],
    annotations: IArray[Any],
    typeAnnotations: IArray[Any],
    isEnum: Boolean,
    inheritedAnnotations: IArray[Any]
) extends Serializable:

  // for backward compatibility with v1.0.0
  def this(
      typeInfo: TypeInfo,
      subtypes: IArray[SealedTrait.Subtype[Typeclass, Type, _]],
      annotations: IArray[Any],
      typeAnnotations: IArray[Any],
      isEnum: Boolean
  ) = this(
    typeInfo,
    subtypes,
    annotations,
    typeAnnotations,
    isEnum,
    IArray.empty[Any]
  )

  // for backward compatibility with v1.0.0
  def copy(
      typeInfo: TypeInfo,
      subtypes: IArray[SealedTrait.Subtype[Typeclass, Type, _]],
      annotations: IArray[Any],
      typeAnnotations: IArray[Any],
      isEnum: Boolean
  ): SealedTrait[Typeclass, Type] = this.copy(
    typeInfo,
    subtypes,
    annotations,
    typeAnnotations,
    isEnum,
    this.inheritedAnnotations
  )

  type Subtype[S] = SealedTrait.SubtypeValue[Typeclass, Type, S]

  override def toString: String =
    s"SealedTrait($typeInfo, IArray[${subtypes.mkString(",")}])"

  /**
   * Provides a way to recieve the type info for the explicit subtype that
   * 'value' is an instance of. So if 'Type' is a Sealed Trait or Scala 3
   * Enum like 'Suit', the 'handle' function will be supplied with the
   * type info for the specific subtype of 'value', eg 'Diamonds'.
   *
   * @param value must be instance of a subtype of typeInfo
   * @param handle function that will be passed the Subtype of 'value'
   * @tparam Return whatever type the 'handle' function wants to return
   * @return whatever the 'handle' function returned!
   */
  def choose[Return](value: Type)(handle: Subtype[_] => Return): Return =
    @tailrec def rec(ix: Int): Return =
      if ix < subtypes.length then
        val sub = subtypes(ix)
        if sub.cast.isDefinedAt(value) then
          handle(SealedTrait.SubtypeValue(sub, value))
        else rec(ix + 1)
      else
        throw new IllegalArgumentException(
          s"The given value `$value` is not a sub type of `$typeInfo`"
        )

    rec(0)

end SealedTrait

object SealedTrait:

  // for backward compatibility with v1.0.0
  def apply[Typeclass[_], Type](
      typeInfo: TypeInfo,
      subtypes: IArray[SealedTrait.Subtype[Typeclass, Type, _]],
      annotations: IArray[Any],
      typeAnnotations: IArray[Any],
      isEnum: Boolean
  ) = new SealedTrait[Typeclass, Type](
    typeInfo,
    subtypes,
    annotations,
    typeAnnotations,
    isEnum,
    IArray.empty[Any]
  )

  /**
   * @tparam Type the type of the Sealed Trait or Scala 3 Enum, eg 'Suit'
   * @tparam SType the type of the subtype, eg 'Diamonds' or 'Clubs'
   */
  class Subtype[Typeclass[_], Type, SType](
      val typeInfo: TypeInfo,
      val annotations: IArray[Any],
      val inheritedAnnotations: IArray[Any],
      val typeAnnotations: IArray[Any],
      val isObject: Boolean,
      val index: Int,
      callByNeed: CallByNeed[Typeclass[SType]],
      isType: Type => Boolean,
      asType: Type => SType & Type
  ) extends PartialFunction[Type, SType & Type],
        Serializable:

    // for backward compatibility with v1.0.0
    def this(
        typeInfo: TypeInfo,
        annotations: IArray[Any],
        typeAnnotations: IArray[Any],
        isObject: Boolean,
        index: Int,
        callByNeed: CallByNeed[Typeclass[SType]],
        isType: Type => Boolean,
        asType: Type => SType & Type
    ) = this(
      typeInfo,
      annotations,
      IArray.empty[Any],
      typeAnnotations,
      isObject,
      index,
      callByNeed,
      isType,
      asType
    )

    /**
     * @return the already-constructed typeclass instance for this subtype
     */
    def typeclass: Typeclass[SType & Type] =
      callByNeed.value.asInstanceOf[Typeclass[SType & Type]]
    def cast: PartialFunction[Type, SType & Type] = this
    def isDefinedAt(t: Type): Boolean = isType(t)
    def apply(t: Type): SType & Type = asType(t)
    override def toString: String = s"Subtype(${typeInfo.full})"

  class SubtypeValue[Typeclass[_], Type, S](
      val subtype: Subtype[Typeclass, Type, S],
      v: Type
  ):
    export subtype.{
      typeclass,
      typeAnnotations,
      annotations,
      inheritedAnnotations,
      cast,
      typeInfo
    }
    def value: S & Type = cast(v)

end SealedTrait

object CallByNeed:
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a)

final class CallByNeed[+A](private[this] var eval: () => A)
    extends Serializable:
  lazy val value: A =
    val result = eval()
    eval = null
    result
