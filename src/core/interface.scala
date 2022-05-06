package magnolia1

import scala.annotation.tailrec
import scala.reflect.*

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
  ):

    type PType

    def typeclass: Typeclass[PType]
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
        def typeclass = cbn.value
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
