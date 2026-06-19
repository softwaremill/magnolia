package magnolia1.parameterised

import magnolia1.*

import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.quoted.Expr

trait ParamaterisedCommonDerivation[TypeClass[_], PS[_]]:
  type Typeclass[T] = TypeClass[T]
  type Ps[T] = PS[T]

  /** Must be implemented by the user of Magnolia to construct a typeclass for case class `T` using the provided type info. E.g. if we are
    * deriving `Show[T]` typeclasses, and `T` is a case class `Foo(...)`, we need to constuct `Show[Foo]`.
    *
    * This method is called 'join' because typically it will _join_ together the typeclasses for all the parameters of the case class, into
    * a single typeclass for the case class itself. The field [[CaseClass.params]] can provide useful information for doing this.
    *
    * @param caseClass
    *   information about the case class `T`, its parameters, and _their_ typeclasses
    */
  def join[T: Ps](caseClass: CaseClass[Typeclass, T]): Typeclass[T]

  inline def derivedMirrorProduct[A: Ps](
      product: Mirror.ProductOf[A]
  ): Typeclass[A] = join(CaseClassDerivation.fromMirror(product))

  inline def getParams__[T, Labels <: Tuple, Params <: Tuple](
      annotations: Map[String, List[Any]],
      inheritedAnnotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      repeated: Map[String, Boolean],
      defaults: Map[String, Option[() => Any]],
      idx: Int = 0
  ): List[CaseClass.Param[Typeclass, T]] = CaseClassDerivation.paramsFromMaps(
    annotations,
    inheritedAnnotations,
    typeAnnotations,
    repeated,
    defaults
  )

  // for backward compatibility with v1.1.1
  inline def getParams_[T, Labels <: Tuple, Params <: Tuple](
      annotations: Map[String, List[Any]],
      inheritedAnnotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      repeated: Map[String, Boolean],
      idx: Int = 0
  ): List[CaseClass.Param[Typeclass, T]] =
    getParams__(annotations, Map.empty, typeAnnotations, repeated, Map(), idx)

  // for backward compatibility with v1.0.0
  inline def getParams[T, Labels <: Tuple, Params <: Tuple](
      annotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      repeated: Map[String, Boolean],
      idx: Int = 0
  ): List[CaseClass.Param[Typeclass, T]] =
    getParams__(annotations, Map.empty, typeAnnotations, repeated, Map(), idx)

end ParamaterisedCommonDerivation

trait ParamaterisedProductDerivation[TypeClass[_], PS[_]] extends ParamaterisedCommonDerivation[TypeClass, PS]:
  inline def derivedMirror[A](using mirror: Mirror.Of[A], ps: PS[A]): Typeclass[A] =
    inline mirror match
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline given derived[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    summonFrom {
      case p: PS[A] => derivedMirror[A](using mirror, p)
      case _        => derivedMirror[A](using mirror, default[A])
    }

  def default[A]: PS[A]
end ParamaterisedProductDerivation

trait ParamaterisedDerivation[TypeClass[_], PS[_]]
    extends ParamaterisedCommonDerivation[TypeClass, PS]
    with ParamaterisedSealedTraitDerivation:

  /** This must be implemented by the user of Magnolia to construct a Typeclass for 'T', where 'T' is a Sealed Trait or Scala 3 Enum, using
    * the provided type info. E.g. if we are deriving 'Show[T]' typeclasses, and T is an enum 'Suit' (eg with values Diamonds, Clubs, etc),
    * we need to constuct 'Show[Suit]'.
    *
    * This method is called 'split' because it will ''split'' the different possible types of the SealedTrait, and handle each one to
    * finally produce a typeclass capable of handling any possible subtype of the trait.
    *
    * A useful function for implementing this method is [[SealedTrait#choose]], which can take a value instance and provide information on
    * the specific subtype of the sealedTrait which that value is.
    */
  def split[T: PS](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T]

  /** This must be implemented by the user in order for value class support to work
    */
  def handleAnyVal: [T <: AnyVal, S] => (WrapAndSerde[T, Typeclass, S], UnwrapAndSerde[T, Typeclass, S]) => Typeclass[S] => Typeclass[T]

  transparent inline def subtypes[T, SubtypeTuple <: Tuple](
      m: Mirror.SumOf[T]
  ): List[SealedTrait.Subtype[Typeclass, T, _]] =
    subtypesFromMirror[T, SubtypeTuple](m)

  inline def derivedMirrorSum[A: PS](sum: Mirror.SumOf[A]): Typeclass[A] =
    split(sealedTraitFromMirror(sum))

  inline def derivedMirror[A](using mirror: Mirror.Of[A], i: PS[A]): Typeclass[A] =
    inline mirror match
      case sum: Mirror.SumOf[A]         => derivedMirrorSum[A](sum)
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline def derived[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    summonFrom {
      case p: PS[A] => derivedMirror[A](using mirror, p)
      case _        => derivedMirror[A](using mirror, default[A])
    }

  protected override inline def deriveSubtype[s](
      m: Mirror.Of[s],
      i: PS[s]
  ): Typeclass[s] = derivedMirror[s](using m, i)
  inline def derived[T <: AnyVal & Product: PS]: TypeClass[T] = deriveAnyValSupport[T]

  inline def handlePrimitive[A]: TypeClass[A] = summonInline[Typeclass[A]]
  inline implicit def deriveAnyValSupport[A <: AnyVal & Product: PS]: TypeClass[A] =
    ${
      ValueClassSupport.deriveAnyValSupportImpl[A, Typeclass, this.type]('handleAnyVal, 'this)
    }

  // alias because I can't write macros well enough to avoid it r/n
  inline def mirrorDerived[A](using mirror: Mirror.Of[A]): Typeclass[A] = derived[A]
  inline def noMirrorDerived[A]: Typeclass[A] = handlePrimitive[A]
  def default[A]: PS[A] = throw new IllegalStateException("No default implemented")
end ParamaterisedDerivation

trait ParamaterisedAutoDerivation[TypeClass[_], PS[_]] extends ParamaterisedDerivation[TypeClass, PS]:
  inline given autoDerived[A](using Mirror.Of[A], PS[A]): TypeClass[A] = derivedMirror[A]
  inline given autoDerived[A <: AnyVal & Product: PS]: TypeClass[A] = derived[A]
