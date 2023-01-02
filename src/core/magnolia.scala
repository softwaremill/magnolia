package magnolia2

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*

import Macro.*

trait CommonDerivation[TypeClass[_]]:
  type Typeclass[T] = TypeClass[T]

  /** Must be implemented by the user of Magnolia to construct a typeclass for
    * case class `T` using the provided type info. E.g. if we are deriving
    * `Show[T]` typeclasses, and `T` is a case class `Foo(...)`, we need to
    * constuct `Show[Foo]`.
    *
    * This method is called 'join' because typically it will _join_ together the
    * typeclasses for all the parameters of the case class, into a single
    * typeclass for the case class itself. The field [[CaseClass.params]] can
    * provide useful information for doing this.
    *
    * @param caseClass
    *   information about the case class `T`, its parameters, and _their_
    *   typeclasses
    */
  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T]

  protected inline def derivedMirrorProduct[A](
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

end CommonDerivation

trait ProductDerivation[TypeClass[_]] extends CommonDerivation[TypeClass]:
  private inline def derivedMirror[A](using
      mirror: Mirror.Of[A]
  ): Typeclass[A] =
    inline mirror match
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline given derived[A](using Mirror.Of[A]): Typeclass[A] = derivedMirror[A]
end ProductDerivation

trait Derivation[TypeClass[_]]
    extends CommonDerivation[TypeClass]
    with SealedTraitDerivation:

  /** This must be implemented by the user of Magnolia to construct a Typeclass
    * for 'T', where 'T' is a Sealed Trait or Scala 3 Enum, using the provided
    * type info. E.g. if we are deriving 'Show[T]' typeclasses, and T is an enum
    * 'Suit' (eg with values Diamonds, Clubs, etc), we need to constuct
    * 'Show[Suit]'.
    *
    * This method is called 'split' because it will ''split'' the different
    * possible types of the SealedTrait, and handle each one to finally produce
    * a typeclass capable of handling any possible subtype of the trait.
    *
    * A useful function for implementing this method is [[SealedTrait#choose]],
    * which can take a value instance and provide information on the specific
    * subtype of the sealedTrait which that value is.
    */
  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T]

  transparent inline def subtypes[T, SubtypeTuple <: Tuple](
      m: Mirror.SumOf[T],
      idx: Int = 0 // no longer used, kept for bincompat
  ): List[SealedTrait.Subtype[Typeclass, T, _]] =
    subtypesFromMirror[T, SubtypeTuple](m, idx)

  private inline def derivedMirrorSum[A](sum: Mirror.SumOf[A]): Typeclass[A] =
    split(sealedTraitFromMirror(sum))

  private inline def derivedMirror[A](using
      mirror: Mirror.Of[A]
  ): Typeclass[A] =
    inline mirror match
      case sum: Mirror.SumOf[A]         => derivedMirrorSum[A](sum)
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  private inline def derivedValueClass[A]: Typeclass[A] = join(
    CaseClassDerivation.valueClassDerivation
  )

  private inline def derivedMirrorless[A]: Typeclass[A] =
    inline if Macro.isValueClass[A] then derivedValueClass[A]
    else
      error(
        "Deriving the typeclass based on mirrors or directly is not possible for " + Macro
          .showType[A] +
          ". Please refer to the documentation or report a feature request."
      )

  inline def derived[A]: Typeclass[A] =
    summonFrom {
      case prod: Mirror.ProductOf[A] => derivedMirrorProduct[A](prod)
      case sum: Mirror.SumOf[A]      => derivedMirrorSum[A](sum)
      case _                         => derivedMirrorless[A]
    }

  protected override inline def deriveSubtype[s](
      m: Mirror.Of[s]
  ): Typeclass[s] = derivedMirror[s](using m)
end Derivation

trait AutoDerivation[TypeClass[_]] extends Derivation[TypeClass]:
  inline given autoDerived[A]: TypeClass[A] =
    derived
