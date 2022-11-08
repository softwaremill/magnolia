package magnolia1

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*

import Macro.*


trait CommonDerivation[TypeClass[_]]:
  type Typeclass[T] = TypeClass[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]

  inline def derivedMirrorProduct[A](
      product: Mirror.ProductOf[A]
  ): Typeclass[A] = join(CaseClassDerivation.fromMirror(product))

  inline def derivedMacroProduct[A]: Typeclass[A] = join(CaseClassDerivation.fromMacro)

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
  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    inline mirror match
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline given derived[A](using Mirror.Of[A]): Typeclass[A] = derivedMirror[A]
end ProductDerivation


trait Derivation[TypeClass[_]]
    extends CommonDerivation[TypeClass]
    with SealedTraitDerivation:
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

  transparent inline def subtypes[T, SubtypeTuple <: Tuple](
      m: Mirror.SumOf[T],
      idx: Int = 0 // no longer used, kept for bincompat
  ): List[SealedTrait.Subtype[Typeclass, T, _]] =
    subtypesFromMirror[T, SubtypeTuple](m, idx)

  inline def derivedMirrorSum[A](sum: Mirror.SumOf[A]): Typeclass[A] =
    split(sealedTraitFromMirror(sum))

  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    inline mirror match
      case sum: Mirror.SumOf[A]         => derivedMirrorSum[A](sum)
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline def derivedValueClass[A]: Typeclass[A] = derivedMacroProduct[A]

  inline def derivedMirrorless[A]: Typeclass[A] = 
    inline if Macro.isValueClass[A] then 
      derivedValueClass[A]
    else error("Mirrorless derivation failed.")


  inline def derived[A]: Typeclass[A] = 
    // derivedMirror[A]
    // inline if Macro.isValueClass[A] then 
    //   println("VC")
    //   derivedValueClass[A]
    // else 
    //   summonFrom {
    //     case prod: Mirror.ProductOf[A] => derivedMirrorProduct[A](prod)
    //     case sum: Mirror.SumOf[A] => derivedMirrorSum[A](sum)
    //     case _ => derivedMirrorless[A]
    //   }
    summonFrom {
      case prod: Mirror.ProductOf[A] => derivedMirrorProduct[A](prod)
      case sum: Mirror.SumOf[A] => derivedMirrorSum[A](sum)
      case _ => derivedMirrorless[A]
    }

  protected override inline def deriveSubtype[s](
      m: Mirror.Of[s]
  ): Typeclass[s] = derivedMirror[s](using m)
end Derivation


trait AutoDerivation[TypeClass[_]] extends Derivation[TypeClass]:
  inline given autoDerived[A]: TypeClass[A] = 
    derived

