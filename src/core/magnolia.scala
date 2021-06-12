package magnolia1

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*

trait AutoDerivation[TypeClass[_]] extends Derivation[TypeClass]:
  inline given autoDerived[A]: TypeClass[A] = derived

trait Derivation[TypeClass[_]]:
  type Typeclass[T] = TypeClass[T]
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]

  inline def derived[T]: Typeclass[T] = doDerive[T]

  inline def doDerive[T]: Typeclass[T] =
    inline if Derivation.isProduct[T] then
      val cc = new CaseClass[Typeclass, T](
        Derivation.typeInfo[T],
        Derivation.isObject[T],
        Derivation.isValueClass[T],
        IArray.from(Derivation.getParams[Typeclass, T](this)),
        IArray.from(Derivation.anns[T]),
        IArray.from(Derivation.inheritedAnns[T]),
        IArray.from(Derivation.typeAnns[T])
      ) {
        def rawConstruct(fieldValues: Seq[Any]): T =
          Derivation.rawConstruct[T](fieldValues.toList)
      }
      join(cc)
    else if Derivation.isSum[T] then
      val sealedTrait = new SealedTrait[Typeclass, T](
        Derivation.typeInfo[T],
        IArray.from(Derivation.getSubtypes[Typeclass, T](this)),
        IArray.from(Derivation.anns[T]),
        IArray.from(Derivation.typeAnns[T]),
        Derivation.isEnum[T],
        IArray.from(Derivation.inheritedAnns[T])
      )
      split(sealedTrait)
    else
      throw new IllegalArgumentException(s"The given type ${Derivation.typeInfo[T].short} was neither a sum nor a product")

end Derivation

object Derivation:

  inline def getParams[Typeclass[_], T](fallback: Derivation[Typeclass]): List[CaseClass.Param[Typeclass, T]] =
    ${ getParamsImpl[Typeclass, T]('fallback) }
  def getParamsImpl[Typeclass[_]: Type, T: Type](using Quotes)(fallback: Expr[Derivation[Typeclass]]): Expr[List[CaseClass.Param[Typeclass, T]]] =
    new DerivationImpl(using quotes).getParamsImpl[Typeclass, T](fallback)

  inline def getSubtypes[Typeclass[_], T](fallback: Derivation[Typeclass]): List[SealedTrait.Subtype[Typeclass, T, _]] =
    ${ getSubtypesImpl[Typeclass, T]('fallback) }
  def getSubtypesImpl[Typeclass[_]: Type, T: Type](fallback: Expr[Derivation[Typeclass]])(using Quotes): Expr[List[SealedTrait.Subtype[Typeclass, T, _]]] =
    new DerivationImpl(using quotes).getSubtypesImpl[Typeclass, T](fallback)

  inline def typeInfo[T]: TypeInfo = ${ typeInfoImpl[T] }
  def typeInfoImpl[T: Type](using Quotes): Expr[TypeInfo] =
    new DerivationImpl(using quotes).typeInfo[T]

  inline def isObject[T]: Boolean = ${ isObjectImpl[T] }
  def isObjectImpl[T: Type](using Quotes): Expr[Boolean] =
    new DerivationImpl(using quotes).isObject[T]

  inline def isEnum[T]: Boolean = ${ isEnumImpl[T] }
  def isEnumImpl[T: Type](using Quotes): Expr[Boolean] =
    new DerivationImpl(using quotes).isEnum[T]

  inline def isValueClass[T]: Boolean = ${ isValueClassImpl[T] }
  def isValueClassImpl[T: Type](using Quotes): Expr[Boolean] =
    new DerivationImpl(using quotes).isValueClass[T]

  inline def anns[T]: List[Any] = ${ annsImpl[T] }
  def annsImpl[T: Type](using Quotes): Expr[List[Any]] =
    new DerivationImpl(using quotes).anns[T]

  inline def inheritedAnns[T]: List[Any] = ${ inheritedAnnsImpl[T] }
  def inheritedAnnsImpl[T: Type](using Quotes): Expr[List[Any]] =
    new DerivationImpl(using quotes).inheritedAnns[T]

  inline def typeAnns[T]: List[Any] = ${ typeAnnsImpl[T] }
  def typeAnnsImpl[T: Type](using Quotes): Expr[List[Any]] =
    new DerivationImpl(using quotes).typeAnns[T]

  inline def isProduct[T]: Boolean = ${ isProductImpl[T] }
  def isProductImpl[T: Type](using Quotes): Expr[Boolean] =
    new DerivationImpl(using quotes).isProduct[T]
  
  inline def isSum[T]: Boolean = ${ isSumImpl[T] }
  def isSumImpl[T: Type](using Quotes): Expr[Boolean] =
    new DerivationImpl(using quotes).isSum[T]

  inline def rawConstruct[T](inline fieldValues: List[Any]): T = ${ rawConstructImpl[T]('fieldValues) }
  def rawConstructImpl[T: Type](fieldValues: Expr[List[Any]])(using Quotes): Expr[T] =
    new DerivationImpl(using quotes).rawConstruct[T](fieldValues)

end Derivation
