package magnolia1.parameterised

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*

import magnolia1.*
import magnolia1.Macro.*

trait ParamaterisedSealedTraitDerivation:
  type Typeclass[T]
  type Ps[T]

  protected inline def deriveSubtype[s](
      m: Mirror.Of[s],
      i: Ps[s]
  ): Typeclass[s]

  protected inline def sealedTraitFromMirror[A](
      m: Mirror.SumOf[A]
  ): SealedTrait[Typeclass, A] =
    SealedTrait(
      typeInfo[A],
      IArray(subtypesFromMirror[A, m.MirroredElemTypes](m)*),
      IArray.from(anns[A]),
      IArray(paramTypeAnns[A]*),
      isEnum[A],
      IArray.from(inheritedAnns[A])
    )

  protected transparent inline def subtypesFromMirror[A, SubtypeTuple <: Tuple](
      m: Mirror.SumOf[A],
      result: List[SealedTrait.Subtype[Typeclass, A, _]] = Nil
  ): List[SealedTrait.Subtype[Typeclass, A, _]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        result.distinctBy(_.typeInfo).sortBy(_.typeInfo.full)
      case _: (s *: tail) =>
        val sub = summonFrom {
          case mm: Mirror.SumOf[`s`] =>
            subtypesFromMirror[A, mm.MirroredElemTypes](
              mm.asInstanceOf[m.type],
              Nil
            )
          case _ => {
            val tc = new SerializableFunction0[Typeclass[s]]:
              override def apply(): Typeclass[s] = summonFrom {
                case tc: Typeclass[`s`] => tc
                case _                  => deriveSubtype(summonInline[Mirror.Of[s]], summonInline[Ps[s]])
              }
            val isType = new SerializableFunction1[A, Boolean]:
              override def apply(a: A): Boolean = a.isInstanceOf[s & A]
            val asType = new SerializableFunction1[A, s & A]:
              override def apply(a: A): s & A = a.asInstanceOf[s & A]
            List(
              new SealedTrait.Subtype[Typeclass, A, s](
                typeInfo[s],
                IArray.from(anns[s]),
                IArray.from(inheritedAnns[s]),
                IArray.from(paramTypeAnns[A]),
                isObject[s],
                0, // unused
                CallByNeed.createLazy(tc),
                isType,
                asType
              )
            )
          }
        }
        subtypesFromMirror[A, tail](m, sub ::: result)
end ParamaterisedSealedTraitDerivation
