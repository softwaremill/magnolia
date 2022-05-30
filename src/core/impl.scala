package magnolia1

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*

import Macro.*

object CaseClassDerivation:
  inline def fromMirror[Typeclass[_], A](
      product: Mirror.ProductOf[A]
  ): CaseClass[Typeclass, A] =
    val parameters = IArray(
      paramsFromMaps[
        Typeclass,
        A,
        product.MirroredElemLabels,
        product.MirroredElemTypes
      ](
        paramAnns[A].to(Map),
        inheritedParamAnns[A].to(Map),
        paramTypeAnns[A].to(Map),
        repeated[A].to(Map),
        defaultValue[A].to(Map)
      )*
    )

    new CaseClass(
      typeInfo[A],
      isObject[A],
      isValueClass[A],
      parameters,
      IArray(anns[A]*),
      IArray(inheritedAnns[A]*),
      IArray[Any](typeAnns[A]*)
    ):
      def construct[PType: ClassTag](makeParam: Param => PType): A =
        product.fromProduct(Tuple.fromArray(params.map(makeParam).to(Array)))

      def rawConstruct(fieldValues: Seq[Any]): A =
        product.fromProduct(Tuple.fromArray(fieldValues.to(Array)))

      def constructEither[Err, PType: ClassTag](
          makeParam: Param => Either[Err, PType]
      ): Either[List[Err], A] =
        params
          .map(makeParam)
          .foldLeft[Either[List[Err], Array[PType]]](Right(Array())) {
            case (Left(errs), Left(err))    => Left(errs ++ List(err))
            case (Right(acc), Right(param)) => Right(acc ++ Array(param))
            case (errs @ Left(_), _)        => errs
            case (_, Left(err))             => Left(List(err))
          }
          .map { params => product.fromProduct(Tuple.fromArray(params)) }

      def constructMonadic[M[_]: Monadic, PType: ClassTag](
          makeParam: Param => M[PType]
      ): M[A] = {
        val m = summon[Monadic[M]]
        m.map {
          params.map(makeParam).foldLeft(m.point(Array())) { (accM, paramM) =>
            m.flatMap(accM) { acc =>
              m.map(paramM)(acc ++ List(_))
            }
          }
        } { params => product.fromProduct(Tuple.fromArray(params)) }
      }

  inline def paramsFromMaps[Typeclass[_], A, Labels <: Tuple, Params <: Tuple](
      annotations: Map[String, List[Any]],
      inheritedAnnotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      repeated: Map[String, Boolean],
      defaults: Map[String, Option[() => Any]],
      idx: Int = 0
  ): List[CaseClass.Param[Typeclass, A]] =
    inline erasedValue[(Labels, Params)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((l *: ltail), (p *: ptail)) =>
        val label = constValue[l].asInstanceOf[String]
        CaseClass.Param[Typeclass, A, p](
          label,
          idx,
          repeated.getOrElse(label, false),
          CallByNeed(summonInline[Typeclass[p]]),
          CallByNeed(defaults.get(label).flatten.map(_.apply.asInstanceOf[p])),
          IArray.from(annotations.getOrElse(label, List())),
          IArray.from(inheritedAnnotations.getOrElse(label, List())),
          IArray.from(typeAnnotations.getOrElse(label, List()))
        ) ::
          paramsFromMaps[Typeclass, A, ltail, ptail](
            annotations,
            inheritedAnnotations,
            typeAnnotations,
            repeated,
            defaults,
            idx + 1
          )
end CaseClassDerivation

trait SealedTraitDerivation[TypeClass[_]]:
  protected inline def deriveSubtype[s](m: Mirror.Of[s]): TypeClass[s]

  protected inline def sealedTraitFromMirror[A](
      m: Mirror.SumOf[A]
  ): SealedTrait[TypeClass, A] =
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
      idx: Int = 0
  ): List[SealedTrait.Subtype[TypeClass, A, _]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        Nil
      case _: (s *: tail) =>
        new SealedTrait.Subtype(
          typeInfo[s],
          IArray.from(anns[s]),
          IArray.from(inheritedAnns[s]),
          IArray.from(paramTypeAnns[A]),
          isObject[s],
          idx,
          CallByNeed(summonFrom {
            case tc: TypeClass[`s`] => tc
            case _ => deriveSubtype(summonInline[Mirror.Of[s]])
          }),
          x => m.ordinal(x) == idx,
          _.asInstanceOf[s & A]
        ) :: subtypesFromMirror[A, tail](m, idx + 1)
end SealedTraitDerivation