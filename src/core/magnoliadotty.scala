package magnolia

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.reflect.*

trait MagnoliaDerivation[TypeClass[_]] {

  type Typeclass[T] = TypeClass[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

  import scala.compiletime._
  transparent inline def getSubtypes[T, SubtypeTuple <: Tuple](m: Mirror.SumOf[T], idx: Int = 0): List[Subtype[Typeclass, T]] = inline erasedValue[SubtypeTuple] match {
    case _: EmptyTuple => Nil
    case _: (s *: tail) =>
      type subType = s
      Subtype[Typeclass, T, s](
        TypeInfo[s],
        idx,
        Array.empty,
        TypeAnnotations[T].toArray,
        CallByNeed(summonInline[Typeclass[s]]),
        x => m.ordinal(x) == idx,
        _.asInstanceOf[s]
      ) :: getSubtypes[T, tail](m, idx + 1)
  }

  inline def getParams[T, Labels <: Tuple, Params <: Tuple](
    paramAnnotations: Map[String, List[Any]],
    paramRepeated: Map[String, Boolean],
    idx: Int = 0
  ): List[Param[Typeclass, T]] = inline erasedValue[(Labels, Params)] match {
    case _: (EmptyTuple, EmptyTuple) => Nil
    case _: ((l *: ltail), (p *: ptail)) =>
      Param[Typeclass, T, p](
        constValue[l].asInstanceOf[String],
        idx,
        paramRepeated.getOrElse(constValue[l].asInstanceOf[String], false),
        CallByNeed(summonInline[Typeclass[p]]),
        CallByNeed(None), //TODO TASTY? There is a Flag HasDefault
        paramAnnotations.getOrElse(constValue[l].asInstanceOf[String], List.empty).toArray,
        TypeAnnotations[T].toArray
      ) :: getParams[T, ltail, ptail](paramAnnotations, paramRepeated, idx + 1)
  }

  inline def derivedMirrorSum[A](s: Mirror.SumOf[A]): Typeclass[A] = {
    val st = new SealedTrait(
      typeInfo = TypeInfo[A],
      subtypesArray = getSubtypes[A, s.MirroredElemTypes](s).toArray,
      annotationsArray = Array.empty,
      typeAnnotationsArray = TypeAnnotations[A].toArray
    )
    dispatch(st)
  }

  inline def derivedMirrorProduct[A](p: Mirror.ProductOf[A]): Typeclass[A] = {
    val cc = new CaseClass[Typeclass, A](_: TypeInfo, _: Boolean, _: Boolean, _: Array[Param[Typeclass, A]], _: Array[Any], _: Array[Any]) {
      def construct[PType](makeParam: Param[Typeclass, A] => PType)(using ClassTag[PType]): A = {
        p.fromProduct(
          Tuple.fromArray(
            this.parameters.map[PType](makeParam(_)).toArray
          )
        )
      }

      def rawConstruct(fieldValues: Seq[Any]): A = {
        p.fromProduct(
          Tuple.fromArray(
            fieldValues.toArray
          )
        )
      }

      def constructEither[Err, PType](makeParam: Param[Typeclass, A] => Either[Err, PType])(using ClassTag[PType]): Either[List[Err], A] = {
        val paramsEither = this
          .parameters
          .map[Either[Err, PType]]{ param =>
            makeParam(param)
          }
          .toArray
          .foldLeft[Either[List[Err], Array[PType]]](Right(Array.empty)) { (accEither, paramEither) =>
            (accEither, paramEither) match { 
              case (Left(errs), Left(err)) => Left(errs ++ List(err))
              case (Right(acc), Right(param)) => Right(acc ++ Array(param))
              case (errs@Left(_), _) => errs
              case (_, Left(err)) => Left(List(err))
            }
          }
        paramsEither.map { params =>
          p.fromProduct(
            Tuple.fromArray(
              params
            )
          )
        }
      }

      import magnolia.Monadic
      def constructMonadic[Monad[_], PType](makeParam: Param[Typeclass, A] => Monad[PType])(using monadic: Monadic[Monad], ct: ClassTag[PType]): Monad[A] = {
        val paramsM = this
          .parameters
          .map[Monad[PType]]{ param =>
            makeParam(param)
          }
          .toArray
          .foldLeft[Monad[Array[PType]]](monadic.point(Array.empty)) { (accM, paramM) =>
            monadic.flatMap(accM) { acc =>
              monadic.map(paramM) { param =>
                acc ++ List(param)
              }
            }
          }
        monadic.map(paramsM) { params =>
          p.fromProduct(
            Tuple.fromArray(
              params
            )
          )
        }
      }
    }

    val ccparams = (
      /* typeInfo */ TypeInfo[A],
      /* isObject */ IsObject[A],
      /* isValueClass */ IsValueClass[A],
      /* parametersArray */ getParams[A, p.MirroredElemLabels, p.MirroredElemTypes](ParamAnnotations[A].toMap, AreRepeatedParams[A].toMap).toArray,
      /* annotationsArray */ Array.empty[Any],
      /* typeAnnotationsArray */ TypeAnnotations[A].toArray
    )

    combine(cc.tupled(ccparams))
  }

  inline def derivedMirror[A](using m: Mirror.Of[A]): Typeclass[A] = {
    inline m match {
      case s: Mirror.SumOf[A] =>
        derivedMirrorSum[A](s)
      case p: Mirror.ProductOf[A] =>
        derivedMirrorProduct[A](p)
    }
  }

  inline given derived[A](using Mirror.Of[A]): Typeclass[A] = {
    derivedMirror[A]
  }
}
