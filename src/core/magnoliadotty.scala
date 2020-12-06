package magnolia

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import mercator._

trait MagnoliaDerivation[Typeclass[_]] {
  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

  inline def getSubtypes[T, SubtypeTuple <: Tuple](m: Mirror.SumOf[T], idx: Int = 0): List[Subtype[Typeclass, T]] = inline erasedValue[SubtypeTuple] match {
    case _: EmptyTuple => Nil
    case _: (s *: tail) =>
      Subtype[Typeclass, T, s](
        TypeInfo[s],
        idx,
        Array.empty, // TODO: Are there even annotations that are not type or param annotations?
        TypeAnnotations[T].toArray,
        CallByNeed(summonInline[Typeclass[s]]),
        x => m.ordinal(x) == idx,
        _.asInstanceOf[s]
      ) :: getSubtypes[T, tail](m, idx + 1)
  }

  inline def getParams[T, Labels <: Tuple, Params <: Tuple](paramAnnotations: Map[String, List[Any]], idx: Int = 0): List[Param[Typeclass, T]] = inline erasedValue[(Labels, Params)] match {
    case _: (EmptyTuple, EmptyTuple) => Nil
    case _: ((l *: ltail), (p *: ptail)) =>
      Param[Typeclass, T, p](
        constValue[l].asInstanceOf[String],
        idx,
        false, // TODO - afaict not compatible with derivation via Scala 3 Mirror
        CallByNeed(summonInline[Typeclass[p]]),
        CallByNeed(None),
        paramAnnotations.getOrElse(constValue[l].asInstanceOf[String], List.empty).toArray,
        TypeAnnotations[T].toArray
      ) :: getParams[T, ltail, ptail](paramAnnotations, idx + 1)
  }

  inline given derived[A](using m: Mirror.Of[A]) as Typeclass[A] = {
    inline m match {
      case s: Mirror.SumOf[A] =>
        val st = new SealedTrait(
          typeInfo = TypeInfo[A],
          subtypesArray = getSubtypes[A, s.MirroredElemTypes](s).toArray,
          annotationsArray = Array.empty, // TODO: Are there even annotations that are not type or param annotations?
          typeAnnotationsArray = TypeAnnotations[A].toArray
        )
        dispatch(st)

      case p: Mirror.ProductOf[A] =>
        // TODO: Workaround for weired compiler error when passing arguments directly - Report as bug?
        val cc = new CaseClass[Typeclass, A](_: TypeInfo, _: Boolean, _: Boolean, _: Array[Param[Typeclass, A]], _: Array[Any], _: Array[Any]) {
          def construct[Return](makeParam: Param[Typeclass, A] => Return): A = ??? // TODO

          def constructMonadic[Monad[_], PType](makeParam: Param[Typeclass, A] => Monad[PType])(implicit monadic: Monadic[Monad]): Monad[A] = ??? // TODO

          def constructEither[Err, PType](makeParam: Param[Typeclass, A] => Either[Err, PType]): Either[List[Err], A] = ??? // TODO

          def rawConstruct(fieldValues: Seq[Any]): A = ??? // TODO
        }

        val ccparams = (
          /* typeInfo */ TypeInfo[A],
          /* isObject */ IsObject[A],
          /* isValueClass */ false, // TODO - unclear how to get this information
          /* parametersArray */ getParams[A, p.MirroredElemLabels, p.MirroredElemTypes](ParamAnnotations[A].toMap).toArray,
          /* annotationsArray */ Array.empty[Any], // TODO: Are there even annotations that are not type or param annotations?
          /* typeAnnotationsArray */ TypeAnnotations[A].toArray
        )

        combine(cc.tupled(ccparams))
    }
  }
}
