package magnolia1

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*

import Macro.*

// scala3 lambda generated during derivation reference outer scope
// This fails the typeclass serialization if the outer scope is not serializable
// workaround with this with a serializable fuction
private trait SerializableFunction0[+R] extends Function0[R] with Serializable:
  def apply(): R
private trait SerializableFunction1[-T1, +R] extends Function1[T1, R] with Serializable:
  def apply(v1: T1): R

object CaseClassDerivation:
  inline def fromMirror[Typeclass[_], A](
      product: Mirror.ProductOf[A]
  ): CaseClass[Typeclass, A] =
    val params = IArray(
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
    ProductCaseClass(
      typeInfo[A],
      isObject[A],
      isValueClass[A],
      params,
      IArray(anns[A]*),
      IArray(inheritedAnns[A]*),
      IArray[Any](typeAnns[A]*),
      product
    )

  class ProductCaseClass[Typeclass[_], A](
      typeInfo: TypeInfo,
      isObject: Boolean,
      isValueClass: Boolean,
      parameters: IArray[CaseClass.Param[Typeclass, A]],
      annotations: IArray[Any],
      inheritedAnnotations: IArray[Any],
      typeAnnotations: IArray[Any],
      product: Mirror.ProductOf[A]
  ) extends CaseClass[Typeclass, A](
        typeInfo,
        isObject,
        isValueClass,
        parameters,
        annotations,
        inheritedAnnotations,
        typeAnnotations
      ):
    def construct[PType: ClassTag](makeParam: Param => PType): A =
      product.fromProduct(Tuple.fromIArray(parameters.map(makeParam)))

    override def rawConstruct(fieldValues: Seq[Any]): A =
      rawConstruct(fieldValues.toArray)

    override def rawConstruct(fieldValues: Array[Any]): A =
      product.fromProduct(Tuple.fromArray(fieldValues))

    def constructEither[Err, PType: ClassTag](
        makeParam: Param => Either[Err, PType]
    ): Either[List[Err], A] =
      parameters
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
        parameters.map(makeParam).foldLeft(m.point(Array())) { (accM, paramM) =>
          m.flatMap(accM) { acc =>
            m.map(paramM)(acc ++ List(_))
          }
        }
      } { params => product.fromProduct(Tuple.fromArray(params)) }
    }

  inline def paramsFromMapsStep[Typeclass[_], A, l, p](
      annotations: Map[String, List[Any]],
      inheritedAnnotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      repeated: Map[String, Boolean],
      defaults: Map[String, Option[() => Any]],
      idx: Int
  ): CaseClass.Param[Typeclass, A] =
    val label = constValue[l].asInstanceOf[String]
    val tc = new SerializableFunction0[Typeclass[p]]:
      override def apply(): Typeclass[p] = summonInline[Typeclass[p]]
    val d =
      defaults.get(label).flatten match {
        case Some(evaluator) =>
          new SerializableFunction0[Option[p]]:
            override def apply(): Option[p] =
              val v = evaluator()
              if ((v: @unchecked).isInstanceOf[p]) new Some(v).asInstanceOf[Option[p]]
              else None
        case _ =>
          returningNone.asInstanceOf[SerializableFunction0[Option[p]]]
      }
    paramFromMaps[Typeclass, A, p](
      label,
      CallByNeed.createLazy(tc),
      CallByNeed.createValueEvaluator(d),
      repeated,
      annotations,
      inheritedAnnotations,
      typeAnnotations,
      idx
    )

  /** This method unrolls recursion by 16, 4, 1 steps to increase maximal size of `Labels` and `Params` tuples due to compiler limitation of
    * maximal nested inlines.
    */
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
      case _: (
              (l1 *: l2 *: l3 *: l4 *: l5 *: l6 *: l7 *: l8 *: l9 *: l10 *: l11 *: l12 *: l13 *: l14 *: l15 *: l16 *: ltail),
              (p1 *: p2 *: p3 *: p4 *: p5 *: p6 *: p7 *: p8 *: p9 *: p10 *: p11 *: p12 *: p13 *: p14 *: p15 *: p16 *: ptail)
          ) =>
        val s1 = paramsFromMapsStep[Typeclass, A, l1, p1](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx)
        val s2 = paramsFromMapsStep[Typeclass, A, l2, p2](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 1)
        val s3 = paramsFromMapsStep[Typeclass, A, l3, p3](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 2)
        val s4 = paramsFromMapsStep[Typeclass, A, l4, p4](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 3)
        val s5 = paramsFromMapsStep[Typeclass, A, l5, p5](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 4)
        val s6 = paramsFromMapsStep[Typeclass, A, l6, p6](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 5)
        val s7 = paramsFromMapsStep[Typeclass, A, l7, p7](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 6)
        val s8 = paramsFromMapsStep[Typeclass, A, l8, p8](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 7)
        val s9 = paramsFromMapsStep[Typeclass, A, l9, p9](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 8)
        val s10 =
          paramsFromMapsStep[Typeclass, A, l10, p10](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 9)
        val s11 =
          paramsFromMapsStep[Typeclass, A, l11, p11](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 10)
        val s12 =
          paramsFromMapsStep[Typeclass, A, l12, p12](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 11)
        val s13 =
          paramsFromMapsStep[Typeclass, A, l13, p13](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 12)
        val s14 =
          paramsFromMapsStep[Typeclass, A, l14, p14](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 13)
        val s15 =
          paramsFromMapsStep[Typeclass, A, l15, p15](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 14)
        val s16 =
          paramsFromMapsStep[Typeclass, A, l16, p16](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 15)

        s1 :: s2 :: s3 :: s4 :: s5 :: s6 :: s7 :: s8 :: s9 :: s10 :: s11 :: s12 :: s13 :: s14 :: s15 :: s16 ::
          paramsFromMaps[Typeclass, A, ltail, ptail](
            annotations,
            inheritedAnnotations,
            typeAnnotations,
            repeated,
            defaults,
            idx + 16
          )
      case _: ((l1 *: l2 *: l3 *: l4 *: ltail), (p1 *: p2 *: p3 *: p4 *: ptail)) =>
        val s1 = paramsFromMapsStep[Typeclass, A, l1, p1](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx)
        val s2 = paramsFromMapsStep[Typeclass, A, l2, p2](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 1)
        val s3 = paramsFromMapsStep[Typeclass, A, l3, p3](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 2)
        val s4 = paramsFromMapsStep[Typeclass, A, l4, p4](annotations, inheritedAnnotations, typeAnnotations, repeated, defaults, idx + 3)

        s1 :: s2 :: s3 :: s4 ::
          paramsFromMaps[Typeclass, A, ltail, ptail](
            annotations,
            inheritedAnnotations,
            typeAnnotations,
            repeated,
            defaults,
            idx + 4
          )
      case _: ((l *: ltail), (p *: ptail)) =>
        paramsFromMapsStep[Typeclass, A, l, p](
          annotations,
          inheritedAnnotations,
          typeAnnotations,
          repeated,
          defaults,
          idx
        ) ::
          paramsFromMaps[Typeclass, A, ltail, ptail](
            annotations,
            inheritedAnnotations,
            typeAnnotations,
            repeated,
            defaults,
            idx + 1
          )

  private def paramFromMaps[Typeclass[_], A, p](
      label: String,
      tc: CallByNeed[Typeclass[p]],
      d: CallByNeed[Option[p]],
      repeated: Map[String, Boolean],
      annotations: Map[String, List[Any]],
      inheritedAnnotations: Map[String, List[Any]],
      typeAnnotations: Map[String, List[Any]],
      idx: Int
  ): CaseClass.Param[Typeclass, A] =
    CaseClass.Param[Typeclass, A, p](
      label,
      idx,
      repeated.getOrElse(label, false),
      tc,
      d,
      IArray.from(annotations.getOrElse(label, List())),
      IArray.from(inheritedAnnotations.getOrElse(label, List())),
      IArray.from(typeAnnotations.getOrElse(label, List()))
    )

  private val returningNone =
    new SerializableFunction0[Option[Any]]:
      override def apply(): Option[Any] = None

end CaseClassDerivation

trait SealedTraitDerivation:
  type Typeclass[T]

  protected inline def deriveSubtype[s](m: Mirror.Of[s]): Typeclass[s]

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

  protected inline def subtypesFromMirrorStep[A, s](
      m: Mirror.SumOf[A],
      idx: Int
  ): List[SealedTrait.Subtype[Typeclass, A, _]] =
    summonFrom {
      case mm: Mirror.SumOf[`s`] =>
        subtypesFromMirror[A, mm.MirroredElemTypes](
          mm.asInstanceOf[m.type],
          0,
          Nil
        )
      case _ => {
        val tc = new SerializableFunction0[Typeclass[s]]:
          override def apply(): Typeclass[s] = summonFrom {
            case tc: Typeclass[`s`] => tc
            case _                  => deriveSubtype(summonInline[Mirror.Of[s]])
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
            IArray.from(typeAnns[s]),
            isObject[s],
            idx,
            CallByNeed.createLazy(tc),
            isType,
            asType
          )
        )
      }
    }

  /** This method unrolls recursion by 16, 4, 1 steps to increase maximal size of `SubtypeTuple` tuple due to compiler limitation of maximal
    * nested inlines.
    */
  protected transparent inline def subtypesFromMirror[A, SubtypeTuple <: Tuple](
      m: Mirror.SumOf[A],
      idx: Int = 0, // no longer used, kept for bincompat
      result: List[SealedTrait.Subtype[Typeclass, A, _]] = Nil
  ): List[SealedTrait.Subtype[Typeclass, A, _]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        result.distinctBy(_.typeInfo).sortBy(_.typeInfo.full)
      case _: (h1 *: h2 *: h3 *: h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: tail) =>
        val sub1 = subtypesFromMirrorStep[A, h1](m, idx)
        val sub2 = subtypesFromMirrorStep[A, h2](m, idx + 1)
        val sub3 = subtypesFromMirrorStep[A, h3](m, idx + 2)
        val sub4 = subtypesFromMirrorStep[A, h4](m, idx + 3)
        val sub5 = subtypesFromMirrorStep[A, h5](m, idx + 4)
        val sub6 = subtypesFromMirrorStep[A, h6](m, idx + 5)
        val sub7 = subtypesFromMirrorStep[A, h7](m, idx + 6)
        val sub8 = subtypesFromMirrorStep[A, h8](m, idx + 7)
        val sub9 = subtypesFromMirrorStep[A, h9](m, idx + 8)
        val sub10 = subtypesFromMirrorStep[A, h10](m, idx + 9)
        val sub11 = subtypesFromMirrorStep[A, h11](m, idx + 10)
        val sub12 = subtypesFromMirrorStep[A, h12](m, idx + 11)
        val sub13 = subtypesFromMirrorStep[A, h13](m, idx + 12)
        val sub14 = subtypesFromMirrorStep[A, h14](m, idx + 13)
        val sub15 = subtypesFromMirrorStep[A, h15](m, idx + 14)
        val sub16 = subtypesFromMirrorStep[A, h16](m, idx + 15)

        subtypesFromMirror[A, tail](
          m,
          idx + 16,
          sub1 ::: sub2 ::: sub3 ::: sub4 ::: sub5 ::: sub6 ::: sub7 ::: sub8 ::: sub9 ::: sub10 ::: sub11 ::: sub12 ::: sub13 ::: sub14 ::: sub15 ::: sub16 ::: result
        )
      case _: (h1 *: h2 *: h3 *: h4 *: tail) =>
        val sub1 = subtypesFromMirrorStep[A, h1](m, idx)
        val sub2 = subtypesFromMirrorStep[A, h2](m, idx + 1)
        val sub3 = subtypesFromMirrorStep[A, h3](m, idx + 2)
        val sub4 = subtypesFromMirrorStep[A, h4](m, idx + 3)
        subtypesFromMirror[A, tail](m, idx + 4, sub1 ::: sub2 ::: sub3 ::: sub4 ::: result)
      case _: (s *: tail) =>
        val sub = subtypesFromMirrorStep[A, s](m, idx)
        subtypesFromMirror[A, tail](m, idx + 1, sub ::: result)
end SealedTraitDerivation
