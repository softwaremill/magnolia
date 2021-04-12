/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package magnolia

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*

trait Derivation[TypeClass[_]]:

  type Typeclass[T] = TypeClass[T]

  def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

  transparent inline def getSubtypes[T, SubtypeTuple <: Tuple]
                                    (m: Mirror.SumOf[T], idx: Int = 0): List[Subtype[Typeclass, T]] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        Nil
      case _: (s *: tail) =>
        type subType = s
        
        Subtype[Typeclass, T, s](Macro.typeInfo[s], idx, IArray[Any](), IArray(Macro.paramTypeAnns[T].to(Array)*),
            CallByNeed(summonInline[Typeclass[s]]), x => m.ordinal(x) == idx, _.asInstanceOf[s & T]) ::
            getSubtypes[T, tail](m, idx + 1)

  inline def getParams[T, Labels <: Tuple, Params <: Tuple]
                      (anns: Map[String, List[Any]],
                       typeAnns: Map[String, List[Any]],
                       repeated: Map[String, Boolean],
                       idx: Int = 0
                      ): List[Param[Typeclass, T]] =
    inline erasedValue[(Labels, Params)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((l *: ltail), (p *: ptail)) =>
        val label = constValue[l].asInstanceOf[String]
        val typeclass = CallByNeed(summonInline[Typeclass[p]])

        Param[Typeclass, T, p](label, idx, repeated.getOrElse(label, false), typeclass, CallByNeed(None),
            IArray(anns.getOrElse(label, List()).to(Array)*), IArray(typeAnns.getOrElse(label, List()).to(Array)*)) ::
            getParams[T, ltail, ptail](anns, typeAnns, repeated, idx + 1)

  inline def derivedMirrorSum[A](sum: Mirror.SumOf[A]): Typeclass[A] =
    val sealedTrait = SealedTrait(Macro.typeInfo[A], IArray(getSubtypes[A, sum.MirroredElemTypes](sum).to(Array)*),
        IArray[Any](), IArray(Macro.paramTypeAnns[A].to(Array)*))
    
    split(sealedTrait)

  inline def derivedMirrorProduct[A](product: Mirror.ProductOf[A]): Typeclass[A] =
    val caseClass = new CaseClass[Typeclass, A](_: TypeInfo, _: Boolean, _: Boolean,
        _: IArray[Param[Typeclass, A]], _: IArray[Any], _: IArray[Any]):
      
      def construct[PType](makeParam: Param[Typeclass, A] => PType)(using ClassTag[PType]): A =
        product.fromProduct(Tuple.fromArray(this.params.map(makeParam(_)).to(Array)))

      def rawConstruct(fieldValues: Seq[Any]): A = product.fromProduct(Tuple.fromArray(fieldValues.to(Array)))

      def constructEither[Err, PType: ClassTag](makeParam: Param[Typeclass, A] => Either[Err, PType]): Either[List[Err], A] =
        val paramsEither = params.map(makeParam(_)).to(Array)
          .foldLeft[Either[List[Err], Array[PType]]](Right(Array())) { (accEither, paramEither) =>
            (accEither, paramEither) match
              case (Left(errs), Left(err)) => Left(errs ++ List(err))
              case (Right(acc), Right(param)) => Right(acc ++ Array(param))
              case (errs@Left(_), _) => errs
              case (_, Left(err)) => Left(List(err))
          }

        paramsEither.map { params => product.fromProduct(Tuple.fromArray(params)) }

      def constructMonadic[M[_]: Monadic, PType: ClassTag](makeParam: Param[Typeclass, A] => M[PType]): M[A] =
        val paramsM = params.map(makeParam(_)).to(Array).foldLeft(summon[Monadic[M]].point(Array())) { (accM, paramM) =>
          summon[Monadic[M]].flatMap(accM) { acc => summon[Monadic[M]].map(paramM)(acc ++ List(_)) }
        }
        
        summon[Monadic[M]].map(paramsM) { params => product.fromProduct(Tuple.fromArray(params)) }

    val ccParams = (Macro.typeInfo[A], Macro.isObject[A], Macro.isValueClass[A], IArray(getParams[A,
        product.MirroredElemLabels, product.MirroredElemTypes](Macro.paramAnns[A].to(Map),
        Macro.paramTypeAnns[A].to(Map), Macro.repeated[A].to(Map)).to(Array)*), IArray(Macro.anns[A].to(Array)*),
        IArray[Any]())

    join(caseClass.tupled(ccParams))

  inline def derivedMirror[A](using m: Mirror.Of[A]): Typeclass[A] = inline m match
    case s: Mirror.SumOf[A]     => derivedMirrorSum[A](s)
    case p: Mirror.ProductOf[A] => derivedMirrorProduct[A](p)

  inline given derived[A](using Mirror.Of[A]): Typeclass[A] = derivedMirror[A]