package magnolia.examples

import scala.language.experimental.macros
import magnolia._
import java.util.concurrent.atomic.AtomicReference

trait SemiPrint[A]:
  def print(a: A): String

object SemiPrint extends SemiPrintDerivation:
  def join[T](ctx: CaseClass[Typeclass, T]): SemiPrint[T] = value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.print(param.deref(value))
    else ctx.params.map { param =>
      param.typeclass.print(param.deref(value))
    }.mkString(s"${ctx.typeInfo.short}(", ",", ")")

  override def split[T](ctx: SealedTrait[SemiPrint, T]): SemiPrint[T] =
    ctx.choose(_) { sub => sub.typeclass.print(sub.value) }
  
  given SemiPrint[String] with
    def print(s: String) = s
  
  given SemiPrint[Int] with
    def print(i: Int) = i.toString
  
  given seq[T](using sp: SemiPrint[T]): SemiPrint[Seq[T]] with
    def print(s: Seq[T]) = s.map(sp.print).mkString(", ")

import scala.quoted.*
import magnolia.examples.SemiPrint

// trait SemiPrintDerivation {
//     import SemiPrintDerivation.*

//     type Typeclass[T] = SemiPrint[T]

//     def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]
//     def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

//     inline def derived[A]: Typeclass[A] = ${derivedImpl[A]}
// }

// import scala.quoted.*

// object SemiPrintDerivation {

//     def derivedImpl[A: Type](using q: Quotes): Expr[SemiPrint[A]] = {
//         import q.reflect.{MatchType => MT, *}
//         /**
//          * PRODUCT
//          * 1. build list of params
//          * 2. for param in params
//          *      3. try to use already generated method for the given typeclass
//          *      4. if succeed return value
//          *          else try to summon matchType
//          *      5. if succeed add method which returns the summoned typeclass 
//          *         else trigger derivation (????)
//          **/


//         val tpe = TypeRepr.of[A]
//         val tpeSymbol = tpe.typeSymbol

//         val caseClassFields = tpeSymbol.memberFields
//         println(s"CASE CLASS FIELDS [$caseClassFields]")

//         val types = tpeSymbol.memberTypes
//         println(s"TYPES [$types]")

//         // val typeclasses = caseClass.fields.map(f => tpeSymbol.memberType(f.name))

//         // Expr.summon
//         // println(s"TypeClasses [${typeclasses.mkString(", ")}]")
//         // println(s"TypeClasses [${typeclasses.mkString(", ")}]")
//         def body: Term = '{
//             new SemiPrint[A] {
//                 override def print(a: A): String = {
//                     println("SPA")
//                     "SPA"
//                 }
//             }
//         }.asTerm

//         val mtlDefSymbol = Symbol.newMethod(Symbol.spliceOwner, "mtl", MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[A]]))
//         val decodeDef = DefDef(
//         mtlDefSymbol, {
//             case t =>
//             println(s"T: [$t]")
//             Some(body.changeOwner(mtlDefSymbol))
//         }
//         )

//         // println(s"DD: [$decodeDef]")
        
//         val e1 = '{
//             new SemiPrint[A] {
//               override def apply(a: Any): Boolean = 
//                 println("MTL")
//                 true
//             }
//         }

//         // val e1 = block.asTerm match {
//         //     case Inlined()
//         // }
        
//         // println(s"MTL: [${e1.asTerm}]")
        
//         val failingSP = '{ 
//             new SemiPrint[A] {
//               override def apply(a: Any): Boolean = 
//                 println("FaililngSP")
//                 ???
//             }
//         }

//         val mtlSymbol = Symbol.requiredMethod("mtL")
//         // println(s"mtlSymbol [${mtlSymbol}]")
//         // val ap = Apply(Select.unique(e1.asTerm, "mtl"), List.empty)
//         // val ap = Apply(Select(decodeDef.asTerm, mtlSymbol), List.empty)
//         val ap = Closure(Ref(mtlDefSymbol), None)
//         // println(s"AP [${ap}]")

    
//     // val statement = Select(DefDef(), )

//         // val a = Block(List(e1.asTerm), Expr.summon[MatchType[A]].getOrElse(failingMt).asTerm).asExprOf[MatchType[A]]
//         val a = Block(List(decodeDef), ap).asExprOf[() => SemiPrint[A]]
//         // println(a.asTerm)
//         '{
//             val fa = $a
//             fa()
//         }
//     }
// }

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import Macro.*

trait SemiPrintDerivation {
  import SemiPrintDerivation.*

  type Typeclass[T] = SemiPrint[T]
  def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T]
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T]

  inline def getTypeName[T]: String = ${ getTypeNameImpl[T] }

  inline def derivedMirrorProduct[A](product: Mirror.ProductOf[A]): Typeclass[A] =
    val parameters = IArray(getParams[A, product.MirroredElemLabels, product.MirroredElemTypes](
        paramAnns[A].to(Map), paramTypeAnns[A].to(Map), repeated[A].to(Map))(using product)*)
    
    val caseClass = new CaseClass[Typeclass, A](typeInfo[A], isObject[A], isValueClass[A], parameters,
        IArray(anns[A]*), IArray[Any](typeAnns[A]*)):
      
      def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): A =
        product.fromProduct(Tuple.fromArray(this.params.map(makeParam(_)).to(Array)))

      def rawConstruct(fieldValues: Seq[Any]): A = product.fromProduct(Tuple.fromArray(fieldValues.to(Array)))

      def constructEither[Err, PType: ClassTag](makeParam: Param => Either[Err, PType]): Either[List[Err], A] =
        params.map(makeParam(_)).to(Array).foldLeft[Either[List[Err], Array[PType]]](Right(Array())) {
          case (Left(errs), Left(err))    => Left(errs ++ List(err))
          case (Right(acc), Right(param)) => Right(acc ++ Array(param))
          case (errs@Left(_), _)          => errs
          case (_, Left(err))             => Left(List(err))
        }.map { params => product.fromProduct(Tuple.fromArray(params)) }

      def constructMonadic[M[_]: Monadic, PType: ClassTag](makeParam: Param => M[PType]): M[A] =
        summon[Monadic[M]].map {
          params.map(makeParam(_)).to(Array).foldLeft(summon[Monadic[M]].point(Array())) {
            (accM, paramM) => summon[Monadic[M]].flatMap(accM) { acc =>
              summon[Monadic[M]].map(paramM)(acc ++ List(_))
            }
          }
        } { params => product.fromProduct(Tuple.fromArray(params)) }

    join(caseClass)

  inline def getParams[T, Labels <: Tuple, Params <: Tuple]
                      (annotations: Map[String, List[Any]], typeAnnotations: Map[String, List[Any]],
                      repeated: Map[String, Boolean], idx: Int = 0)(using prod: Mirror.ProductOf[T]): List[CaseClass.Param[Typeclass, T]] =
    inline erasedValue[(Labels, Params)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((l *: ltail), (p *: ptail)) =>
        val label = constValue[l].asInstanceOf[String]
        val typeclasses =  resolveTypeclasses[p]
        val name = getTypeName[p]

        println(s"Looking for name [$name] in [${typeclasses.mkString(", ")}]")
        val typeclass = CallByNeed(typeclasses(name).asInstanceOf[Typeclass[p]])

        CaseClass.Param[Typeclass, T, p](label, idx, repeated.getOrElse(label, false), typeclass,
            CallByNeed(None), IArray.from(annotations.getOrElse(label, List())),
            IArray.from(typeAnnotations.getOrElse(label, List()))) ::
            getParams[T, ltail, ptail](annotations, typeAnnotations, repeated, idx + 1)

  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    findCached[A] match {
      case Some(t) => t
      case None => inline mirror match
        case sum: Mirror.SumOf[A]         => ???
        case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)
    }

  inline def findCached[A]: Option[SemiPrint[A]] = ${ findCachedImpl[A] }

  // inline def resolveTypeclasses[A](using m: Mirror.ProductOf[A]): Map[String, Typeclass[Any]] = ${ SemiPrintDerivation.resolveTypeclasses[A]}
  inline def resolveTypeclasses[A]: Map[String, Typeclass[Any]] = 
    ${ SemiPrintDerivation.resolveTypeclasses[A] }

    //FIXME derived[NonRecursive] -> derived[Recursive] works, derived[Recursive] -> derived[NonRecursive] fails
  inline def derived[A](using Mirror.Of[A]): Typeclass[A] = derivedMirror[A]
}

object SemiPrintDerivation {
  //typeclasses accessors definitions (DefDef)
  val typeclassDefinitions: AtomicReference[List[Any]] = new AtomicReference(List.empty)
  
  //TypeName -> accessor reference (Symbol)
  val typeclassCache: AtomicReference[Map[String, Any]] =
    new AtomicReference(Map.empty)

  def resolveTypeclasses[A: Type](using q: Quotes): Expr[Map[String, SemiPrint[Any]]] = {
    import q.reflect.*
    
    case class Definition(refSymbol: Symbol, body: Term) {
      val ref = Closure(Ref(refSymbol), None)
      val defdef = DefDef(
        refSymbol, {
            case t => Some(body.changeOwner(refSymbol))
        }
        )
    }

    def applySymbol[T: Type](symbol: Any) =  '{

      val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => Any] }
      closure().asInstanceOf[T]
    }

    def addNewAccessor[T](name:String, body: Expr[SemiPrint[T]]): Unit = {
      val defSymbol = Symbol.newMethod(Symbol.spliceOwner, name, MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[A]]))
      val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })

      typeclassCache.updateAndGet(_.+((name, defSymbol)))
      typeclassDefinitions.updateAndGet(_.appended(defdef))
    }

    def go[T: Type]: Unit = {
      val tpe = TypeRepr.of[T]
      val symbol = tpe.typeSymbol
      
      val ctx = typeclassCache.get


      // Expr.summon[Mirror.Of[T]] match {
      //   case Some(x: Expr[Mirror.Of[`T`]]) => '{
      //     $x match {
      //       case _: Mirror.Of[`T`] => 
      //       case _: Mirror.SumOf[`T`] => ???
      //     }
      //   }
      //   case None => println("NONE")
      // }
      // ???

      ctx.get(symbol.name) match {
        case Some(_) => println(s"FOUND INSTANCE FOR [${symbol.name}]")
        case None =>  Expr.summon[SemiPrint[T]] match {
          case Some(expr) => addNewAccessor(symbol.name, expr)
          case None => Expr.summon[Mirror.Of[T]] match {
            case Some(m: Expr[Mirror.Of[T]]) => addNewAccessor(symbol.name, '{SemiPrint.derived[T](using $m)}) 
            case None => println(s"MISSING INSTANCE [${symbol.name}]")
            ???
          }
        }
      }
    }

    
    go[A]

    val typeclasses = '{
      ${Expr.ofList(
      
      typeclassCache.get.toList.map {
        case (name, symbol) => '{(${Expr(name)}, ${ applySymbol[SemiPrint[Any]](symbol)})}
      }
    )}.toMap
    }

    Block(
      typeclassDefinitions.get.map(_.asInstanceOf[DefDef]), 
      typeclasses.asTerm
    ).asExprOf[Map[String, SemiPrint[Any]]]

  }

  def findCachedImpl[T: Type](using q: Quotes): Expr[Option[SemiPrint[T]]] = {
    import q.reflect.*
    
    def applySymbol[T: Type](symbol: Any) =  '{

      val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => T] }
      closure()
    }
    

    val symbol = TypeRepr.of[T].typeSymbol
    
    typeclassCache.get.get(symbol.name) match {
      case Some(symbol) => '{ Some(${applySymbol[SemiPrint[T]](symbol)})}
      case None => '{ None }
    }
     
    
  }
 
  def getTypeNameImpl[T: Type](using q: Quotes): Expr[String] = {
    import q.reflect.*

    val tpe = TypeRepr.of[T]

    Expr(tpe.typeSymbol.name)
  }
}