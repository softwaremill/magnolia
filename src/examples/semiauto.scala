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
  
    //FIXME in recursive case, compiler is not able to provide instance for SemiPrint[T] implicitly
  given seq[T](using sp: SemiPrint[T]): SemiPrint[Seq[T]] with
    def print(s: Seq[T]) = s.map(sp.print).mkString(", ")

import scala.quoted.*
import magnolia.examples.SemiPrint

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

    val result = join(caseClass)
    cacheInstance(result)
    result


  inline def getParams[T, Labels <: Tuple, Params <: Tuple]
                      (annotations: Map[String, List[Any]], typeAnnotations: Map[String, List[Any]],
                      repeated: Map[String, Boolean], idx: Int = 0)(using prod: Mirror.ProductOf[T]): List[CaseClass.Param[Typeclass, T]] =

    inline erasedValue[(Labels, Params)] match
      case _: (EmptyTuple, EmptyTuple) =>
        Nil
      case _: ((l *: ltail), (p *: ptail)) =>
        val label = constValue[l].asInstanceOf[String]
        resolveTypeclasses[p]
        
        val name = getTypeName[p]

        println(s"Looking for name [$name]")
        val typeclass = CallByNeed(getTypeclass[p].asInstanceOf[Typeclass[p]])

        CaseClass.Param[Typeclass, T, p](label, idx, repeated.getOrElse(label, false), typeclass,
            CallByNeed(None), IArray.from(annotations.getOrElse(label, List())),
            IArray.from(typeAnnotations.getOrElse(label, List()))) ::
            getParams[T, ltail, ptail](annotations, typeAnnotations, repeated, idx + 1)

  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    findCached[A] match {
      case Some(t) => t
      case None =>
        // addInProgress[A]
        inline mirror match
          case sum: Mirror.SumOf[A]         => ???
          case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)
    }

  inline def findCached[A]: Option[SemiPrint[A]] = ${ findCachedImpl[A] }

  // inline def resolveTypeclasses[A](using m: Mirror.ProductOf[A]): Map[String, Typeclass[Any]] = ${ SemiPrintDerivation.resolveTypeclasses[A]}
  // inline def resolveTypeclasses[A]: Map[String, Typeclass[Any]] = 
  inline def resolveTypeclasses[A]: Unit = 
    ${ SemiPrintDerivation.resolveTypeclasses[A] }

  inline def addInProgress[A]: Unit = ${ addInProgressImpl[A] }
  inline def getTypeclasses = ${ getTypeclassesImpl }
  inline def getTypeclass[T] = ${ getTypeclassImpl[T] }
    //FIXME derived[NonRecursive] -> derived[Recursive] works, derived[Recursive] -> derived[NonRecursive] fails
  inline def derived[A](using Mirror.Of[A]): Typeclass[A] = {
    derivedMirror[A]
  }

  inline def derivedAny[A]: Typeclass[A] = {
    derivedMirror[A](using summonMirror[A])
  }

  inline def summonMirror[T]: Mirror.Of[T] = ${ summonMirrorImpl[T] }

  inline def cacheInstance[T](instance: SemiPrint[T]) = ${ cacheInstanceImpl[T]('instance) }
}

object SemiPrintDerivation {
  def summonMirrorImpl[T: Type](using q: Quotes) = {
    Expr.summon[Mirror.Of[T]].get
  }
  //typeclasses accessors definitions (DefDef)
  val typeclassDefinitions: AtomicReference[List[Any]] = new AtomicReference(List.empty)
  
  //TypeName -> accessor reference (Symbol)
  val typeclassCache: AtomicReference[Map[String, Any]] =
    new AtomicReference(Map.empty)

  def cacheInstanceImpl[T: Type](body: Expr[SemiPrint[T]])(using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name
    println(s"ADDING TO CACHE INSTANCE [$name]")

    val defSymbol = Symbol.newMethod(
      Symbol.noSymbol,
      // Symbol.spliceOwner,
      name,
      MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[T]]), 
      // Flags.Given,
      // Symbol.noSymbol
      )


    val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })
    typeclassCache.updateAndGet(_.+((name, defSymbol)))
    typeclassDefinitions.updateAndGet(_.appended(defdef))
    '{}
  }

  def addInProgressImpl[T: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name
    val defSymbol = Symbol.newMethod(
      // Symbol.spliceOwner,
      Symbol.noSymbol,
      name,
      MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[T]]), 
      // Flags.Given,
      // Symbol.noSymbol
      )
    val body = '{
      new SemiPrint[T] {
        def print(a: T): String = ???
      }
    }

    val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })
    typeclassCache.updateAndGet(_.+((name, defSymbol)))
    // typeclassDefinitions.updateAndGet(_.appended(defdef))

    '{}
  }

  // def resolveTypeclasses[A: Type](using q: Quotes): Expr[Map[String, SemiPrint[Any]]] = {
  def resolveTypeclasses[A: Type](using q: Quotes): Expr[Unit] = {
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
      println(s"Adding accessor for [$name]")
      val defSymbol = Symbol.newMethod(
        Symbol.spliceOwner,
        name,
        MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[A]]),
        // Flags.Given,
        // Symbol.noSymbol
        )
      val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })
      // val defdef = ValDef.let(defSymbol, { body.asTerm.changeOwner(defSymbol) })

      // println(s"UPDATE BEFORE [${typeclassCache}]")
      typeclassCache.updateAndGet(_.+((name, defSymbol)))
      typeclassDefinitions.updateAndGet(_.appended(defdef))
    }

    // def localSummon[A]: Expr[Option[SemiPrint[A]]] = {
    //   import q.reflect.*

    //   Expr.summon[SemiPrint[A]] match {
    //     case s@Some(expr) => '{Some($expr)}
    //     case None => '{None}
    //   }
    // }

    val tpe = TypeRepr.of[A]
    val symbol = tpe.typeSymbol
    
    val ctx = typeclassCache.get

    // ctx.get(symbol.name) match {
    //   case Some(_) => println(s"FOUND INSTANCE FOR [${symbol.name}] IN CACHE")
    //   case None => '{
    //     //TODO extract to separate level
    //     ${ localSummon[A] } match {
    //       case Some(semiPrint) => 
    //       println(s"SUMMONED INSTANCE FOR [${symbol.name}]")
    //       addNewAccessor(symbol.name, '{semiPrint})
    //       //TODO add implicit definitions from typeclassDefinitions to the scope 
    //     case None => Expr.summon[Mirror.Of[A]] match {
    //       case Some(m: Expr[Mirror.Of[A]]) => 
    //         println(s"SUMMONED MIRROR FOR [${symbol.name}]")
    //         addNewAccessor(symbol.name, '{SemiPrint.derived[A](using $m)}) 
    //       case None => println(s"MISSING INSTANCE [${symbol.name}]")
    //       ???
    //     }
    //     }
    //   }
    // }


    val op = ctx.get(symbol.name) match {
      case Some(_) => 
        println(s"FOUND INSTANCE FOR [${symbol.name}] IN CACHE")
        '{}
      case None => Expr.summon[SemiPrint[A]] match {
        case Some(expr) => 
          println(s"SUMMONED INSTANCE FOR [${symbol.name}]")
          addNewAccessor(symbol.name, expr)
          '{}
          //TODO add implicit definitions from typeclassDefinitions to the scope 
        case None => 
          println(s"NOT FOUND INSTANCE FOR [${symbol.name}]")

          Expr.summon[Mirror.Of[A]] match {
            case Some(m: Expr[Mirror.Of[A]]) => 
              println(s"SUMMONED MIRROR FOR [${symbol.name}]")
              '{ SemiPrint.derived[A](using $m) }
              // addNewAccessor(symbol.name, '{SemiPrint.derived[A](using $m)}) 
            case None => println(s"MISSING INSTANCE [${symbol.name}]")
            ???
          }
      }
    }

    // val typeclasses = '{
    //   ${Expr.ofList(
    //   typeclassCache.get.toList.map {
    //     case (name, symbol) => '{(${Expr(name)}, ${ applySymbol[SemiPrint[Any]](symbol)})}//TODO run derivation only once
    //   }
    // )}.toMap
    // }

    // println(s"DEFS [${typeclassDefinitions.get.mkString("\n")}]")
    // val block =Block(
    //   op.asTerm :: typeclassDefinitions.get.map(_.asInstanceOf[DefDef]), 
    //   typeclasses.asTerm
    // ).asExprOf[Map[String, SemiPrint[Any]]]

    val block = Block(
      List(op.asTerm), 
      '{}.asTerm
    ).asExprOf[Unit]

    // println(block.show)

    block
  }

  def getTypeclassesImpl(using q: Quotes): Expr[Map[String, SemiPrint[Any]]] = {
    import q.reflect.*


    def applySymbol[T: Type](symbol: Any) =  '{

      val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => Any] }
      closure().asInstanceOf[T]
    }

    val typeclasses = '{
      ${Expr.ofList(
      typeclassCache.get.toList.map {
        case (name, symbol) => '{(${Expr(name)}, ${ applySymbol[SemiPrint[Any]](symbol)})}
      }
    )}.toMap
    }

    // println(s"DEFS [${typeclassDefinitions.get.mkString("\n")}]")
    Block(
      typeclassDefinitions.get.map(_.asInstanceOf[DefDef]), 
      typeclasses.asTerm
    ).asExprOf[Map[String, SemiPrint[Any]]]
  }

  def getTypeclassImpl[T: Type](using q: Quotes): Expr[SemiPrint[Any]] = {
    import q.reflect.*


    def applySymbol[T: Type](symbol: Any) =  '{

      val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => Any] }
      closure().asInstanceOf[T]
    }
//  val symbol = typeclassCache.get.apply($name) 
    // val typeclasses = 
    // println(s"DEFS [${typeclassDefinitions.get.mkString("\n")}]")
        val defSymbol = Symbol.newMethod(
      // Symbol.spliceOwner,
      Symbol.noSymbol,
      TypeRepr.of[T].typeSymbol.name,
      MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[T]]), 
      // Flags.Given,
      // Symbol.noSymbol
      )

    Block(
      typeclassDefinitions.get.map(_.asInstanceOf[DefDef]), 
      
       
        // val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => Any] }
        Apply(Ref(defSymbol), List.empty)
        
       
    ).asExprOf[SemiPrint[T]].asInstanceOf[Expr[SemiPrint[Any]]]
    
  }

  def findCachedImpl[T: Type](using q: Quotes): Expr[Option[SemiPrint[T]]] = {
    import q.reflect.*
    
    def applySymbol[T: Type](symbol: Any) =  {
      val methodCall = '{
        val closure = ${ Closure(Ref(symbol.asInstanceOf[Symbol]), None).asExprOf[() => T] }
        closure()
      }
      Block(
      typeclassDefinitions.get.map(_.asInstanceOf[DefDef]), 
      methodCall.asTerm
      ).asExprOf[T]
    } 
    

    val s = TypeRepr.of[T].typeSymbol
    typeclassCache.get.get(s.name) match {
      case Some(symbol) => 
          println(s"FOUND CACHED INSTANCES FOR [${s.name}]")
        '{ Some(${applySymbol[SemiPrint[T]](symbol)})}
      case None => 
      println(s"NOT FOUND CACHED INSTANCES FOR [${s.name}]")
      '{ None }
    }
     
    
  }
 
  def getTypeNameImpl[T: Type](using q: Quotes): Expr[String] = {
    import q.reflect.*

    val tpe = TypeRepr.of[T]

    Expr(tpe.typeSymbol.name)
  }
}