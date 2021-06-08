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
    val name = getTypeName[A]
    println(s"ADDING CACHED INSTANCES OF [$name] - [$result]")

    // cacheInstance(result)
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
        
        val typeclass = getTypeclass[p]

        CaseClass.Param[Typeclass, T, p](label, idx, repeated.getOrElse(label, false), typeclass,
            CallByNeed(None), IArray.from(annotations.getOrElse(label, List())),
            IArray.from(typeAnnotations.getOrElse(label, List()))) ::
            getParams[T, ltail, ptail](annotations, typeAnnotations, repeated, idx + 1)

  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    findCached[A] match {
      case Some(t) => t
      case None =>
        addInProgress[A]
        inline mirror match
          case sum: Mirror.SumOf[A]         => ???
          case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)
    }

  inline def findCached[A]: Option[SemiPrint[A]] = ${ findCachedImpl[A] }

  inline def resolveTypeclasses[A]: Unit = ${ SemiPrintDerivation.resolveTypeclasses[A] }

  inline def addInProgress[A]: Unit = ${ addInProgressImpl[A] }
  inline def getTypeclasses = ${ getTypeclassesImpl }
  inline def getTypeclass[T] = ${ getTypeclassImpl[T] }
    //FIXME derived[NonRecursive] -> derived[Recursive] works, derived[Recursive] -> derived[NonRecursive] fails
  inline def derived[A](using Mirror.Of[A]): Typeclass[A] = {
    addDefinitions(() =>  derivedMirror[A])
  }

  inline def addDefinitions[T](f: () => T) = ${ addDefinitionsImpl[T]('f) }


  inline def cacheInstance[T](instance: SemiPrint[T]) = ${ cacheInstanceImpl[T]('instance) }
}

object SemiPrintDerivation {
  //typeclasses accessors definitions (DefDef)
  val typeclassDefinitions: AtomicReference[List[Any]] = new AtomicReference(List.empty)
  
  //TypeName -> accessor reference (Symbol)
  val typeclassCache: AtomicReference[Map[String, Any]] =
    new AtomicReference(Map.empty)

    //Symbol
  val cachedRootSymbol: AtomicReference[Option[Any]] = new AtomicReference(None) 

  def buildMethodSymbol[T: Type](using q: Quotes): q.reflect.Symbol = {
    import q.reflect.*

    val name = TypeRepr.of[T].typeSymbol.name
    val rootSymbol = (cachedRootSymbol.get match {
        case None => 
          val so = Symbol.spliceOwner
          cachedRootSymbol.updateAndGet(_ => Some(so))
          so
        case Some(so) => Symbol.spliceOwner
      }).asInstanceOf[Symbol]
    
    Symbol.newMethod(
      rootSymbol,
      s"${name}_impl",
      MethodType(List())(_ => List(), _ => TypeRepr.of[SemiPrint[T]]), 
      Flags.Given,
      Symbol.noSymbol
    )

  }
  
  def cacheInstanceImpl[T: Type](body: Expr[SemiPrint[T]])(using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name
    val defSymbol = buildMethodSymbol[T]

    println(s"ADDING TO CACHE [$name]")

    val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })
    typeclassCache.updateAndGet(_.+((name, defSymbol)))
    typeclassDefinitions.updateAndGet(_.appended(defdef))
    '{}
  }

  def addInProgressImpl[T: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name
    val defSymbol = buildMethodSymbol[T]
    
    typeclassCache.updateAndGet(_.+((name, defSymbol)))

    '{}
  }

  def resolveTypeclasses[A: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*
    
    def addNewAccessor[T: Type](name:String, body: Expr[SemiPrint[T]]): Unit = {
      println(s"Adding accessor for [$name]")
      
      val defSymbol = buildMethodSymbol[T]
      val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })

      typeclassCache.updateAndGet(_.+((name, defSymbol)))
      typeclassDefinitions.updateAndGet(_.appended(defdef))
    }

    val tpe = TypeRepr.of[A]
    val symbol = tpe.typeSymbol
    
    val ctx = typeclassCache.get

    ctx.get(symbol.name) match {
      case Some(_) => 
        println(s"FOUND INSTANCE FOR [${symbol.name}] IN CACHE")
        '{}
      case None => Expr.summon[SemiPrint[A]](using summon[Type[SemiPrint[A]]])(using q) match {
        case Some(expr) => 
          println(s"SUMMONED INSTANCE FOR [${symbol.name}]")
          addNewAccessor(symbol.name, expr)
          '{}
        case None => 
          report.throwError(s"Not found instance fo ${symbol.name}")

          // Expr.summon[Mirror.Of[A]] match {
          //   case Some(m: Expr[Mirror.Of[A]]) => 
          //   //TODO error
          //     println(s"SUMMONED MIRROR FOR [${symbol.name}]")
          //     addNewAccessor(symbol.name, '{ SemiPrint.derived[A](using $m)} )

          //     '{ 
          //       // SemiPrint.derived[A](using $m)
          //       ()
          //     }
          //   case None => println(s"MISSING INSTANCE [${symbol.name}]")
          //   ???
          // }
      }
    }
  }

  def getTypeclassesImpl(using q: Quotes): Expr[Map[String, SemiPrint[Any]]] = {
    import q.reflect.*

    Expr.ofList(
      typeclassCache.get.toList.map {
        case (name, symbol) => '{(${Expr(name)}, ${ Apply(Ref(symbol.asInstanceOf[Symbol]), List.empty).asExprOf[() => SemiPrint[Any]] })}
      }
    ).asExprOf[Map[String, SemiPrint[Any]]]
  }

  def getTypeclassImpl[T: Type](using q: Quotes): Expr[CallByNeed[SemiPrint[T]]] = {
    import q.reflect.*

    val defSymbol = typeclassCache.get.apply(TypeRepr.of[T].typeSymbol.name).asInstanceOf[Symbol]
    
    '{
      CallByNeed(${Apply(Ref(defSymbol), List.empty).asExprOf[SemiPrint[T]]})
    }
  }

  def findCachedImpl[T: Type](using q: Quotes): Expr[Option[SemiPrint[T]]] = {
    import q.reflect.*
    
    val name = TypeRepr.of[T].typeSymbol.name

    typeclassCache.get.get(name) match {
      case Some(symbol) => 
        println(s"FOUND CACHED INSTANCES FOR [${name}]")
        '{ Some(${ Apply(Ref(symbol.asInstanceOf[Symbol]), List.empty).asExprOf[SemiPrint[T]] })}
      case None => 
        println(s"NOT FOUND CACHED INSTANCES FOR [${name}]")
        '{ None }
    }
    
  }
 
  def getTypeNameImpl[T: Type](using q: Quotes): Expr[String] = {
    import q.reflect.*

    val tpe = TypeRepr.of[T]

    Expr(tpe.typeSymbol.name)
  }

  def addDefinitionsImpl[T: Type](fe: Expr[() => T])(using q: Quotes): Expr[T] = {
    import q.reflect.*
    println(s"Add def ")
    val result = '{
      val f = $fe
      f()
    }
      val b = Block(
        typeclassDefinitions.get.map(_.asInstanceOf[DefDef]),
        result.asTerm
      ).asExprOf[T]

      println(s"FINAL BLOCK $b")
      b
  }
}