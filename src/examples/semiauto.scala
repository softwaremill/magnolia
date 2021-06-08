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
      println(s"PARAM [$param], TC: [${param.typeclass}] VALUE: [$value], DEREFED VALUE [${param.deref(value)}]")
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

  inline def derivedMirrorProduct[A](product: Mirror.ProductOf[A]): Typeclass[A] = ${ deriveForCaseClass[A]('join) }

  inline def getParams[T](using Mirror.ProductOf[T]): List[CaseClass.Param[Typeclass, T]] = ${ getParamsImpl[T] }

  // inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] =
  //   findCached[A] match {
  //     case Some(t) => t
  //     case None =>
  //       addInProgress[A]
  //       inline mirror match
  //         case sum: Mirror.SumOf[A]         => ???
  //         case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)
  //   }

  inline def derivedMirror[A](using mirror: Mirror.Of[A]): Typeclass[A] = ${ derivedImpl[A]('join)}

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
  //TypeName -> typeclasses accessors definitions (DefDef)
  val typeclassDefinitions: AtomicReference[Map[String, Any]] = new AtomicReference(Map.empty)
  
  //TypeName -> accessor reference (Symbol)
  val typeclassCache: AtomicReference[Map[String, Any]] =
    new AtomicReference(Map.empty)

    //Symbol
  val cachedRootSymbol: AtomicReference[Option[Any]] = new AtomicReference(None) 

  def derivedImpl[T: Type](join: Expr[CaseClass[SemiPrint, T] => SemiPrint[T]])(using q: Quotes): Expr[SemiPrint[T]] = {
    import q.reflect.*
    
    val name = TypeRepr.of[T].typeSymbol.name

    val r = typeclassCache.get.get(name) match {
      case Some(symbol) => 
        println(s"FOUND CACHED INSTANCES FOR [${name}]")
        Apply(Ref(symbol.asInstanceOf[Symbol]), List.empty).asExprOf[SemiPrint[T]] 
      case None => 
        println(s"NOT FOUND CACHED INSTANCES FOR [${name}]")

        deriveForCaseClass[T](join)
  }

  val b = Block(
      typeclassDefinitions.get.values.map(_.asInstanceOf[DefDef]).toList,
      '{$r}.asTerm
    )
    // println(s"Block: [${b.show}]")
    
    b.asExprOf[SemiPrint[T]]
}
  def deriveForCaseClass[T: Type](join: Expr[CaseClass[SemiPrint, T] => SemiPrint[T]])(using q: Quotes): Expr[SemiPrint[T]] = {
    import q.reflect.*
    Expr.summon[Mirror.ProductOf[T]] match {
      case None => ???
      case Some(m) => '{
        $addInProgressImpl
        
        val product = $m
        
        val parameters = IArray(${getParamsImpl[T]}*)
        val caseClass = new CaseClass[SemiPrint, T](typeInfo[T], isObject[T], isValueClass[T], parameters,
        IArray(anns[T]*), IArray[Any](typeAnns[T]*)) {
      
          def construct[PType](makeParam: Param => PType)(using ClassTag[PType]): T =
            product.fromProduct(Tuple.fromArray(this.params.map(makeParam(_)).to(Array)))

          def rawConstruct(fieldValues: Seq[Any]): T = product.fromProduct(Tuple.fromArray(fieldValues.to(Array)))

          def constructEither[Err, PType: ClassTag](makeParam: Param => Either[Err, PType]): Either[List[Err], T] =
            params.map(makeParam(_)).to(Array).foldLeft[Either[List[Err], Array[PType]]](Right(Array())) {
              case (Left(errs), Left(err))    => Left(errs ++ List(err))
              case (Right(acc), Right(param)) => Right(acc ++ Array(param))
              case (errs@Left(_), _)          => errs
              case (_, Left(err))             => Left(List(err))
            }.map { params => product.fromProduct(Tuple.fromArray(params)) }

          def constructMonadic[M[_]: Monadic, PType: ClassTag](makeParam: Param => M[PType]): M[T] =
            summon[Monadic[M]].map {
              params.map(makeParam(_)).to(Array).foldLeft(summon[Monadic[M]].point(Array())) {
                (accM, paramM) => summon[Monadic[M]].flatMap(accM) { acc =>
                  summon[Monadic[M]].map(paramM)(acc ++ List(_))
                }
              }
            } { params => product.fromProduct(Tuple.fromArray(params)) }
          }

      $removeInProgressImpl
      ${join}.apply(caseClass)
    }
  

    
  }
  }
  
  def getParamsImpl[T: Type](using q: Quotes): Expr[List[CaseClass.Param[SemiPrint, T]]] = {
    import q.reflect.*

    var i = -1
    val params = TypeRepr.of[T].typeSymbol.caseFields
    .map(_.tree)
    .map { case ValDef(_, tt, _) => tt }
    .map(_.tpe.asType)
    .map {
      case x@'[t] => {
        resolveTypeclasses[t]
        x
      }
    }
    .map {
      case '[t] => {
        val tpe = TypeRepr.of[t]
        val name = tpe.typeSymbol.name
        i += 1
        '{ 
                  val typeclass = ${getTypeclassImpl[t]}
                  val n = ${Expr(name)}
        println(s"NAME: [$n] TC: [$typeclass]")

        
          CaseClass.Param[SemiPrint, T, t](${Expr(name)}, ${Expr(i)}, false, typeclass,
            CallByNeed(None), IArray.from[Any](List()),
            IArray.from[Any](List())) }
      }
    }

    println(s"CACHE: [${typeclassCache.get.keySet.mkString(", ")}]")
    
    // val b = Block(
    //   typeclassDefinitions.get.map(_.asInstanceOf[DefDef]),
    //   Expr.ofList(params).asTerm
    // )
    // println(s"Block: [${b.show}]")
    // b.asExprOf[List[CaseClass.Param[SemiPrint, T]]]

    Expr.ofList(params)
  }
    
  def buildMethodSymbol[T: Type](using q: Quotes): q.reflect.Symbol = {
    import q.reflect.*

    val name = TypeRepr.of[T].typeSymbol.name
    val rootSymbol = (cachedRootSymbol.get match {
        case None => 
          val so = Symbol.spliceOwner
          cachedRootSymbol.updateAndGet(_ => Some(so))
          so
        case Some(so) => Symbol.spliceOwner
        // case Some(so) => so
      }).asInstanceOf[Symbol]
    
    Symbol.newMethod(
      rootSymbol,
      // s"${name}_impl",
      name,
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
    typeclassDefinitions.updateAndGet(_.+((name, defdef)))
    '{}
  }

  def addInProgressImpl[T: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name
    val defSymbol = buildMethodSymbol[T]
    val body = '{new SemiPrint[T] {
      def print(t: T): String = {
        ???
      }
    }}

    val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })

    typeclassCache.updateAndGet(_.+((name, defSymbol)))
    typeclassDefinitions.updateAndGet(_.+((name, defdef)))

    '{}
  }

  def removeInProgressImpl[T: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*  
    
    val name = TypeRepr.of[T].typeSymbol.name


    typeclassCache.updateAndGet(_.-(name))
    typeclassDefinitions.updateAndGet(_.-(name))

    '{}
  }

  def resolveTypeclasses[A: Type](using q: Quotes): Expr[Unit] = {
    import q.reflect.*
    
    def addNewAccessor[T: Type](name:String, body: Expr[SemiPrint[T]]): Unit = {
      println(s"Adding accessor for [$name] - CACHED DEFS: [${typeclassDefinitions.get.mkString(", ")}]")
      
      val defSymbol = buildMethodSymbol[T]
      val defdef = DefDef(defSymbol, { case t => Some(body.asTerm.changeOwner(defSymbol)) })

      typeclassCache.updateAndGet(_.+((name, defSymbol)))
      typeclassDefinitions.updateAndGet(_.+((name, defdef)))
    }

    val tpe = TypeRepr.of[A]
    val symbol = tpe.typeSymbol
    
    val ctx = typeclassCache.get

    ctx.get(symbol.name) match {
      case Some(_) => 
        println(s"FOUND INSTANCE FOR [${symbol.name}] IN CACHE")
        '{}
    
      case None => Expr.summon[SemiPrint[A]] match {
        case Some(expr) => 
          println(s"SUMMONED INSTANCE FOR [${symbol.name}]")
          addNewAccessor(symbol.name, expr)
          '{}
        case None => 
          // report.throwError(s"Not found instance fo ${symbol.name}")


          Expr.summon[Mirror.Of[A]] match {
            case Some(m: Expr[Mirror.Of[A]]) => 
            //TODO error
              println(s"SUMMONED MIRROR FOR [${symbol.name}]")
              addNewAccessor(symbol.name, derivedImpl[A]( '{(_: CaseClass[SemiPrint, A]) => new SemiPrint[A] {
                def print(a: A): String = "JOIN?"
              }} ) )
              '{ 
                // SemiPrint.derived[A](using $m)
                ()
              }
            case None => 
              println(s"MISSING INSTANCE [${symbol.name}]")
              // import scala.quoted.staging.Compiler; given Compiler = Compiler.make(this.getClass.getClassLoader)
              
                
              '{
                import scala.quoted.staging.Compiler; given Compiler = Compiler.make(SemiPrintDerivation.getClass.getClassLoader) 
                val r = scala.quoted.staging.run(nestedSummon[SemiPrint[A]]) 
                println(s"SEQ [${r}]")
                ???
              }
              
            
          }
      }
    }
  }

  def nestedSummon[T: Type](using q: Quotes) = {
    import q.reflect.*

    Implicits.search(TypeRepr.of[T]) match {
      case iss: ImplicitSearchSuccess => iss.tree.asExpr.asInstanceOf[Expr[T]]
      case isf: ImplicitSearchFailure => ???
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
    val name = TypeRepr.of[T].typeSymbol.name
    '{
      println(s"CALL [${${Expr(defSymbol.name)}} ] for [${${Expr(name)}}]")
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
        typeclassDefinitions.get.values.map(_.asInstanceOf[DefDef]).toList,
        result.asTerm
      ).asExprOf[T]

      println(s"FINAL BLOCK ${b.show}")
      b
  }
}