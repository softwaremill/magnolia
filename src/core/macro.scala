package magnolia1

import scala.quoted.*

object Macro:
  inline def isObject[T]: Boolean = ${ isObject[T] }
  inline def isEnum[T]: Boolean = ${ isEnum[T] }
  inline def anns[T]: List[Any] = ${ anns[T] }
  inline def inheritedAnns[T]: List[Any] = ${ inheritedAnns[T] }
  inline def typeAnns[T]: List[Any] = ${ typeAnns[T] }
  inline def paramAnns[T]: List[(String, List[Any])] = ${ paramAnns[T] }
  inline def inheritedParamAnns[T]: List[(String, List[Any])] = ${
    inheritedParamAnns[T]
  }
  inline def isValueClass[T]: Boolean = ${ isValueClass[T] }
  inline def defaultValue[T]: List[(String, Option[() => Any])] = ${
    defaultValue[T]
  }
  inline def paramTypeAnns[T]: List[(String, List[Any])] = ${ paramTypeAnns[T] }
  inline def repeated[T]: List[(String, Boolean)] = ${ repeated[T] }
  inline def typeInfo[T]: TypeInfo = ${ typeInfo[T] }

  inline def getValueClassParam[TC[_], A]: CaseClass.Param[TC, A] = ${ getValueClassParamImpl[TC, A] }

  def getValueClassParamImpl[TC[_]: Type, A: Type](using Quotes): Expr[CaseClass.Param[TC, A]] = 
    import quotes.reflect.*

    def getDefaultValue(dict: Map[String, Expr[Any]], name: String): Expr[CallByNeed[Option[Any]]] = 
      dict.get(name) match 
        case Some(e) => '{ new CallByNeed(() => Some($e)) }
        case None => '{ new CallByNeed(() => None) }


    extension (term: Term)
      def asCallByNeed(tpe: TypeRepr): Term = 
        Select(
          '{CallByNeed}.asTerm, 
          TypeRepr.of[CallByNeed.type].termSymbol.declaredMethod("apply").head
          )
          .appliedToType(tpe)
          .appliedTo(term)

    // TODO most likely to be deleted
    extension (tpe: TypeRepr)
      def defaultValues: Map[String, Expr[Any]] = 
        val sym = tpe.typeSymbol
        val compModule = Ref(sym.companionModule)
        val names = 
          for 
            a <- sym.caseFields if a.flags.is(Flags.HasDefault)
          yield a.name
        val statements = sym.companionClass.tree.asInstanceOf[ClassDef].body
        val refs: List[Ref] = 
          for 
            case defdef @ DefDef(name, _, _, _) <- statements if name.startsWith("$lessinit$greater$default")
          yield compModule.select(defdef.symbol)

        val refsTerms = refs.map(_.asExpr)

        (names zip refsTerms).toMap 

    extension [B: Type](e: Expr[B])
      def asCallByNeedExpr: Expr[CallByNeed[B]] = 
        '{ new CallByNeed[B](() => $e) }
        

    val tpe: TypeRepr = TypeRepr.of[A]
    val sym: Symbol = tpe.typeSymbol
    val ctor: Symbol = sym.primaryConstructor
    val defaultValuesDict = tpe.defaultValues

    ctor.paramSymss match 
      case List(paramSymbol: Symbol) :: Nil => 
        val paramTypeTree = 
          paramSymbol.tree match 
            case v: ValDef => v.tpt
            case _ => report.errorAndAbort("Error handling param symbol tree '${paramSymbol.tree}'")
        val paramTypeTpe = paramTypeTree.tpe

        // instance of TC[P]
        val tcOfParamType = AppliedType(TypeRepr.of[TC], List(paramTypeTpe))
        val paramCallByNeed = tcOfParamType.asType match 
          case '[t] => 
            // Debug.printTypeStruct[t]
            Expr.summon[t].map{ e => Debug.printTreeImpl(e); e.asCallByNeedExpr }.getOrElse {
              report.errorAndAbort(s"Cannot summon instance for ${Type.show[t]}")
          }

        // default value
        val dv = getDefaultValue(defaultValuesDict, paramSymbol.name)

        val applyFuncTerm = 
          '{CaseClass.Param}
            .asTerm
            .select(TypeRepr.of[CaseClass.Param.type].termSymbol.declaredMethod("apply").head)
            .appliedToTypes(TypeRepr.of[TC] :: TypeRepr.of[A] :: paramTypeTree.tpe :: Nil)

        val args = List(
          Expr(paramSymbol.name).asTerm,            // name
          Expr(0).asTerm,                           // index
          Expr(false).asTerm,                       // repeated
          paramCallByNeed.asTerm,                   // call-by-need instance of TC[P]: CallByNeed[TC[P]]
          dv.asTerm,                                // call-by-need isntance of maybe default value: CallByNeed[Option[P]]
          Expr.ofList(List.empty).asTerm,           // annotations TODO 
          Expr.ofList(List.empty).asTerm,           // inherited annotations TODO 
          Expr.ofList(List.empty).asTerm            // type annotations TODO
        )

        val app = 
          Apply(
            fun = applyFuncTerm, 
            args = args
            )

        app.asExprOf[CaseClass.Param[TC, A]]
      
      case _ => report.errorAndAbort("Error handling symbol '${sym.name}'")
    
  def isObject[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))

  def isEnum[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))

  def anns[T: Type](using Quotes): Expr[List[Any]] =
    new CollectAnnotations[T].anns

  def inheritedAnns[T: Type](using Quotes): Expr[List[Any]] =
    new CollectAnnotations[T].inheritedAnns

  def typeAnns[T: Type](using Quotes): Expr[List[Any]] =
    new CollectAnnotations[T].typeAnns

  def paramAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[T].paramAnns

  def inheritedParamAnns[T: Type](using
      Quotes
  ): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[T].inheritedParamAnns

  def isValueClass[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(
      TypeRepr.of[T].baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))
    )

  def defaultValue[T: Type](using
      Quotes
  ): Expr[List[(String, Option[() => Any])]] =
    import quotes.reflect._
    def exprOfOption(
        oet: (Expr[String], Option[Expr[Any]])
    ): Expr[(String, Option[() => Any])] = oet match {
      case (label, None)     => Expr(label.valueOrAbort -> None)
      case (label, Some(et)) => '{ $label -> Some(() => $et) }
    }
    val tpe = TypeRepr.of[T].typeSymbol
    val terms = tpe.primaryConstructor.paramSymss.flatten
      .filter(_.isValDef)
      .zipWithIndex
      .map { case (field, i) =>
        exprOfOption {
          Expr(field.name) -> tpe.companionClass
            .declaredMethod(s"$$lessinit$$greater$$default$$${i + 1}")
            .headOption
            .flatMap(_.tree.asInstanceOf[DefDef].rhs)
            .map(_.asExprOf[Any])
        }
      }
    Expr.ofList(terms)

  def paramTypeAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    import quotes.reflect._

    def getAnnotations(t: TypeRepr): List[Term] = t match
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _                         => Nil

    Expr.ofList {
      TypeRepr
        .of[T]
        .typeSymbol
        .caseFields
        .map { field =>
          val tpeRepr = field.tree match
            case v: ValDef => v.tpt.tpe
            case d: DefDef => d.returnTpt.tpe

          Expr(field.name) -> getAnnotations(tpeRepr)
            .filter { a =>
              a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
              a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
            }
            .map(_.asExpr.asInstanceOf[Expr[Any]])
        }
        .filter(_._2.nonEmpty)
        .map { (name, annots) => Expr.ofTuple(name, Expr.ofList(annots)) }
    }

  def repeated[T: Type](using Quotes): Expr[List[(String, Boolean)]] =
    import quotes.reflect.*

    def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match
      case a: AnnotatedType =>
        a.annotation.tpe match
          case tr: TypeRef => tr.name == "Repeated"
          case _           => false
      case _ => false

    val tpe = TypeRepr.of[T]
    val symbol: Option[Symbol] =
      if tpe.typeSymbol.isNoSymbol then None else Some(tpe.typeSymbol)
    val constr: Option[DefDef] =
      symbol.map(_.primaryConstructor.tree.asInstanceOf[DefDef])

    val areRepeated = constr.toList
      .flatMap(_.paramss)
      .flatMap(_.params.flatMap {
        case ValDef(name, tpeTree, _) => Some(name -> isRepeated(tpeTree.tpe))
        case _                        => None
      })

    Expr(areRepeated)

  def typeInfo[T: Type](using Quotes): Expr[TypeInfo] =
    import quotes.reflect._

    def normalizedName(s: Symbol): String =
      if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
    def name(tpe: TypeRepr): Expr[String] = tpe match
      case TermRef(typeRepr, name) if tpe.typeSymbol.flags.is(Flags.Module) =>
        Expr(name.stripSuffix("$"))
      case TermRef(typeRepr, name) => Expr(name)
      case _                       => Expr(normalizedName(tpe.typeSymbol))

    def ownerNameChain(sym: Symbol): List[String] =
      if sym.isNoSymbol then List.empty
      else if sym == defn.EmptyPackageClass then List.empty
      else if sym == defn.RootPackage then List.empty
      else if sym == defn.RootClass then List.empty
      else ownerNameChain(sym.owner) :+ normalizedName(sym)

    def owner(tpe: TypeRepr): Expr[String] = Expr(
      ownerNameChain(tpe.typeSymbol.maybeOwner).mkString(".")
    )

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] = tpe match
      case AppliedType(tpe, args) =>
        '{
          TypeInfo(
            ${ owner(tpe) },
            ${ name(tpe) },
            ${ Expr.ofList(args.map(typeInfo)) }
          )
        }
      case _ =>
        '{ TypeInfo(${ owner(tpe) }, ${ name(tpe) }, Nil) }

    typeInfo(TypeRepr.of[T])

  private class CollectAnnotations[T: Type](using val quotes: Quotes) {
    import quotes.reflect.*

    private val tpe: TypeRepr = TypeRepr.of[T]

    def anns: Expr[List[Any]] =
      Expr.ofList {
        tpe.typeSymbol.annotations
          .filter(filterAnnotation)
          .map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    def inheritedAnns: Expr[List[Any]] =
      Expr.ofList {
        tpe.baseClasses
          .filterNot(isObjectOrScala)
          .collect {
            case s if s != tpe.typeSymbol => s.annotations
          } // skip self
          .flatten
          .filter(filterAnnotation)
          .map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    def typeAnns: Expr[List[Any]] = {

      def getAnnotations(t: TypeRepr): List[Term] = t match
        case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
        case _                         => Nil

      val symbol: Option[Symbol] =
        if tpe.typeSymbol.isNoSymbol then None else Some(tpe.typeSymbol)
      Expr.ofList {
        symbol.toList.map(_.tree).flatMap {
          case ClassDef(_, _, parents, _, _) =>
            parents
              .collect { case t: TypeTree => t.tpe }
              .flatMap(getAnnotations)
              .filter(filterAnnotation)
              .map(_.asExpr.asInstanceOf[Expr[Any]])
          case _ =>
            List.empty
        }
      }
    }

    def paramAnns: Expr[List[(String, List[Any])]] =
      Expr.ofList {
        groupByParamName {
          (fromConstructor(tpe.typeSymbol) ++ fromDeclarations(tpe.typeSymbol))
            .filter { case (_, anns) => anns.nonEmpty }
        }
      }

    def inheritedParamAnns: Expr[List[(String, List[Any])]] =
      Expr.ofList {
        groupByParamName {
          tpe.baseClasses
            .filterNot(isObjectOrScala)
            .collect {
              case s if s != tpe.typeSymbol =>
                (fromConstructor(s) ++ fromDeclarations(s)).filter {
                  case (_, anns) => anns.nonEmpty
                }
            }
            .flatten
        }
      }

    private def fromConstructor(from: Symbol): List[(String, List[Expr[Any]])] =
      from.primaryConstructor.paramSymss.flatten.map { field =>
        field.name -> field.annotations
          .filter(filterAnnotation)
          .map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    private def fromDeclarations(
        from: Symbol
    ): List[(String, List[Expr[Any]])] =
      from.declarations.collect {
        case field: Symbol if field.tree.isInstanceOf[ValDef] =>
          field.name -> field.annotations
            .filter(filterAnnotation)
            .map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    private def groupByParamName(anns: List[(String, List[Expr[Any]])]) =
      anns
        .groupBy { case (name, _) => name }
        .toList
        .map { case (name, l) => name -> l.flatMap(_._2) }
        .map { (name, anns) => Expr.ofTuple(Expr(name), Expr.ofList(anns)) }

    private def isObjectOrScala(bc: Symbol) =
      bc.name.contains("java.lang.Object") || bc.fullName.startsWith("scala.")

    private def filterAnnotation(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
  }
