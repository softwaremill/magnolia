package magnolia1

import scala.quoted.*
import scala.annotation.meta.field
import scala.annotation.Annotation

object Macro:

  inline def showType[A]: String =
    ${ showTypeImpl[A] }

  inline def isObject[T]: Boolean =
    ${ isObject[T] }

  inline def isEnum[T]: Boolean =
    ${ isEnum[T] }

  inline def anns[T]: List[Any] =
    ${ anns[T] }

  inline def inheritedAnns[T]: List[Any] =
    ${ inheritedAnns[T] }

  inline def typeAnns[T]: List[Any] =
    ${ typeAnns[T] }

  inline def paramAnns[T]: List[(String, List[Any])] =
    ${ paramAnns[T] }

  inline def inheritedParamAnns[T]: List[(String, List[Any])] =
    ${ inheritedParamAnns[T] }

  inline def isValueClass[T]: Boolean =
    ${ isValueClass[T] }

  inline def defaultValue[T]: List[(String, Option[() => Any])] =
    ${ defaultValue2[T] }

  inline def paramTypeAnns[T]: List[(String, List[Any])] =
    ${ paramTypeAnns[T] }

  inline def repeated[T]: List[(String, Boolean)] =
    ${ repeated[T] }

  inline def typeInfo[T]: TypeInfo =
    ${ typeInfo[T] }

  private def showTypeImpl[A: Type](using Quotes): Expr[String] =
    Expr(Type.show[A])

  def isObject[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))

  def isEnum[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))

  def paramTypeAnns[T: Type](using
      q: Quotes
  ): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[q.type, T].paramTypeAnns

  def anns[T: Type](using q: Quotes): Expr[List[Any]] =
    new CollectAnnotations[q.type, T].anns

  def inheritedAnns[T: Type](using q: Quotes): Expr[List[Any]] =
    new CollectAnnotations[q.type, T].inheritedAnns

  def typeAnns[T: Type](using q: Quotes): Expr[List[Any]] =
    new CollectAnnotations[q.type, T].typeAnns

  def paramAnns[T: Type](using
      q: Quotes
  ): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[q.type, T].paramAnns

  def inheritedParamAnns[T: Type](using
      q: Quotes
  ): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[q.type, T].inheritedParamAnns

  def isValueClass[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(
      TypeRepr.of[T].baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))
    )

  def defaultValue2[T: Type](using
      Quotes
  ): Expr[List[(String, Option[() => Any])]] =
    import quotes.reflect.*

    def exprOfOption(
        oet: (Expr[String], Option[Expr[Any]])
    ): Expr[(String, Option[() => Any])] =
      oet match
        case (label, None)     => Expr(label.valueOrAbort -> None)
        case (label, Some(et)) => '{ $label -> Some(() => $et) }

    Expr.ofList {
      defaultValueOnTerms
        .map { p =>
          exprOfOption(Expr(p._1) -> p._2)
        }
    }

  private def defaultValueOnTerms[T: Type](using
      Quotes
  ): List[(String, Option[Expr[Any]])] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T].typeSymbol

    tpe.primaryConstructor.paramSymss.flatten
      .filter(_.isValDef)
      .zipWithIndex
      .map { case (field, i) =>
        field.name -> tpe.companionClass
          .declaredMethod(s"$$lessinit$$greater$$default$$${i + 1}")
          .headOption
          .flatMap(_.tree.asInstanceOf[DefDef].rhs)
          .map(_.asExprOf[Any])
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
    import quotes.reflect.*

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

  private class CollectAnnotations[Q <: Quotes, T: Type](using val quotes: Q):
    import quotes.reflect.*

    val tpe: TypeRepr = TypeRepr.of[T]

    def anns: Expr[List[Any]] =
      Expr.ofList {
        tpe.typeSymbol.annotations
          .filter(filterAnnotation)
          .map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    def inheritedAnns: Expr[List[Any]] =
      Expr.ofList {
        val what = tpe.baseClasses
          .filterNot(isObjectOrScala)
          .collect {
            case s if s != tpe.typeSymbol => s.annotations
          } // skip self
          .flatten
          .filter(filterAnnotation)

        what.map(_.asExpr.asInstanceOf[Expr[Any]])
      }

    def typeAnns: Expr[List[Any]] =
      val symbol: Option[Symbol] =
        if tpe.typeSymbol.isNoSymbol then None else Some(tpe.typeSymbol)

      Expr.ofList {
        symbol.toList.map(_.tree).flatMap {
          case ClassDef(_, _, parents, _, _) =>
            parents
              .collect {

                case t: TypeTree => t.tpe

                // case for AnyVal type annotations: with "-Yretain-trees" scalac option, the TypeTree of the annotation gets erased,
                // so we need to extract the annotations from apply
                case Apply(Select(New(a), _), _) => a.tpe
              }
              .flatMap { tpe =>
                val anns = loopForAnnotations(tpe); anns
              }
              .filter(filterAnnotation)
              .map { _.asExpr.asInstanceOf[Expr[Any]] }
          case e =>
            List.empty
        }
      }

    def paramTypeAnnsOnTerms: List[(String, List[Term])] =
      tpe.typeSymbol.caseFields
        .map { field =>
          val tpeRepr = field.tree match
            case v: ValDef => v.tpt.tpe
            case d: DefDef => d.returnTpt.tpe

          field.name -> loopForAnnotations(tpeRepr).filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }

        }
        .filter(_._2.nonEmpty)

    def paramTypeAnns: Expr[List[(String, List[Any])]] =
      liftTermsDict(paramTypeAnnsOnTerms)

    def paramAnnsOnTerms: List[(String, List[Term])] =
      val terms = (annotationsFromConstructorOnTerms(
        tpe.typeSymbol
      ) ++ annotationsFromDeclarationsOnTerms(tpe.typeSymbol))
        .filter { case (_, as) => as.nonEmpty }

      groupByNameOnTerms(terms)

    def paramAnns: Expr[List[(String, List[Any])]] =
      liftTermsDict(paramAnnsOnTerms)

    def inheritedParamAnnsOnTerms: List[(String, List[Term])] =
      val annTerms: List[(String, List[Term])] =
        tpe.baseClasses
          .filterNot(isObjectOrScala)
          .collect {
            case s if s != tpe.typeSymbol =>
              (annotationsFromConstructorOnTerms(
                s
              ) ++ annotationsFromDeclarationsOnTerms(s))
                .filter { case (_, anns) =>
                  anns.nonEmpty
                }
          }
          .flatten

      groupByNameOnTerms(annTerms)

    def inheritedParamAnns: Expr[List[(String, List[Any])]] =
      liftTermsDict(inheritedParamAnnsOnTerms)

    private def loopForAnnotations(t: TypeRepr): List[Term] =
      t match
        case AnnotatedType(inner, ann) => ann :: loopForAnnotations(inner)
        case _                         => Nil

    private def liftTermsDict(
        tss: List[(String, List[Term])]
    ): Expr[List[(String, List[Any])]] =
      Expr.ofList {
        tss
          .map { case (name, terms) =>
            Expr.ofTuple(
              Expr(name),
              Expr.ofList(terms.map(_.asExpr))
            )
          }
      }

    private def annotationsFromConstructorOnTerms(
        from: Symbol
    ): List[(String, List[Term])] =
      from.primaryConstructor.paramSymss.flatten
        .map { field =>
          field.name -> field.annotations.filter(filterAnnotation)
        }

    private def annotationsfromConstructor(
        from: Symbol
    ): List[(String, List[Expr[Any]])] =
      annotationsFromConstructorOnTerms(from)
        .map { p =>
          p._1 -> p._2.map(_.asExpr.asInstanceOf[Expr[Any]])
        }

    private def annotationsFromDeclarationsOnTerms(
        from: Symbol
    ): List[(String, List[Term])] =
      from.declarations
        .collect {
          case field: Symbol if field.tree.isInstanceOf[ValDef] =>
            field.name -> field.annotations.filter(filterAnnotation)
        }

    private def annotationsFromDeclarations(
        from: Symbol
    ): List[(String, List[Expr[Any]])] =
      annotationsFromDeclarationsOnTerms(from)
        .map { p =>
          p._1 -> p._2.map(_.asExpr.asInstanceOf[Expr[Any]])
        }

    private def groupByNameOnTerms(
        tss: List[(String, List[Term])]
    ): List[(String, List[Term])] =
      tss
        .groupBy { case (name, _) => name }
        .toList
        .map { case (name, l) => name -> l.flatMap(_._2) }

    private def isObjectOrScala(bc: Symbol) =
      bc.name.contains("java.lang.Object") ||
        bc.fullName.startsWith("scala.")

    private def filterAnnotation(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

  object ValueClassDerivation:

    inline def getValueClassParam[TC[_], V]: CaseClass.Param[TC, V] =
      ${ getValueClassParamImpl[TC, V] }

    inline def rawContructByMacro[V](as: List[Any]): V =
      ${ rawContructByMacroImpl[V]('as) }

    private def getValueClassParamImpl[TC[_]: Type, A: Type](using
        q: Quotes
    ): Expr[CaseClass.Param[TC, A]] =
      import quotes.reflect.*

      def selectTermsByName(
          tss: List[(String, List[Term])],
          name: String
      ): List[Term] =
        tss.toMap
          .getOrElse(name, Nil)

      def getDefault[P: Type](
          os: List[(String, Option[Expr[Any]])],
          name: String
      )(using Quotes): Expr[CallByNeed[Option[Any]]] =
        import quotes.reflect.*

        os.toMap
          .get(name)
          .flatten match
          case None => '{ new CallByNeed(() => None) }
          case Some(expr) =>
            '{ new CallByNeed(() => Some(($expr).asInstanceOf[P])) }

      extension [B: Type](e: Expr[B])
        def asCallByNeedExpr: Expr[CallByNeed[B]] =
          '{ new CallByNeed[B](() => $e) }

      val aTpe: TypeRepr = TypeRepr.of[A]
      val aSym: Symbol = aTpe.typeSymbol
      val aCtor: Symbol = aSym.primaryConstructor

      aCtor.paramSymss match
        case List(paramSymbol: Symbol) :: Nil =>
          val paramTypeTree =
            paramSymbol.tree match
              case v: ValDef => v.tpt
              case _ =>
                report.errorAndAbort(
                  s"Error handling param symbol tree '${paramSymbol.tree}'."
                )

          val paramTypeTpe = paramTypeTree.tpe
          val paramTC = TypeRepr.of[TC].appliedTo(paramTypeTpe)

          val paramCallByNeed = paramTC.asType match
            case '[t] =>
              Expr
                .summon[t]
                .map { _.asCallByNeedExpr }
                .getOrElse {
                  report.errorAndAbort(
                    s"Cannot summon instance for ${Type.show[t]}."
                  )
                }

          val defaultValue = (aTpe.asType, paramTypeTpe.asType) match
            case ('[a], '[p]) =>
              getDefault[p](defaultValueOnTerms[a], paramSymbol.name)

          def selectFromDictAsTerm(
              dict: List[(String, List[Term])]
          ) =
            Expr
              .ofList {
                selectTermsByName(
                  dict,
                  paramSymbol.name
                )
                  .map(_.asExpr)
              }

          val applyFuncTerm =
            '{ CaseClass.Param }.asTerm
              .select(
                TypeRepr
                  .of[CaseClass.Param.type]
                  .termSymbol
                  .declaredMethod("apply")
                  .head
              )
              .appliedToTypes(
                TypeRepr.of[TC] :: TypeRepr.of[A] :: paramTypeTree.tpe :: Nil
              )

          val args = List(
            Expr(paramSymbol.name).asTerm,
            Expr(0).asTerm,
            Expr(false).asTerm,
            paramCallByNeed.asTerm,
            defaultValue.asTerm,
            selectFromDictAsTerm(
              new CollectAnnotations[q.type, A].paramAnnsOnTerms
            ).asTerm,
            selectFromDictAsTerm(
              new CollectAnnotations[q.type, A].inheritedParamAnnsOnTerms
            ).asTerm,
            selectFromDictAsTerm(
              new CollectAnnotations[q.type, A].paramTypeAnnsOnTerms
            ).asTerm
          )

          val app =
            Apply(
              fun = applyFuncTerm,
              args = args
            )

          app.asExprOf[CaseClass.Param[TC, A]]

        case _ => report.errorAndAbort(s"Error handling symbol '${aSym.name}")

    private def rawContructByMacroImpl[V: Type](es: Expr[List[Any]])(using
        Quotes
    ): Expr[V] =
      import quotes.reflect.*

      val vTpe = TypeRepr.of[V]
      val vCtor = vTpe.typeSymbol.primaryConstructor
      val vApp = New(Inferred(vTpe)).select(vCtor)

      val argsTerm: Term = es.asTerm
      val appl = argsTerm.tpe.typeSymbol.methodMember("apply").head
      val argApp = argsTerm.select(appl).appliedTo(Literal(IntConstant(0)))
      val as = argApp.tpe.typeSymbol.methodMember("asInstanceOf").head

      val arg =
        vCtor.paramSymss match
          case List(paramSymbol: Symbol) :: Nil =>
            val paramTypeTree =
              paramSymbol.tree match
                case v: ValDef => v.tpt
                case _ =>
                  report.errorAndAbort(
                    s"Error handling param symbol tree '${paramSymbol.tree}'."
                  )
            val aTpe = paramTypeTree.tpe
            argApp.select(as).appliedToType(aTpe)

          case _ =>
            report.errorAndAbort(
              s"Error handling symbol '${vTpe.typeSymbol.name}'."
            )

      vApp.appliedToArgs(arg :: Nil).asExprOf[V]

  end ValueClassDerivation

end Macro
