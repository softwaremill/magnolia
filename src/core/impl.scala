package magnolia1

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*

class DerivationImpl(using Quotes):
  import quotes.reflect.*

  def rawConstruct[T: Type](fieldValuesExpr: Expr[List[Any]]): Expr[T] = {
    val tpe = TypeRepr.of[T]
    if (tpe.typeSymbol.flags.is(Flags.Case) || tpe.baseClasses.contains(defn.AnyValClass)) && !tpe.typeSymbol.flags.is(Flags.Module) then
      val fieldValues = fieldValuesExpr.asTerm
      val getIdx = fieldValues.select(fieldValues.tpe.typeSymbol.methodMember("apply").head)
      val init = tpe.typeSymbol.primaryConstructor
      val applies = tpe.typeSymbol.companionModule.methodMember("apply")
      val (prefix, constrSym) =
        if applies.size >= 1 && !tpe.baseClasses.contains(defn.AnyValClass) then
          Ref(tpe.typeSymbol.companionModule) -> applies.head
        else
          New(Inferred(tpe)) -> init
      val typeParamLookup = tpe match {
        case AppliedType(_, typeArgs) =>
          constrSym.paramSymss.flatten.filter(_.isType).zip(typeArgs).toMap
        case _ =>
          Map.empty
      }
      val vals = constrSym.paramSymss.flatten.filter(_.isValDef).zipWithIndex.map { case (paramSym, idx) =>
        val elem = getIdx.appliedTo(Literal(IntConstant(idx)))
        val valdef = paramSym.tree.asInstanceOf[ValDef]
        val t = valdef.tpt.tpe.alphaRenameTypes(typeParamLookup)
        if isRepeated(t) then
          val drop = fieldValues.select(fieldValues.tpe.typeSymbol.methodMember("drop").head)
          val rest = drop.appliedTo(Literal(IntConstant(idx)))
          val AnnotatedType(AppliedType(_, List(aTpe)), _) = t: @unchecked
          val size = rest.select(rest.tpe.typeSymbol.methodMember("size").head)
          val equals = size.select(size.tpe.typeSymbol.methodMember("==").head).appliedTo(Literal(IntConstant(1)))
          If(
            equals, //TODO(kπ) I'm not sure about this (seems like a hack TBH)
            Typed(elem, Inferred(defn.RepeatedParamClass.typeRef.appliedTo(aTpe))),
            Typed(rest, Inferred(defn.RepeatedParamClass.typeRef.appliedTo(aTpe)))
          )
        else
          elem.select(elem.tpe.typeSymbol.methodMember("asInstanceOf").head).appliedToType(t)
      }
      val select = prefix.select(constrSym)
      val method = tpe match {
        case AppliedType(_, typeArgs) =>
          select.appliedToTypes(typeArgs)
        case _ =>
          select
      }
      method
        .appliedToArgs(vals)
        .asExprOf[T]
    else // if tpe.widen.typeSymbol.flags.is(Flags.Module) || tpe.termSymbol.flags.is(Flags.Case & Flags.Enum) then
      tpe match {
        case _: TypeRef =>
          Ref(tpe.typeSymbol.companionModule)
            .asExprOf[T]
        case _ =>
          Ref(tpe.termSymbol)
            .asExprOf[T]
      }
  }

  def isProduct[T: Type]: Expr[Boolean] = Expr(isProductImpl(TypeRepr.of[T]))
  def isSum[T: Type]: Expr[Boolean] = Expr(isSumImpl(TypeRepr.of[T]))

  private def isProductImpl(tpe: TypeRepr): Boolean =
    val typeSymbol = tpe.typeSymbol
    tpe.isSingleton ||
      (typeSymbol.isClassDef && tpe.baseClasses.contains(defn.AnyValClass)) ||
        (typeSymbol.isClassDef && typeSymbol.flags.is(Flags.Case))

  private def isSumImpl(tpe: TypeRepr): Boolean =
    val typeSymbol = tpe.typeSymbol
    !tpe.isSingleton &&
      ((typeSymbol.flags.is(Flags.Sealed) && typeSymbol.flags.is(Flags.Trait)) ||
        typeSymbol.flags.is(Flags.Enum) ||
        (!typeSymbol.flags.is(Flags.Case) && !typeSymbol.children.isEmpty))

  private def repeatedParams(tpe: TypeRepr): List[(String, Boolean)] =
    val symbol: Option[Symbol] = if tpe.typeSymbol.isNoSymbol then None else Some(tpe.typeSymbol)
    val constr: Option[DefDef] = symbol.map(_.primaryConstructor.tree.asInstanceOf[DefDef])

    constr.toList.flatMap(_.paramss).flatMap(_.params.flatMap {
      case ValDef(name, tpeTree, _) => Some(name -> isRepeated(tpeTree.tpe))
      case _                        => None
    })

  private def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match
    case a: AnnotatedType =>
      a.annotation.tpe match
        case tr: TypeRef => tr.name == "Repeated"
        case _           => false
    case _ => false

  def isObject[T: Type]: Expr[Boolean] = Expr(isObject(TypeRepr.of[T]))
  
  private def isObject(typeRepr: TypeRepr): Boolean =
    typeRepr.typeSymbol.flags.is(Flags.Module)

  def isEnum[T: Type]: Expr[Boolean] =
    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))
  
  def isValueClass[T: Type]: Expr[Boolean] =
    Expr(TypeRepr.of[T].baseClasses.contains(defn.AnyValClass))
  
  def typeInfo[T: Type]: Expr[TypeInfo] = Expr(typeInfo(TypeRepr.of[T]))
  given ToExpr[TypeInfo] = new ToExpr[TypeInfo] {
    def apply(x: TypeInfo)(using Quotes): Expr[TypeInfo] =
      '{
        TypeInfo(
          ${ Expr(x.owner) },
          ${ Expr(x.short) },
          ${ Expr.ofList(x.typeParams.toSeq.map(Expr.apply)) }
        )
      }
  }
  private def typeInfo(tpe: TypeRepr): TypeInfo = {

    def normalizedName(s: Symbol): String =
      if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
    
    def name(tpe: TypeRepr): String = tpe match
      case TermRef(typeRepr, name) if tpe.typeSymbol.flags.is(Flags.Module) =>
        name.stripSuffix("$")
      case TermRef(typeRepr, name) => name
      case _ => normalizedName(tpe.typeSymbol)

    def ownerNameChain(sym: Symbol): List[String] =
      if sym.isNoSymbol then List.empty
      else if sym == defn.EmptyPackageClass then List.empty
      else if sym == defn.RootPackage then List.empty
      else if sym == defn.RootClass then List.empty
      else ownerNameChain(sym.owner) :+ normalizedName(sym)

    def owner(tpe: TypeRepr): String =
      ownerNameChain(tpe.typeSymbol.maybeOwner).mkString(".")
    
    tpe match
      case AppliedType(tpe, args) =>
        val typeParamBoundLookup: Map[Symbol, TypeRepr] =
          tpe.typeSymbol.typeMembers.flatMap { tm =>
            tm.tree match {
              case TypeDef(_, tpt: TypeTree) => Some(tm -> tpt.tpe.widenIfBounds)
              case _ => None
            }
          }.toMap
        TypeInfo(
          owner(tpe),
          name(tpe),
          args.map(_.alphaRenameTypes(typeParamBoundLookup)).map(typeInfo)
        )
      case _ =>
        TypeInfo(owner(tpe), name(tpe), Nil)
  }

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{ (x: T) => ${ f('x) } }

  //TODO(kπ) will be needed for AnyVals
  private def derefImpl(paramSym: Symbol, valueTpe: TypeRepr, paramTpe: TypeRepr): Term = {
    val methodType = MethodType(List("value"))(_ => List(valueTpe), _ => paramTpe)
    val defdefSymbol = Symbol.newMethod(Symbol.spliceOwner, "$anonfun", methodType)
    val defdef = DefDef(
      defdefSymbol,
      {
        case List(List(value)) =>
          Some(value.asInstanceOf[Term].select(paramSym))
      }
    )
    Block(List(defdef), Closure(Ref(defdefSymbol), None))
  }

  private def isType[T: Type](typeRepr: TypeRepr): Expr[T => Boolean] = to { other =>
    val term = other.asTerm
    term
      .select(term.tpe.typeSymbol.methodMember("isInstanceOf").head)
      .appliedToType(typeRepr)
      .asExprOf[Boolean]
  }

  private def asType[T: Type, S: Type](parent: TypeRepr, child: TypeRepr): Expr[T => S & T] = to[T, S & T] { other =>
    val term = other.asTerm
    term
      .select(term.tpe.typeSymbol.methodMember("asInstanceOf").head)
      .appliedToType(AndType(child, parent))
      .asExprOf[S & T]
  }

  private def getTypeAnnotations(t: TypeRepr): List[Term] = t match
    case AnnotatedType(inner, ann) => ann :: getTypeAnnotations(inner)
    case _                         => Nil

  private def isObjectOrScala(bc: Symbol) =
    bc.name.contains("java.lang.Object") || bc.fullName.startsWith("scala.")

  private def groupByParamName(anns: List[(String, List[Term])]) =
    anns
      .groupBy { case (name, _) => name }
      .toList
      .map { case (name, l) => name -> l.flatMap(_._2) }

  private def fromConstructor(from: Symbol): List[(String, List[Term])] =
    from.primaryConstructor.paramSymss.flatten.map { field =>
      field.name -> field.annotations
        .filter(filterAnnotation)
    }

  private def fromDeclarations(from: Symbol): List[(String, List[Term])] =
    from.declarations.collect {
      case (field: Symbol) if field.isValDef || field.isDefDef =>
        field.name -> field.annotations.filter(filterAnnotation)
    }

  def anns[T: Type]: Expr[List[Any]] =
    Expr.ofList(anns(TypeRepr.of[T]).map(_.asExpr))

  private def anns(tpe: TypeRepr): List[Term] =
    tpe.typeSymbol.annotations
      .filter(filterAnnotation)

  def typeAnns[T: Type]: Expr[List[Any]] =
    Expr.ofList(typeAnns(TypeRepr.of[T]).map(_.asExpr))

  private def typeAnns(tpe: TypeRepr): List[Term] = {
    val symbol: Option[Symbol] = if tpe.typeSymbol.isNoSymbol then None else Some(tpe.typeSymbol)
    symbol.toList.map(_.tree).flatMap {
      case ClassDef(_, _, parents, _, _) =>
          parents
            .collect { case t: TypeTree => t.tpe }
            .flatMap(getTypeAnnotations)
            .filter(filterAnnotation)
      case _ =>
        List.empty
    }
  }

  def inheritedAnns[T: Type]: Expr[List[Any]] =
    Expr.ofList(inheritedAnns(TypeRepr.of[T]).map(_.asExpr))

  private def inheritedAnns(tpe: TypeRepr): List[Term] = {
    tpe.baseClasses
      .filterNot(isObjectOrScala)
      .collect {
        case s if s != tpe.typeSymbol => s.annotations
      }
      .flatten
      .filter(filterAnnotation)
  }

    private def paramAnns(tpe: TypeRepr): List[(String, List[Term])] =
    groupByParamName {
      (fromConstructor(tpe.typeSymbol) ++ fromDeclarations(tpe.typeSymbol))
        .filter { case (_, anns) => anns.nonEmpty }
    }

  private def paramTypeAnns(tpe: TypeRepr): List[(String, List[Term])] = {
    tpe
      .typeSymbol
      .caseFields
      .map { field =>
        val tpeRepr = field.tree match
          case v: ValDef => v.tpt.tpe
          case d: DefDef => d.returnTpt.tpe

        field.name -> getTypeAnnotations(tpeRepr)
          .filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }
      }
      .filter(_._2.nonEmpty)
  }

  private def inheritedParamAnns(typeRepr: TypeRepr): List[(String, List[Term])] =
    groupByParamName {
      typeRepr.baseClasses
        .filterNot(isObjectOrScala)
        .collect {
          case s if s != typeRepr.typeSymbol =>
            (fromConstructor(s) ++ fromDeclarations(s)).filter {
              case (_, anns) => anns.nonEmpty
            }
        }
        .flatten
    }

  private def defaultValue(tpe: TypeRepr): List[(String, Option[Term])] =
    val typeSymbol = tpe.typeSymbol
    val optionSymbol = Symbol.requiredMethod("scala.Some.apply")
    typeSymbol.primaryConstructor.paramSymss.flatten
      .filter(_.isValDef)
      .zipWithIndex
      .map { case (field, i) =>
        field.name -> typeSymbol.companionClass
          .declaredMethod(s"$$lessinit$$greater$$default$$${i + 1}")
          .headOption
          .map { m =>
            m.tree.asInstanceOf[DefDef].rhs match {
              case None => '{None}.asTerm
              case Some(rhs) => Ref(optionSymbol).appliedToType(rhs.tpe).appliedTo(rhs)
            }
          }
      }

  private def filterAnnotation(a: Term): Boolean =
    a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
      a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

  private def wrapInCallByNeedTerm(term: Term, tpe: TypeRepr): Term =
    val callByNeedTerm = '{CallByNeed}.asTerm
    val callByNeedApply = TypeRepr.of[CallByNeed.type].termSymbol.declaredMethod("apply").head
    Select(callByNeedTerm, callByNeedApply).appliedToType(tpe).appliedTo(term)

  def getParamsImpl[Typeclass[_]: Type, T: Type](fallback: Expr[Derivation[Typeclass]]): Expr[List[CaseClass.Param[Typeclass, T]]] =

    val typeSymbol = TypeRepr.of[T].typeSymbol

    val typeParamLookup: Map[Symbol, TypeRepr] = TypeRepr.of[T] match {
      case tpe@AppliedType(_, argTypes) =>
        val typeParams =
          tpe.typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
        typeParams.zip(argTypes).toMap
      case _ => Map.empty
    }
    val typeParamBoundLookup: Map[Symbol, TypeRepr] =
      typeSymbol.typeMembers.flatMap { tm =>
        tm.tree match {
          case TypeDef(_, tpt: TypeTree) => Some(tm -> tpt.tpe.widenIfBounds)
          case _ => None
        }
      }.toMap

    val paramObj = '{CaseClass.Param}.asTerm
    val paramConstrSymbol = TypeRepr.of[CaseClass.Param.type].termSymbol.declaredMethod("apply").filter(_.paramSymss.flatten.size == 11).head

    val annotationsLookup = paramAnns(TypeRepr.of[T]).toMap
    val inheritedAnnotationsLookup = inheritedParamAnns(TypeRepr.of[T]).toMap
    val typeAnnotationsLookup = paramTypeAnns(TypeRepr.of[T]).toMap
    val defaultValuesLookup = defaultValue(TypeRepr.of[T]).toMap
    val repeatedLookup = repeatedParams(TypeRepr.of[T]).toMap

    Expr.ofList {
      typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTerm).zipWithIndex.collect {
        case (paramSymbol: Symbol, idx: Int) =>
          val paramTpt = paramSymbol.tree match
            case valDef: ValDef => valDef.tpt
            case defdef: DefDef => defdef.returnTpt
          val paramTypeTpe = paramTpt.tpe.alphaRenameTypes(typeParamLookup).alphaRenameTypes(typeParamBoundLookup)
          val paramTypeclassTpe = AppliedType(TypeRepr.of[Typeclass], List(paramTypeTpe))
          val Inlined(_, _, TypeApply(summonInlineTerm, _)) = '{scala.compiletime.summonInline}.asTerm: @unchecked
          val summonInlineApp = summonInlineTerm.appliedToType(paramTypeclassTpe)
          val instance = wrapInCallByNeedTerm(summonInlineApp, paramTypeclassTpe)
          val unit = '{()}.asTerm
          val fallbackTerm = fallback.asTerm match
            case Inlined(_, _, i) => i
          val appliedFallbackTerm = Select(Ref(fallbackTerm.symbol), fallbackTerm.symbol.methodMember("doDerive").head).appliedToType(paramTypeTpe)
          val fallbackCallByNeedTerm = wrapInCallByNeedTerm(appliedFallbackTerm, paramTypeclassTpe)
          val optionSymbol = Symbol.requiredClass("scala.Option")
          val defaultVal = defaultValuesLookup.get(paramSymbol.name).flatten.map(t => wrapInCallByNeedTerm(t, optionSymbol.typeRef.appliedTo(paramTypeTpe)))
          Apply(
            fun =
              paramObj
                .select(paramConstrSymbol)
                .appliedToTypes(List(TypeRepr.of[Typeclass], TypeRepr.of[T], paramTypeTpe)),
            args = List(
              /*name =*/ Expr(paramSymbol.name).asTerm,
              /*idx =*/ Expr(idx).asTerm,
              /*repeated =*/ Expr(repeatedLookup(paramSymbol.name)).asTerm,
              /*cbn =*/ paramTypeclassTpe.asType match
                case '[p] =>
                  Expr.summon[p].map(_ => instance).getOrElse(fallbackCallByNeedTerm)
              ,
              /*defaultVal =*/ defaultVal.getOrElse('{CallByNeed(None)}.asTerm),
              /*annotations =*/ Expr.ofList(annotationsLookup.getOrElse(paramSymbol.name, List.empty).toSeq.map(_.asExpr)).asTerm,
              /*inheritedAnns =*/ Expr.ofList(inheritedAnnotationsLookup.getOrElse(paramSymbol.name, List.empty).toSeq.map(_.asExpr)).asTerm,
              /*typeAnnotations =*/ Expr.ofList(typeAnnotationsLookup.getOrElse(paramSymbol.name, List.empty).toSeq.map(_.asExpr)).asTerm
            )
          ).asExprOf[CaseClass.Param[Typeclass, T]]
      }
    }
  end getParamsImpl

  def getSubtypesImpl[Typeclass[_]: Type, T: Type](fallback: Expr[Derivation[Typeclass]]): Expr[List[SealedTrait.Subtype[Typeclass, T, _]]] =

    val typeSymbol = TypeRepr.of[T].typeSymbol
    val parentArgs = TypeRepr.of[T].extractArgs

    val subTypeObj = '{SealedTrait.Subtype}.asTerm
    val subTypeConstrSymbol = TypeRepr.of[SealedTrait.Subtype.type].termSymbol.declaredMethod("apply").head

    val annotations = paramAnns(TypeRepr.of[T]).toMap
    val typeAnnotations = paramTypeAnns(TypeRepr.of[T]).toMap

    Expr.ofList {
      typeSymbol.children.zipWithIndex.collect {
        case (subTypeSymbol: Symbol, idx: Int) =>
          val subTypeTpe = subTypeSymbol.tree match {
            case valDef: ValDef =>
              subTypeSymbol.termRef
            case classDef: ClassDef =>
              val parentExtendsTpt =
                classDef.parents
                  .collect { case tpt: TypeTree if tpt.tpe.typeSymbol == typeSymbol => tpt }.head              
              val paramLookup = parentExtendsTpt.tpe.extractArgs.map(_.typeSymbol).zip(parentArgs).toMap
              val declaredTypesLookup =
                classDef.symbol.declaredTypes.map(s => s.name -> s).toMap
              val args =
                classDef.constructor.paramss.flatMap(_.params)
                  .map(_.symbol)
                  .filter(_.isTypeParam)
                  .map(s => declaredTypesLookup(s.name))
                  .map(_.typeRef)
              if args.isEmpty then
                subTypeSymbol.typeRef
              else
                AppliedType(subTypeSymbol.typeRef, args).alphaRenameTypes(paramLookup)
          }
          val subTypeTypeclassTpe = AppliedType(TypeRepr.of[Typeclass], List(subTypeTpe))
          val Inlined(_, _, TypeApply(summonInlineTerm, _)) = '{scala.compiletime.summonInline}.asTerm: @unchecked
          val summonInlineApp = summonInlineTerm.appliedToType(subTypeTypeclassTpe)
          val instance = wrapInCallByNeedTerm(summonInlineApp, subTypeTypeclassTpe)
          val fallbackTerm = fallback.asTerm match
            case Inlined(_, _, i) => i
          val appliedFallbackTerm = Select(Ref(fallbackTerm.symbol), fallbackTerm.symbol.methodMember("doDerive").head).appliedToType(subTypeTpe)
          val fallbackCallByNeedTerm = wrapInCallByNeedTerm(appliedFallbackTerm, subTypeTypeclassTpe)
          Apply(
            fun =
              subTypeObj
                .select(subTypeConstrSymbol)
                .appliedToTypes(List(TypeRepr.of[Typeclass], TypeRepr.of[T], subTypeTpe)),
            args = List(
              /*name =*/ Expr(typeInfo(subTypeTpe)).asTerm,
              /*annotations =*/ Expr.ofList(anns(subTypeTpe).toSeq.map(_.asExpr)).asTerm,
              /*typeAnnotations =*/ Expr.ofList(typeAnns(subTypeTpe).toSeq.map(_.asExpr)).asTerm,
              /*inheritedAnnotations =*/ Expr.ofList(inheritedAnns(subTypeTpe).toSeq.map(_.asExpr)).asTerm,
              /*isObject =*/ Expr(isObject(subTypeTpe)).asTerm,
              /*idx =*/ Expr(idx).asTerm,
              /*cbn =*/ subTypeTypeclassTpe.asType match {
                case '[p] =>
                  Expr.summon[p].map(_ => instance).getOrElse(fallbackCallByNeedTerm)
              },
              /*isType =*/ isType(subTypeTpe).asTerm
            )
          ).asExprOf[SealedTrait.Subtype[Typeclass, T, _]]
      }
    }
  end getSubtypesImpl
  
  extension (tpe: TypeRepr)
    private def alphaRenameTypes(renames: Map[Symbol, TypeRepr]): TypeRepr = tpe match {
      case tpe: NamedType => renames.get(tpe.typeSymbol).getOrElse(tpe)
      case AppliedType(tpe, args) => AppliedType(tpe.alphaRenameTypes(renames), args.map(_.alphaRenameTypes(renames)))
      case AnnotatedType(tpe, annot) => AnnotatedType(tpe.alphaRenameTypes(renames), annot)
      case _ => tpe //TODO(kπ) probably needs some more cases
    }

    private def widenIfBounds: TypeRepr = tpe match {
      case TypeBounds(lo, hi) =>
        if hi.typeSymbol != defn.AnyClass then hi
        else lo
      case _ => tpe
    }

    private def extractArgs: List[TypeRepr] = tpe match {
      case tpe: NamedType => List.empty
      case AppliedType(tpe, args) => args
      case AnnotatedType(tpe, _) => tpe.extractArgs
      case _ => List.empty
    }

end DerivationImpl
