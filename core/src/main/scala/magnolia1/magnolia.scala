package magnolia1

import magnolia1.Monadic.Ops

import scala.annotation.{compileTimeOnly, tailrec}
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.macros._

/** the object which defines the Magnolia macro */
object Magnolia {
  import CompileTimeState._

  /** derives a generic typeclass instance for the type `T`
    *
    * This is a macro definition method which should be bound to a method defined inside a Magnolia generic derivation object, that is, one
    * which defines the methods `join`, `split` and the type constructor, `Typeclass[_]`. This will typically look like, <pre> object
    * Derivation { // other definitions implicit def gen[T]: Typeclass[T] = Magnolia.gen[T] } </pre> which would support automatic
    * derivation of typeclass instances by calling `Derivation.gen[T]` or with `implicitly[Typeclass[T]]`, if the implicit method is
    * imported into the current scope.
    *
    * If the `gen` is not `implicit`, semi-auto derivation is used instead, whereby implicits will not be generated outside of this ADT.
    *
    * The definition expects a type constructor called `Typeclass`, taking one *-kinded type parameter to be defined on the same object as a
    * means of determining how the typeclass should be genericized. While this may be obvious for typeclasses like `Show[T]` which take only
    * a single type parameter, Magnolia can also derive typeclass instances for types such as `Decoder[Format, Type]` which would typically
    * fix the `Format` parameter while varying the `Type` parameter.
    *
    * While there is no "interface" for a derivation, in the object-oriented sense, the Magnolia macro expects to be able to call certain
    * methods on the object within which it is bound to a method.
    *
    * Specifically, for deriving case classes (product types), the macro will attempt to call the `join` method with an instance of
    * [[CaseClass]], like so, <pre> &lt;derivation&gt;.join(&lt;caseClass&gt;): Typeclass[T] </pre> That is to say, the macro expects there
    * to exist a method called `join` on the derivation object, which may be called with the code above, and for it to return a type which
    * conforms to the type `Typeclass[T]`. The implementation of `join` will therefore typically look like this, <pre> def
    * join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = ... </pre> however, there is the flexibility to provide additional type
    * parameters or additional implicit parameters to the definition, provided these do not affect its ability to be invoked as described
    * above.
    *
    * Likewise, for deriving sealed traits (coproduct or sum types), the macro will attempt to call the `split` method with an instance of
    * [[SealedTrait]], like so, <pre> &lt;derivation&gt;.split(&lt;sealedTrait&gt;): Typeclass[T] </pre> so a definition such as, <pre> def
    * split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ... </pre> will suffice, however the qualifications regarding
    * additional type parameters and implicit parameters apply equally to `split` as to `join`.
    */
  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = Stack.withContext(c) { (stack, depth) =>
    import c.internal._
    import c.universe._
    import definitions._

    val genericType = weakTypeOf[T]
    val genericSymbol = genericType.typeSymbol

    def error(message: => String): Nothing = c.abort(c.enclosingPosition, if (depth > 1) "" else s"magnolia: $message")
    def warning(message: String): Unit = c.warning(c.enclosingPosition, s"magnolia: $message")

    val prefixType = c.prefix.tree.tpe
    val prefixObject = prefixType.typeSymbol
    val prefixName = prefixObject.name.decodedName

    val TypeClassNme = TypeName("Typeclass")
    val typeDefs = prefixType.baseClasses.flatMap { baseClass =>
      baseClass.asType.toType.decls.collectFirst {
        case tpe: TypeSymbol if tpe.name == TypeClassNme =>
          tpe.toType.asSeenFrom(prefixType, baseClass)
      }
    }

    val typeConstructor = typeDefs.headOption.fold(
      error(s"the derivation $prefixObject does not define the Typeclass type constructor")
    )(_.typeConstructor)

    val searchType = appliedType(typeConstructor, genericType)
    val directlyReentrant = stack.top.exists(_.searchType =:= searchType)
    if (directlyReentrant) error("attempt to recurse directly")

    val ArrayObj = reify(Array).tree
    val CallByNeedObj = reify(CallByNeed).tree
    val CaseClassSym = symbolOf[CaseClass[Any, Any]]
    val DebugTpe = typeOf[debug]
    val EitherSym = symbolOf[Either[Any, Any]]
    val JavaAnnotationTpe = typeOf[java.lang.annotation.Annotation]
    val LeftObj = reify(Left).tree
    val MagnoliaUtilObj = reify(MagnoliaUtil).tree
    val MagnoliaMonadicOpsSym = symbolOf[Ops[Any, Any]]
    val MonadicSym = symbolOf[Monadic[Any]]
    val NoneObj = reify(None).tree
    val ParamObj = reify(Param).tree
    val ParamSym = symbolOf[Param[Any, Any]]
    val ReadOnlyCaseClassSym = symbolOf[ReadOnlyCaseClass[Any, Any]]
    val ReadOnlyParamObj = reify(ReadOnlyParam).tree
    val ReadOnlyParamSym = symbolOf[ReadOnlyParam[Any, Any]]
    val RightObj = reify(Right).tree
    val SealedTraitSym = symbolOf[SealedTrait[Any, Any]]
    val SeqTpe = typeOf[Seq[Any]].typeConstructor
    val SomeObj = reify(Some).tree
    val SubtypeObj = reify(Subtype).tree
    val SubtypeTpe = typeOf[Subtype[Any, Any]].typeConstructor
    val TypeNameObj = reify(magnolia1.TypeName).tree
    val ArrayTpe = typeOf[Array[Any]].typeConstructor

    val debug = c.macroApplication.symbol.annotations
      .find(_.tree.tpe <:< DebugTpe)
      .flatMap(_.tree.children.tail.collectFirst {
        case Literal(Constant(arg: String))                            => arg
        case tree if DebugTpe.companion.decls.exists(_ == tree.symbol) => "" // Default constructor, i.e. @debug or @debug()
        case other => error(s"Invalid argument $other in @debug annotation. Only string literals or empty constructor supported")
      })

    object DeferredRef {
      private val symbol = symbolOf[Deferred.type].asClass.module

      def apply(searchType: Type, method: String): Tree =
        q"$symbol.apply[$searchType]($method)"

      def unapply(tree: Tree): Option[String] = tree match {
        case q"$module.apply[$_](${Literal(Constant(method: String))})" if module.symbol == symbol => Some(method)
        case _                                                                                     => None
      }
    }

    /** Returns the chain of owners of `symbol` up to the root package in reverse order. The owner of a symbol is the enclosing
      * package/trait/class/object/method/val/var where it is defined. More efficient than [[ownerChainOf]] because it does not materialize
      * the owner chain.
      */
    def reverseOwnerChainOf(symbol: Symbol): Iterator[Symbol] =
      Iterator.iterate(symbol)(_.owner).takeWhile(owner => owner != null && owner != NoSymbol)

    /** Returns the chain of owners of `symbol` up to the root package.
      * @see
      *   [[reverseOwnerChainOf]]
      */
    def ownerChainOf(symbol: Symbol): Iterator[Symbol] =
      reverseOwnerChainOf(symbol).toVector.reverseIterator

    /** Returns a type-checked reference to the companion object of `clazz` if any. Unlike `clazz.companion` works also for local classes
      * nested in methods/vals/vars.
      */
    def companionOf(clazz: ClassSymbol): Option[Tree] = {
      val fastCompanion = clazz.companion
      if (fastCompanion != NoSymbol) {
        Some(internal.gen.mkAttributedRef(fastCompanion))
      } else {
        val path = ownerChainOf(clazz)
          .zipAll(ownerChainOf(enclosingOwner), NoSymbol, NoSymbol)
          .dropWhile { case (x, y) => x == y }
          .takeWhile(_._1 != NoSymbol)
          .map(_._1.name.toTermName)

        if (path.isEmpty) None
        else {
          val companion = c.typecheck(path.foldLeft[Tree](Ident(path.next()))(Select(_, _)), silent = true)
          if (companion.isEmpty) None else Some(companion)
        }
      }
    }

    val enclosingVals = reverseOwnerChainOf(enclosingOwner)
      .collect {
        case enclosing: TermSymbol if enclosing.isVal || enclosing.isLazy => enclosing
      }
      .toSet[Symbol]

    def knownSubclassesOf(parent: ClassSymbol): Set[Symbol] = {
      val (abstractChildren, concreteChildren) = parent.knownDirectSubclasses.partition(_.isAbstract)
      for (child <- concreteChildren) {
        child.typeSignature // load type signature
        if (!child.isFinal && !child.asClass.isCaseClass)
          error(s"child $child of $parent is neither final nor a case class")
      }

      concreteChildren union abstractChildren.flatMap { child =>
        child.typeSignature // load type signature
        val childClass = child.asClass
        if (childClass.isSealed) knownSubclassesOf(childClass)
        else error(s"child $child of $parent is not sealed")
      }
    }

    def annotationTrees(annotations: List[Annotation]): List[Tree] =
      annotations.collect {
        // filtering out NoType annotations due to https://github.com/scala/bug/issues/12536
        case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) && !(annotation.tree.tpe <:< NoType) =>
          annotation.tree
      }

    def annotationsOf(symbol: Symbol): (List[Tree], List[Tree]) = {
      @tailrec
      def fromBaseClassesMembers(owner: Symbol): List[Annotation] =
        if (owner.isClass) {
          val baseClasses = owner.asClass.baseClasses
            .filterNot(bc => bc.fullName.contains("java.lang.Object") || bc.fullName.startsWith("scala."))

          val fromMembers = baseClasses
            .flatMap(_.asType.toType.members)
            .filter(s =>
              (symbol, s) match {
                case (m1: MethodSymbol, m2: MethodSymbol) if m1.name == m2.name && m1.paramLists.size == m2.paramLists.size => true
                case (t1: TermSymbol, t2: TermSymbol) if t1.name == t2.name                                                 => true
                case _                                                                                                      => false
              }
            )
            .flatMap(_.annotations)

          val fromConstructorParameters = baseClasses
            .flatMap {
              case c: ClassSymbol =>
                c.primaryConstructor match {
                  case m: MethodSymbol =>
                    m.paramLists.flatten
                      .filter { s =>
                        (symbol, s) match {
                          case (t1: TermSymbol, t2: TermSymbol) if t1.name == t2.name => true
                          case _                                                      => false
                        }
                      }
                      .flatMap(_.annotations)
                  case _ => List.empty
                }
              case _ => List.empty
            }

          fromMembers ++ fromConstructorParameters
        } else fromBaseClassesMembers(owner.owner)

      def fromBaseClasses(): List[Annotation] =
        symbol.asClass.baseClasses.collect { case s if s.name != symbol.name => s.annotations }.flatten

      val inherited = if (symbol.isClass) fromBaseClasses() else fromBaseClassesMembers(symbol.owner)

      (annotationTrees(symbol.annotations), annotationTrees(inherited))
    }

    def typeAnnotationsOf(symbol: Symbol, fromParents: Boolean): List[Tree] = {
      val typeAnnotations = if (fromParents) {
        symbol.typeSignature match {
          case ClassInfoType(parents, _, _) =>
            parents.flatMap {
              case AnnotatedType(typeAnnotations, _) => typeAnnotations
              case _                                 => Nil
            }
          case _ => Nil
        }
      } else {
        symbol.typeSignature match {
          case AnnotatedType(typeAnnotations, _) => typeAnnotations
          case _                                 => Nil
        }
      }

      annotationTrees(typeAnnotations)
    }

    def checkMethod(termName: String, category: String, expected: String): Unit = {
      val firstParamBlock = extractParameterBlockFor(termName, category)
      if (firstParamBlock.lengthCompare(1) != 0)
        error(s"the method `$termName` should take a single parameter of type $expected")
    }

    def extractParameterBlockFor(termName: String, category: String): List[Symbol] = {
      val term = TermName(termName)
      val classWithTerm = c.prefix.tree.tpe.baseClasses
        .find(cls => cls.asType.toType.decl(term) != NoSymbol)
        .getOrElse {
          val searchType = stack.top.map(_.searchType.toString)
          val additionalInfo = searchType match {
            case Some(tpe) => s"unable to derive $tpe -- "
            case None      => ""
          }

          error(
            s"${additionalInfo}the method `$termName` must be defined on the derivation $prefixObject to derive typeclasses for $category"
          )
        }

      classWithTerm.asType.toType.decl(term).asTerm.asMethod.paramLists.head
    }

    lazy val (isReadOnly, caseClassSymbol, paramSymbol) =
      extractParameterBlockFor("join", "case classes").headOption.map(_.typeSignature.typeSymbol) match {
        case Some(ReadOnlyCaseClassSym) => (true, ReadOnlyCaseClassSym, ReadOnlyParamSym)
        case Some(CaseClassSym)         => (false, CaseClassSym, ParamSym)
        case _                          => error("Parameter for `join` needs be either magnolia1.CaseClass or magnolia1.ReadOnlyCaseClass")
      }

    // fullAuto means we should directly infer everything, including external
    // members of the ADT, that isn't inferred by the compiler.
    def fullAuto: Boolean = c.macroApplication.symbol.isImplicit

    // semiAuto means that we should directly derive only the sealed ADT but not
    // external members (i.e. things that are not a subtype of T).
    def semiAuto(subType: Type): Boolean =
      genericSymbol.isClass && genericSymbol.asClass.isSealed && subType <:< genericType

    // Trees that contain Deferred references might not be self contained and should not be cached.
    def shouldCache(tree: Tree): Boolean = !tree.exists {
      case DeferredRef(_) => true
      case _              => false
    }

    val expandDeferred = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case DeferredRef(method) => q"${TermName(method)}"
        case _                   => super.transform(tree)
      }
    }

    def deferredVal(name: TermName, tpe: Type, rhs: Tree): Tree = {
      val shouldBeLazy = rhs.exists {
        case DeferredRef(_) => true
        case tree           => enclosingVals.contains(tree.symbol)
      }

      if (shouldBeLazy) q"lazy val $name: $tpe = $rhs"
      else q"val $name = $rhs"
    }

    def typeclassTree(genericType: Type, typeConstructor: Type, assignedName: TermName): Either[String, Tree] = {
      val searchType = appliedType(typeConstructor, genericType)
      val deferredRef =
        for (methodName <- stack find searchType)
          yield DeferredRef(searchType, methodName.decodedName.toString)

      deferredRef.fold {
        val path = ChainedImplicit(s"$prefixName.Typeclass", genericType.toString)
        val frame = stack.Frame(path, searchType, assignedName)
        stack.recurse(frame, searchType, shouldCache) {
          Option(c.inferImplicitValue(searchType))
            .filterNot(_.isEmpty)
            .orElse {
              if (!fullAuto && !semiAuto(genericType)) None
              else directInferImplicit(genericType, typeConstructor)
            }
            .toRight {
              if (depth > 1) ""
              else {
                val (top, paths) = stack.trace
                val missingType = top.fold(searchType)(_.searchType)
                val typeClassName = s"${missingType.typeSymbol.name.decodedName}.Typeclass"
                val genericType = missingType.typeArgs.head
                val trace = paths.mkString("    in ", "\n    in ", "\n")
                s"could not find $typeClassName for type $genericType\n$trace"
              }
            }
        }
      }(Right(_))
    }

    def directInferImplicit(genericType: Type, typeConstructor: Type): Option[Tree] = {
      val genericTypeName = genericType.typeSymbol.name.decodedName.toString.toLowerCase
      val assignedName = c.freshName(TermName(s"${genericTypeName}Typeclass")).encodedName.toTermName
      val typeSymbol = genericType.typeSymbol
      val classType = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isRefinedType = PartialFunction.cond(genericType.dealias) { case _: RefinedType => true }
      val isCaseClass = classType.exists(_.isCaseClass)
      val isCaseObject = classType.exists(_.isModuleClass)

      val isSealedTrait = classType.exists(ct => ct.isSealed && !ct.isJavaEnum)
      val (classAnnotationTrees, inheritedClassAnnotationTrees) = annotationsOf(typeSymbol)
      val classTypeAnnotationTrees = typeAnnotationsOf(typeSymbol, fromParents = true)

      val primitives = Set(
        DoubleTpe,
        FloatTpe,
        ShortTpe,
        ByteTpe,
        IntTpe,
        LongTpe,
        CharTpe,
        BooleanTpe,
        UnitTpe
      )

      val isValueClass = genericType <:< AnyValTpe && !primitives.exists(_ =:= genericType)
      val resultType = appliedType(typeConstructor, genericType)
      val typeName = c.freshName(TermName("typeName"))

      def typeNameOf(tpe: Type): Tree = {
        val symbol = tpe.typeSymbol
        val typeArgNames = for (typeArg <- tpe.typeArgs) yield typeNameOf(typeArg)
        q"$TypeNameObj(${symbol.owner.fullName}, ${symbol.name.decodedName.toString}, $typeArgNames)"
      }

      val typeNameDef = q"val $typeName = ${typeNameOf(genericType.dealias)}"
      lazy val paramType = appliedType(paramSymbol, typeConstructor, genericType)
      lazy val caseClassType = appliedType(caseClassSymbol, typeConstructor, genericType)

      def construct(impl: Tree): Tree = q"""
        override def construct[Return](makeParam: $paramType => Return): $genericType =
          $impl
      """

      def constructMonadic(f: TypeName, impl: Tree): Tree = q"""
        def constructMonadic[$f[_], Return](makeParam: $paramType => $f[Return])(implicit monadic: $MonadicSym[$f]): $f[$genericType] =
          $impl
      """

      def constructEither(impl: Tree): Tree = q"""
        def constructEither[Err, PType](makeParam: $paramType => $EitherSym[Err, PType]): $EitherSym[$ListClass[Err], $genericType] =
          $impl
      """

      def rawConstruct(impl: Tree): Tree = q"""
        def rawConstruct(fieldValues: ${typeOf[Seq[Any]]}): $genericType =
          $impl
      """

      def constructPartialAssignmentFunction(typeclasses: List[(Type, Tree)], arrayElementType: Type): (TermName, Tree) = {
        val functionName = c.freshName(TermName("partialAssignments"))
        val arrayVal = c.freshName(TermName("arr"))
        val startVal = c.freshName(TermName("start"))
        val assignments = typeclasses.zipWithIndex.map { case ((subType, typeclass), idx) =>
          val symbol = subType.typeSymbol
          val (annotationTrees, inheritedAnnotationTrees) = annotationsOf(symbol)
          q"""$arrayVal($startVal + $idx) = $SubtypeObj[$typeConstructor, $genericType, $subType](
            ${typeNameOf(subType)},
            $idx,
            $ArrayObj[$AnyTpe](..${annotationTrees}),
            $ArrayObj[$AnyTpe](..${inheritedAnnotationTrees}),
            $ArrayObj[$AnyTpe](..${typeAnnotationsOf(symbol, fromParents = true)}),
            $CallByNeedObj($typeclass),
            (t: $genericType) => t.isInstanceOf[$subType],
            (t: $genericType) => t.asInstanceOf[$subType]
          )"""
        }
        val arrayTpe = appliedType(ArrayTpe, arrayElementType)
        val tree =
          q"""def $functionName($arrayVal: $arrayTpe, $startVal: $IntTpe) = {
              ..$assignments
            }
          """

        (functionName, tree)
      }

      val result = if (isRefinedType) {
        error(s"could not infer $prefixName.Typeclass for refined type $genericType")
      } else if (isCaseObject) {
        val classBody =
          if (isReadOnly) List(EmptyTree)
          else {
            val module = Ident(genericType.typeSymbol.asClass.module)
            List(
              construct(module),
              constructMonadic(c.freshName(TypeName("F")), q"monadic.point($module)"),
              constructEither(q"$RightObj($module)"),
              rawConstruct(module)
            )
          }

        val impl = q"""
          $typeNameDef
          ${c.prefix}.join(new $caseClassType(
            $typeName,
            true,
            false,
            new $ArrayClass(0),
            $ArrayObj(..$classAnnotationTrees),
            $ArrayObj(..$inheritedClassAnnotationTrees),
            $ArrayObj(..$classTypeAnnotationTrees)
          ) {
            ..$classBody
          })
        """
        Some(impl)
      } else if (isCaseClass || isValueClass) {
        val headParamList = classType
          .flatMap(_.primaryConstructor.asMethod.typeSignatureIn(genericType).paramLists.headOption)
          .map(_.map(_.asTerm))

        val caseClassParameters = genericType.decls.sorted.collect(
          if (isValueClass) { case p: TermSymbol if p.isParamAccessor && p.isMethod => p }
          else { case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p }
        )

        val (factoryObject, factoryMethod) = {
          if (isReadOnly && isValueClass) ReadOnlyParamObj -> TermName("valueParam")
          else if (isReadOnly) ReadOnlyParamObj -> TermName("apply")
          else if (isValueClass) ParamObj -> TermName("valueParam")
          else ParamObj -> TermName("apply")
        }

        case class CaseParam(paramName: TermName, repeated: Boolean, typeclass: Tree, paramType: Type, ref: TermName, paramTypeName: Tree) {
          def compile(
              params: TermName,
              idx: Int,
              default: Option[Tree],
              annotations: List[Tree],
              inheritedAnnotations: List[Tree],
              typeAnnotations: List[Tree]
          ): Tree =
            q"""$params($idx) = $factoryObject.$factoryMethod[$typeConstructor, $genericType, $paramType](
              ${paramName.toString.trim},
              $paramTypeName,
              ${if (isValueClass) q"(t: $genericType) => t.$paramName" else q"$idx"},
              $repeated,
              $CallByNeedObj($ref),
              ..${default.toList.map(d => q"$CallByNeedObj.withValueEvaluator($d)")},
              $ArrayObj(..$annotations): _root_.scala.Array[_root_.scala.Any],
              $ArrayObj(..$inheritedAnnotations): _root_.scala.Array[_root_.scala.Any],
              $ArrayObj(..$typeAnnotations): _root_.scala.Array[_root_.scala.Any]
            )"""
        }

        val caseParamsReversed = caseClassParameters.foldLeft[List[CaseParam]](Nil) { (acc, param) =>
          val paramName = param.name.decodedName.toTermName
          val (repeated, paramType) = param.typeSignatureIn(genericType).resultType match {
            case TypeRef(_, symbol, typeArgs) if symbol == RepeatedParamClass =>
              true -> appliedType(SeqTpe, typeArgs)
            case tpe =>
              false -> tpe
          }
          val paramTypeName = q"${typeNameOf(paramType)}"

          acc
            .find(_.paramType =:= paramType)
            .fold {
              val path = ProductType(paramName.toString.trim, genericType.toString)
              val frame = stack.Frame(path, resultType, assignedName)
              val searchType = appliedType(typeConstructor, paramType)
              val ref = c.freshName(TermName("paramTypeclass"))
              val derivedImplicit = stack
                .recurse(frame, searchType, shouldCache) {
                  typeclassTree(paramType, typeConstructor, ref)
                }
                .fold(error(_), identity)
              val assigned = deferredVal(ref, searchType, derivedImplicit)
              CaseParam(paramName, repeated, assigned, paramType, ref, paramTypeName) :: acc
            } { backRef =>
              CaseParam(paramName, repeated, q"()", paramType, backRef.ref, paramTypeName) :: acc
            }
        }

        val caseParams = caseParamsReversed.reverse
        val paramsWithIndex = caseParams.zipWithIndex
        val paramsVal = c.freshName(TermName("parameters"))
        val annotations = headParamList.getOrElse(Nil).map(annotationsOf(_))
        val typeAnnotations = headParamList.getOrElse(Nil).map(typeAnnotationsOf(_, fromParents = false))

        val assignments = if (isReadOnly) {
          for ((((param, idx), (annList, inheritedAnnList)), tpeAnnList) <- paramsWithIndex zip annotations zip typeAnnotations)
            yield param.compile(paramsVal, idx, None, annList, inheritedAnnList, tpeAnnList)
        } else {
          val defaults = headParamList.fold[List[Tree]](Nil) { params =>
            def allNone = params.map(_ => NoneObj)
            if (!params.exists(_.isParamWithDefault)) allNone
            else
              classType.flatMap(ct => companionOf(ct)).fold(allNone) { companion =>
                val companionType = companion.tpe
                for ((p, idx) <- params.zipWithIndex)
                  yield
                    if (!p.isParamWithDefault) NoneObj
                    else {
                      val default = companionType.member(TermName(s"<init>$$default$$${idx + 1}").encodedName)
                      if (default.typeSignature.finalResultType <:< p.typeSignature) {
                        q"$SomeObj($companion.$default)"
                      } else {
                        warning(s"ignoring default value for parameter ${p.name} of $genericType due to type mismatch")
                        NoneObj
                      }
                    }
              }
          }

          for (
            ((((param, idx), default), (annList, inheritedAnnList)), typeAnnList) <-
              paramsWithIndex zip defaults zip annotations zip typeAnnotations
          )
            yield param.compile(paramsVal, idx, Some(default), annList, inheritedAnnList, typeAnnList)
        }

        val caseClassBody =
          if (isReadOnly) List(EmptyTree)
          else {
            val genericParams = paramsWithIndex.map { case (typeclass, idx) =>
              val arg = q"makeParam($paramsVal($idx)).asInstanceOf[${typeclass.paramType}]"
              if (typeclass.repeated) q"$arg: _*" else arg
            }

            val rawGenericParams = paramsWithIndex.map { case (typeclass, idx) =>
              val arg = q"fieldValues($idx).asInstanceOf[${typeclass.paramType}]"
              if (typeclass.repeated) q"$arg: _*" else arg
            }

            val f = c.freshName(TypeName("F"))
            val forParams = paramsWithIndex.map { case (typeclass, idx) =>
              val p = TermName(s"p$idx")
              (
                if (typeclass.repeated) q"$p: _*" else q"$p",
                fq"$p <- new $MagnoliaMonadicOpsSym(makeParam($paramsVal($idx)).asInstanceOf[$f[${typeclass.paramType}]])"
              )
            }

            val constructMonadicImpl =
              if (forParams.isEmpty) q"monadic.point(new $genericType())"
              else q"for(..${forParams.map(_._2)}) yield new $genericType(..${forParams.map(_._1)})"

            val constructEitherImpl =
              if (caseParams.isEmpty) q"$RightObj(new $genericType())"
              else {
                val eitherVals = paramsWithIndex.map { case (param, idx) =>
                  val p = TermName(s"p$idx")
                  val v = TermName(s"v$idx")
                  (
                    p,
                    if (param.repeated) q"$v: _*" else q"$v",
                    q"val $p = makeParam($paramsVal($idx)).asInstanceOf[$EitherSym[Err, ${param.paramType}]]",
                    pq"$RightObj($v)"
                  )
                }

                // DESNOTE(2019-12-05, pjrt): Due to limits on tuple sizes, and lack of <*>, we split the params
                // into a list of tuples of at most 22 in size
                val limited = eitherVals.grouped(22).toList

                q"""
                ..${eitherVals.map(_._3)}
                (..${limited.map(k => q"(..${k.map(_._1)})")}) match {
                  case (..${limited.map(k => q"(..${k.map(_._4)})")}) =>
                    $RightObj(new $genericType(..${eitherVals.map(_._2)}))
                  case _ =>
                    $LeftObj($MagnoliaUtilObj.keepLeft(..${eitherVals.map(_._1)}))
                }
              """
              }

            List(
              construct(q"new $genericType(..$genericParams)"),
              constructMonadic(f, constructMonadicImpl),
              constructEither(constructEitherImpl),
              rawConstruct(q"""
              $MagnoliaUtilObj.checkParamLengths(fieldValues, $paramsVal.length, $typeName.full)
              new $genericType(..$rawGenericParams)
            """)
            )
          }

        Some(q"""{
            ..${caseParams.map(_.typeclass)}
            val $paramsVal = new $ArrayClass[$paramType](${assignments.length})
            ..$assignments
            $typeNameDef
            ${c.prefix}.join(new $caseClassType(
              $typeName,
              false,
              $isValueClass,
              $paramsVal,
              $ArrayObj(..$classAnnotationTrees),
              $ArrayObj(..$inheritedClassAnnotationTrees),
              $ArrayObj(..$classTypeAnnotationTrees)
            ) {
              ..$caseClassBody
            })
          }""")
      } else if (isSealedTrait) {
        checkMethod("split", "sealed traits", "SealedTrait[Typeclass, _]")
        val genericSubtypes = knownSubclassesOf(classType.get).toList.sortBy(_.fullName)
        val subtypes = genericSubtypes.flatMap { sub =>
          val subType = sub.asType.toType // FIXME: Broken for path dependent types
          val typeParams = sub.asType.typeParams
          val typeArgs = thisType(sub).baseType(genericType.typeSymbol).typeArgs
          val mapping = typeArgs.map(_.typeSymbol).zip(genericType.typeArgs).toMap
          val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
          val applied = appliedType(subType.typeConstructor, newTypeArgs)
          if (applied <:< genericType) existentialAbstraction(typeParams, applied) :: Nil else Nil
        }

        if (subtypes.isEmpty) {
          error(s"could not find any direct subtypes of $typeSymbol")
        }

        val typeclasses = for (subType <- subtypes) yield {
          val path = CoproductType(genericType.toString)
          val frame = stack.Frame(path, resultType, assignedName)
          subType -> stack
            .recurse(frame, appliedType(typeConstructor, subType), shouldCache) {
              typeclassTree(subType, typeConstructor, termNames.ERROR)
            }
            .fold(error(_), identity)
        }

        val subType = appliedType(SubtypeTpe, typeConstructor, genericType)
        val groupSize = 500
        val (functionNames, partialAssignmentFunctions) = typeclasses
          .grouped(groupSize)
          .toList
          .map(constructPartialAssignmentFunction(_, subType))
          .unzip

        val subtypesVal = c.freshName(TermName("subtypes"))
        val combinations = functionNames.zipWithIndex.map { case (name, idx) => q"""$name($subtypesVal, ${idx * groupSize})""" }

        Some(q"""{
          ..$partialAssignmentFunctions
          val $subtypesVal = new $ArrayClass[$subType](${typeclasses.size})
          ..$combinations
          $typeNameDef
          ${c.prefix}.split(new $SealedTraitSym(
            $typeName,
            $subtypesVal: $ArrayClass[$subType],
            $ArrayObj(..$classAnnotationTrees),
            $ArrayObj(..$inheritedClassAnnotationTrees),
            $ArrayObj(..$classTypeAnnotationTrees)
          ))
        }""")
      } else if (!typeSymbol.isParameter) {
        c.prefix.tree.tpe.baseClasses
          .find { cls =>
            cls.asType.toType.decl(TermName("fallback")) != NoSymbol
          }
          .map { _ =>
            warning(s"using fallback derivation for $genericType")
            q"""${c.prefix}.fallback[$genericType]"""
          }
      } else None

      for (term <- result)
        yield q"""{
        ${deferredVal(assignedName, resultType, term)}
        $assignedName
      }"""
    }

    val result = stack
      .find(searchType)
      .map(enclosingRef => DeferredRef(searchType, enclosingRef.toString))
      .orElse(directInferImplicit(genericType, typeConstructor))

    for (tree <- result) if (debug.isDefined && genericType.toString.contains(debug.get)) {
      c.echo(c.enclosingPosition, s"Magnolia macro expansion for $genericType")
      c.echo(NoPosition, s"... = ${showCode(tree)}\n\n")
    }

    val dereferencedResult =
      if (stack.nonEmpty) result
      else for (tree <- result) yield c.untypecheck(expandDeferred.transform(tree))

    dereferencedResult.getOrElse {
      error(s"could not infer $prefixName.Typeclass for type $genericType")
    }
  }

  private[Magnolia] def subtype[Tc[_], T, S <: T](
      name: TypeName,
      idx: Int,
      anns: Array[Any],
      inheritedAnns: Array[Any],
      tpeAnns: Array[Any],
      tc: CallByNeed[Tc[S]],
      isType: T => Boolean,
      asType: T => S
  ): Subtype[Tc, T] = Subtype(name, idx, anns, inheritedAnns, tpeAnns, tc, isType, asType)

  private[Magnolia] def readOnlyParam[Tc[_], T, P](
      name: String,
      typeNameParam: TypeName,
      idx: Int,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      annotationsArrayParam: Array[Any],
      inheritedAnnotationsArrayParam: Array[Any],
      typeAnnotationsArrayParam: Array[Any]
  ): ReadOnlyParam[Tc, T] =
    ReadOnlyParam(
      name,
      typeNameParam,
      idx,
      isRepeated,
      typeclassParam,
      annotationsArrayParam,
      inheritedAnnotationsArrayParam,
      typeAnnotationsArrayParam
    )

  private[Magnolia] def readOnlyValueParam[Tc[_], T, P](
      name: String,
      typeNameParam: TypeName,
      deref: T => P,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      annotationsArrayParam: Array[Any],
      inheritedAnnotationsArrayParam: Array[Any],
      typeAnnotationsArrayParam: Array[Any]
  ): ReadOnlyParam[Tc, T] =
    ReadOnlyParam.valueParam(
      name,
      typeNameParam,
      deref,
      isRepeated,
      typeclassParam,
      annotationsArrayParam,
      inheritedAnnotationsArrayParam,
      typeAnnotationsArrayParam
    )

  /** constructs a new [[Param]] instance
    *
    * This method is intended to be called only from code generated by the Magnolia macro, and should not be called directly from users'
    * code.
    */
  private[Magnolia] def param[Tc[_], T, P](
      name: String,
      typeNameParam: TypeName,
      idx: Int,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      defaultVal: CallByNeed[Option[P]],
      annotationsArrayParam: Array[Any],
      inheritedAnnotationsArrayParam: Array[Any],
      typeAnnotationsArrayParam: Array[Any]
  ): Param[Tc, T] =
    Param.apply(
      name,
      typeNameParam,
      idx,
      isRepeated,
      typeclassParam,
      defaultVal,
      annotationsArrayParam,
      inheritedAnnotationsArrayParam,
      typeAnnotationsArrayParam
    )

  private[Magnolia] def valueParam[Tc[_], T, P](
      name: String,
      typeNameParam: TypeName,
      deref: T => P,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      defaultVal: CallByNeed[Option[P]],
      annotationsArrayParam: Array[Any],
      inheritedAnnotationsArrayParam: Array[Any],
      typeAnnotationsArrayParam: Array[Any]
  ): Param[Tc, T] =
    Param.valueParam(
      name,
      typeNameParam,
      deref,
      isRepeated,
      typeclassParam,
      defaultVal,
      annotationsArrayParam,
      inheritedAnnotationsArrayParam,
      typeAnnotationsArrayParam
    )

  private[Magnolia] final def checkParamLengths(fieldValues: Seq[Any], paramsLength: Int, typeName: String): Unit =
    MagnoliaUtil.checkParamLengths(fieldValues, paramsLength, typeName)

  private[Magnolia] final def keepLeft[A](values: Either[A, _]*): List[A] = MagnoliaUtil.keepLeft(values: _*)

}

@compileTimeOnly("magnolia1.Deferred is used for derivation of recursive typeclasses")
object Deferred { def apply[T](method: String): T = ??? }

private[magnolia1] object CompileTimeState {

  sealed abstract class TypePath(path: String) { override def toString: String = path }
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String) extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
      extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack[C <: whitebox.Context with Singleton] {
    private var frames = List.empty[Frame]
    private var errors = List.empty[Frame]
    private val cache = mutable.Map.empty[C#Type, C#Tree]

    def isEmpty: Boolean = frames.isEmpty
    def nonEmpty: Boolean = frames.nonEmpty
    def top: Option[Frame] = frames.headOption
    def pop(): Unit = frames = frames drop 1
    def push(frame: Frame): Unit = frames ::= frame

    def within[A](frame: Frame)(thunk: => A): A = {
      push(frame)
      try thunk
      finally pop()
    }

    def clear(): Unit = {
      frames = Nil
      errors = Nil
      cache.clear()
    }

    def find(searchType: C#Type): Option[C#TermName] = frames.collectFirst {
      case Frame(_, tpe, term) if tpe =:= searchType => term
    }

    def recurse[T <: C#Tree](frame: Frame, searchType: C#Type, shouldCache: C#Tree => Boolean)(
        thunk: => Either[String, C#Tree]
    ): Either[String, C#Tree] = within(frame) {
      cache.get(searchType) match {
        case Some(cached) =>
          errors = Nil
          Right(cached)
        case None =>
          thunk match {
            case failure @ Left(_) =>
              errors ::= frame
              failure
            case success @ Right(tree) =>
              if (shouldCache(tree)) cache(searchType) = tree
              errors = Nil
              success
          }
      }
    }

    def trace: (Option[Frame], List[TypePath]) = {
      val allFrames = errors reverse_::: frames
      val trace = allFrames.drop(1).zip(allFrames).collect {
        case (Frame(path, tp1, _), Frame(_, tp2, _)) if !(tp1 =:= tp2) => path
      }
      (allFrames.headOption, trace)
    }

    override def toString: String =
      frames.mkString("magnolia stack:\n", "\n", "\n")

    case class Frame(path: TypePath, searchType: C#Type, term: C#TermName)
  }

  object Stack {
    // Cheating to satisfy Singleton bound (which improves type inference).
    private val dummyContext: whitebox.Context = null
    private val threadLocalStack = ThreadLocal.withInitial[Stack[dummyContext.type]](() => new Stack[dummyContext.type])
    private val threadLocalWorkSet = ThreadLocal.withInitial[mutable.Set[whitebox.Context#Symbol]](() => mutable.Set.empty)

    def withContext(c: whitebox.Context)(fn: (Stack[c.type], Int) => c.Tree): c.Tree = {
      val stack = threadLocalStack.get()
      val workSet = threadLocalWorkSet.get()
      workSet += c.macroApplication.symbol
      val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
      try fn(stack.asInstanceOf[Stack[c.type]], depth)
      finally if (depth <= 1) {
        threadLocalStack.remove()
        threadLocalWorkSet.remove()
      }
    }
  }
}

object CallByNeed {

  /** Initializes a class that allows for suspending evaluation of a value until it is needed. Evaluation of a value via `.value` can only
    * happen once. Evaluation of a value via `.valueEvaluator` will return None. For evaluating a value multiple times, please construct a
    * CallByNeed via CallByNeed.withValueEvaluator(value)
    */
  def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a, () => false)

  /** Initializes a class that allows for suspending evaluation of a value until it is needed. Evaluation of a value via `.value` can only
    * happen once. Evaluation of a value via `.valueEvaluator.map(evaluator => evaluator())` will happen every time the evaluator is called
    */
  def withValueEvaluator[A](a: => A): CallByNeed[A] = new CallByNeed(() => a, () => true)
}

// Both params are later nullified to reduce overhead and increase performance.
// The supportDynamicValueEvaluation is passed as a function so that it can be nullified. Otherwise, there is no need for the function value.
final class CallByNeed[+A] private (private[this] var eval: () => A, private var supportDynamicValueEvaluation: () => Boolean)
    extends Serializable {

  // This second constructor is necessary to support backwards compatibility for v1.1.9 and earlier
  def this(eval: () => A) = this(eval, () => false)

  val valueEvaluator: Option[() => A] = {
    val finalRes = if (supportDynamicValueEvaluation()) {
      val res = Some(eval.fv)
      eval = null
      res
    } else {
      None
    }
    supportDynamicValueEvaluation = null
    finalRes
  }

  lazy val value: A = {
    if (eval == null) {
      valueEvaluator.get.fv()
    } else {
      val result = eval()
      eval = null
      result
    }
  }
}
