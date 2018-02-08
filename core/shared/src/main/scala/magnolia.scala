package magnolia

import scala.collection.mutable
import scala.language.existentials
import scala.language.higherKinds
import scala.reflect.macros._

/** the object which defines the Magnolia macro */
object Magnolia {
  import CompileTimeState._

  /** derives a generic typeclass instance for the type `T`
    *
    *  This is a macro definition method which should be bound to a method defined inside a Magnolia
    *  generic derivation object, that is, one which defines the methods `combine`, `dispatch` and
    *  the type constructor, `Typeclass[_]`. This will typically look like,
    *  <pre>
    *  object Derivation {
    *    // other definitions
    *    implicit def gen[T]: Typeclass[T] = Magnolia.gen[T]
    *  }
    *  </pre>
    *  which would support automatic derivation of typeclass instances by calling
    *  `Derivation.gen[T]` or with `implicitly[Typeclass[T]]`, if the implicit method is imported
    *  into the current scope.
    *
    *  The definition expects a type constructor called `Typeclass`, taking one *-kinded type
    *  parameter to be defined on the same object as a means of determining how the typeclass should
    *  be genericized. While this may be obvious for typeclasses like `Show[T]` which take only a
    *  single type parameter, Magnolia can also derive typeclass instances for types such as
    *  `Decoder[Format, Type]` which would typically fix the `Format` parameter while varying the
    *  `Type` parameter.
    *
    *  While there is no "interface" for a derivation, in the object-oriented sense, the Magnolia
    *  macro expects to be able to call certain methods on the object within which it is bound to a
    *  method.
    *
    *  Specifically, for deriving case classes (product types), the macro will attempt to call the
    *  `combine` method with an instance of [[CaseClass]], like so,
    *  <pre>
    *    &lt;derivation&gt;.combine(&lt;caseClass&gt;): Typeclass[T]
    *  </pre>
    *  That is to say, the macro expects there to exist a method called `combine` on the derivation
    *  object, which may be called with the code above, and for it to return a type which conforms
    *  to the type `Typeclass[T]`. The implementation of `combine` will therefore typically look
    *  like this,
    *  <pre>
    *    def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  however, there is the flexibility to provide additional type parameters or additional
    *  implicit parameters to the definition, provided these do not affect its ability to be invoked
    *  as described above.
    *
    *  Likewise, for deriving sealed traits (coproduct or sum types), the macro will attempt to call
    *  the `dispatch` method with an instance of [[SealedTrait]], like so,
    *  <pre>
    *    &lt;derivation&gt;.dispatch(&lt;sealedTrait&gt;): Typeclass[T]
    *  </pre>
    *  so a definition such as,
    *  <pre>
    *    def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  will suffice, however the qualifications regarding additional type parameters and implicit
    *  parameters apply equally to `dispatch` as to `combine`.
    *  */
  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = Stack.withContext(c) { stack =>
    import c.universe._
    import internal._

    val debug = c.macroApplication.symbol.annotations
      .find(_.tree.tpe <:< typeOf[debug])
      .flatMap(_.tree.children.tail.collectFirst { case Literal(Constant(s: String)) => s })

    val magnoliaPkg = c.mirror.staticPackage("magnolia")
    val scalaPkg = c.mirror.staticPackage("scala")

    val repeatedParamClass = definitions.RepeatedParamClass
    val scalaSeqType = typeOf[Seq[_]].typeConstructor

    val prefixType = c.prefix.tree.tpe
    val prefixObject = prefixType.typeSymbol
    val prefixName = prefixObject.name.decodedName

    val typeDefs = prefixType.baseClasses.flatMap { cls =>
      cls.asType.toType.decls.filter(_.isType).find(_.name.toString == "Typeclass").map { tpe =>
        tpe.asType.toType.asSeenFrom(prefixType, cls)
      }
    }

    val typeConstructor = typeDefs.headOption.fold {
      c.abort(
        c.enclosingPosition,
        s"magnolia: the derivation $prefixObject does not define the Typeclass type constructor"
      )
    }(_.typeConstructor)

    def checkMethod(termName: String, category: String, expected: String): Unit = {
      val term = TermName(termName)
      val combineClass = c.prefix.tree.tpe.baseClasses
        .find { cls =>
          cls.asType.toType.decl(term) != NoSymbol
        }
        .getOrElse {
          c.abort(
            c.enclosingPosition,
            s"magnolia: the method `$termName` must be defined on the derivation $prefixObject to derive typeclasses for $category"
          )
        }
      val firstParamBlock = combineClass.asType.toType.decl(term).asTerm.asMethod.paramLists.head
      if (firstParamBlock.lengthCompare(1) != 0)
        c.abort(c.enclosingPosition,
                s"magnolia: the method `combine` should take a single parameter of type $expected")
    }

    // FIXME: Only run these methods if they're used, particularly `dispatch`
    checkMethod("combine", "case classes", "CaseClass[Typeclass, _]")
    checkMethod("dispatch", "sealed traits", "SealedTrait[Typeclass, _]")

    val removeDeferred = new Transformer {
      override def transform(tree: Tree) = tree match {
        case q"$magnolia.Deferred.apply[$_](${Literal(Constant(method: String))})"
            if magnolia.symbol == magnoliaPkg =>
          q"${TermName(method)}"
        case _ =>
          super.transform(tree)
      }
    }

    def typeclassTree(genericType: Type, typeConstructor: Type): Tree = {
      val searchType = appliedType(typeConstructor, genericType)
      val deferredRef = for (methodName <- stack find searchType) yield {
        val methodAsString = methodName.decodedName.toString
        q"$magnoliaPkg.Deferred.apply[$searchType]($methodAsString)"
      }

      deferredRef.getOrElse {
        val path = ChainedImplicit(s"$prefixName.Typeclass", genericType.toString)
        val frame = stack.Frame(path, searchType, termNames.EMPTY)
        stack.recurse(frame, searchType) {
          Option(c.inferImplicitValue(searchType))
            .filterNot(_.isEmpty)
            .orElse(directInferImplicit(genericType, typeConstructor))
            .getOrElse {
              val missingType = stack.top.fold(searchType)(_.searchType.asInstanceOf[Type])
              val typeClassName = s"${missingType.typeSymbol.name.decodedName}.Typeclass"
              val genericType = missingType.typeArgs.head
              val trace = stack.trace.mkString("    in ", "\n    in ", "\n")
              c.abort(c.enclosingPosition,
                      s"magnolia: could not find $typeClassName for type $genericType\n$trace")
            }
        }
      }
    }

    def directInferImplicit(genericType: Type, typeConstructor: Type): Option[Tree] = {
      val genericTypeName = genericType.typeSymbol.name.decodedName.toString.toLowerCase
      val assignedName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
      val typeSymbol = genericType.typeSymbol
      val classType = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isCaseClass = classType.exists(_.isCaseClass)
      val isCaseObject = classType.exists(_.isModuleClass)
      val isSealedTrait = classType.exists(_.isSealed)

      val classAnnotationTrees = typeSymbol.annotations.map(_.tree)

      val primitives = Set(typeOf[Double],
                           typeOf[Float],
                           typeOf[Short],
                           typeOf[Byte],
                           typeOf[Int],
                           typeOf[Long],
                           typeOf[Char],
                           typeOf[Boolean],
                           typeOf[Unit])

      val isValueClass = genericType <:< typeOf[AnyVal] && !primitives.exists(_ =:= genericType)

      val resultType = appliedType(typeConstructor, genericType)

      val typeName = TermName(c.freshName("typeName"))
      val typeNameDef = {
        val ts = genericType.typeSymbol
        q"val $typeName = $magnoliaPkg.TypeName(${ts.owner.fullName}, ${ts.name.decodedName.toString})"
      }

      val result = if (isCaseObject) {
        val impl = q"""
          $typeNameDef
          ${c.prefix}.combine($magnoliaPkg.Magnolia.caseClass[$typeConstructor, $genericType](
            $typeName,
            true,
            false,
            new $scalaPkg.Array(0),
            $scalaPkg.Array(..$classAnnotationTrees),
            _ => ${genericType.typeSymbol.asClass.module}
          ))
        """
        Some(impl)
      } else if (isCaseClass || isValueClass) {

        val companionRef = GlobalUtil.patchedCompanionRef(c)(genericType.dealias)

        val headParamList = {
          val primaryConstructor = classType map (_.primaryConstructor)
          val optList: Option[List[c.universe.Symbol]] =
            primaryConstructor flatMap (_.asMethod.typeSignature.paramLists.headOption)
          optList.map(_.map(_.asTerm))
        }

        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
            m.asMethod
        }

        case class CaseParam(sym: MethodSymbol,
                             repeated: Boolean,
                             typeclass: Tree,
                             paramType: Type,
                             ref: TermName
                            )

        val caseParamsReversed = caseClassParameters.foldLeft[List[CaseParam]](Nil) {
          (acc, param) =>
            val paramName = param.name.decodedName.toString
            val paramTypeSubstituted = param.typeSignatureIn(genericType).resultType

            val (repeated, paramType) = paramTypeSubstituted match {
              case TypeRef(_, `repeatedParamClass`, typeArgs) =>
                true -> appliedType(scalaSeqType, typeArgs)
              case tpe =>
                false -> tpe
            }

            acc
              .find(_.paramType =:= paramType)
              .fold {
                val path = ProductType(paramName, genericType.toString)
                val frame = stack.Frame(path, resultType, assignedName)
                val derivedImplicit =
                  stack.recurse(frame, appliedType(typeConstructor, paramType)) {
                    typeclassTree(paramType, typeConstructor)
                  }

                val ref = TermName(c.freshName("paramTypeclass"))
                val assigned = q"""lazy val $ref = $derivedImplicit"""
                CaseParam(param, repeated, assigned, paramType, ref) :: acc
              } { backRef =>
                CaseParam(param, repeated, q"()", paramType, backRef.ref) :: acc
              }
        }

        val caseParams = caseParamsReversed.reverse

        val paramsVal: TermName = TermName(c.freshName("parameters"))
        val fieldValues: TermName = TermName(c.freshName("fieldValues"))

        val preAssignments = caseParams.map(_.typeclass)


        val defaults = headParamList map { plist =>
          // note: This causes the namer/typer to generate the synthetic default methods by forcing
          // the typeSignature of the "default" factory method to be visited.
          // It feels like it shouldn't be needed, but we'll get errors otherwise (as discovered after 6 hours debugging)

          val companionSym = companionRef.symbol.asModule.info
          val primaryFactoryMethod = companionSym.decl(TermName("apply")).alternatives.lastOption
          primaryFactoryMethod.foreach(_.asMethod.typeSignature)

          val indexedConstructorParams = plist.zipWithIndex
          indexedConstructorParams.map {
            case (p, idx) =>
              if (p.isParamWithDefault) {
                val method = TermName("apply$default$" + (idx + 1))
                q"$scalaPkg.Some($companionRef.$method)"
              } else q"$scalaPkg.None"
          }
        } getOrElse List(q"$scalaPkg.None")

        val annotations: List[List[Tree]] = headParamList.toList.flatten map { param =>
          param.annotations map { _.tree }
        }

        val assignments = caseParams.zip(defaults).zip(annotations).zipWithIndex.map {
          case (((CaseParam(param, repeated, typeclass, paramType, ref), defaultVal), annList), idx) =>
            q"""$paramsVal($idx) = $magnoliaPkg.Magnolia.param[$typeConstructor, $genericType,
                $paramType](
            ${param.name.decodedName.toString},
            $repeated,
            $ref,
            $defaultVal,
            _.${param.name},
            $scalaPkg.Array(..$annList)
          )"""
        }

        Some(q"""{
            ..$preAssignments
            val $paramsVal: $scalaPkg.Array[$magnoliaPkg.Param[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.length})
            ..$assignments

            $typeNameDef
            
            ${c.prefix}.combine($magnoliaPkg.Magnolia.caseClass[$typeConstructor, $genericType](
              $typeName,
              false,
              $isValueClass,
              $paramsVal,
              $scalaPkg.Array(..$classAnnotationTrees),
              ($fieldValues: $scalaPkg.Seq[Any]) => {
                if ($fieldValues.lengthCompare($paramsVal.length) != 0) {
                  val msg = "`" + $typeName.full + "` has " + $paramsVal.length + " fields, not " + $fieldValues.size
                  throw new java.lang.IllegalArgumentException(msg)
                }
                new $genericType(..${caseParams.zipWithIndex.map {
          case (typeclass, idx) =>
            val arg = q"$fieldValues($idx).asInstanceOf[${typeclass.paramType}]"
            if (typeclass.repeated) q"$arg: _*" else arg
        }})}))
          }""")
      } else if (isSealedTrait) {
        val genericSubtypes = classType.get.knownDirectSubclasses.to[List]
        val subtypes = genericSubtypes.map { sub =>
          val subType = sub.asType.toType // FIXME: Broken for path dependent types
          val typeParams = sub.asType.typeParams
          val typeArgs = thisType(sub).baseType(genericType.typeSymbol).typeArgs
          val mapping = (typeArgs.map(_.typeSymbol), genericType.typeArgs).zipped.toMap
          val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
          val applied = appliedType(subType.typeConstructor, newTypeArgs)
          existentialAbstraction(typeParams, applied)
        }

        if (subtypes.isEmpty) {
          c.info(c.enclosingPosition,
                 s"magnolia: could not find any direct subtypes of $typeSymbol",
                 force = true)

          c.abort(c.enclosingPosition, "")
        }

        val subtypesVal: TermName = TermName(c.freshName("subtypes"))

        val typeclasses = for (subType <- subtypes) yield {
          val path = CoproductType(genericType.toString)
          val frame = stack.Frame(path, resultType, assignedName)
          subType -> stack.recurse(frame, appliedType(typeConstructor, subType)) {
            typeclassTree(subType, typeConstructor)
          }
        }

        val assignments = typeclasses.zipWithIndex.map {
          case ((typ, typeclass), idx) =>
            q"""$subtypesVal($idx) = $magnoliaPkg.Magnolia.subtype[$typeConstructor, $genericType, $typ](
            $magnoliaPkg.TypeName(${typ.typeSymbol.owner.fullName}, ${typ.typeSymbol.name.decodedName.toString}),
            $typeclass,
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }

        Some(q"""{
            val $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.size})
            
            ..$assignments

            $typeNameDef
            
            ${c.prefix}.dispatch(new $magnoliaPkg.SealedTrait(
              $typeName,
              $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]],
              $scalaPkg.Array(..$classAnnotationTrees)
            )): $resultType
          }""")
      } else None

      for (term <- result) yield q"""{
        lazy val $assignedName: $resultType = $term
        $assignedName
      }"""
    }

    val genericType: Type = weakTypeOf[T]
    val searchType = appliedType(typeConstructor, genericType)
    val directlyReentrant = stack.top.exists(_.searchType =:= searchType)
    if (directlyReentrant) throw DirectlyReentrantException()

    val result = stack
      .find(searchType)
      .map { enclosingRef =>
        q"$magnoliaPkg.Deferred[$searchType](${enclosingRef.toString})"
      }
      .orElse {
        directInferImplicit(genericType, typeConstructor)
      }

    for (tree <- result) if (debug.isDefined && genericType.toString.contains(debug.get)) {
      c.echo(c.enclosingPosition, s"Magnolia macro expansion for $genericType")
      c.echo(NoPosition, s"... = ${showCode(tree)}\n\n")
    }

    val dereferencedResult =
      if (stack.nonEmpty) result
      else for (tree <- result) yield c.untypecheck(removeDeferred.transform(tree))

    dereferencedResult.getOrElse {
      c.abort(c.enclosingPosition,
              s"magnolia: could not infer $prefixName.Typeclass for type $genericType")
    }
  }

  /** constructs a new [[Subtype]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def subtype[Tc[_], T, S <: T](name: TypeName,
                                tc: => Tc[S],
                                isType: T => Boolean,
                                asType: T => S): Subtype[Tc, T] =
    new Subtype[Tc, T] with PartialFunction[T, S] {
      type SType = S
      def typeName: TypeName = name
      def typeclass: Tc[SType] = tc
      def cast: PartialFunction[T, SType] = this
      def isDefinedAt(t: T) = isType(t)
      def apply(t: T): SType = asType(t)
      override def toString: String = s"Subtype(${typeName.full})"
    }

  /** constructs a new [[Param]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def param[Tc[_], T, P](name: String,
                         isRepeated: Boolean,
                         typeclassParam: => Tc[P],
                         defaultVal: => Option[P],
                         deref: T => P,
                         annotationsArrayParam: Array[Any]
                        ): Param[Tc, T] = new Param[Tc, T] {
    type PType = P
    def label: String = name
    def repeated: Boolean = isRepeated
    def default: Option[PType] = defaultVal
    def typeclass: Tc[PType] = typeclassParam
    def dereference(t: T): PType = deref(t)
    def annotationsArray: Array[Any] = annotationsArrayParam
  }

  /** constructs a new [[CaseClass]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def caseClass[Tc[_], T](name: TypeName,
                          obj: Boolean,
                          valClass: Boolean,
                          params: Array[Param[Tc, T]],
                          annotations: Array[Any],
                          constructor: Seq[Any] => T): CaseClass[Tc, T] =
    new CaseClass[Tc, T](name, obj, valClass, params, annotations) {
      def rawConstruct(fieldValues: Seq[Any]): T = constructor(fieldValues)
    }
}

private[magnolia] final case class DirectlyReentrantException()
    extends Exception("attempt to recurse directly")

private[magnolia] object Deferred { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed abstract class TypePath(path: String) { override def toString = path }
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String)
      extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
      extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack[C <: whitebox.Context] {
    private var frames = List.empty[Frame]
    private val cache = mutable.Map.empty[C#Type, C#Tree]

    def isEmpty: Boolean = frames.isEmpty
    def nonEmpty: Boolean = frames.nonEmpty
    def top: Option[Frame] = frames.headOption
    def pop(): Unit = frames = frames drop 1
    def push(frame: Frame): Unit = frames ::= frame

    def clear(): Unit = {
      frames = Nil
      cache.clear()
    }

    def find(searchType: C#Type): Option[C#TermName] = frames.collectFirst {
      case Frame(_, tpe, term) if tpe =:= searchType => term
    }

    def recurse[T <: C#Tree](frame: Frame, searchType: C#Type)(fn: => T): T = {
      push(frame)
      val result = cache.getOrElseUpdate(searchType, fn)
      pop()
      result.asInstanceOf[T]
    }

    def trace: List[TypePath] =
      frames
        .drop(1)
        .foldLeft[(C#Type, List[TypePath])]((null, Nil)) {
          case ((_, Nil), frame) =>
            (frame.searchType, frame.path :: Nil)
          case (continue @ (tpe, acc), frame) =>
            if (tpe =:= frame.searchType) continue
            else (frame.searchType, frame.path :: acc)
        }
        ._2
        .reverse

    override def toString: String =
      frames.mkString("magnolia stack:\n", "\n", "\n")

    final case class Frame(path: TypePath, searchType: C#Type, term: C#TermName)
  }

  object Stack {
    private val global = new Stack[whitebox.Context]
    private val workSet = mutable.Set.empty[whitebox.Context#Symbol]

    def withContext(c: whitebox.Context)(fn: Stack[c.type] => c.Tree): c.Tree = {
      workSet += c.macroApplication.symbol
      val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
      try fn(global.asInstanceOf[Stack[c.type]])
      finally if (depth <= 1) {
        global.clear()
        workSet.clear()
      }
    }
  }
}
