package magnolia

import scala.reflect._, macros._
import scala.collection.immutable.ListMap
import language.existentials
import language.higherKinds

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
  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    import internal._

    val magnoliaPkg = q"_root_.magnolia"
    val scalaPkg = q"_root_.scala"

    val repeatedParamClass = definitions.RepeatedParamClass
    val scalaSeqType = typeOf[Seq[_]].typeConstructor

    val prefixType = c.prefix.tree.tpe

    def companionRef(tpe: Type): Tree = {
      val global = c.universe match { case global: scala.tools.nsc.Global => global }
      val globalTpe = tpe.asInstanceOf[global.Type]
      val companion = globalTpe.typeSymbol.companionSymbol
      if (companion != NoSymbol)
        global.gen.mkAttributedRef(globalTpe.prefix, companion).asInstanceOf[Tree]
      else q"${tpe.typeSymbol.name.toTermName}"
    }

    val typeDefs = prefixType.baseClasses.flatMap { cls =>
      cls.asType.toType.decls.filter(_.isType).find(_.name.toString == "Typeclass").map { tpe =>
        tpe.asType.toType.asSeenFrom(prefixType, cls)
      }
    }

    val typeConstructorOpt =
      typeDefs.headOption.map(_.typeConstructor)

    val typeConstructor = typeConstructorOpt.getOrElse {
      c.abort(c.enclosingPosition,
              "magnolia: the derivation object does not define the Typeclass type constructor")
    }

    def checkMethod(termName: String, category: String, expected: String): Unit = {
      val term = TermName(termName)
      val combineClass = c.prefix.tree.tpe.baseClasses
        .find { cls =>
          cls.asType.toType.decl(term) != NoSymbol
        }
        .getOrElse {
          c.abort(
            c.enclosingPosition,
            s"magnolia: the method `$termName` must be defined on the derivation object to derive typeclasses for $category"
          )
        }
      val firstParamBlock = combineClass.asType.toType.decl(term).asTerm.asMethod.paramLists.head
      if (firstParamBlock.length != 1)
        c.abort(c.enclosingPosition,
                s"magnolia: the method `combine` should take a single parameter of type $expected")
    }

    // FIXME: Only run these methods if they're used, particularly `dispatch`
    checkMethod("combine", "case classes", "CaseClass[Typeclass, _]")
    checkMethod("dispatch", "sealed traits", "SealedTrait[Typeclass, _]")

    def findType(key: Type): Option[TermName] =
      recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

    case class Typeclass(typ: c.Type, tree: c.Tree)

    def recurse[T](path: TypePath, key: Type, value: TermName)(fn: => T): Option[T] = {
      val oldRecursionStack = recursionStack.get(c.enclosingPosition)
      recursionStack = recursionStack.updated(
        c.enclosingPosition,
        oldRecursionStack.map(_.push(path, key, value)).getOrElse {
          Stack(Map(), List(Frame(path, key, value)), Nil)
        }
      )

      try Some(fn)
      catch { case e: Exception => None } finally {
        val currentStack = recursionStack(c.enclosingPosition)
        recursionStack = recursionStack.updated(c.enclosingPosition, currentStack.pop())
      }
    }

    val removeDeferred: Transformer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"$magnoliaPkg.Deferred.apply[$returnType](${Literal(Constant(method: String))})" =>
          q"${TermName(method)}"
        case _ =>
          super.transform(tree)
      }
    }

    def typeclassTree(paramName: Option[String],
                      genericType: Type,
                      typeConstructor: Type,
                      assignedName: TermName): Tree = {

      val searchType = appliedType(typeConstructor, genericType)

      val deferredRef = findType(genericType).map { methodName =>
        val methodAsString = methodName.decodedName.toString
        q"$magnoliaPkg.Deferred.apply[$searchType]($methodAsString)"
      }

      val foundImplicit = deferredRef.orElse {
        val (inferredImplicit, newStack) =
          recursionStack(c.enclosingPosition).lookup(c)(searchType) {
            val implicitSearchTry = scala.util.Try {
              val genericTypeName: String =
                genericType.typeSymbol.name.decodedName.toString.toLowerCase

              val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))

              recurse(ChainedImplicit(genericType.toString), genericType, assignedName) {
                c.inferImplicitValue(searchType, false, false)
              }.get
            }

            implicitSearchTry.toOption.orElse(
              directInferImplicit(genericType, typeConstructor).map(_.tree)
            )
          }
        recursionStack = recursionStack.updated(c.enclosingPosition, newStack)
        inferredImplicit
      }

      foundImplicit.getOrElse {
        val currentStack: Stack = recursionStack(c.enclosingPosition)

        val error = ImplicitNotFound(genericType.toString,
                                     recursionStack(c.enclosingPosition).frames.map(_.path))

        val updatedStack = currentStack.copy(errors = error :: currentStack.errors)
        recursionStack = recursionStack.updated(c.enclosingPosition, updatedStack)

        val stackPaths = recursionStack(c.enclosingPosition).frames.map(_.path)
        val stack = stackPaths.mkString("    in ", "\n    in ", "\n")

        c.abort(c.enclosingPosition,
                s"magnolia: could not find typeclass for type $genericType\n$stack")
      }
    }

    def directInferImplicit(genericType: c.Type, typeConstructor: Type): Option[Typeclass] = {

      val genericTypeName: String = genericType.typeSymbol.name.decodedName.toString.toLowerCase
      val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
      val typeSymbol = genericType.typeSymbol
      val classType = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isCaseClass = classType.exists(_.isCaseClass)
      val isCaseObject = classType.exists(_.isModuleClass)
      val isSealedTrait = classType.exists(_.isSealed)

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
        
      val className = s"${genericType.typeSymbol.owner.fullName}.${genericType.typeSymbol.name.decodedName}"

      val result = if (isCaseObject) {
        val obj = companionRef(genericType)

        val impl = q"""
          ${c.prefix}.combine($magnoliaPkg.Magnolia.caseClass[$typeConstructor, $genericType](
            $className, true, false, new $scalaPkg.Array(0), _ => $obj)
          )
        """
        Some(Typeclass(genericType, impl))
      } else if (isCaseClass || isValueClass) {
        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
            m.asMethod
        }

        case class CaseParam(sym: c.universe.MethodSymbol,
                             repeated: Boolean,
                             typeclass: c.Tree,
                             paramType: c.Type,
                             ref: c.TermName)

        val caseParamsReversed = caseClassParameters.foldLeft[List[CaseParam]](Nil) {
          (acc, param) =>
            val paramName = param.name.decodedName.toString
            val paramTypeSubstituted = param.returnType.typeSignatureIn(genericType).resultType

            val (repeated, paramType) = paramTypeSubstituted match {
              case TypeRef(_, `repeatedParamClass`, typeArgs) =>
                true -> appliedType(scalaSeqType, typeArgs)
              case tpe =>
                false -> tpe
            }

            val predefinedRef = acc.find(_.paramType == paramType)

            val caseParamOpt = predefinedRef.map { backRef =>
              CaseParam(param, repeated, q"()", paramType, backRef.ref) :: acc
            }

            caseParamOpt.getOrElse {
              val derivedImplicit =
                recurse(ProductType(paramName, genericType.toString), genericType, assignedName) {
                  typeclassTree(Some(paramName), paramType, typeConstructor, assignedName)
                }.getOrElse(
                  c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
                )

              val ref = TermName(c.freshName("paramTypeclass"))
              val assigned = q"""val $ref = $derivedImplicit"""
              CaseParam(param, repeated, assigned, paramType, ref) :: acc
            }
        }

        val caseParams = caseParamsReversed.reverse

        val paramsVal: TermName = TermName(c.freshName("parameters"))
        val fnVal: TermName = TermName(c.freshName("fn"))

        val preAssignments = caseParams.map(_.typeclass)

        val defaults = if (!isValueClass) {
          val caseClassCompanion = genericType.companion

          // If a companion object is defined with alternative apply methods
          // it is needed get all the alternatives
          val constructorMethods =
            caseClassCompanion.decl(TermName("apply")).alternatives.map(_.asMethod)

          // The last apply method in the alternatives is the one that belongs
          // to the case class, not the user defined companion object
          val indexedConstructorParams =
            constructorMethods.last.paramLists.head.map(_.asTerm).zipWithIndex

          indexedConstructorParams.map {
            case (p, idx) =>
              if (p.isParamWithDefault) {
                val method = TermName("apply$default$" + (idx + 1))
                q"$scalaPkg.Some(${genericType.typeSymbol.companion.asTerm}.$method)"
              } else q"$scalaPkg.None"
          }
        } else List(q"$scalaPkg.None")

        val assignments = caseParams.zip(defaults).zipWithIndex.map {
          case ((CaseParam(param, repeated, typeclass, paramType, ref), defaultVal), idx) =>
            q"""$paramsVal($idx) = $magnoliaPkg.Magnolia.param[$typeConstructor, $genericType,
                $paramType](
            ${param.name.decodedName.toString}, $repeated, $ref, $defaultVal, _.${param.name}
          )"""
        }

        Some(
          Typeclass(
            genericType,
            q"""{
            ..$preAssignments
            val $paramsVal: $scalaPkg.Array[$magnoliaPkg.Param[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.length})
            ..$assignments
            
            ${c.prefix}.combine($magnoliaPkg.Magnolia.caseClass[$typeConstructor, $genericType](
              $className,
              false,
              $isValueClass,
              $paramsVal,
              ($fnVal: $magnoliaPkg.Param[$typeConstructor, $genericType] => Any) =>
                new $genericType(..${caseParams.zipWithIndex.map {
              case (typeclass, idx) =>
                val arg = q"$fnVal($paramsVal($idx)).asInstanceOf[${typeclass.paramType}]"
                if (typeclass.repeated) q"$arg: _*" else arg
            }})
            ))
          }"""
          )
        )
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
                 true)

          c.abort(c.enclosingPosition, "")
        }

        val subtypesVal: TermName = TermName(c.freshName("subtypes"))

        val typeclasses = subtypes.map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            (searchType, typeclassTree(None, searchType, typeConstructor, assignedName))
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }

        val assignments = typeclasses.zipWithIndex.map {
          case ((typ, typeclass), idx) =>
            q"""$subtypesVal($idx) = $magnoliaPkg.Magnolia.subtype[$typeConstructor, $genericType, $typ](
            ${s"${typ.typeSymbol.owner.fullName}.${typ.typeSymbol.name.decodedName}"},
            $typeclass,
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }
            
        Some {
          Typeclass(
            genericType,
            q"""{
            val $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.size})
            
            ..$assignments
            
            ${c.prefix}.dispatch(new $magnoliaPkg.SealedTrait(
              $className,
              $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]])
            ): $resultType
          }"""
          )
        }
      } else None

      result.map {
        case Typeclass(t, r) =>
          Typeclass(t, q"""{
          def $assignedName: $resultType = $r
          $assignedName
        }""")
      }
    }

    val genericType: Type = weakTypeOf[T]

    val currentStack: Stack =
      recursionStack.getOrElse(c.enclosingPosition, Stack(Map(), List(), List()))

    val directlyReentrant = currentStack.frames.headOption.exists(_.genericType == genericType)

    if (directlyReentrant) throw DirectlyReentrantException()

    currentStack.errors.foreach { error =>
      if (!emittedErrors.contains(error)) {
        emittedErrors += error
        val trace = error.path.mkString("\n    in ", "\n    in ", "\n \n")

        val msg = s"magnolia: could not derive $typeConstructor instance for type " +
          s"${error.genericType}"

        c.info(c.enclosingPosition, msg + trace, true)
      }
    }

    val result: Option[Tree] = if (currentStack.frames.nonEmpty) {
      findType(genericType) match {
        case None =>
          directInferImplicit(genericType, typeConstructor).map(_.tree)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"$magnoliaPkg.Deferred[$searchType]($methodAsString)")
      }
    } else directInferImplicit(genericType, typeConstructor).map(_.tree)

    if (currentStack.frames.isEmpty) recursionStack = ListMap()

    val dereferencedResult = result.map { tree =>
      if (currentStack.frames.isEmpty) c.untypecheck(removeDeferred.transform(tree)) else tree
    }

    dereferencedResult.getOrElse {
      c.abort(c.enclosingPosition, s"magnolia: could not infer typeclass for type $genericType")
    }
  }

  /** constructs a new [[Subtype]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def subtype[Tc[_], T, S <: T](name: String, tc: => Tc[S], isType: T => Boolean, asType: T => S) =
    new Subtype[Tc, T] {
      type SType = S
      def label: String = name
      def typeclass: Tc[SType] = tc
      def cast: PartialFunction[T, SType] = new PartialFunction[T, S] {
        def isDefinedAt(t: T) = isType(t)
        def apply(t: T): SType = asType(t)
      }
    }

  /** constructs a new [[Param]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def param[Tc[_], T, P](name: String,
                         isRepeated: Boolean,
                         typeclassParam: Tc[P],
                         defaultVal: => Option[P],
                         deref: T => P) = new Param[Tc, T] {
    type PType = P
    def label: String = name
    def repeated: Boolean = isRepeated
    def default: Option[PType] = defaultVal
    def typeclass: Tc[PType] = typeclassParam
    def dereference(t: T): PType = deref(t)
  }

  /** constructs a new [[CaseClass]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def caseClass[Tc[_], T](name: String,
                          obj: Boolean,
                          valClass: Boolean,
                          params: Array[Param[Tc, T]],
                          constructor: (Param[Tc, T] => Any) => T) =
    new CaseClass[Tc, T](name, obj, valClass, params) {
      def construct[R](param: Param[Tc, T] => R): T = constructor(param)
    }
}

private[magnolia] case class DirectlyReentrantException()
    extends Exception("attempt to recurse directly")

private[magnolia] object Deferred { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed class TypePath(path: String) { override def toString = path }
  case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  case class ProductType(paramName: String, typeName: String)
      extends TypePath(s"parameter '$paramName' of product type $typeName")

  case class ChainedImplicit(typeName: String)
      extends TypePath(s"chained implicit of type $typeName")

  case class ImplicitNotFound(genericType: String, path: List[TypePath])

  case class Stack(cache: Map[whitebox.Context#Type, Option[whitebox.Context#Tree]],
                   frames: List[Frame],
                   errors: List[ImplicitNotFound]) {

    def lookup(c: whitebox.Context)(t: c.Type)(orElse: => Option[c.Tree]): (Option[c.Tree], Stack) =
      if (cache.contains(t)) {
        (cache(t).asInstanceOf[Option[c.Tree]], this)
      } else {
        val value = orElse
        (value, copy(cache.updated(t, value)))
      }

    def push(path: TypePath, key: whitebox.Context#Type, value: whitebox.Context#TermName): Stack =
      Stack(cache, Frame(path, key, value) :: frames, errors)

    def pop(): Stack = Stack(cache, frames.tail, errors)
  }

  case class Frame(path: TypePath,
                   genericType: whitebox.Context#Type,
                   term: whitebox.Context#TermName) {
    def termName(c: whitebox.Context): c.TermName = term.asInstanceOf[c.TermName]
  }

  var recursionStack: ListMap[api.Position, Stack] = ListMap()
  var emittedErrors: Set[ImplicitNotFound] = Set()
}
