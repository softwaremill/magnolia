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
   *  which would support automatic derivation of typeclass instances by calling `Derivation.gen[T]`
   *  or with `implicitly[Typeclass[T]]`, if the implicit method is imported into the current scope.
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
   *  object, which may be called with the code above, and for it to return a type which conforms to
   *  the type `Typeclass[T]`. The implementation of `combine` will therefore typically look like
   *  this,
   *  <pre>
   *    def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = ...
   *  </pre>
   *  however, there is the flexibility to provide additional type parameters or additional implicit
   *  parameters to the definition, provided these do not affect its ability to be invoked as
   *  described above.
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
    import scala.util.{Try, Success, Failure}

    val typeConstructor: c.Type =
      c.prefix.tree.tpe.member(TypeName("Typeclass")).asType.toType.typeConstructor

    def findType(key: Type): Option[TermName] =
      recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

    case class Typeclass(typ: c.Type, tree: c.Tree)

    def recurse[T](path: TypePath, key: Type, value: TermName)(fn: => T):
        Option[T] = {
      recursionStack = recursionStack.updated(
        c.enclosingPosition,
        recursionStack.get(c.enclosingPosition).map(_.push(path, key, value)).getOrElse(
            Stack(Map(), List(Frame(path, key, value)), Nil))
      )

      try Some(fn) catch { case e: Exception => None } finally {
        val currentStack = recursionStack(c.enclosingPosition)
        recursionStack = recursionStack.updated(c.enclosingPosition,
            currentStack.pop())
      }
    }

    val removeDeferred: Transformer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"_root_.magnolia.Deferred.apply[$returnType](${Literal(Constant(method: String))})" =>
          q"${TermName(method)}"
        case _ =>
          super.transform(tree)
      }
    }

    def typeclassTree(paramName: Option[String], genericType: Type, typeConstructor: Type,
                      assignedName: TermName): Tree = {
      
      val searchType = appliedType(typeConstructor, genericType)
      
      findType(genericType).map { methodName =>
        val methodAsString = methodName.encodedName.toString
        q"_root_.magnolia.Deferred.apply[$searchType]($methodAsString)"
      }.orElse {
        val (inferredImplicit, newStack) = recursionStack(c.enclosingPosition).lookup(c)(searchType) {
          scala.util.Try {
            val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
            val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
            recurse(ChainedImplicit(genericType.toString), genericType, assignedName) {
              c.inferImplicitValue(searchType, false, false)
            }.get
          }.toOption.orElse(directInferImplicit(genericType, typeConstructor).map(_.tree))
        }
        recursionStack = recursionStack.updated(c.enclosingPosition, newStack)
        inferredImplicit
      }.getOrElse {
        val currentStack: Stack = recursionStack(c.enclosingPosition)

        val error = ImplicitNotFound(genericType.toString,
            recursionStack(c.enclosingPosition).frames.map(_.path))

        val updatedStack = currentStack.copy(errors = error :: currentStack.errors)
        recursionStack = recursionStack.updated(c.enclosingPosition, updatedStack)
        val stack = recursionStack(c.enclosingPosition).frames.map(_.path).mkString("    in ", "\n    in ", "\n")
        c.abort(c.enclosingPosition, s"magnolia: could not find typeclass for type $genericType\n$stack")
      }
    }

    def directInferImplicit(genericType: c.Type,
           typeConstructor: Type): Option[Typeclass] = {

      val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
      val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
      val typeSymbol = genericType.typeSymbol
      val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
      val isCaseObject = classType.map(_.isModuleClass).getOrElse(false)
      val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
      val isValueClass = genericType <:< typeOf[AnyVal]

      val resultType = appliedType(typeConstructor, genericType)

      // FIXME: Handle AnyVals
      val result = if(isCaseObject) {
        // FIXME: look for an alternative which isn't deprecated on Scala 2.12+
        val obj = genericType.typeSymbol.companionSymbol.asTerm
        val className = obj.fullName
        val impl = q"""
          ${c.prefix}.combine(_root_.magnolia.Magnolia.caseClass[$typeConstructor, $genericType](
            $className, true, new _root_.scala.Array(0), _ => $obj)
          )
        """
        Some(Typeclass(genericType, impl))
      } else if(isCaseClass) {
        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }
        val className = genericType.typeSymbol.fullName

        case class CaseParam(sym: c.universe.MethodSymbol, typeclass: c.Tree, paramType: c.Type, ref: c.TermName)

        val caseParams: List[CaseParam] = caseClassParameters.foldLeft(List[CaseParam]()) { case (acc, param) =>
          val paramName = param.name.encodedName.toString
          val paramType = param.returnType.substituteTypes(genericType.etaExpand.typeParams, genericType.typeArgs)

          acc.find(_.paramType == paramType).map { backRef =>
            CaseParam(param, q"()", paramType, backRef.ref) :: acc
          }.getOrElse {
            val derivedImplicit = recurse(ProductType(paramName, genericType.toString), genericType,
                assignedName) {
              typeclassTree(Some(paramName), paramType, typeConstructor, assignedName)
            }.getOrElse(c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType"))
            
            val ref = TermName(c.freshName("paramTypeclass"))
            val assigned = q"""val $ref = $derivedImplicit"""
            CaseParam(param, assigned, paramType, ref) :: acc
          }
        }.to[List].reverse

        val paramsVal: TermName = TermName(c.freshName("parameters"))
        val fnVal: TermName = TermName(c.freshName("fn"))
        
        val preAssignments = caseParams.map(_.typeclass)
        
        val caseClassCompanion = genericType.companion
        val defaults = caseClassCompanion.decl(TermName("apply")).asMethod.paramLists.head.map(_.asTerm).zipWithIndex.map { case (p, idx) =>
          if(p.isParamWithDefault) q"_root_.scala.Some(${genericType.typeSymbol.companionSymbol.asTerm}.${TermName("apply$default$"+(idx + 1))})"
          else q"_root_.scala.None"
        }

        val assignments = caseParams.zip(defaults).zipWithIndex.map { case ((CaseParam(param, typeclass, paramType, ref), defaultVal), idx) =>
          q"""$paramsVal($idx) = _root_.magnolia.Magnolia.param[$typeConstructor, $genericType, $paramType](
            ${param.name.toString}, $ref, $defaultVal, _.${TermName(param.name.toString)}
          )"""
        }

        Some(Typeclass(genericType,
          q"""{
            ..$preAssignments
            val $paramsVal: _root_.scala.Array[Param[$typeConstructor, $genericType]] =
              new _root_.scala.Array(${assignments.length})
            ..$assignments
            
            ${c.prefix}.combine(_root_.magnolia.Magnolia.caseClass[$typeConstructor, $genericType](
              $className,
              false,
              $paramsVal,
              ($fnVal: Param[$typeConstructor, $genericType] => Any) =>
                new $genericType(..${caseParams.zipWithIndex.map { case (typeclass, idx) =>
                  q"$fnVal($paramsVal($idx)).asInstanceOf[${typeclass.paramType}]"
                } })
            ))
          }"""
        ))
      } else if(isSealedTrait) {
        val genericSubtypes = classType.get.knownDirectSubclasses.to[List]
        val subtypes = genericSubtypes.map { sub =>
          val typeArgs = sub.asType.typeSignature.baseType(genericType.typeSymbol).typeArgs
          val mapping = typeArgs.zip(genericType.typeArgs).toMap
          val newTypeParams = sub.asType.toType.typeArgs.map(mapping(_))
          appliedType(sub.asType.toType.typeConstructor, newTypeParams)
        }

        if(subtypes.isEmpty) {
          c.info(c.enclosingPosition,
              s"magnolia: could not find any direct subtypes of $typeSymbol", true)
          
          c.abort(c.enclosingPosition, "")
        }
        
        val subtypesVal: TermName = TermName(c.freshName("subtypes"))
      
        val assignments = subtypes.map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            (searchType, typeclassTree(None, searchType, typeConstructor, assignedName))
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }.zipWithIndex.map { case ((typ, typeclass), idx) =>
          q"""$subtypesVal($idx) = _root_.magnolia.Magnolia.subtype[$typeConstructor, $genericType, $typ](
            ${typ.typeSymbol.fullName},
            $typeclass,
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }
          
        Some {
          Typeclass(genericType, q"""{
            val $subtypesVal: _root_.scala.Array[_root_.magnolia.Subtype[$typeConstructor, $genericType]] =
              new _root_.scala.Array(${assignments.size})
            
            ..$assignments
            
            ${c.prefix}.dispatch(new _root_.magnolia.SealedTrait(
              $genericTypeName,
              $subtypesVal: _root_.scala.Array[_root_.magnolia.Subtype[$typeConstructor, $genericType]])
            ): $resultType
          }""")
        }
      } else None

      result.map { case Typeclass(t, r) =>
        Typeclass(t, q"""{
          def $assignedName: $resultType = $r
          $assignedName
        }""")
      }
    }

    val genericType: Type = weakTypeOf[T]
    
    val currentStack: Stack =
      recursionStack.get(c.enclosingPosition).getOrElse(Stack(Map(), List(), List()))
    
    val directlyReentrant = Some(genericType) == currentStack.frames.headOption.map(_.genericType)
    
    if(directlyReentrant) throw DirectlyReentrantException()
   
    currentStack.errors.foreach { error =>
      if(!emittedErrors.contains(error)) {
        emittedErrors += error
        val trace = error.path.mkString("\n    in ", "\n    in ", "\n \n")
        
        val msg = s"magnolia: could not derive ${typeConstructor} instance for type "+
            s"${error.genericType}"
        
        c.info(c.enclosingPosition, msg+trace, true)
      }
    }

    val result: Option[Tree] = if(!currentStack.frames.isEmpty) {
      findType(genericType) match {
        case None =>
          directInferImplicit(genericType, typeConstructor).map(_.tree)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"_root_.magnolia.Deferred[$searchType]($methodAsString)")
      }
    } else directInferImplicit(genericType, typeConstructor).map(_.tree)
   
    if(currentStack.frames.isEmpty) recursionStack = ListMap()

    result.map { tree =>
      if(currentStack.frames.isEmpty) c.untypecheck(removeDeferred.transform(tree)) else tree
    }.getOrElse {
      c.abort(c.enclosingPosition, s"magnolia: could not infer typeclass for type $genericType")
    }
  }

  /** constructs a new [[Subtype]] instance
   *
   *  This method is intended to be called only from code generated by the Magnolia macro, and
   *  should not be called directly from users' code. */
  def subtype[Tc[_], T, S <: T](name: String, tc: => Tc[S], isType: T => Boolean, asType: T => S) = new Subtype[Tc, T] {
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
  def param[Tc[_], T, P](name: String, typeclassParam: Tc[P], defaultVal: => Option[P], deref: T => P) = new Param[Tc, T] {
    type PType = P
    def label: String = name
    def default: Option[PType] = defaultVal
    def typeclass: Tc[PType] = typeclassParam
    def dereference(t: T): PType = deref(t)
  }
  
  /** constructs a new [[CaseClass]] instance
   *
   *  This method is intended to be called only from code generated by the Magnolia macro, and
   *  should not be called directly from users' code. */
  def caseClass[Tc[_], T](name: String, obj: Boolean, params: Array[Param[Tc, T]], constructor: (Param[Tc, T] => Any) => T) =
    new CaseClass[Tc, T](name, obj, params) {
      def construct[R](param: Param[Tc, T] => R): T = constructor(param)
    }
}

private[magnolia] case class DirectlyReentrantException() extends
    Exception("attempt to recurse directly")

private[magnolia] object Deferred { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed class TypePath(path: String) { override def toString = path }
  case class CoproductType(typeName: String) extends
      TypePath(s"coproduct type $typeName")
  
  case class ProductType(paramName: String, typeName: String) extends
      TypePath(s"parameter '$paramName' of product type $typeName")
  
  case class ChainedImplicit(typeName: String) extends
      TypePath(s"chained implicit of type $typeName")

  case class ImplicitNotFound(genericType: String, path: List[TypePath])

  case class Stack(cache: Map[whitebox.Context#Type, Option[whitebox.Context#Tree]], frames: List[Frame], errors: List[ImplicitNotFound]) {
    
    def lookup(c: whitebox.Context)(t: c.Type)(orElse: => Option[c.Tree]): (Option[c.Tree], Stack) =
      if(cache.contains(t)) {
        (cache(t).asInstanceOf[Option[c.Tree]], this)
      } else {
        val value = orElse
        (value, copy(cache.updated(t, value)))
      }

    def push(path: TypePath, key: whitebox.Context#Type,
        value: whitebox.Context#TermName): Stack =
      Stack(cache, Frame(path, key, value) :: frames, errors)
    
    def pop(): Stack = Stack(cache, frames.tail, errors)
  }

  case class Frame(path: TypePath, genericType: whitebox.Context#Type,
      term: whitebox.Context#TermName) {
    def termName(c: whitebox.Context): c.TermName = term.asInstanceOf[c.TermName]
  }

  var recursionStack: ListMap[api.Position, Stack] = ListMap()
  var emittedErrors: Set[ImplicitNotFound] = Set()
}
