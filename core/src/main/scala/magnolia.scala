package magnolia

import scala.reflect._, macros._
import scala.collection.immutable.ListMap
import language.existentials
import language.higherKinds
import language.experimental.macros

trait Subclass[Tc[_], T] {
  type S <: T
  def label: String
  def typeclass: Tc[S]
  def cast: PartialFunction[T, S]
}

object Subclass {
  def apply[Tc[_], T, S1 <: T](name: String, tc: => Tc[S1], pf: => PartialFunction[T, S1]) = new Subclass[Tc, T] {
    type S = S1
    def label: String = name
    def typeclass: Tc[S] = tc
    def cast: PartialFunction[T, S] = pf
  }
}

object Param {
  def apply[Tc[_], T, S1](name: String, tc: Tc[S1], deref: T => S1) = new Param[Tc, T] {
    type S = S1
    def label = name
    def typeclass: Tc[S] = tc
    def dereference(t: T): S = deref(t)
  }
}

trait Param[Tc[_], T] {
  type S
  def label: String
  def typeclass: Tc[S]
  def dereference(param: T): S
}

object JoinContext {
  def apply[Tc[_], T](name: String, obj: Boolean, params: Array[Param[Tc, T]], constructor: (Param[Tc, T] => Any) => T) =
    new JoinContext[Tc, T](name, obj, params) {
      def construct(param: Param[Tc, T] => Any): T = constructor(param)
    }
}

abstract class JoinContext[Tc[_], T](val typeName: String, val isObject: Boolean, val parameters: Array[Param[Tc, T]]) {
  def construct(param: ((Param[Tc, T]) => Any)): T
}

object Magnolia {
  import CompileTimeState._

  def generic[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    import scala.util.{Try, Success, Failure}

    val typeConstructor: c.Type =
      c.prefix.tree.tpe.member(TypeName("Typeclass")).asType.toType.typeConstructor

    def findType(key: Type): Option[TermName] =
      recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

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

    def typeclassTree(paramName: Option[String],
                     genericType: Type,
                     typeConstructor: Type,
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
          }.toOption.orElse(directInferImplicit(genericType, typeConstructor))
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
           typeConstructor: Type): Option[c.Tree] = {

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
        val termSym = genericType.typeSymbol.companionSymbol
        val obj = termSym.asTerm
        val className = obj.name.toString
        val impl = q"""
          ${c.prefix}.join(_root_.magnolia.JoinContext[$typeConstructor, $genericType]($className, true, _root_.scala.Array(), $obj))
        """
        Some(impl)
      } else if(isCaseClass) {
        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }
        val className = genericType.toString

        val typeclasses: List[(c.universe.MethodSymbol, c.Tree, c.Type)] = caseClassParameters.map { param =>
          val paramName = param.name.encodedName.toString
          val paramType = param.returnType.substituteTypes(genericType.etaExpand.typeParams, genericType.typeArgs)

          val derivedImplicit = recurse(ProductType(paramName, genericType.toString), genericType,
              assignedName) {

            typeclassTree(Some(paramName), paramType, typeConstructor, assignedName)

          }.getOrElse(c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType"))

          (param, derivedImplicit, paramType)
        }.to[List]

        val callables = typeclasses.map { case (param, typeclass, paramType) =>
          q"""_root_.magnolia.Param[$typeConstructor, $genericType, $paramType](${param.name.toString}, $typeclass, p => p.${TermName(param.name.toString)})"""
        }

        Some {
          q"""
            val parameters: _root_.scala.Array[Param[$typeConstructor, $genericType]] = _root_.scala.Array(..$callables)
            ${c.prefix}.join(_root_.magnolia.JoinContext[$typeConstructor, $genericType]($className, false, parameters,
              (fn: (Param[$typeConstructor, $genericType] => Any)) => new $genericType(..${typeclasses.zipWithIndex.map { case (typeclass, idx) =>
                q"fn(parameters($idx)).asInstanceOf[${typeclass._3}]"
              } })
            ))
          """
        }
      } else if(isSealedTrait) {
        val genericSubtypes = classType.get.knownDirectSubclasses.to[List]
        val subtypes = genericSubtypes.map { sub =>
          val mapping = sub.asType.typeSignature.baseType(genericType.typeSymbol).typeArgs.zip(genericType.typeArgs).toMap
          val newTypeParams = sub.asType.toType.typeArgs.map(mapping(_))
          appliedType(sub.asType.toType.typeConstructor, newTypeParams)
        }

        if(subtypes.isEmpty) {
          c.info(c.enclosingPosition,
              s"magnolia: could not find any direct subtypes of $typeSymbol", true)
          
          c.abort(c.enclosingPosition, "")
        }
      
        val subclasses = subtypes.map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            (searchType, typeclassTree(None, searchType, typeConstructor, assignedName))
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }.map { case (typ, typeclass) =>
          val pf = q"""
          new _root_.scala.PartialFunction[$genericType, $typ] {
            def isDefinedAt(t: $genericType): Boolean = t.isInstanceOf[$typ]
            def apply(t: $genericType): $typ = t.asInstanceOf[$typ]
          }"""

          q"""_root_.magnolia.Subclass[$typeConstructor, $genericType, $typ](${typ.typeSymbol.name.toString}, $typeclass, $pf)"""
        }
          
        Some {
          q"""{
            ${c.prefix}.split(_root_.scala.collection.immutable.List[_root_.magnolia.Subclass[$typeConstructor, $genericType]](..$subclasses))
          }"""
        }
      } else None

      result.map { r =>
        q"""{
          def $assignedName: $resultType = $r
          $assignedName
        }"""
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
          directInferImplicit(genericType, typeConstructor)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"_root_.magnolia.Deferred[$searchType]($methodAsString)")
      }
    } else directInferImplicit(genericType, typeConstructor)
   
    if(currentStack.frames.isEmpty) recursionStack = ListMap()

    result.map { tree =>
      if(currentStack.frames.isEmpty) {
        val out = c.untypecheck(removeDeferred.transform(tree))
        println(s"Bytes: ${out.toString.size}")
        out
      } else tree
    }.getOrElse {
      c.abort(c.enclosingPosition, s"magnolia: could not infer typeclass for type $genericType")
    }
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
