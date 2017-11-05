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
  def apply[Tc[_], T, S1 <: T](name: String, tc: => Tc[S1], isType: T => Boolean, asType: T => S1) = new Subclass[Tc, T] {
    type S = S1
    def label: String = name
    def typeclass: Tc[S] = tc
    def cast: PartialFunction[T, S] = new PartialFunction[T, S] {
      def isDefinedAt(t: T) = isType(t)
      def apply(t: T): S = asType(t)
    }
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

abstract class JoinContext[Tc[_], T](val typeName: String, val isObject: Boolean, params: Array[Param[Tc, T]]) {
  def construct(param: ((Param[Tc, T]) => Any)): T
  def parameters: Seq[Param[Tc, T]] = params
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
        val obj = genericType.typeSymbol.companion.asTerm
        val className = obj.name.toString
        val impl = q"""
          ${c.prefix}.join(_root_.magnolia.JoinContext[$typeConstructor, $genericType]($className, true, new _root_.scala.Array(0), $obj))
        """
        Some(Typeclass(genericType, impl))
      } else if(isCaseClass) {
        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }
        val className = genericType.toString

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
        
        val assignments = caseParams.zipWithIndex.map { case (CaseParam(param, typeclass, paramType, ref), idx) =>
          q"""$paramsVal($idx) = _root_.magnolia.Param[$typeConstructor, $genericType, $paramType](
            ${param.name.toString}, $ref, _.${TermName(param.name.toString)}
          )"""
        }

        Some(Typeclass(genericType,
          q"""{
            ..$preAssignments
            val $paramsVal: _root_.scala.Array[Param[$typeConstructor, $genericType]] =
              new _root_.scala.Array(${assignments.length})
            ..$assignments
            
            ${c.prefix}.join(_root_.magnolia.JoinContext[$typeConstructor, $genericType](
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
        
        val subclassesVal: TermName = TermName(c.freshName("subclasses"))
      
        val assignments = subtypes.map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            (searchType, typeclassTree(None, searchType, typeConstructor, assignedName))
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }.zipWithIndex.map { case ((typ, typeclass), idx) =>
          q"""$subclassesVal($idx) = _root_.magnolia.Subclass[$typeConstructor, $genericType, $typ](
            ${typ.typeSymbol.name.toString},
            $typeclass,
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }
          
        Some {
          Typeclass(genericType, q"""{
            val $subclassesVal: _root_.scala.Array[_root_.magnolia.Subclass[$typeConstructor, $genericType]] =
              new _root_.scala.Array(${assignments.size})
            
            ..$assignments
            
            ${c.prefix}.dispatch($subclassesVal: _root_.scala.Seq[_root_.magnolia.Subclass[$typeConstructor, $genericType]])
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
