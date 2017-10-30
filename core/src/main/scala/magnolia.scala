package magnolia

import scala.reflect._, macros._
import scala.collection.immutable.ListMap
import language.existentials
import language.higherKinds
import language.experimental.macros

trait Subclass[Tc[_], T] {
  type S <: T
  def typeclass: Tc[S]
  def label: String
  def cast: PartialFunction[T, S]
}

trait Param[Tc[_], T] {
  type S
  def typeclass: Tc[S]
  def label: String
  def dereference(param: T): S
}

trait JoinContext[Tc[_], T] {
  def construct[R](param: ((Param[Tc, T]) => Any)): T
  def typeName: String
  def parameters: List[Param[Tc, T]]
}

object Magnolia {
  import CompileTimeState._

  type Construct[Call[_], T] = ((Call[R] => R) forSome { type R }) => T

  def generic[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    import scala.util.{Try, Success, Failure}
    
    def javaClassName(sym: Symbol): String =
      if(sym.owner.isPackage) sym.fullName
      else if(sym.owner.isModuleClass) s"${javaClassName(sym.owner)}$$${sym.name}"
      else s"${javaClassName(sym.owner)}.${sym.name}"

    def getModule[M](tpe: Type): M = {
      val typeName = javaClassName(tpe.typeSymbol)

      try {
        val cls = Class.forName(s"$typeName$$")
        cls.getField("MODULE$").get(cls).asInstanceOf[M]
      } catch {
        case e: ClassNotFoundException =>
          c.abort(c.enclosingPosition, s"""Class "${typeName}" could not be found. This usually means you are trying to use an interpolator in the same compilation unit as the one in which you defined it. Please try compiling interpolators first, separately from the code using them.""")
      }
    }
    
    val typeConstructor: c.Type = c.prefix.tree.tpe.companion.typeConstructor

    def findType(key: Type): Option[TermName] =
      recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

    def recurse[T](path: TypePath, key: Type, value: TermName)(fn: => T):
        Option[T] = {
      recursionStack = recursionStack.updated(
        c.enclosingPosition,
        recursionStack.get(c.enclosingPosition).map(_.push(path, key, value)).getOrElse(
            Stack(List(Frame(path, key, value)), Nil))
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

    def implicitTree(paramName: Option[String],
                     genericType: Type,
                     typeConstructor: Type,
                     assignedName: TermName): Tree = {

      val searchType = appliedType(typeConstructor, genericType)
      findType(genericType).map { methodName =>
        val methodAsString = methodName.encodedName.toString
        q"_root_.magnolia.Deferred.apply[$searchType]($methodAsString)"
      }.orElse {
        scala.util.Try {
          val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
          val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
          recurse(ChainedImplicit(genericType.toString), genericType, assignedName) {
            val inferredImplicit = c.inferImplicitValue(searchType, false, false)
            q"""{
              def $assignedName: $searchType = $inferredImplicit
              $assignedName
            }"""
          }.get
        }.toOption.orElse(directInferImplicit(genericType, typeConstructor))
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
      val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
      val isValueClass = genericType <:< typeOf[AnyVal]

      val resultType = appliedType(typeConstructor, genericType)


      // FIXME: Handle AnyVals
      if(isCaseClass) {
        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }
        val className = genericType.toString

        val implicits: List[(c.universe.MethodSymbol, c.Tree)] = caseClassParameters.map { param =>
          val paramName = param.name.encodedName.toString

          val derivedImplicit = recurse(ProductType(paramName, genericType.toString), genericType,
              assignedName) {

            implicitTree(Some(paramName), param.returnType, typeConstructor, assignedName)

          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
          }

          (param, derivedImplicit)
        }.to[List]

        Some {
          val callables = implicits.map { case (param, imp) =>
            val label = param.name.toString
            q"""new _root_.magnolia.Param[$typeConstructor, ${genericType}] {
              type S = ${param.returnType}
              def typeclass: ${appliedType(typeConstructor, param.returnType)} = $imp
              def label: _root_.java.lang.String = $label
              def dereference(param: ${genericType}): ${param.returnType} = param.${TermName(label)}
            }"""
          }

          val constructor = q"""new $genericType(..${callables.zip(implicits).map { case (call, imp) =>
            q"fn($call).asInstanceOf[${imp._1.returnType}]"
          } })"""

          val impl = q"""
            ${c.prefix}.join(new _root_.magnolia.JoinContext[$typeConstructor, $genericType] {
              def construct[R](fn: ((Param[${typeConstructor}, $genericType]) => Any)): $genericType = $constructor
              def typeName: _root_.java.lang.String = $className
              def parameters: _root_.scala.List[Param[$typeConstructor, $genericType]] =
                _root_.scala.List(..$callables)
            })
          """
          
          q"""
            def $assignedName: $resultType = $impl
            $assignedName
          """
        }
      } else if(isSealedTrait) {

        val subtypes = classType.get.knownDirectSubclasses.to[List]

        if(subtypes.isEmpty) {
          c.info(c.enclosingPosition,
              s"magnolia: could not find any direct subtypes of $typeSymbol", true)
          
          c.abort(c.enclosingPosition, "")
        }
        
        Some {

          val subclasses = subtypes.map(_.asType.toType).map { searchType =>
            recurse(CoproductType(genericType.toString), genericType, assignedName) {
              (searchType, implicitTree(None, searchType, typeConstructor, assignedName))
            }.getOrElse {
              c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
            }
          }.map { case (typ, typeclass) =>
            val caseClause = cq"(t: $typ) => t"
            val pf = q"""new _root_.scala.PartialFunction[$genericType, $typ] {
              def isDefinedAt(t: $genericType): Boolean = t.isInstanceOf[$typ]
              def apply(t: $genericType): $typ = t.asInstanceOf[$typ]
            }"""

            q"""new _root_.magnolia.Subclass[$typeConstructor, $genericType] {
              type S = $typ
              def label: _root_.java.lang.String = ${typ.typeSymbol.name.toString}
              def typeclass: ${appliedType(typeConstructor, typ)} = $typeclass
              def cast: _root_.scala.PartialFunction[$genericType, $typ] = $pf
            }"""
          }
          
          val impl = q"""{
            ${c.prefix}.split(_root_.scala.collection.immutable.List[_root_.magnolia.Subclass[$typeConstructor, $genericType]](..$subclasses))
          }"""
          
          q"""
            def $assignedName: $resultType = $impl
            $assignedName
          """
        }
      } else None
    }

    val genericType: Type = weakTypeOf[T]
    
    val currentStack: Stack =
      recursionStack.get(c.enclosingPosition).getOrElse(Stack(List(), List()))
    
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
      val out = if(currentStack.frames.isEmpty) c.untypecheck(removeDeferred.transform(tree))
          else tree
      out
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

  case class Stack(frames: List[Frame], errors: List[ImplicitNotFound]) {
    
    def push(path: TypePath, key: whitebox.Context#Type,
        value: whitebox.Context#TermName): Stack =
      Stack(Frame(path, key, value) :: frames, errors)
    
    def pop(): Stack = Stack(frames.tail, errors)
  }

  case class Frame(path: TypePath, genericType: whitebox.Context#Type,
      term: whitebox.Context#TermName) {
    def termName(c: whitebox.Context): c.TermName = term.asInstanceOf[c.TermName]
  }

  var recursionStack: ListMap[api.Position, Stack] = ListMap()
  var emittedErrors: Set[ImplicitNotFound] = Set()
}
