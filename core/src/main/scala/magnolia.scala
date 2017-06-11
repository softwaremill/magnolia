package magnolia

import scala.reflect._, macros._
import macrocompat.bundle
import scala.util.Try
import language.existentials
import language.higherKinds

@bundle
class Macros(val c: whitebox.Context) {
  import c.universe._
  import CompileTimeState._

  private def findType(key: c.universe.Type): Option[c.TermName] =
    recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

  private def recurse[T](path: TypePath, key: c.universe.Type, value: c.TermName)(fn: => T):
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
  
  private val removeLazy: Transformer = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case q"magnolia.Lazy.apply[$returnType](${Literal(Constant(method: String))})" =>
        q"${TermName(method)}"
      case _ =>
        super.transform(tree)
    }
  }
  
  private def getImplicit(paramName: Option[String],
                          genericType: c.universe.Type,
                          typeConstructor: c.universe.Type,
                          assignedName: c.TermName,
                          dereferencerImplicit: c.Tree): c.Tree = {
    
    findType(genericType).map { methodName =>
      val methodAsString = methodName.encodedName.toString
      val searchType = appliedType(typeConstructor, genericType)
      q"_root_.magnolia.Lazy[$searchType]($methodAsString)"
    }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      findType(genericType).map { _ =>
        directInferImplicit(genericType, typeConstructor, dereferencerImplicit)
      }.getOrElse {
        scala.util.Try {
          val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
          val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
          recurse(RecursiveCall(genericType.toString), genericType, assignedName) {
            val inferredImplicit = c.inferImplicitValue(searchType, false, false)
            q"""{
              def $assignedName: $searchType = $inferredImplicit
              $assignedName
            }"""
          }.get
        }.toOption.orElse(directInferImplicit(genericType, typeConstructor, dereferencerImplicit))
      }
    }.getOrElse {
      val currentStack: Stack = recursionStack(c.enclosingPosition)
      
      val error = ImplicitNotFound(genericType.toString,
          recursionStack(c.enclosingPosition).frames.map(_.path))
      
      val updatedStack = currentStack.copy(errors = error :: currentStack.errors) 
      recursionStack = recursionStack.updated(c.enclosingPosition, updatedStack)
      c.abort(c.enclosingPosition, s"Could not find type class for type $genericType")
    }
  }
  
  private def directInferImplicit(genericType: c.universe.Type,
         typeConstructor: c.universe.Type,
         dereferencerImplicit: c.Tree): Option[c.Tree] = {

    val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
    val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
    val typeSymbol = genericType.typeSymbol
    val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
    val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
    val isValueClass = genericType <:< typeOf[AnyVal]
    
    val resultType = appliedType(typeConstructor, genericType)

    val construct = if(isCaseClass) {
      val implicits = genericType.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map { param =>
        val paramName = param.name.encodedName.toString
        val derivedImplicit = recurse(ProductType(paramName, genericType.toString), genericType, assignedName) {
          getImplicit(Some(paramName), param.returnType, typeConstructor, assignedName, dereferencerImplicit)
        }.getOrElse {
          c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
        }
        
        val dereferencedValue = q"$dereferencerImplicit.dereference(sourceParameter, ${param.name.toString})"
        
        q"$dereferencerImplicit.delegate($derivedImplicit, $dereferencedValue)"
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {

      val subtypes = classType.get.knownDirectSubclasses.to[List]
      
      Some {
        val reduction = subtypes.map(_.asType.toType).map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            getImplicit(None, searchType, typeConstructor, assignedName, dereferencerImplicit)
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }.reduce { (left, right) => q"$dereferencerImplicit.combine($left, $right)" }
        
        q"$dereferencerImplicit.delegate($reduction, sourceParameter)"
      }
    } else None

    construct.map { const =>
      q"""{
        def $assignedName: $resultType = $dereferencerImplicit.construct { sourceParameter => $const }
        $assignedName
      }"""
    }
  }
  
  def magnolia[T: c.WeakTypeTag, Typeclass: c.WeakTypeTag]: c.Tree = {
    import c.universe._

    val genericType: Type = weakTypeOf[T]
    val currentStack: List[Frame] = recursionStack.get(c.enclosingPosition).map(_.frames).getOrElse(List())
    val directlyReentrant = Some(genericType) == currentStack.headOption.map(_.genericType)
    val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
    val dereferencerTypeclass = weakTypeOf[Dereferencer[_]].typeConstructor
    val dereferencerType = appliedType(dereferencerTypeclass, typeConstructor)
    val dereferencerImplicit = c.untypecheck(c.inferImplicitValue(dereferencerType, false, false))
    
    if(directlyReentrant) throw DirectlyReentrantException()
    
    val result: Option[c.Tree] = if(!recursionStack.isEmpty) {
      findType(genericType) match {
        case None =>
          directInferImplicit(genericType, typeConstructor, dereferencerImplicit)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"_root_.magnolia.Lazy[$searchType]($methodAsString)")
      }
    } else {
      val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
      directInferImplicit(genericType, typeConstructor, dereferencerImplicit)
    }
    
    result.map { tree =>
      if(currentStack.isEmpty) c.untypecheck(removeLazy.transform(tree)) else tree
    }.getOrElse {
      if(currentStack.isEmpty) println("Foo")
      c.abort(c.enclosingPosition, "could not infer typeclass for type $genericType")
    }

  }
}

private[magnolia] case class DirectlyReentrantException() extends
    Exception("attempt to recurse directly")

private[magnolia] object Lazy { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed trait TypePath
  case class CoproductType(typeName: String) extends TypePath {
    override def toString = s"coproduct type $typeName"
  }

  case class ProductType(paramName: String, typeName: String) extends TypePath {
    override def toString = s"parameter '$paramName' of product type $typeName"
  }

  case class RecursiveCall(typeName: String) extends TypePath {
    override def toString = s"recursive implicit of type $typeName"
  }

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

  private[magnolia] var recursionStack: Map[api.Position, Stack] =
    Map()
}

trait Dereferencer[Typeclass[_]] {
  type Value
  def dereference(value: Value, param: String): Value
  def delegate[T](typeclass: Typeclass[T], value: Value): T
  def combine[Supertype, Right <: Supertype](left: Typeclass[_ <: Supertype], right: Typeclass[Right]): Typeclass[Supertype]
  def construct[T](body: Value => T): Typeclass[T]
}
