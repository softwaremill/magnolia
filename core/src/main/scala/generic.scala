package magnolia

import scala.reflect._, macros._
import macrocompat.bundle
import scala.util.Try
import language.existentials

import scala.collection.immutable.ListMap

abstract class Transformation[C <: whitebox.Context](val c: C) {
  def typeclassBody(genericType: c.Type, implementation: c.Tree): c.Tree
  def coproductReduction(left: c.Tree, right: c.Tree): c.Tree
  def dereferenceValue(value: c.Tree, elem: String): c.Tree
  def callDelegateMethod(value: c.Tree, argument: c.Tree): c.Tree
}

abstract class MagnoliaMacro(val c: whitebox.Context) {
  import c.universe._
  import CompileTimeState._

  protected def transformation(c: whitebox.Context): Transformation[c.type]

  private def findType(key: c.universe.Type): Option[c.universe.TermName] =
    recursionStack(c.enclosingPosition).get(key).map(_.asInstanceOf[c.universe.TermName])

  private def recurse[T](key: c.universe.Type, value: c.universe.TermName)(fn: => T): Option[T] = {
    recursionStack = recursionStack.updated(
      c.enclosingPosition,
      recursionStack.get(c.enclosingPosition).map { m =>
        m.updated(key, value)
      }.getOrElse(ListMap(key -> value))
    )
    
    try Some(fn) catch { case e: Exception => None } finally {
      recursionStack = recursionStack.updated(c.enclosingPosition,
          recursionStack(c.enclosingPosition).init)
    }
  }
  
  private val transformer = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case q"_root_.magnolia.Lazy[$returnType](${Literal(Constant(method: String))})" =>
        q"${TermName(method)}"
      case _ => super.transform(tree)
    }
  }
  

  private def getImplicit(genericType: c.universe.Type,
                  typeConstructor: c.universe.Type,
                  assignedName: c.universe.TermName): c.Tree = {
    
    findType(genericType).map { methodName =>
      val methodAsString = methodName.encodedName.toString
      val searchType = appliedType(typeConstructor, genericType)
      q"_root_.magnolia.Lazy[$searchType]($methodAsString)"
    }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      if(findType(genericType).isEmpty) {
        lastSearchType = Some(genericType)
        val inferredImplicit = try Some({
          val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
          val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
          recurse(genericType, assignedName) {
            val imp = c.inferImplicitValue(searchType, false, false)
            q"""{
              def $assignedName: $searchType = $imp
              $assignedName
            }"""
          }.get
        }) catch {
          case e: Exception => None
        }

        inferredImplicit.orElse {
          directInferImplicit(genericType, typeConstructor)
        }
      } else {
        directInferImplicit(genericType, typeConstructor)
      }
    }.getOrElse {
      c.abort(c.enclosingPosition, "Could not find extractor for type "+genericType)
    }
  }
  
  private def directInferImplicit(genericType: c.universe.Type,
         typeConstructor: c.universe.Type): Option[c.Tree] = {

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
        
        val derivedImplicit = recurse(genericType, assignedName) {
          getImplicit(param.returnType, typeConstructor, assignedName)
        }.getOrElse {
          c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
        }
        
        val dereferencedValue = transformation(c).dereferenceValue(q"src", param.name.toString)
        
        transformation(c).callDelegateMethod(
          derivedImplicit,
          dereferencedValue
        )
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {

      val subtypes = classType.get.knownDirectSubclasses.to[List]
      
      Some(
        transformation(c)callDelegateMethod(subtypes.map(_.asType.toType).map { searchType =>
          recurse(genericType, assignedName) {
            getImplicit(searchType, typeConstructor, assignedName)
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }.reduce(transformation(c).coproductReduction), q"src")
      )
    } else None

    construct.map { const =>
      val bodyImplementation = transformation(c).typeclassBody(genericType, const)
      
      q"""{
        def $assignedName: $resultType = new $resultType { $bodyImplementation }
        $assignedName
      }"""
    }
  }
  
  def magnolia[T: c.WeakTypeTag, Typeclass: c.WeakTypeTag]: c.Tree = try {
    import c.universe._

    val genericType: Type = weakTypeOf[T]
    val directlyReentrant = Some(genericType) == lastSearchType
    if(directlyReentrant) throw DirectlyReentrantException()
    val result: Option[c.Tree] = if(lastSearchType != None) {
      findType(genericType) match {
        case None =>
          val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
          directInferImplicit(genericType, typeConstructor)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"_root_.magnolia.Lazy[$searchType]($methodAsString)")
      }
    } else {
      val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
      directInferImplicit(genericType, typeConstructor)
    }
    
    result.map { tree =>
      val recursionDepth = recursionStack.get(c.enclosingPosition).map(_.size).getOrElse(0)
      if(recursionDepth == 0) c.untypecheck(transformer.transform(tree)) else tree
    }.getOrElse {
      c.abort(c.enclosingPosition, "Could not infer typeclass. Sorry.")
    }
  } catch {
    case e@DirectlyReentrantException() => throw e
    case e: Exception =>
      e.printStackTrace()
      ???
  }
}

private[magnolia] case class DirectlyReentrantException() extends
    Exception("attempt to recurse directly")

private[magnolia] object Lazy { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  private[magnolia] var recursionStack: Map[api.Position, ListMap[
    c.universe.Type forSome { val c: whitebox.Context },
    c.universe.TermName forSome { val c: whitebox.Context }
  ]] = Map()
 
  private[magnolia] var lastSearchType: Option[Universe#Type] = None
}


@bundle
class Macros(val context: whitebox.Context) extends MagnoliaMacro(context) {
  protected def transformation(c: whitebox.Context): Transformation[c.type] =
    new Transformation[c.type](c) {
      import c.universe._

      def typeclassBody(genericType: c.Type, implementation: c.Tree): c.Tree =
        q"""def extract(src: _root_.magnolia.Thing): $genericType = $implementation"""

      def dereferenceValue(value: c.Tree, elem: String): c.Tree = q"$value.access($elem)"
      def callDelegateMethod(value: c.Tree, argument: c.Tree): c.Tree = q"$value.extract($argument)"
      def coproductReduction(left: c.Tree, right: c.Tree): c.Tree = q"$left.orElse($right)"
    }
}
