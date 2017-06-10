package magnolia

import scala.reflect._, macros._
import macrocompat.bundle
import scala.util.Try
import language.existentials

import scala.collection.immutable.ListMap

import GlobalMutableState.log

case class Pos(pos: api.Position) {
  override def toString = s"${pos.line}:${pos.column}"
}

case class DirectlyReentrantException() extends Exception("attempt to recurse directly")

object Lazy {
  def apply[T](method: String): T = ???
}

object GlobalMutableState {

  def log(c: whitebox.Context)(msg: String) = msg.split("/n").foreach { ln =>
    println("  "*stackSize(c)+ln)
  }

  private[magnolia] var state: Map[Pos, ListMap[c.universe.Type forSome { val c: whitebox.Context }, c.universe.TermName forSome { val c: whitebox.Context }]] = Map()
 

  private[magnolia] def wrap[T](c: whitebox.Context)(key: c.universe.Type, value: c.universe.TermName)(fn: => T): Option[T] = {
    push(c)(key, value)
    try Some(fn) catch {
      case e: Exception => None
    } finally {
      pop(c)
    }
  }
  private def push(c: whitebox.Context)(key: c.universe.Type, value: c.universe.TermName): Unit = {
    state = state.updated(Pos(c.enclosingPosition), state.get(Pos(c.enclosingPosition)).map { m =>
      m.updated(key, value)
    }.getOrElse(ListMap(key -> value)))
  }
  
  private def pop(c: whitebox.Context): Unit = {
    state = state.updated(Pos(c.enclosingPosition), state(Pos(c.enclosingPosition)).init)
  }
  
  private[magnolia] def find(c: whitebox.Context)(key: c.universe.Type): Option[c.universe.TermName] = {
    try state(Pos(c.enclosingPosition)).get(key).asInstanceOf[Option[c.universe.TermName]] catch {
      case e: Exception =>
        ???
    }
  }

  private[magnolia] def enclosingTypes(c: whitebox.Context): List[c.universe.Type] =
    state.get(Pos(c.enclosingPosition)).to[List].flatMap(_.keySet.map(_.asInstanceOf[c.universe.Type]).to[List])

  private[magnolia] def stackSize(c: whitebox.Context): Int = state.get(Pos(c.enclosingPosition)).map(_.size).getOrElse(0)

  private[magnolia] var searchType: Option[Universe#Type] = None
}

@bundle
class Macros(val context: whitebox.Context) extends GenericMacro(context) {
  
  protected def classBody(context: whitebox.Context)(genericType: context.Type, implementation: context.Tree): context.Tree = {
    import context.universe._
    q"""def extract(src: _root_.magnolia.Thing): $genericType = $implementation"""
  }

  protected def dereferenceValue(context: whitebox.Context)(value: context.Tree, elem: String): context.Tree = {
    import context.universe._
    q"$value.access($elem)"
  }
  
  protected def callDelegateMethod(context: whitebox.Context)(value: context.Tree, argument: context.Tree): context.Tree = {
    import context.universe._
    q"$value.extract($argument)"
  }
  
  protected def coproductReduction(context: whitebox.Context)(left: context.Tree, right: context.Tree): context.Tree = {
    import context.universe._
    q"$left.orElse($right)"
  }
}

abstract class GenericMacro(whiteboxContext: whitebox.Context) {

  val c = whiteboxContext
 
  import c.universe._
  val transformer = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case q"_root_.magnolia.Lazy[$returnType](${Literal(Constant(method: String))})" =>
        q"${TermName(method)}"
      case _ => super.transform(tree)
    }
  }
  

  def getImplicit(genericType: c.universe.Type,
                  typeConstructor: c.universe.Type,
                  myName: c.universe.TermName): c.Tree = {
    
    GlobalMutableState.find(c)(genericType).map { methodName =>
      val methodAsString = methodName.encodedName.toString
      val searchType = appliedType(typeConstructor, genericType)
      q"_root_.magnolia.Lazy[$searchType]($methodAsString)"
    }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      if(GlobalMutableState.find(c)(genericType).isEmpty) {
        log(c)(s"could not find type $genericType in current context")
        GlobalMutableState.searchType = Some(genericType)
        val inferredImplicit = try Some({
          val myName: TermName = TermName(c.freshName(genericType.typeSymbol.name.encodedName.toString.toLowerCase+"Extractor"))
          GlobalMutableState.wrap(c)(genericType, myName) {
            val imp = c.inferImplicitValue(searchType, false, false)
            q"""{
              def $myName: $searchType = $imp
              $myName
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
      log(c)("failed this branch of derivation for type "+genericType)
      c.abort(c.enclosingPosition, "Could not find extractor for type "+genericType)
    }
  }
  
  def directInferImplicit(genericType: c.universe.Type,
         typeConstructor: c.universe.Type): Option[c.Tree] = {
   
    log(c)(s"directInferImplicit($genericType) given definitions for ${GlobalMutableState.enclosingTypes(c).mkString("{", ", ", "}")}")

    val myName: TermName = TermName(c.freshName(genericType.typeSymbol.name.encodedName.toString.toLowerCase+"Extractor"))
    val typeSymbol = genericType.typeSymbol
    val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
    val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
    val isAnyVal = genericType <:< typeOf[AnyVal]
    
    val resultType = appliedType(typeConstructor, genericType)

    val construct = if(isCaseClass) {
      log(c)(s"deriving $genericType as a product type")
      val implicits = genericType.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map { param =>
        log(c)(s"dealing with parameter '${param.name}'")
        val returnType = param.returnType
        val imp = GlobalMutableState.wrap(c)(genericType, myName) {
          getImplicit(returnType, typeConstructor, myName)
        }.map { imp =>
          val impString = if(imp.toString.contains("\\n")) "<synthetic>" else imp.toString
          log(c)(s"found an implicit for type $returnType: $impString")
          imp
        }.getOrElse {
          log(c)(s"failed to get implicit for type $genericType")
          c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
        }
        val dereferenced = dereferenceValue(c)(q"src", param.name.toString)
        callDelegateMethod(c)(imp, dereferenced)
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {
      log(c)(s"deriving $genericType as a coproduct type")
      val subtypes = classType.get.knownDirectSubclasses.to[List]
      Some {
        subtypes.map(_.asType.toType).map { searchType =>
          log(c)(s"exploring coproduct subtype $searchType")
          val imp = GlobalMutableState.wrap(c)(genericType, myName) {
            getImplicit(searchType, typeConstructor, myName)
          }.getOrElse {
            log(c)("exploration was unsuccessful")
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
          log(c)(s"successful exploration: $imp")
          imp
        }.reduce(coproductReduction(c))
      }.map { imp =>
        callDelegateMethod(c)(imp, q"src")
      }
      
    } else None

    construct.map { const =>
      val methodImplementation = classBody(c)(genericType, const)
      
      q"""{
        def $myName: $resultType = new $resultType {
          $methodImplementation
        }
        $myName
      }"""
    }
  }
  
  protected def classBody(c: whitebox.Context)(genericType: c.Type, implementation: c.Tree): c.Tree
  protected def coproductReduction(c: whitebox.Context)(left: c.Tree, right: c.Tree): c.Tree
  protected def dereferenceValue(c: whitebox.Context)(value: c.Tree, elem: String): c.Tree
  protected def callDelegateMethod(c: whitebox.Context)(value: c.Tree, argument: c.Tree): c.Tree

  def generic[T: c.WeakTypeTag, Typeclass: c.WeakTypeTag]: c.Tree = try {
    import c.universe._

    val genericType: Type = weakTypeOf[T]
    val directlyReentrant = Some(genericType) == GlobalMutableState.searchType
    //log(c)(s"previous: ${GlobalMutableState.searchType}; THIS TYPE = $genericType")
    val result: Option[c.Tree] = if(directlyReentrant) {
      log(c)(s"detected direct reentry into generic macro for $genericType; aborting")
      throw DirectlyReentrantException()
    } else if(GlobalMutableState.searchType != None) {
      log(c)(s"searching for in-scope implicit for $genericType")
      GlobalMutableState.find(c)(genericType) match {
        case None =>
          log(c)(s"could not find suitable implicit, so recursing on $genericType")
          val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
          directInferImplicit(genericType, typeConstructor)
        case Some(enclosingRef) =>
          log(c)(s"found an enclosing implicit for $enclosingRef")
          val str = enclosingRef.toString
          val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"$str.asInstanceOf[${searchType}]")
      }
    } else {
      val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
      directInferImplicit(genericType, typeConstructor)
    }
    
    result.map { tree =>
      val depth = GlobalMutableState.stackSize(c)
      println("depth = "+depth)
      val transformedTree = if(depth == 0) c.untypecheck(transformer.transform(tree)) else tree
      val treeString = if(tree.toString.contains("\\n")) "<synthetic>" else tree.toString
      log(c)(s"returning a complete tree, which may or may not typecheck: $treeString")
      transformedTree
    }.getOrElse {
      log(c)("failed to derive a tree")
      c.abort(c.enclosingPosition, "Could not infer typeclass. Sorry.")
    }
  } catch {
    case e@DirectlyReentrantException() => throw e
    case e: Exception =>
      log(c)("Oh no, there was a problem: "+e)
      e.printStackTrace()
      ???
  }

}

