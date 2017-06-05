package magnolia

import scala.reflect._, macros._
import macrocompat.bundle
import scala.util.Try
import language.existentials

import scala.collection.immutable.ListMap

case class Pos(pos: api.Position) {
  override def toString = s"${pos.line}:${pos.column}"
}

case class ReentrantException() extends Exception("attempt to recurse directly")

object GlobalMutableState {
  private[magnolia] var state: Map[Pos, ListMap[c.universe.Type forSome { val c: whitebox.Context }, c.universe.TermName forSome { val c: whitebox.Context }]] = Map()
  
  private[magnolia] def push(c: whitebox.Context)(key: c.universe.Type, value: c.universe.TermName): Unit = {
    println(s"push($key, $value)")
    state = state.updated(Pos(c.enclosingPosition), state.get(Pos(c.enclosingPosition)).map { m =>
      m.updated(key, value)
    }.getOrElse(ListMap(key -> value)))
    println("state = "+state)
  }
  
  private[magnolia] def pop(c: whitebox.Context): Unit = {
    println(s"pop(${state(Pos(c.enclosingPosition)).last})")
    state = state.updated(Pos(c.enclosingPosition), state(Pos(c.enclosingPosition)).init)
    println("state = "+state)
  }
  
  private[magnolia] def has(c: whitebox.Context)(key: c.universe.Type): Option[c.universe.TermName] = {
    try state(Pos(c.enclosingPosition)).get(key).asInstanceOf[Option[c.universe.TermName]] catch {
      case e: Exception =>
        ???
    }
  }

  private[magnolia] var searchType: AnyRef = null
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

  def getImplicit(genericType: c.universe.Type,
                  typeConstructor: c.universe.Type,
                  myName: c.universe.TermName): c.Tree = {
    
    import c.universe._
    //println(s"getImplicit1($genericType)")
    val x = GlobalMutableState.has(c)(genericType)
    val result = x.map { nm =>
      q"$nm"
    }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      if(GlobalMutableState.has(c)(genericType).isEmpty) {
        GlobalMutableState.searchType = genericType
        val inferredImplicit = try Some({
          //println(s"ONE: $genericType -> $myName")
          //GlobalMutableState.push(c)(genericType, myName)
          val imp = c.inferImplicitValue(searchType, false, false)
          //GlobalMutableState.pop(c)
          q"""{
            def $myName = $imp
            $myName
          }"""
        }) catch {
          case e: Exception => None
        }

        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case ta@TypeApply(Select(Literal(Constant(method: String)), TermName("asInstanceOf")), List(tpe)) =>
              println(s"FOUND TYPEAPPLY: ${ta}")
              val m = TermName(method)
              q"$m"
            case _ => super.transform(tree)
          }
        }

        inferredImplicit.map { imp =>
          val x = c.untypecheck(imp)
          val z = c.untypecheck(transformer.transform(imp))
          println("x: "+x+", z: "+z)
          z
        }.orElse {
          directInferImplicit(genericType, typeConstructor)
        }
      } else {
        directInferImplicit(genericType, typeConstructor)
      }
    }.getOrElse {
      println("Really failed to find extractor for type "+genericType)
      c.abort(c.enclosingPosition, "Could not find extractor for type "+genericType)
    }

    result
  }
  
  def directInferImplicit(genericType: c.universe.Type,
         typeConstructor: c.universe.Type): Option[c.Tree] = {
    import c.universe._
   
    //println(s"directInferImplicit($genericType)")

    val myName: TermName = TermName(c.freshName(genericType.typeSymbol.name.encodedName.toString.toLowerCase+"Extractor"))
    val typeSymbol = genericType.typeSymbol
    val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
    val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
    val isAnyVal = genericType <:< typeOf[AnyVal]
    
    val resultType = appliedType(typeConstructor, genericType)

    val construct = if(isCaseClass) {
      val implicits = genericType.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map { param =>
        val returnType = param.returnType
        println(s"TWO: $genericType -> $myName")
        GlobalMutableState.push(c)(genericType, myName)
        val imp = getImplicit(returnType, typeConstructor, myName)
        GlobalMutableState.pop(c)
        val dereferenced = dereferenceValue(c)(q"src", param.name.toString)
        callDelegateMethod(c)(imp, dereferenced)
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {
      val subtypes = classType.get.knownDirectSubclasses.to[List]
      Some(subtypes.map(_.asType.toType).map { searchType =>
        println(s"THREE: $genericType -> $myName")
        GlobalMutableState.push(c)(genericType, myName)
        val res = getImplicit(searchType, typeConstructor, myName)
        GlobalMutableState.pop(c)
        res
      }.reduce(coproductReduction(c))).map { imp =>
        callDelegateMethod(c)(imp, q"src")
      }
      
    } else None

    val result = construct.map { const =>
      
      val methodImplementation = classBody(c)(genericType, const)
      q"""{
        def $myName: $resultType = new $resultType {
          $methodImplementation
        }
        $myName
      }"""
    }


    result
  }
  
  protected def classBody(c: whitebox.Context)(genericType: c.Type, implementation: c.Tree): c.Tree
  protected def coproductReduction(c: whitebox.Context)(left: c.Tree, right: c.Tree): c.Tree
  protected def dereferenceValue(c: whitebox.Context)(value: c.Tree, elem: String): c.Tree
  protected def callDelegateMethod(c: whitebox.Context)(value: c.Tree, argument: c.Tree): c.Tree

  def generic[T: c.WeakTypeTag, Tc: c.WeakTypeTag]: c.Tree = try {
    import c.universe._

    //println("Entering generic for type "+weakTypeOf[T])

    val genericType: Type = weakTypeOf[T]
    val reentrant = genericType == GlobalMutableState.searchType
    //println(s"previous: ${GlobalMutableState.searchType}; THIS TYPE = $genericType")
    val result: Option[c.Tree] = if(reentrant) {
      //println("Reentrant.")
      throw ReentrantException()
    } else if(GlobalMutableState.searchType != null) {
      GlobalMutableState.has(c)(genericType) match {
        case None =>
          val typeConstructor: Type = weakTypeOf[Tc].typeConstructor
          directInferImplicit(genericType, typeConstructor)
        case Some(t) =>
          val str = t.toString
          val typeConstructor: Type = weakTypeOf[Tc].typeConstructor
          val searchType = appliedType(typeConstructor, genericType)
          println(s"$str ===>>> ${searchType}")
          Some(q"$str.asInstanceOf[${searchType}]")
      }
    } else {
      val typeConstructor: Type = weakTypeOf[Tc].typeConstructor
      directInferImplicit(genericType, typeConstructor)
    }
    
    println("Final result: "+result)

    result.getOrElse {
      println("FAIL.")
      c.abort(c.enclosingPosition, "Could not infer extractor. Sorry.")
    }
  } catch {
    case e@ReentrantException() => throw e
    case e: Exception =>
      println("Oh no, there was a problem: "+e)
      e.printStackTrace()
      ???
  }

}

