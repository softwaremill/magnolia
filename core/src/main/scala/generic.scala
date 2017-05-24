package magnolia

import scala.reflect._, macros._
import macrocompat.bundle

object GlobalState {
  var globalState: Map[AnyRef, AnyRef] = Map()
}

@bundle
class Macros(val c: whitebox.Context) {

  def getImplicit(genericType: c.universe.Type,
                  typeConstructor: c.universe.Type/*,
                  scope: Map[c.universe.Type, c.universe.TermName]*/): c.Tree = {
    
    import c.universe._
   
    val scope = GlobalState.globalState.asInstanceOf[Map[Type, TermName]]

    scope.get(genericType) match {
      case Some(ref) =>
        q"$ref"
      case None =>
        
      val searchType = appliedType(typeConstructor, genericType)
      println(s"${scope.keySet} vs $genericType")
      println(s"inferring on $genericType")
      try c.inferImplicitValue(searchType, false, false) catch {
        case e: Exception =>
          go(genericType, typeConstructor/*, scope*/)
      }
    }

    scope.get(genericType).map { nm =>
      println("substituting "+nm)
      q"$nm"
    }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      println(s"${scope.keySet} vs $genericType")
      if(!scope.keySet.contains(genericType)) {
        println(s"inferring on $genericType")
        Option({
          val x = try c.inferImplicitValue(searchType, false, false) catch {
            case e: Exception => null
          }
          println("Managed to infer "+x)
          x
        }).orElse {
          println("Failed, so recursing")
          go(genericType, typeConstructor/*, scope*/)
        }
      } else {
        println("recursing")
        go(genericType, typeConstructor/*, scope*/)
      }
    }.getOrElse {
      c.abort(c.enclosingPosition, "Could not find extractor for type "+genericType)
    }
  }
  
  def go(genericType: c.universe.Type,
         typeConstructor: c.universe.Type/*,
         scope: Map[c.universe.Type, c.universe.TermName]*/): Option[c.Tree] = {
    import c.universe._
   
    println(s"go($genericType, ${GlobalState.globalState})")

    
    val myName = TermName(c.freshName("extractor$"))
    println(s"before: ${GlobalState.globalState}")
    GlobalState.globalState = GlobalState.globalState + (genericType -> myName)
    println(s"after: ${GlobalState.globalState}")
    val typeSymbol = genericType.typeSymbol
    val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
    val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
    val isAnyVal = genericType <:< typeOf[AnyVal]
    
    val resultType = appliedType(typeConstructor, genericType)

    val construct = if(isCaseClass) {
      val implicits = genericType.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map { p =>
        val ret = p.returnType
        val imp = getImplicit(ret, typeConstructor/*, newScope*/)
        q"$imp.extract(src)"
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {
      //println(s"$resultType a sealed trait")
      val subtypes = classType.get.knownDirectSubclasses.to[List]
      
      val tries = subtypes.map(_.asType.toType).map(t => getImplicit(t, typeConstructor/*, newScope*/)).foldLeft(q"null": c.Tree) { (a, b) =>
        q"(try { $b.extract(src) } catch { case e: _root_.java.lang.Exception => $a })"
      }
      
      Some(q"$tries.asInstanceOf[$genericType]")
      
    } else None

    val result = construct.map { c =>
      q"""{
        def $myName: $resultType = new $resultType {
          def extract(src: _root_.java.lang.String): $genericType = $c
        }
        $myName
      }"""
    }

    //println(s"Generated result for $genericType: $result")

    result
  }
  def generic[T: c.WeakTypeTag, Tc: c.WeakTypeTag]: c.Tree = try {
    import c.universe._

    val genericType: Type = weakTypeOf[T]
    val typeConstructor: Type = weakTypeOf[Tc].typeConstructor

    val result = go(genericType, typeConstructor)

    println(result)

    result.getOrElse {
      c.abort(c.enclosingPosition, "Could not infer extractor. Sorry.")
    }
  } catch {
    case e: Exception =>
      println("Macro failed!!! "+e)
      //e.printStackTrace()
      ???
  }

}

