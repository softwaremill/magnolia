package magnolia

import scala.reflect._, macros._
import macrocompat.bundle

import scala.collection.immutable.ListMap

object GlobalMutableState {
  private[magnolia] var state: ListMap[AnyRef, AnyRef] = ListMap()
  private[magnolia] def push(key: AnyRef, value: AnyRef): Unit = state += ((key, value))
  private[magnolia] def pop(): Unit = state = state.init
  
  private[magnolia] def has(c: whitebox.Context)(key: AnyRef): Option[c.universe.TermName] =
    state.get(key).asInstanceOf[Option[c.universe.TermName]]
}

@bundle
class Macros(val c: whitebox.Context) {

  def dereference(path: c.Tree, elem: String): c.Tree = path

  def getImplicit(genericType: c.universe.Type,
                  typeConstructor: c.universe.Type,
                  myName: c.universe.TermName): c.Tree = {
    
    import c.universe._
   
    GlobalMutableState.push(genericType, myName)
    
    val result = GlobalMutableState.has(c)(genericType).map { nm => q"$nm" }.orElse {
      val searchType = appliedType(typeConstructor, genericType)
      if(GlobalMutableState.has(c)(genericType).isEmpty) {
        val inferredImplicit =
          try Some(c.inferImplicitValue(searchType, false, false)) catch {
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

    GlobalMutableState.pop()

    result
  }
  
  def directInferImplicit(genericType: c.universe.Type,
         typeConstructor: c.universe.Type): Option[c.Tree] = {
    import c.universe._
   
    val myName = TermName(c.freshName("extractor$"))
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
        val imp = getImplicit(ret, typeConstructor, myName)
        q"$imp.extract(src)"
      }

      Some(q"new $genericType(..$implicits)")
    } else if(isSealedTrait) {
      val subtypes = classType.get.knownDirectSubclasses.to[List]
      
      Some(subtypes.map(_.asType.toType).map(t => getImplicit(t, typeConstructor, myName)).foldLeft(q"null": c.Tree) { (a, b) =>
        q"(try { $b.extract(src) } catch { case e: _root_.java.lang.Exception => $a })"
      })
      
    } else None

    val result = construct.map { c =>
      q"""{
        def $myName: $resultType = new $resultType {
          def extract(src: _root_.java.lang.String): $genericType = $c
        }
        $myName
      }"""
    }

    //GlobalMutableState.pop()

    println(result)

    result
  }
  
  def generic[T: c.WeakTypeTag, Tc: c.WeakTypeTag]: c.Tree = {
    import c.universe._

    val genericType: Type = weakTypeOf[T]
    val typeConstructor: Type = weakTypeOf[Tc].typeConstructor

    val result = directInferImplicit(genericType, typeConstructor)

    result.getOrElse {
      c.abort(c.enclosingPosition, "Could not infer extractor. Sorry.")
    }
  }

}

