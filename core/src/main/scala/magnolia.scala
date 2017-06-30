package magnolia

import scala.reflect._, macros._
import macrocompat.bundle
import scala.collection.immutable.ListMap
import language.existentials
import language.higherKinds
import language.experimental.macros

@bundle
class Macros(val c: whitebox.Context) {
  import c.universe._
  import CompileTimeState._

  sealed trait GeneralDerivationImplicit { def tree: Tree }
  case class DerivationImplicit(tree: Tree) extends GeneralDerivationImplicit
  sealed trait CoderivationImplicit extends GeneralDerivationImplicit
  case class Coderivation1Implicit(tree: Tree) extends CoderivationImplicit
  case class Coderivation2Implicit(tree: Tree) extends CoderivationImplicit

  private def findType(key: Type): Option[TermName] =
    recursionStack(c.enclosingPosition).frames.find(_.genericType == key).map(_.termName(c))

  private def recurse[T](path: TypePath, key: Type, value: TermName)(fn: => T):
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
      case q"_root_.magnolia.Lazy.apply[$returnType](${Literal(Constant(method: String))})" =>
        q"${TermName(method)}"
      case _ =>
        super.transform(tree)
    }
  }

  private def getImplicit(paramName: Option[String],
                          genericType: Type,
                          typeConstructor: Type,
                          assignedName: TermName,
                          derivationImplicit: GeneralDerivationImplicit): Tree = {

    val searchType = appliedType(typeConstructor, genericType)
    findType(genericType).map { methodName =>
      val methodAsString = methodName.encodedName.toString
      q"_root_.magnolia.Lazy.apply[$searchType]($methodAsString)"
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
      }.toOption.orElse(directInferImplicit(genericType, typeConstructor, derivationImplicit))
    }.getOrElse {
      val currentStack: Stack = recursionStack(c.enclosingPosition)

      val error = ImplicitNotFound(genericType.toString,
          recursionStack(c.enclosingPosition).frames.map(_.path))

      val updatedStack = currentStack.copy(errors = error :: currentStack.errors)
      recursionStack = recursionStack.updated(c.enclosingPosition, updatedStack)
      c.abort(c.enclosingPosition, s"magnolia: could not find typeclass for type $genericType")
    }
  }

  private def directInferImplicit(genericType: Type,
         typeConstructor: Type,
         derivationImplicit: GeneralDerivationImplicit): Option[Tree] = {

    val genericTypeName: String = genericType.typeSymbol.name.encodedName.toString.toLowerCase
    val assignedName: TermName = TermName(c.freshName(s"${genericTypeName}Typeclass"))
    val typeSymbol = genericType.typeSymbol
    val classType = if(typeSymbol.isClass) Some(typeSymbol.asClass) else None
    val isCaseClass = classType.map(_.isCaseClass).getOrElse(false)
    val isSealedTrait = classType.map(_.isSealed).getOrElse(false)
    val isValueClass = genericType <:< typeOf[AnyVal]

    val resultType = appliedType(typeConstructor, genericType)

    val construct = if(isCaseClass) {
      val caseClassParameters = genericType.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }
      val className = genericType.toString

      val implicits = caseClassParameters.map { param =>
        val paramName = param.name.encodedName.toString

        val derivedImplicit = recurse(ProductType(paramName, genericType.toString), genericType,
            assignedName) {

          getImplicit(Some(paramName), param.returnType, typeConstructor, assignedName,
              derivationImplicit)

        }.getOrElse {
          c.abort(c.enclosingPosition, s"failed to get implicit for type $genericType")
        }

        derivationImplicit match {
          case DerivationImplicit(impl) =>
            val dereferencedValue = q"$impl.dereference(sourceParameter, ${param.name.toString})"
            q"$impl.call($derivedImplicit, $dereferencedValue)"
          case Coderivation1Implicit(impl) =>
            val paramName = TermName(param.name.toString)
            val dereferencedValue = q"sourceParameter.$paramName"
            q"$impl.call($derivedImplicit, $dereferencedValue)"
          case Coderivation2Implicit(impl) =>
            val paramName = TermName(param.name.toString)
            val dereferencedValue1 = q"sourceParameter1.$paramName"
            val dereferencedValue2 = q"sourceParameter2.$paramName"
            q"$impl.call($derivedImplicit, $dereferencedValue1, $dereferencedValue2)"
        }
      }

      derivationImplicit match {
        case DerivationImplicit(_) =>
          Some(q"new $genericType(..$implicits)")
        case coderivation: CoderivationImplicit =>
          val namedImplicits = caseClassParameters.zip(implicits).map { case (param, tree) =>
            q"(${param.name.encodedName.toString}, $tree)"
          }
          
          Some(q"""${coderivation.tree}.join(
            $className,
            _root_.scala.collection.immutable.ListMap(..$namedImplicits)
          )""")
      }
    } else if(isSealedTrait) {

      val subtypes = classType.get.knownDirectSubclasses.to[List]

      if(subtypes.isEmpty) {
        c.info(c.enclosingPosition, s"magnolia: could not find any direct subtypes of $typeSymbol", true)
        c.abort(c.enclosingPosition, "")
      }
      
      Some {
        val components = subtypes.map(_.asType.toType).map { searchType =>
          recurse(CoproductType(genericType.toString), genericType, assignedName) {
            getImplicit(None, searchType, typeConstructor, assignedName, derivationImplicit)
          }.getOrElse {
            c.abort(c.enclosingPosition, s"failed to get implicit for type $searchType")
          }
        }
        
        derivationImplicit match {
          case DerivationImplicit(impl) =>
            val reduction = components.reduce { (left, right) => q"$impl.combine($left, $right)" }
            q"$impl.call($reduction, sourceParameter)"

          case Coderivation2Implicit(impl) =>
            val parts = subtypes.tail.zip(components.tail)
            val base = q"""
              $impl.call(
                ${components.head},
                sourceParameter1.asInstanceOf[${subtypes.head}],
                sourceParameter2.asInstanceOf[${subtypes.head}]
              )
            """
            parts.foldLeft(base) { case (aggregated, (componentType, derivedImplicit)) =>
              q"""
                if(sourceParameter1.isInstanceOf[$componentType] &&
                    sourceParameter2.isInstanceOf[$componentType])
                  $impl.call($derivedImplicit, sourceParameter1.asInstanceOf[$componentType],
                      sourceParameter2.asInstanceOf[$componentType])
                else $aggregated"""
            }
          case Coderivation1Implicit(impl) =>
            val parts = subtypes.zip(components)
            
            val caseClauses = parts.map { case (subtype, component) =>
              cq"(value: $subtype) => $impl.call($component, value)"
            }

            q"""(sourceParameter: @_root_.scala.annotation.switch) match {
              case ..$caseClauses
            }"""
        }
      }
    } else None

    construct.map { const =>

      derivationImplicit match {
        case DerivationImplicit(_) =>
          ???
        case Coderivation1Implicit(impl) =>
          q"""{
            def $assignedName: $resultType = $impl.construct { sourceParameter => $const }
            $assignedName
          }"""
        case Coderivation2Implicit(impl) =>
          q"""{
            def $assignedName: $resultType = $impl.construct {
              case (sourceParameter1, sourceParameter2) => $const
            }
            $assignedName
          }"""
      }
    }
  }
  
  def magnolia[T: WeakTypeTag, Typeclass: WeakTypeTag, DerivationType: WeakTypeTag]: Tree = {
    import scala.util.{Try, Success, Failure}

    val genericType: Type = weakTypeOf[T]
    
    val currentStack: Stack =
      recursionStack.get(c.enclosingPosition).getOrElse(Stack(List(), List()))
    
    val directlyReentrant = Some(genericType) == currentStack.frames.headOption.map(_.genericType)
    val typeConstructor: Type = weakTypeOf[Typeclass].typeConstructor
    
    val DerivationTypeclass = weakTypeOf[Derivation[_]].typeConstructor
    val CoderivationTypeclass = weakTypeOf[Coderivation[_]].typeConstructor
    val Coderivation2Typeclass = weakTypeOf[Coderivation2[_]].typeConstructor

    val derivationType = appliedType(DerivationTypeclass, List(typeConstructor))
    val coderivationType = appliedType(CoderivationTypeclass, List(typeConstructor))
    val coderivation2Type = appliedType(Coderivation2Typeclass, List(typeConstructor))

    val derivationImplicit = weakTypeOf[DerivationType].typeConstructor match {
      case DerivationTypeclass =>
        DerivationImplicit(c.prefix.tree)
      case CoderivationTypeclass =>
        Coderivation1Implicit(c.prefix.tree)
      case Coderivation2Typeclass =>
        Coderivation2Implicit(c.prefix.tree)
    }

    if(directlyReentrant) throw DirectlyReentrantException()
   
    currentStack.errors.foreach { error =>
      if(!emittedErrors.contains(error)) {
        emittedErrors += error
        val trace = error.path.mkString("\n    in ", "\n    in ", "\n \n")
        val msg = s"magnolia: could not derive ${typeConstructor} instance for type ${error.genericType}"
        c.info(c.enclosingPosition, msg+trace, true)
      }
    }

    val result: Option[Tree] = if(!currentStack.frames.isEmpty) {
      findType(genericType) match {
        case None =>
          directInferImplicit(genericType, typeConstructor, derivationImplicit)
        case Some(enclosingRef) =>
          val methodAsString = enclosingRef.toString
          val searchType = appliedType(typeConstructor, genericType)
          Some(q"_root_.magnolia.Lazy[$searchType]($methodAsString)")
      }
    } else directInferImplicit(genericType, typeConstructor, derivationImplicit)
   
    if(currentStack.frames.isEmpty) recursionStack = ListMap()

    result.map { tree =>
      if(currentStack.frames.isEmpty) c.untypecheck(removeLazy.transform(tree)) else tree
    }.getOrElse {
      c.abort(c.enclosingPosition, s"magnolia: could not infer typeclass for type $genericType")
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

  case class ChainedImplicit(typeName: String) extends TypePath {
    override def toString = s"chained implicit of type $typeName"
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

  private[magnolia] var recursionStack: ListMap[api.Position, Stack] =
    ListMap()
  
  private[magnolia] var emittedErrors: Set[ImplicitNotFound] = Set()
}

abstract class Derivation[Typeclass[_]] {
  type Value
  def dereference(value: Value, param: String): Value
  def call[T](typeclass: Typeclass[T], value: Value): T
  def construct[T](body: Value => T): Typeclass[T]
  
  def combine[Supertype, Right <: Supertype](left: Typeclass[_ <: Supertype],
      right: Typeclass[Right]): Typeclass[Supertype]

  implicit def generic[T]: Typeclass[T] = macro Macros.magnolia[T, Typeclass[_],
      Derivation[Tc] forSome { type Tc[_] }]
}

abstract class Coderivation[Typeclass[_]] {
  type Return
  def call[T](typeclass: Typeclass[T], value: T): Return
  def construct[T](body: T => Return): Typeclass[T]
  def join(name: String, elements: ListMap[String, Return]): Return
  
  implicit def generic[T]: Typeclass[T] = macro Macros.magnolia[T, Typeclass[_],
      Coderivation[Tc] forSome { type Tc[_] }]
}

abstract class Coderivation2[Typeclass[_]] {
  type Return
  def call[T](typeclass: Typeclass[T], value1: T, value2: T): Return
  def construct[T](body: (T, T) => Return): Typeclass[T]
  def join(name: String, elements: ListMap[String, Return]): Return
  
  implicit def generic[T]: Typeclass[T] = macro Macros.magnolia[T, Typeclass[_],
      Coderivation2[Tc] forSome { type Tc[_] }]
}

