/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package magnolia

import scala.quoted.*

object Macro:
  inline def isObject[T]: Boolean = ${isObject[T]}

  def isObject[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] =
    import qctx.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
  
  inline def paramAnns[T]: List[(String, List[Any])] = ${paramAnns[T]}

  def paramAnns[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, List[Any])]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList(
      tpe
      .typeSymbol
      .primaryConstructor
      .paramSymss
      .flatten
      .map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        }
        .map(_.asExpr.asInstanceOf[Expr[Any]])
      }
      .filter(_._2.nonEmpty)
      .map{ case (name, annots) => Expr.ofTuple(name, Expr.ofList(annots)) }
    )
  inline def anns[T]: List[Any] = ${anns[T]}

  def anns[T](using qctx: Quotes, tpe: Type[T]): Expr[List[Any]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[T]
    
    Expr.ofList(tpe.typeSymbol.annotations.filter { a =>
      a.tpe.typeSymbol.maybeOwner.isNoSymbol || a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
    }.map(_.asExpr.asInstanceOf[Expr[Any]]))
  
  inline def isValueClass[T]: Boolean = ${isValueClass[T]}

  def isValueClass[T](using qctx: Quotes, tpe: Type[T]): Expr[Boolean] =
    import qctx.reflect.*

    val anyVal: Symbol = Symbol.classSymbol("scala.AnyVal")
    val baseClasses = TypeRepr.of[T].baseClasses
    
    Expr(baseClasses.contains(anyVal))
  
  inline def defaultValue[T]: List[(String, Option[Any])] = ${defaultValue[T]}

  def defaultValue[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, Option[Any])]] =
    import qctx.reflect._

    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]

    Expr.ofList(tpe.typeSymbol.caseFields.map { case ValDef(name, _, rhs) => Expr(name -> None/*TODO rhs*/) })
  
  inline def paramTypeAnns[T]: List[(String, List[Any])] = ${paramTypeAnns[T]}

  def paramTypeAnns[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, List[Any])]] =
    import qctx.reflect._

    val tpe = TypeRepr.of[T]

    def getAnnotations(t: TypeRepr): List[Term] = t match
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _                         => Nil

    Expr.ofList(
      tpe.typeSymbol.caseFields.map { field =>
        val tpeRepr = field.tree match
          case v: ValDef => v.tpt.tpe
          case d: DefDef => d.returnTpt.tpe
        
        Expr(field.name) -> getAnnotations(tpeRepr).filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
              a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }.map(_.asExpr.asInstanceOf[Expr[Any]])
      }.filter(_._2.nonEmpty).map { (name, annots) => Expr.ofTuple(name, Expr.ofList(annots)) }
    )
  
  inline def repeated[T]: List[(String, Boolean)] = ${repeated[T]}

  def repeated[T](using qctx: Quotes, tpe: Type[T]): Expr[List[(String, Boolean)]] =
    import qctx.reflect.*
    
    def isRepeated[T](tpeRepr: TypeRepr): Boolean = tpeRepr match
      case a: AnnotatedType =>
        a.annotation.tpe match
          case tr: TypeRef => tr.name == "Repeated"
          case _           => false
      case _ => false

    val tr = TypeRepr.of[T]
    val symbol = tr.typeSymbol
    val constr = symbol.primaryConstructor.tree.asInstanceOf[DefDef]
    
    val areRepeated = constr.paramss.flatMap { clause =>
      clause.params.flatMap {
        case ValDef(name, tpeTree, _) =>
          Some(name -> isRepeated(tpeTree.tpe))
        case _ =>
          None
      }
    }
    
    Expr(areRepeated)

  inline def typeInfo[T]: TypeInfo = ${typeInfo[T]}

  def typeInfo[T](using qctx: Quotes, tpe: Type[T]): Expr[TypeInfo] =
    import qctx.reflect._

    def normalizedName(s: Symbol): String = if s.flags.is(Flags.Module) then s.name.stripSuffix("$") else s.name
    def name(tpe: TypeRepr) : Expr[String] = Expr(normalizedName(tpe.typeSymbol))

    def owner(tpe: TypeRepr): Expr[String] =
      if tpe.typeSymbol.maybeOwner.isNoSymbol then Expr("<no owner>")
      else if (tpe.typeSymbol.owner == defn.EmptyPackageClass) Expr("")
      else Expr(tpe.typeSymbol.owner.name)

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] =
      tpe match
        case AppliedType(tpe, args) =>
          '{TypeInfo(${owner(tpe)}, ${name(tpe)}, ${Expr.ofList(args.map(typeInfo))})}
        case _ =>
          '{TypeInfo(${owner(tpe)}, ${name(tpe)}, Nil)}

    typeInfo(TypeRepr.of[T])