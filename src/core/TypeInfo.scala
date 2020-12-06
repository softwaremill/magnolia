package magnolia

import scala.quoted._

sealed trait TypeInfo {
  def full: String
}

case class BaseTypeInfo(owner: String, name: String, typeParams: List[TypeInfo]) extends TypeInfo {
  override def full: String = "" // TODO
}
case class OrTypeInfo(subtypes: List[TypeInfo]) extends TypeInfo {
  override def full: String = "" // TODO
}
case class AndTypeInfo(subtypes: List[TypeInfo]) extends TypeInfo {
  override def full: String = "" // TODO
}

object TypeInfo {
  inline def apply[T]: TypeInfo = ${ typeInfoImpl[T] }

  def typeInfoImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[TypeInfo] = {
    import qctx.reflect._

    def name(tpe: TypeRepr) : Expr[String] =
      Expr(tpe.typeSymbol.name)

    def owner(tpe: TypeRepr): Expr[String] =
      if (tpe.typeSymbol.maybeOwner.isNoSymbol) {
        println("Debug: No owner - " + tpe) // TODO - remove
        Expr("<no owner>") // TODO: can this happen any more? are all cases catched? how to deal with unhandled cases?
      } else if (tpe.typeSymbol.owner == defn.EmptyPackageClass) Expr("")
      else Expr(tpe.typeSymbol.owner.fullName)

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] =
      tpe match {
        case OrType(subtypes) =>
        '{OrTypeInfo(${Expr.ofList(subtypes.toList.map(typeInfo))})}
        case AndType(subtypes) =>
        '{AndTypeInfo(${Expr.ofList(subtypes.toList.map(typeInfo))})}
        case AppliedType(tpe, args) =>
        '{BaseTypeInfo(${owner(tpe)}, ${name(tpe)}, ${Expr.ofList(args.map(typeInfo))})}
        case _ =>
        '{BaseTypeInfo(${owner(tpe)}, ${name(tpe)}, List.empty)}
      }

    typeInfo(TypeRepr.of[T])
  }
}
