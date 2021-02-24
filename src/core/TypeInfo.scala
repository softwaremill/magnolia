package magnolia

import scala.quoted._

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]) {
  def full: String = s"$owner.$short"
}

object TypeInfo {
  inline def apply[T]: TypeInfo = ${ typeInfoImpl[T] }

  def typeInfoImpl[T](using qctx: Quotes, tpe: Type[T]): Expr[TypeInfo] = {
    import qctx.reflect._

    def name(tpe: TypeRepr) : Expr[String] =
      Expr(tpe.typeSymbol.name)

    def owner(tpe: TypeRepr): Expr[String] =
      if (tpe.typeSymbol.maybeOwner.isNoSymbol) {
        Expr("<no owner>")
      } else if (tpe.typeSymbol.owner == defn.EmptyPackageClass) Expr("")
      else Expr(tpe.typeSymbol.owner.name)

    def typeInfo(tpe: TypeRepr): Expr[TypeInfo] =
      tpe match {
        case AppliedType(tpe, args) =>
        '{TypeInfo(${owner(tpe)}, ${name(tpe)}, ${Expr.ofList(args.map(typeInfo))})}
        case _ =>
        '{TypeInfo(${owner(tpe)}, ${name(tpe)}, List.empty)}
      }

    typeInfo(TypeRepr.of[T])
  }
}
