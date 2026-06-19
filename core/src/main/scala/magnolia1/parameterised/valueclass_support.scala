package magnolia1.parameterised

object ValueClassSupport {

  import scala.quoted.*

  def deriveAnyValSupportImpl[A <: AnyVal, Typeclass[_]: Type, DerivedClass <: ParamaterisedDerivation[Typeclass, ?]](
      fn: Expr[[T <: AnyVal, S] => (WrapAndSerde[T, Typeclass, S], UnwrapAndSerde[T, Typeclass, S]) => Typeclass[S] => Typeclass[T]],
      self: Expr[DerivedClass]
  )(using quotes: Quotes, tpe: Type[A]): Expr[Typeclass[A]] = {
    import quotes.*, quotes.reflect.*
    val wrapperSym = TypeRepr.of[A].typeSymbol
    val constructor = wrapperSym.primaryConstructor
    val arg = constructor.paramSymss.head.head
    val argName = arg.name
    val theType = arg.tree match {
      case ValDef(_, tt: TypeTree, _) => tt
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "expecting AnyVal with Product to have a single constructor arg"
        )
    }
    theType.tpe.asType match {
      case '[t] =>
        val encoder =
          if (theType.symbol.declaredFields.nonEmpty)
            Implicits.search(TypeRepr.of[scala.deriving.Mirror.Of[t]]) match {
              case mirror: ImplicitSearchSuccess =>
                Apply(
                  TypeApply(
                    Select.unique(self.asTerm, "mirrorDerived"),
                    List(theType)
                  ),
                  List(mirror.tree)
                )
              case _ =>
                report.errorAndAbort(s"Unable to find constructor for type ${theType.show}")
            }
          else
            TypeApply(
              Select.unique(self.asTerm, "noMirrorDerived"),
              List(theType)
            )
        val applyMtpe = MethodType(List("v"))(_ => List(TypeRepr.of[t]), _ => TypeRepr.of[A])

        def doApply = Lambda(
          Symbol.noSymbol,
          applyMtpe,
          {
            case (_, List(arg)) =>
              Apply(
                Select(New(TypeIdent(wrapperSym)), constructor),
                List(Ref(arg.symbol))
              )
            case _ =>
              quotes.reflect.report.errorAndAbort(
                "expecting AnyVal constructor to be called with a single arg"
              )
          }
        ).asExprOf[t => A]

        val unapplyMtpe = MethodType(List("v"))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[t])
        def doUnapply = Lambda(
          Symbol.noSymbol,
          unapplyMtpe,
          {
            case (_, List(arg)) =>
              Select(Ref(arg.symbol), wrapperSym.fieldMember(argName))
            case _ =>
              quotes.reflect.report.errorAndAbort(
                "expecting AnyVal constructor to be called with a single arg"
              )
          }
        ).asExprOf[A => t]

        '{
          FullDerivedClassSupp[A, Typeclass, t](
            WrapAndSerde[A, Typeclass, t](${ doApply })(using ${ encoder.asExprOf[Typeclass[t]] }),
            UnwrapAndSerde[A, Typeclass, t](${ doUnapply })(using ${ encoder.asExprOf[Typeclass[t]] }),
            ${ fn }[A, t]
          ).tc(using ${ encoder.asExprOf[Typeclass[t]] })
        }.asExprOf[Typeclass[A]]
    }
  }
}

case class FullDerivedClassSupp[S <: AnyVal, Typeclass[_], T](
    wrapAndSerde: WrapAndSerde[S, Typeclass, T],
    unwrapAndSerde: UnwrapAndSerde[S, Typeclass, T],
    fn: (WrapAndSerde[S, Typeclass, T], UnwrapAndSerde[S, Typeclass, T]) => Typeclass[T] => Typeclass[S]
) {
  def tc(using t: Typeclass[T]): Typeclass[S] = fn(wrapAndSerde, unwrapAndSerde)(t)
}

case class WrapAndSerde[S <: AnyVal, Typeclass[_], T: Typeclass](fn: T => S)

case class UnwrapAndSerde[S <: AnyVal, Typeclass[_], T: Typeclass](fn: S => T)
