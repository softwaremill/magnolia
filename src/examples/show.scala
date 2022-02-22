package magnolia1.examples

import magnolia1._

/** shows one type as another, often as a string
  *
  * Note that this is a more general form of `Show` than is usual, as it permits
  * the return type to be something other than a string.
  */
trait Show[Out, T] { def show(value: T): Out }

trait GenericShow[Out] extends AutoDerivation[[X] =>> Show[Out, X]] {

  def joinElems(typeName: String, strings: Seq[String]): Out
  def prefix(s: String, out: Out): Out

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`)
    * the result of showing each parameter, and prefixing it with the class name
    */
  def join[T](ctx: CaseClass[Typeclass, T]): Show[Out, T] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.show(param.deref(value))
    else
      val paramStrings = ctx.params.map { param =>
        val attribStr =
          if (param.annotations.isEmpty && param.inheritedAnnotations.isEmpty)
            ""
          else {
            (param.annotations ++ param.inheritedAnnotations).distinct
              .mkString("{", ",", "}")
          }

        val tpeAttribStr =
          if (param.typeAnnotations.isEmpty) ""
          else {
            param.typeAnnotations.mkString("{", ",", "}")
          }

        s"${param.label}$attribStr$tpeAttribStr=${param.typeclass.show(param.deref(value))}"
      }

      val anns = (ctx.annotations ++ ctx.inheritedAnnotations).distinct
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      val tpeAnns = ctx.typeAnnotations
      val typeAnnotationStr =
        if (tpeAnns.isEmpty) "" else tpeAnns.mkString("{", ",", "}")

      def typeArgsString(typeInfo: TypeInfo): String =
        if typeInfo.typeParams.isEmpty then ""
        else
          typeInfo.typeParams
            .map(arg => s"${arg.short}${typeArgsString(arg)}")
            .mkString("[", ",", "]")

      joinElems(
        ctx.typeInfo.short + typeArgsString(
          ctx.typeInfo
        ) + annotationStr + typeAnnotationStr,
        paramStrings
      )
  }

  /** choose which typeclass to use based on the subtype of the sealed trait and
    * prefix with the annotations as discovered on the subtype.
    */
  override def split[T](ctx: SealedTrait[Typeclass, T]): Show[Out, T] =
    (value: T) =>
      ctx.choose(value) { sub =>
        val anns = (sub.annotations ++ sub.inheritedAnnotations).distinct

        val annotationStr =
          if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

        prefix(annotationStr, sub.typeclass.show(sub.value))
      }
}

/** companion object to [[Show]] */
object Show extends GenericShow[String]:

  def prefix(s: String, out: String): String = s + out
  def joinElems(typeName: String, params: Seq[String]): String =
    params.mkString(s"$typeName(", ",", ")")

  given Show[String, String] = identity(_)
  given Show[String, Int] = _.toString
  given Show[String, Long] = _.toString + "L"
  given Show[String, Boolean] = _.toString
  given [A](using A: Show[String, A]): Show[String, Seq[A]] =
    _.iterator.map(A.show).mkString("[", ",", "]")
