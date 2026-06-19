package magnolia1.examples

import magnolia1._
import magnolia1.parameterised._

/** shows one type as another, often as a string
  *
  * Note that this is a more general form of `Show` than is usual, as it permits the return type to be something other than a string.
  */
trait ParamaterisedShow[Out, T] extends Serializable {
  def show(value: T): Out
  def contramap[S](fn: S => T): ParamaterisedShow[Out, S] = (value: S) => show(fn(value))
}

trait ShowConfig[T] {
  def hiddenFields: Set[String]
}
object ShowConfig {
  given default[T]: ShowConfig[T] = new ShowConfig[T] {
    override def hiddenFields: Set[String] = Set.empty
  }
}

trait ParamaterisedGenericShow[Out] extends ParamaterisedAutoDerivation[[X] =>> ParamaterisedShow[Out, X], ShowConfig] {

  def joinElems(typeName: String, strings: Seq[String]): Out
  def prefix(s: String, out: Out): Out

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of showing each parameter, and prefixing it with
    * the class name
    */
  def join[T: ShowConfig](ctx: CaseClass[Typeclass, T]): ParamaterisedShow[Out, T] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.show(param.deref(value))
    else
      val paramStrings = ctx.params.map { param =>
        val attribStr =
          if (param.annotations.isEmpty && param.inheritedAnnotations.isEmpty)
            ""
          else {
            (param.annotations.map(_.toString) ++ param.inheritedAnnotations.map(a => s"[i]$a")).distinct
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

  /** choose which typeclass to use based on the subtype of the sealed trait and prefix with the annotations as discovered on the subtype.
    */
  override def split[T: ShowConfig](ctx: SealedTrait[Typeclass, T]): ParamaterisedShow[Out, T] =
    (value: T) =>
      ctx.choose(value) { sub =>
        val anns = (sub.annotations ++ sub.inheritedAnnotations).distinct

        val annotationStr =
          if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

        prefix(annotationStr, sub.typeclass.show(sub.value))
      }
}

/** companion object to [[Show]] */
object ParamaterisedShow extends ParamaterisedGenericShow[String]:

  def prefix(s: String, out: String): String = s + out
  def joinElems(typeName: String, params: Seq[String]): String =
    params.mkString(s"$typeName(", ",", ")")

  given ParamaterisedShow[String, String] = identity(_)
  given ParamaterisedShow[String, Int] = _.toString
  given ParamaterisedShow[String, Long] = _.toString + "L"
  given ParamaterisedShow[String, Boolean] = _.toString
  given [A](using A: ParamaterisedShow[String, A]): ParamaterisedShow[String, Seq[A]] =
    _.iterator.map(A.show).mkString("[", ",", "]")

  override def handleAnyVal: [T <: AnyVal, S] => (
      WrapAndSerde[T, ParamaterisedShow.Typeclass, S],
      UnwrapAndSerde[T, ParamaterisedShow.Typeclass, S]
  ) => ParamaterisedShow.Typeclass[S] => ParamaterisedShow.Typeclass[T] = [T <: AnyVal, S] =>
    (_: WrapAndSerde[T, ParamaterisedShow.Typeclass, S], u: UnwrapAndSerde[T, ParamaterisedShow.Typeclass, S]) =>
      (_: ParamaterisedShow.Typeclass[S]).contramap(u.fn)
