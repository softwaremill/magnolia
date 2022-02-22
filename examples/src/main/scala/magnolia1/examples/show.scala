package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait, TypeName}

import scala.language.experimental.macros

/** shows one type as another, often as a string
  *
  * Note that this is a more general form of `Show` than is usual, as it permits the return type to be something other than a string.
  */
trait Show[Out, T] { def show(value: T): Out }

trait GenericShow[Out] {

  /** the type constructor for new [[Show]] instances
    *
    * The first parameter is fixed as `String`, and the second parameter varies generically.
    */
  type Typeclass[T] = Show[Out, T]

  def joinElems(typeName: String, strings: Seq[String]): Out
  def prefix(s: String, out: Out): Out

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of showing each parameter, and prefixing it with
    * the class name
    */
  def join[T](ctx: CaseClass[Typeclass, T]): Show[Out, T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.show(param.dereference(value))
    } else {
      val paramStrings = ctx.parameters.map { param =>
        val attribStr =
          if (param.annotations.isEmpty && param.inheritedAnnotations.isEmpty) ""
          else {
            (param.annotations ++ param.inheritedAnnotations).distinct
              .filterNot(_.isInstanceOf[scala.annotation.implicitNotFound])
              .mkString("{", ",", "}")
          }
        val tpeAttribStr =
          if (param.typeAnnotations.isEmpty) ""
          else {
            param.typeAnnotations.mkString("{", ",", "}")
          }
        s"${param.label}$attribStr$tpeAttribStr=${param.typeclass.show(param.dereference(value))}"
      }

      val anns = (ctx.annotations ++ ctx.inheritedAnnotations).distinct
        .filterNot(_.isInstanceOf[scala.SerialVersionUID])
        .filterNot(_.isInstanceOf[scala.annotation.implicitNotFound])
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      val tpeAnns = ctx.typeAnnotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val typeAnnotationStr = if (tpeAnns.isEmpty) "" else tpeAnns.mkString("{", ",", "}")

      def typeArgsString(typeName: TypeName): String =
        if (typeName.typeArguments.isEmpty) ""
        else typeName.typeArguments.map(arg => s"${arg.short}${typeArgsString(arg)}").mkString("[", ",", "]")

      joinElems(ctx.typeName.short + typeArgsString(ctx.typeName) + annotationStr + typeAnnotationStr, paramStrings)
    }
  }

  /** choose which typeclass to use based on the subtype of the sealed trait and prefix with the annotations as discovered on the subtype.
    */
  def split[T](ctx: SealedTrait[Typeclass, T]): Show[Out, T] = (value: T) =>
    ctx.split(value) { sub =>
      val anns = (sub.annotations ++ sub.inheritedAnnotations).distinct
        .filterNot(_.isInstanceOf[scala.SerialVersionUID])
        .filterNot(_.isInstanceOf[scala.annotation.implicitNotFound])
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      prefix(annotationStr, sub.typeclass.show(sub.cast(value)))
    }

  /** bind the Magnolia macro to this derivation object */
  implicit def gen[T]: Show[Out, T] = macro Magnolia.gen[T]
}

/** companion object to [[Show]] */
object Show extends GenericShow[String] {

  def prefix(s: String, out: String): String = s + out
  def joinElems(typeName: String, params: Seq[String]): String =
    params.mkString(s"$typeName(", ",", ")")

  implicit val string: Show[String, String] = identity
  implicit val int: Show[String, Int] = _.toString
  implicit val long: Show[String, Long] = _.toString + "L"
  implicit val boolean: Show[String, Boolean] = _.toString

  /** show typeclass for sequences */
  implicit def seq[A](implicit A: Show[String, A]): Show[String, Seq[A]] =
    _.iterator.map(A.show).mkString("[", ",", "]")
}
