package magnolia.examples

import magnolia._
import scala.language.experimental.macros

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `Show` than is usual, as it permits the return type to
  *  be something other than a string. */
trait Show[Out, T] { def show(value: T): Out }

trait GenericShow[Out] {

  /** the type constructor for new [[Show]] instances
    *
    *  The first parameter is fixed as `String`, and the second parameter varies generically. */
  type Typeclass[T] = Show[Out, T]

  def join(typeName: String, strings: Seq[String]): Out

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def combine[T](ctx: CaseClass[Typeclass, T]): Show[Out, T] = new Show[Out, T] {
    def show(value: T) =
      if (ctx.isValueClass) {
        val param = ctx.parameters.head
        param.typeclass.show(param.dereference(value))
      } else {
        val paramStrings = ctx.parameters.map { param =>
          s"${param.label}=${param.typeclass.show(param.dereference(value))}"
        }

        join(ctx.typeName.short, paramStrings)
      }
  }

  /** choose which typeclass to use based on the subtype of the sealed trait */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Show[Out, T] = new Show[Out, T] {
    def show(value: T): Out = ctx.dispatch(value) { sub =>
      sub.typeclass.show(sub.cast(value))
    }
  }

  /** bind the Magnolia macro to this derivation object */
  implicit def gen[T]: Show[Out, T] = macro Magnolia.gen[T]
}

/** companion object to [[Show]] */
object Show extends GenericShow[String] {

  /** show typeclass for strings */
  implicit val string: Show[String, String] = new Show[String, String] {
    def show(s: String): String = s
  }

  def join(typeName: String, params: Seq[String]): String =
    params.mkString(s"$typeName(", ",", ")")

  /** show typeclass for integers */
  implicit val int: Show[String, Int] = new Show[String, Int] {
    def show(s: Int): String = s.toString
  }

  /** show typeclass for sequences */
  implicit def seq[A](implicit A: Show[String, A]): Show[String, Seq[A]] =
    new Show[String, Seq[A]] {
      def show(as: Seq[A]): String = as.iterator.map(A.show).mkString("[", ",", "]")
    }
}
