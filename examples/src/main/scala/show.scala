package magnolia.examples

import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.language.experimental.macros

/** shows one type as another, often as a string
 *
 *  Note that this is a more general form of `Show` than is usual, as it permits the return type to
 *  be something other than a string. */
trait Show[Out, T] { def show(value: T): Out }

/** companion object to [[Show]] */
object Show {

  /** the type constructor for new [[Show]] instances
   *
   *  The first parameter is fixed as `String`, and the second parameter varies generically. */
  type Typeclass[T] = Show[String, T]

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of
   *  showing each parameter, and prefixing it with the class name */
  def combine[T](ctx: CaseClass[Typeclass, T]): Show[String, T] = new Show[String, T] {
    def show(value: T) = ctx.parameters.map { param =>
      s"${param.label}=${param.typeclass.show(param.dereference(value))}"
    }.mkString(s"${ctx.typeName.split("\\.").last}(", ",", ")")
  }

  /** choose which typeclass to use based on the subtype of the sealed trait */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Show[String, T] = new Show[String, T] {
    def show(value: T): String = ctx.dispatch(value) { sub => sub.typeclass.show(sub.cast(value)) }
  }

  /** show typeclass for strings */
  implicit val string: Show[String, String] = new Show[String, String] { def show(s: String): String = s }
  
  /** show typeclass for integers */
  implicit val int: Show[String, Int] = new Show[String, Int] { def show(s: Int): String = s.toString }
  
  /** bind the Magnolia macro to this derivation object */
  implicit def gen[T]: Show[String, T] = macro Magnolia.gen[T]
}
