package magnolia.examples

import magnolia._

import scala.language.experimental.macros

/** typeclass for testing the equality of two values of the same type */
trait Eq[T] { def equal(value: T, value2: T): Boolean }

/** companion object to [[Eq]] */
object Eq {

  /** type constructor for the equality typeclass */
  type Typeclass[T] = Eq[T]

  /** defines equality for this case class in terms of equality for all its parameters */
  def combine[T](ctx: CaseClass[Eq, T]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T) = ctx.parameters.forall { param =>
      param.typeclass.equal(param.dereference(value1), param.dereference(value2))
    }
  }

  /** choose which equality subtype to defer to
    *
    *  Note that in addition to dispatching based on the type of the first parameter to the `equal`
    *  method, we check that the second parameter is the same type. */
  def dispatch[T](ctx: SealedTrait[Eq, T]): Eq[T] = new Eq[T] {
    def equal(value1: T, value2: T): Boolean = ctx.dispatch(value1) {
      case sub =>
        sub.cast.isDefinedAt(value2) && sub.typeclass.equal(sub.cast(value1), sub.cast(value2))
    }
  }

  /** equality typeclass instance for strings */
  implicit val string: Eq[String] = new Eq[String] { def equal(v1: String, v2: String) = v1 == v2 }

  /** equality typeclass instance for integers */
  implicit val int: Eq[Int] = new Eq[Int] { def equal(v1: Int, v2: Int) = v1 == v2 }

  implicit def eqOption[T](implicit T: Eq[T]): Eq[Option[T]] = {
    case (Some(v1), Some(v2)) => T.equal(v1, v2)
    case (None, None) => true
    case _ => false
  }

  implicit def eqIterable[T, C[x] <: Iterable[x]](implicit T: Eq[T]): Eq[C[T]] = { (v1, v2) =>
    v1.size == v2.size && (v1.iterator zip v2.iterator).forall((T.equal _).tupled)
  }

  /** binds the Magnolia macro to the `gen` method */
  implicit def gen[T]: Eq[T] = macro Magnolia.gen[T]
}
