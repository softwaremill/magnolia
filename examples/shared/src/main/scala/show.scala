package magnolia.examples

import magnolia._
import scala.language.experimental.macros

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `Show` than is usual, as it permits the return type to
  *  be something other than a string. */
trait Show[Out, T] { def show(value: T): Out }

case class ShowParam[Out, T, P](label: String, typeclass: Show[Out, P], deref: T => P) {
  type PType = P
  def show: Show[Out, PType] = typeclass
  def dereference(t: T): PType = deref(t)
}

object Dflt {
  implicit def any[T]: Dflt[T] = new Dflt[T] { def default: T = null.asInstanceOf[T] }
}

trait Dflt[T] { def default: T }

trait GenericShow[Out] {

  /** the type constructor for new [[Show]] instances
    *
    *  The first parameter is fixed as `String`, and the second parameter varies generically. */
  type Typeclass[T] = Show[Out, T]
  type ParamType[T, P] = ShowParam[Out, T, P]
  type SubtypeType[T, P] = Subtype[Typeclass, T]

  case class Derivation[T, P](name: String, isValueClass: Boolean, parameters: Seq[P])

  def join(typeName: String, strings: Seq[String]): Out

  def param[T, P](name: String, typeclass: Show[Out, P], dereference: T => P)(
    implicit auto: Dflt[P]
  ) =
    ShowParam[Out, T, P](name, typeclass, dereference)

  def caseClass[T, P](name: String, parameters: Array[P], isValueClass: Boolean): Derivation[T, P] =
    Derivation(name, isValueClass, parameters)

  def subtype[T, S <: T](name: String,
                         typeclass: => Show[Out, S],
                         isType: T => Boolean,
                         asType: T => S): Subtype[Typeclass, T] = {
    def typeclassVal = typeclass
    new Subtype[Typeclass, T] {
      type SType = S
      def label = name
      def typeclass = typeclassVal
      def cast = new PartialFunction[T, S] {
        def isDefinedAt(t: T) = isType(t)
        def apply(t: T): SType = asType(t)
      }
    }
  }

  def sealedTrait[T](name: String,
                     subtypes: Array[Subtype[Typeclass, T]]): SealedTrait[Typeclass, T] =
    new SealedTrait[Typeclass, T](name, subtypes)

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def combine[T](ctx: Derivation[T, ShowParam[Out, T, _]]): Show[Out, T] = new Show[Out, T] {
    def show(value: T) =
      if (ctx.isValueClass) {
        val param = ctx.parameters.head
        param.show.show(param.dereference(value))
      } else {
        val paramStrings = ctx.parameters.map { param =>
          s"${param.label}=${param.show.show(param.dereference(value))}"
        }

        join(ctx.name.split("\\.").last, paramStrings)
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
