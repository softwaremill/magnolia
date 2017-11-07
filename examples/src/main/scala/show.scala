package magnolia.examples

import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.language.experimental.macros

trait Show[Out, T] { def show(value: T): Out }

object Show {
  type Typeclass[T] = Show[String, T]
  def join[T](ctx: JoinContext[Typeclass, T]): Show[String, T] = new Show[String, T] {
    def show(value: T) = ctx.parameters.map { param =>
      s"${param.label}=${param.typeclass.show(param.dereference(value))}"
    }.mkString(s"${ctx.typeName.split("\\.").last}(", ",", ")")
  }

  def dispatch[T](ctx: DispatchContext[Typeclass, T]): Show[String, T] = new Show[String, T] {
    def show(value: T): String = ctx.dispatch(value) { sub => sub.typeclass.show(sub.cast(value)) }
  }

  implicit val string: Show[String, String] = new Show[String, String] { def show(s: String): String = s }
  implicit val int: Show[String, Int] = new Show[String, Int] { def show(s: Int): String = s.toString }
  implicit def generic[T]: Show[String, T] = macro Magnolia.generic[T]
}
