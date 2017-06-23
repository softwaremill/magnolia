package magnolia.examples

import scala.collection.immutable.ListMap
import scala.language.experimental.macros

import cats.Show
import magnolia.ContravariantDerivation
import magnolia.Macros

object catsShowDerivation {

  implicit val showDerivation: ContravariantDerivation[Show] =
    new ContravariantDerivation[Show] {
      type Return = String
      def call[T](show: Show[T], value: T): String = show.show(value)
      def construct[T](body: T => String): Show[T] = body(_)
      def join(xs: ListMap[String, String]): String =
        xs.map { case (k, v) => s"$k=$v" }.mkString("{", ", ", "}")
    }
  
  implicit def genericShow[T]: Show[T] = macro Macros.magnolia[T, Show[_]]
}
