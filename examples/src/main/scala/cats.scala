package magnolia.examples

import scala.collection.immutable.ListMap
import scala.language.experimental.macros

import cats.Show
import magnolia.{Coderivation, Macros}

object catsShowDerivation {

  val ShowDerivation = new Coderivation[Show] {
      type Return = String
      def call[T](show: Show[T], value: T): String = show.show(value)
      def construct[T](body: T => String): Show[T] = body(_)
      def join(name: String, xs: ListMap[String, String]): String =
        xs.map { case (k, v) => s"$k=$v" }.mkString(s"$name(", ", ", ")")
    }
  
}
